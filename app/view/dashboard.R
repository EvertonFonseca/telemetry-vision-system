box::use(
  shiny[...],
  shinyjs,
  plotly[...],
  jsonlite,
  dplyr[filter, arrange, mutate, select, across, everything, distinct, relocate, rename, bind_rows],
  tidyr[unnest_wider],
  lubridate,
  DBI,
  dbp  = ../infra/db_pool,
  ./global[ actionWebUser]
)


# ===============================
# Utilidades
# ===============================
# Flatten recursivo para ATRIBUTOS (JSON). Produz nomes do tipo "PRENSA.ESTADO".
.flatten_once <- function(x, prefix = NULL) {
  out <- list()
  if (is.list(x)) {
    nm <- names(x)
    if (is.null(nm)) nm <- seq_along(x)
    for (i in seq_along(x)) {
      key <- as.character(nm[i])
      pfx <- if (length(prefix)) paste0(prefix, ".", key) else key
      val <- x[[i]]
      if (is.list(val)) {
        out <- c(out, .flatten_once(val, pfx))
      } else {
        out[[pfx]] <- val
      }
    }
  } else {
    out[[prefix %||% "value"]] <- x
  }
  out
}

safe_parse_atributos <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(tibble::tibble())
  obj <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) return(tibble::tibble())
  flat <- .flatten_once(obj)
  tibble::as_tibble(flat)
}

# Constrói SQL base; aplica filtros opcionais.
build_sql <- function(minutos, setor = NULL, objeto = NULL, limite = 2000L) {
  # Usa COALESCE para suportar esquemas com oc.ATRIBUTOS OU oc.DATA_OC
  base <- paste(
    "SELECT",
    "  COALESCE(oc.ATRIBUTOS, oc.DATA_OC) AS ATRIBUTOS,",
    "  oc.DT_HR_LOCAL                      AS DATE_TIME,",
    "  o.NAME_OBJETO                       AS OBJETO,",
    "  s.NAME_SETOR                        AS SETOR",
    "FROM objeto_contexto oc",
    "LEFT JOIN objeto o ON o.CD_ID_OBJETO = oc.CD_ID_OBJETO",
    "LEFT JOIN setor  s ON s.CD_ID_SETOR  = oc.CD_ID_SETOR",
    "WHERE oc.DT_HR_LOCAL >= NOW() - INTERVAL ? MINUTE",
    sep = "
"
  )
  cond <- character()
  if (!is.null(setor) && nzchar(setor)) cond <- c(cond, "s.NAME_SETOR = ?")
  if (!is.null(objeto) && nzchar(objeto)) cond <- c(cond, "o.NAME_OBJETO = ?")
  if (length(cond)) base <- paste(base, "AND", paste(cond, collapse = " AND "))
  base <- paste(base, "ORDER BY oc.DT_HR_LOCAL DESC")
  if (!is.null(limite) && limite > 0) base <- paste(base, sprintf("LIMIT %d", as.integer(limite)))
  base
}

# Executa consulta de forma segura
run_query <- function(pool, minutos, setor = NULL, objeto = NULL, limite = 2000L) {
  sql <- build_sql(minutos, setor, objeto, limite)
  # Bind params na mesma ordem: minutos, (setor?), (objeto?)
  params <- list(as.integer(minutos))
  if (!is.null(setor) && nzchar(setor))  params <- c(params, setor)
  if (!is.null(objeto) && nzchar(objeto)) params <- c(params, objeto)
  DBI::dbGetQuery(pool, sql, params = params)
}

# Transforma tabela "longa" expandindo ATRIBUTOS -> colunas dinâmicas
expand_atributos <- function(df_raw) {
  if (!nrow(df_raw)) return(df_raw)
  # Parse linha a linha
  rows <- lapply(seq_len(nrow(df_raw)), function(i) {
    base   <- df_raw[i, c("DATE_TIME","OBJETO","SETOR"), drop = FALSE]
    extra  <- safe_parse_atributos(df_raw$ATRIBUTOS[i])
    if (!ncol(extra)) extra <- tibble::tibble()
    dplyr::bind_cols(base, extra)
  })
  out <- dplyr::bind_rows(rows)
  # Ordena por tempo asc p/ gráficos
  out <- out |>
    dplyr::mutate(DATE_TIME = lubridate::as_datetime(DATE_TIME)) |>
    dplyr::arrange(DATE_TIME)
  out
}

# Extrai lista de métricas numéricas e categóricas com base nas colunas após expandir
split_metrics <- function(df) {
  base_cols <- c("DATE_TIME","OBJETO","SETOR")
  attr_cols <- setdiff(names(df), base_cols)
  # Detectar numéricas vs texto
  num_cols <- attr_cols[vapply(df[attr_cols], function(v) is.numeric(v) || is.integer(v), logical(1))]
  chr_cols <- attr_cols[vapply(df[attr_cols], function(v) is.character(v) || is.factor(v), logical(1))]
  list(numeric = num_cols, categorical = chr_cols)
}

# ===============================
# UI
# ===============================
#' @export
ui <- function(ns) {
  # Paleta do tema (combina com Telemetry Vision)
  THEME <- list(
    bg      = "#EEF3F8",
    card_bg = "#FFFFFF",
    border  = "#E6EDF3",
    text    = "#243746",
    subtext = "#5F6B76",
    primary = "#2F7EA2",
    primary_hover = "#2A6E8F"
  )
  shiny::tagList(
    shinyjs::useShinyjs(),
    # CSS extra para cards de KPI coloridos e lista de atividades
    tags$head(tags$style(HTML('
      .tv-kpi { border:0; border-radius:12px; padding:18px; color:#fff; }
      .tv-kpi.kpi-orange { background: linear-gradient(135deg, #F2994A, #F2C94C); }
      .tv-kpi.kpi-green  { background: linear-gradient(135deg, #27AE60, #6FCF97); }
      .tv-kpi.kpi-red    { background: linear-gradient(135deg, #EB5757, #F2994A); }
      .tv-kpi.kpi-teal   { background: linear-gradient(135deg, #2F7EA2, #56CCF2); }
      .tv-kpi .k-title { font-size:12px; opacity:.9; }
      .tv-kpi .k-value { font-size:26px; font-weight:700; }
      .ua-item { display:flex; gap:10px; align-items:center; padding:10px 0; border-bottom:1px dashed #E6EDF3; }
      .ua-item:last-child { border-bottom:0; }
      .ua-avatar { width:36px; height:36px; border-radius:50%; background:#E0EDF5; display:flex; align-items:center; justify-content:center; font-weight:700; color:#2F7EA2; }
      .ua-meta { font-size:12px; color:#5F6B76; }
    '))),
    # CSS do tema
    tags$head(tags$style(HTML(sprintf('
      body { background:%s; }
      .tv-card { background:%s; border:1px solid %s; border-radius:10px; box-shadow:0 6px 12px rgba(0,0,0,.03); padding:14px; }
      .tv-card h5 { margin:0 0 8px 0; color:%s; font-weight:600; }
      .tv-kpi { border:1px solid %s; border-radius:10px; background:%s; padding:16px; text-align:center; }
      .tv-kpi .k-title { font-size:12px; color:%s; }
      .tv-kpi .k-value { font-size:26px; font-weight:700; color:%s; }
      .btn-primary { background:%s; border-color:%s; }
      .btn-primary:hover, .btn-primary:focus { background:%s; border-color:%s; }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current { background:%s !important; color:#fff !important; border:1px solid %s !important; }
      .form-select, .form-control { border-radius:8px; }
    ', THEME$bg, THEME$card_bg, THEME$border, THEME$text, THEME$border, THEME$card_bg, THEME$subtext, THEME$text, THEME$primary, THEME$primary, THEME$primary_hover, THEME$primary_hover, THEME$primary, THEME$primary))),
    div(class = "p-4",
      div(class = "tv-card mb-3",
        div(class = "d-flex flex-wrap gap-2 align-items-end",
          div(style = "min-width:220px", selectInput(ns("setor"), "Setor (opcional)", choices = c(""), selected = "")),
          div(style = "min-width:220px", selectInput(ns("objeto"), "Objeto (opcional)", choices = c(""), selected = "")),
          div(style = "min-width:200px", numericInput(ns("janela"), "Janela (min)", value = 60, min = 1, step = 1)),
          div(style = "min-width:220px", numericInput(ns("auto_refresh"), "Auto-refresh (seg)", value = 30, min = 5, step = 5)),
          actionButton(ns("btn_refresh"), "Atualizar agora", class = "btn btn-primary")
        )
      ),
      div(class = "tv-card mb-3",
        div(class = "row g-3",
          div(class = "col-12 col-md-3", uiOutput(ns("kpi_total_linhas"))),
          div(class = "col-12 col-md-3", uiOutput(ns("kpi_objetos"))),
          div(class = "col-12 col-md-3", uiOutput(ns("kpi_setores"))),
          div(class = "col-12 col-md-3", uiOutput(ns("kpi_metricas")))
        )
      ),
      div(class = "tv-card mb-3",
        h5("Sales Analytics"),
        div(class = "row g-3 align-items-end",
          div(class = "col-md-4", selectInput(ns("metric_select"), "Métrica numérica (Y)", choices = c(), selected = NULL)),
          div(class = "col-md-4", selectInput(ns("group_by"), "Agrupar por", choices = c("OBJETO", "SETOR"), selected = "OBJETO")),
          div(class = "col-md-4", sliderInput(ns("downsample"), "Amostragem p/ gráfico", min = 1, max = 10, value = 1, step = 1))
        ),
        plotlyOutput(ns("ts_plot"), height = "320px")
      ),
      div(class = "tv-card mb-3",
        h5("Project Risk"),
        plotlyOutput(ns("risk_gauge"), height = "220px"),
        div(class = "d-grid", actionButton(ns("btn_download_report"), "Download Overall Report", class = "btn btn-primary"))
      ),
      div(class = "tv-card mb-3",
        h5("Application Metrics"),
        dataTableOutput(ns("tbl_apps"))
      ),
      div(class = "tv-card",
        h5("User Activity"),
        uiOutput(ns("user_activity"))
      )
    )
  ))
  
}

# Componente simples de KPI
kpi_card <- function(title, value, variant = c("orange","green","red","teal")) {
  variant <- match.arg(variant)
  cls <- switch(variant,
    orange = "kpi-orange",
    green  = "kpi-green",
    red    = "kpi-red",
    teal   = "kpi-teal"
  )
  div(class = paste("tv-kpi", cls),
      div(class = "k-title", title),
      div(class = "k-value", value)
  )
}

# ===============================
# Server
# ===============================
#' @export
server <- function(ns, input, output, session) {
  pool <- dbp$get_pool()

  # Atualização automática
  timer <- reactiveTimer(1000)
  observe({
    timer()
    ar <- as.integer(input$auto_refresh %||% 0)
    if (!is.na(ar) && ar > 0) invalidateLater(ar * 1000, session)
  })

  # Dispara consultas
  trigger <- reactiveVal(Sys.time())
  observeEvent(input$btn_refresh, { trigger(Sys.time()) })
  observeEvent(input$auto_refresh, { trigger(Sys.time()) })

  # Consulta base (raw)
  df_raw <- reactive({
    req(pool)
    # mantém janela mínima válida
    janela <- as.integer(input$janela %||% 60)
    if (is.na(janela) || janela <= 0) janela <- 60L

    # Re-dispara periodicamente
    trigger();

    out <- tryCatch({
      run_query(
        pool = pool,
        minutos = janela,
        setor   = input$setor %||% NULL,
        objeto  = input$objeto %||% NULL,
        limite  = 5000L
      )
    }, error = function(e) {
      warning("DB query error: ", e$message)
      data.frame()
    })

    out
  })

  # Popular filtros (setor/objeto) a partir do retorno
  observe({
    d <- df_raw()
    if (!nrow(d)) return()
    updateSelectInput(session, "setor", choices = c("", sort(unique(d$SETOR))), selected = isolate(input$setor))
    updateSelectInput(session, "objeto", choices = c("", sort(unique(d$OBJETO))), selected = isolate(input$objeto))
  })

  # Data expandida
  df_exp <- reactive({
    d <- df_raw()
    if (!nrow(d)) return(d)
    expand_atributos(d)
  })

  # Métricas disponíveis
  metrics <- reactive({
    d <- df_exp()
    split_metrics(d)
  })

  observe({
    m <- metrics()
    updateSelectInput(session, "metric_select", choices = m$numeric, selected = isolate(input$metric_select %||% (m$numeric[1] %||% NA)))
  })

  # KPIs
  output$kpi_total_linhas <- renderUI({
    d <- df_raw(); kpi_card("All Earnings (regs)", format(nrow(d), big.mark = ".", decimal.mark = ","), "orange")
  })
  output$kpi_objetos <- renderUI({
    d <- df_raw(); kpi_card("Objetos ativos", length(unique(d$OBJETO)), "green")
  })
  output$kpi_setores <- renderUI({
    d <- df_raw(); kpi_card("Setores", length(unique(d$SETOR)), "teal")
  })
  output$kpi_metricas <- renderUI({
    m <- metrics(); kpi_card("Métricas numéricas", length(m$numeric), "red")
  })

  # Gráfico de séries
  output$ts_plot <- renderPlotly({
    d <- df_exp()
    req(nrow(d))
    met <- req(input$metric_select)
    grp <- input$group_by %||% "OBJETO"
    k   <- as.integer(input$downsample %||% 1)
    if (!met %in% names(d)) return(NULL)

    dd <- d |>
      select(DATE_TIME, all_of(grp), all_of(met)) |>
      filter(!is.na(.data[[met]])) |>
      arrange(DATE_TIME)

    if (!is.na(k) && k > 1) {
      dd <- dd |>
        dplyr::group_by(.data[[grp]]) |>
        dplyr::mutate(.row = dplyr::row_number()) |>
        dplyr::filter(.row %% k == 0) |>
        dplyr::ungroup() |>
        select(-.row)
    }

    plot_ly(dd, x = ~DATE_TIME, y = dd[[met]], color = dd[[grp]], type = "scatter", mode = "lines+markers") |>
      layout(
        xaxis = list(title = "Data/Hora", gridcolor = "#E8EEF5"),
        yaxis = list(title = met, gridcolor = "#E8EEF5"),
        legend = list(orientation = "h"),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor  = "#F6FAFE",
        font = list(color = "#243746"),
        colorway = c("#F2994A", "#27AE60", "#9B59B6", "#2D9CDB", "#EB5757")
      )
  })

  # Tabela
  output$tbl <- renderDataTable({
    d <- df_exp()
    req(nrow(d))
    # Reordena base first
    base_cols <- c("DATE_TIME","OBJETO","SETOR")
    extra <- setdiff(names(d), base_cols)
    d <- d |>
      relocate(all_of(base_cols)) |>
      arrange(desc(DATE_TIME))
    d
  }, options = list(scrollX = TRUE, pageLength = 15))

  # Download CSV
  output$btn_csv <- downloadHandler(
    filename = function() sprintf("contexto_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
    content  = function(file) {
      d <- df_exp(); utils::write.csv(d, file, row.names = FALSE, na = "")
    }
  )

  # Encerramento
  # ---- Application table & User Activity ----
  output$tbl_apps <- renderDataTable({
    d <- df_exp(); req(nrow(d))
    met <- input$metric_select %||% (metrics()$numeric[1] %||% NULL)
    grp <- input$group_by %||% "OBJETO"
    if (is.null(met) || !met %in% names(d)) return(data.frame())
    dplyr::group_by(d, .data[[grp]]) |>
      dplyr::summarise(
        Registros = dplyr::n(),
        Media = suppressWarnings(mean(as.numeric(.data[[met]]), na.rm = TRUE)),
        `.Última leitura` = max(DATE_TIME, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(desc(Registros))
  }, options = list(scrollX = TRUE, pageLength = 8))

  output$user_activity <- renderUI({
    d <- df_raw(); req(nrow(d))
    dd <- d |>
      arrange(desc(DATE_TIME)) |>
      utils::head(8)
    tagList(lapply(seq_len(nrow(dd)), function(i) {
      who <- dd$OBJETO[i]
      initials <- toupper(substr(who %||% "?", 1, 1))
      div(class = "ua-item",
          div(class = "ua-avatar", initials),
          div(
            strong(who %||% "—"), br(),
            span(class = "ua-meta", paste(format(dd$DATE_TIME[i], "%d/%m %H:%M"), "—", dd$SETOR[i]))
          )
      )
    }))
  })

  # Risk gauge (0-10). Se houver coluna com 'ESTADO', usa % PARADO como risco; senão, usa missingness
  output$risk_gauge <- renderPlotly({
    d <- df_exp(); req(nrow(d))
    # tenta localizar alguma coluna categórica com ESTADO
    cats <- metrics()$categorical
    estado_col <- cats[grepl("ESTADO", cats, ignore.case = TRUE)][1]
    score <- 5
    if (!is.na(estado_col) && estado_col %in% names(d)) {
      tab <- table(toupper(as.character(d[[estado_col]])))
      p_parado <- as.numeric(tab["PARADO"]) / sum(tab)
      p_parado[is.na(p_parado)] <- 0
      score <- round(10 * p_parado, 1)
    } else {
      miss_rate <- mean(is.na(d[[metrics()$numeric[1] %||% names(d)[1]]]))
      score <- round(10 * miss_rate, 1)
    }
    plot_ly(type = "indicator", mode = "gauge+number", value = score,
            gauge = list(axis = list(range = list(NULL, 10)),
                         bar = list(color = "#EB5757"),
                         steps = list(list(range = c(0,3), color = "#6FCF97"),
                                      list(range = c(3,7), color = "#F2C94C"),
                                      list(range = c(7,10), color = "#EB5757"))))
  })

  observeEvent(input$btn_download_report, { showModal(modalDialog(title = "Relatório", "Em breve.", easyClose = TRUE)) })

  session$onSessionEnded(function() {
    # Pool é gerenciado globalmente; não fechar aqui.
  })
}
