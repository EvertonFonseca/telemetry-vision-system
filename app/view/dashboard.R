box::use(
  shiny[...],
  shinyjs,
  shinyWidgets[airDatepickerInput,timepickerOptions],
  ggplot2,
  plotly,
  jsonlite,
  dplyr[...],
  tidyr[unnest_wider],
  utils[...],
  lubridate,
  shinydashboardPlus[box],
  stats[aggregate, na.omit, median],
  DBI,
  DT,
  dbp  = ../infra/db_pool,
  ./global[ actionWebUser,tagAppendAttributesFind,debugLocal],
  memoise,
  cachem,
  ../logic/objeto_dao[selectAllObjetos],
  ./video_clip[video_clip_open],
  ../logic/dashboard_dao[...]
)

.cache_obj <- cachem::cache_mem(max_size = 200 * 1024^2) # 200 MB

compute_dados <- function(pool,
                          dt_de_utc, dt_ate_utc,
                          dt_de_local, dt_ate_local,
                          setor = NULL, maquina = NULL,
                          tzL = Sys.timezone()) {

  df_raw <- run_query(pool, dt_de_utc, dt_ate_utc, setor, maquina)
  if (!nrow(df_raw)) return(NULL)

  df_raw$DATE_TIME <- from_utc(df_raw$DATE_TIME, tz = tzL)
  df <- expand_atributos(df_raw)

  df <- apply_setup_state(df, "BOBINA.ESTADO", "AREA_DE_TRABALHO_A.TRABALHADOR", min_parado_secs = 60, by_cols = c("SETOR","OBJETO"))
  df <- apply_setup_state(df, "PRENSA.ESTADO", "AREA_DE_TRABALHO_B.TRABALHADOR", min_parado_secs = 60, by_cols = c("SETOR","OBJETO"))
  df <- apply_setup_state(df, "DOBRA.ESTADO",  "AREA_DE_TRABALHO.TRABALHADOR",   min_parado_secs = 60, by_cols = c("SETOR","OBJETO"))
  df <- apply_setup_state(df, "SOLDA.ESTADO",  "AREA_DE_TRABALHO.TRABALHADOR",   min_parado_secs = 60, by_cols = c("SETOR","OBJETO"))

  ep <- build_episodes(df, min_secs = 60, dt_ate = dt_ate_local, max_gap_secs = 300)
  ep <- cap_episodes(ep, dt_de_local, dt_ate_local)
  if (is.null(ep) || !nrow(ep)) return(NULL)

  ep_m <- ep[ep$ESTADO %in% c("OPERANDO","PARADO","SETUP"), , drop = FALSE]

  list(
    df_raw = df_raw, df = df, ep = ep, ep_m = ep_m,
    dt_de_local = dt_de_local, dt_ate_local = dt_ate_local,
    dt_de_utc = dt_de_utc, dt_ate_utc = dt_ate_utc
  )
}

compute_dados_cached <- memoise::memoise(
  compute_dados,
  cache = .cache_obj
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ===============================
# Utilidades JSON / ATRIBUTOS
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
  if (is.na(txt) || !nzchar(txt)) return(tibble())
  obj <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) return(tibble())
  flat <- .flatten_once(obj)
  as.data.frame(flat, stringsAsFactors = FALSE) |> tibble()
}


# ===============================
# Expandir ATRIBUTOS -> colunas
# ===============================
expand_atributos <- function(df_raw) {
  if (!nrow(df_raw)) return(df_raw)
  rows <- lapply(seq_len(nrow(df_raw)), function(i) {
    base  <- df_raw[i, c("DATE_TIME", "OBJETO", "SETOR"), drop = FALSE]
    extra <- safe_parse_atributos(df_raw$ATRIBUTOS[i])
    if (!ncol(extra)) extra <- tibble()
    bind_rows(bind_cols(base, extra))
  })
  out <- bind_rows(rows)
 out |>
  mutate(DATE_TIME = as.POSIXct(DATE_TIME)) |>
  arrange(DATE_TIME)
}

# ===============================
# Derivar ESTADO de SETUP
# ===============================
# máquina_parada + trabalhador_presente  =>  SETUP
apply_setup_state <- function(df,
                              col_estado_maquina,
                              col_trabalhador,
                              estado_parado         = "PARADO",
                              trabalhador_presente  = "TRABALHANDO",
                              nome_setup            = "SETUP",
                              min_parado_secs       = 60,
                              by_cols               = c("SETOR","OBJETO"),
                              max_gap_secs          = Inf) {

  stopifnot("DATE_TIME" %in% names(df))
  need <- c("DATE_TIME", col_estado_maquina, col_trabalhador, by_cols)
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    #warning("apply_setup_state(): colunas ausentes: ", paste(miss, collapse = ", "))
    return(df)
  }

  tz_use <- attr(df$DATE_TIME, "tzone") %||% Sys.timezone()
  norm_chr <- function(x) toupper(trimws(as.character(x)))

  df |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by_cols, "DATE_TIME")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) |>
    dplyr::group_modify(function(d, key){

      t <- as.POSIXct(d$DATE_TIME, tz = tz_use)
      st_raw <- as.character(d[[col_estado_maquina]])
      wk_raw <- as.character(d[[col_trabalhador]])

      st <- norm_chr(st_raw)
      wk <- norm_chr(wk_raw)

      n <- length(t)
      dur_cand <- numeric(n)

      cand <- (st == toupper(estado_parado)) & (wk == toupper(trabalhador_presente))

      if (n >= 2L) {
        for (i in 2:n) {
          if (!isTRUE(cand[i])) { dur_cand[i] <- 0; next }
          if (!isTRUE(cand[i-1])) { dur_cand[i] <- 0; next }

          dt <- as.numeric(difftime(t[i], t[i-1], units = "secs"))
          if (is.na(dt) || dt < 0 || dt > max_gap_secs) {
            dur_cand[i] <- 0
          } else {
            dur_cand[i] <- dur_cand[i-1] + dt
          }
        }
      }

      is_setup <- cand & (dur_cand >= as.numeric(min_parado_secs))

      d[[col_estado_maquina]] <- ifelse(is_setup, nome_setup, st_raw)
      d
    }) |>
    dplyr::ungroup()
}


# ===============================
# Episódios de estado (run-length)
# ===============================
build_episodes <- function(df, min_secs = 60, dt_ate = NULL,
                           max_gap_secs = 300, tail_secs = NULL) {

  if (!nrow(df)) return(NULL)

  base_cols <- c("DATE_TIME", "OBJETO", "SETOR")
  attr_cols <- setdiff(names(df), base_cols)

  attr_cols <- attr_cols[vapply(df[attr_cols], function(v) is.character(v) || is.factor(v), logical(1))]
  if (!length(attr_cols)) return(NULL)

  long <- df |>
    dplyr::select(dplyr::all_of(base_cols), dplyr::all_of(attr_cols)) |>
    tidyr::pivot_longer(dplyr::all_of(attr_cols), names_to = "COMPONENTE", values_to = "ESTADO") |>
    dplyr::mutate(
      DATE_TIME  = as.POSIXct(DATE_TIME, tz = attr(df$DATE_TIME, "tzone") %||% Sys.timezone()),
      OBJETO     = as.character(OBJETO),
      SETOR      = as.character(SETOR),
      COMPONENTE = as.character(COMPONENTE),
      ESTADO     = as.character(ESTADO)
    ) |>
    dplyr::filter(!is.na(DATE_TIME), !is.na(OBJETO), !is.na(COMPONENTE), !is.na(ESTADO)) |>
    dplyr::arrange(OBJETO, COMPONENTE, DATE_TIME)

  if (!nrow(long)) return(NULL)

  norm_chr <- function(x) toupper(trimws(as.character(x)))

  make_ep <- function(d) {
    d <- d[order(d$DATE_TIME), ]
    n <- nrow(d)
    if (n == 0) return(NULL)

    tz_use <- attr(d$DATE_TIME, "tzone") %||% Sys.timezone()
    times  <- as.POSIXct(d$DATE_TIME, tz = tz_use)

    # dt_ate “real” do range (em vez de Sys.time()), mas SEM esticar se não tem dado
    dt_ate_use <- dt_ate
    if (is.null(dt_ate_use) || is.na(dt_ate_use)) dt_ate_use <- max(times, na.rm = TRUE)
    dt_ate_use <- as.POSIXct(dt_ate_use, tz = tz_use)

    # tail padrão = passo típico (limitado por max_gap)
    if (is.null(tail_secs)) {
      dd <- diff(as.numeric(times))
      step <- suppressWarnings(stats::median(dd[dd > 0], na.rm = TRUE))
      if (!is.finite(step)) step <- 60
      tail_secs <- min(max_gap_secs, as.numeric(step))
    }

    st_raw <- as.character(d$ESTADO)
    st     <- norm_chr(st_raw)

    clean <- st_raw

    # (seu smoothing de PARADO -> OPERANDO/SETUP, mantive igual)
    keep_states <- c("OPERANDO", "SETUP")
    if (n >= 2L) {
      for (i in seq_len(n - 1L)) {
        if (st[i] != "PARADO") next
        last_j <- NA_integer_
        j <- i + 1L
        while (j <= n) {
          dt <- as.numeric(difftime(times[j], times[i], units = "secs"))
          if (is.na(dt) || dt < 0 || dt > min_secs) break
          if (st[j] %in% keep_states) last_j <- j
          j <- j + 1L
        }
        if (!is.na(last_j)) clean[i] <- st_raw[last_j]
      }
    }

    # end_obs com controle de gap
    end_obs <- c(times[-1], pmin(dt_ate_use, times[n] + tail_secs))
    end_obs <- pmin(end_obs, times + max_gap_secs)  # ✅ não estica além do gap

    end_obs[end_obs < times] <- times[end_obs < times]

    start_idx <- which(c(TRUE, clean[-1] != clean[-n]))
    end_idx   <- c(start_idx[-1] - 1L, n)

    data.frame(
      SETOR      = d$SETOR[1],
      OBJETO     = d$OBJETO[1],
      COMPONENTE = d$COMPONENTE[1],
      ESTADO     = clean[start_idx],
      start_time = times[start_idx],
      end_time   = end_obs[end_idx],
      dur_secs   = as.numeric(difftime(end_obs[end_idx], times[start_idx], units = "secs")),
      stringsAsFactors = FALSE
    )
  }

  parts <- split(long, list(long$OBJETO, long$COMPONENTE), drop = TRUE)
  ep <- do.call(rbind, lapply(parts, make_ep))
  if (is.null(ep) || !nrow(ep)) return(NULL)

  ep$dur_secs[is.na(ep$dur_secs) | ep$dur_secs < 0] <- 0
  ep <- ep[ep$dur_secs > 0, , drop = FALSE]
  ep
}


# ===============================
# Métricas de utilização diária
# ===============================
build_daily_util <- function(ep) {
  if (is.null(ep) || !nrow(ep)) return(NULL)

  # garanta tipos
  ep$start_time <- as.POSIXct(ep$start_time, tz = attr(ep$start_time, "tzone") %||% Sys.timezone())
  ep$dur_secs   <- as.numeric(ep$dur_secs)

  # remove lixo
  ep <- ep[!is.na(ep$start_time) & !is.na(ep$dur_secs) & ep$dur_secs >= 0, , drop = FALSE]
  if (!nrow(ep)) return(NULL)

  ep$date <- as.Date(ep$start_time)

  # total por dia
  tot <- aggregate(dur_secs ~ date, data = ep, sum, na.rm = TRUE)
  if (is.null(tot) || !nrow(tot)) return(NULL)

  # operando por dia (pode não existir)
  op <- ep[ep$ESTADO == "OPERANDO", , drop = FALSE]
  if (nrow(op)) {
    op <- aggregate(dur_secs ~ date, data = op, sum, na.rm = TRUE)
    names(op)[2] <- "dur_op"
    dfu <- merge(tot, op, by = "date", all.x = TRUE)
    dfu$dur_op[is.na(dfu$dur_op)] <- 0
  } else {
    dfu <- tot
    dfu$dur_op <- 0
  }

  dfu$util <- ifelse(dfu$dur_secs > 0, dfu$dur_op / dfu$dur_secs * 100, NA_real_)
  dfu[order(dfu$date), ]
}

roll_mean <- function(x, k) {
  n <- length(x)
  out <- rep(NA_real_, n)
  if (n == 0) return(out)
  for (i in seq_len(n)) {
    j0 <- max(1L, i - k + 1L)
    out[i] <- mean(x[j0:i], na.rm = TRUE)
  }
  out
}

# ===============================
# Classificação de turnos
# ===============================
classificar_turno <- function(dt) {

  if (length(dt) == 0) return(character(0))

  h  <- lubridate::hour(dt)
  m  <- lubridate::minute(dt)
  mm <- h * 60 + m

  turno_chr <- dplyr::case_when(
    mm >= (7 * 60 + 30) & mm <= (17 * 60 + 18) ~ "Manhã",
    mm >= (17 * 60 + 19) | mm == 0             ~ "Tarde",
    mm >= 1 & mm <= (7 * 60 + 29)              ~ "Noite",
    TRUE                                       ~ NA_character_
  )

  factor(turno_chr, levels = c("Manhã", "Tarde", "Noite"))
}


# ===============================
# Temas Plotly / ggplot
# ===============================
themasPlotyGGplot <- function(args = NULL, text.x.angle = 0){
  ggplot2::theme(
    args,
    axis.text.x = ggplot2::element_text(angle = text.x.angle),
    legend.title = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = 'gray'),
    panel.grid.major = ggplot2::element_line(
      linewidth = 0.5,
      linetype  = 'solid',
      colour = "lightgray"
    )
  )
}

layoutPloty <- function(x, legend.text = ''){
  plotly::layout(
    x,
    plot_bgcolor = 'transparent',
    paper_bgcolor = 'transparent',
    modebar = list(
      bgcolor = 'transparent',
      color = 'transparent',
      activecolor = 'transparent'
    ),
    showlegend = TRUE,
    legend  = list(
      title = list(text = legend.text, font = list(color = 'black', size = 12)),
      font  = list(color = 'black', size = 10),
    ),
    xaxis = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE)
  ) |>
    htmlwidgets::onRender(
      "function(el,x){
         el.on('plotly_legendclick', function(){ return false; })
       }"
    )
}

layoutPlotyDefault <- function(x, legend.text = ''){
  plotly::layout(
    x,
    plot_bgcolor = 'transparent',
    paper_bgcolor = 'transparent',
    showlegend = TRUE,
    modebar = list(
      bgcolor = 'transparent',
      color = 'lightgray',
      activecolor = 'darkgray'
    ),
    legend  = list(
      title = list(text = legend.text, font = list(color = 'black', size = 12)),
      font  = list(color = 'black', size = 10)
    )
  )
}

# ===============================
# UI
# ===============================
#' @export
ui <- function(ns) {

  tagList(
    fluidRow(
      style = "margin-top: 50px;",
      # Box de filtros
      column(12,
        box(
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          title = tags$span("Filtros", style = "font-size: 16px;"),
          fluidRow(
            column(3,
              shiny::selectizeInput(
                ns("f_setor"), "Setor",
                choices = NULL, multiple = TRUE,
                options = list(placeholder = "Todos os setores")
              )
            ),
            column(3,
              shiny::selectizeInput(
                ns("f_maquina"), "Máquina",
                choices = NULL, multiple = TRUE,
                options = list(placeholder = "Todas as máquinas")
              )
            ),
            column(3,
              shinyWidgets::airDatepickerInput(
                ns("f_de"), "De",
                value = NULL,
                timepicker = TRUE,
                placeholder = "Sem filtro (padrão: últimas 24h)",
                timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 1),
                autoClose = TRUE,
                readonly   = TRUE,
                dateFormat = "dd/MM/yyyy",
                language = "pt-BR"
              )
            ),
            column(2,
              shinyWidgets::airDatepickerInput(
                ns("f_ate"), "Até",
                value = NULL,
                timepicker = TRUE,
                placeholder = "Sem filtro (padrão: últimas 24h)",
                timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 1),
                autoClose = TRUE,
                readonly   = TRUE,
                dateFormat = "dd/MM/yyyy",
                language = "pt-BR"
              )
            ),
            column(1,
              div(style="padding-top: 25px; margin-left: -10px; display:flex; gap:4px;",
              shiny::actionButton(
                ns("bt_apply_filters"),
                label = NULL,
                icon = shiny::icon("search"),
                class = "btn btn-secondary",
                title = "Aplicar filtros"
              ),
              shiny::actionButton(
                ns("bt_clear_filters"),
                label = NULL,
                icon = shiny::icon("eraser"),
                class = "btn btn-warning",
                title = "Limpar filtros (padrão: últimas 24h / tudo)"
              )
            )
          )
        )
      )
    ),
    # Box 1 – horas OPERANDO
    column(3,
      shinydashboard::valueBox(
          width = '100%',
          value = uiOutput(ns(paste0('vbOperando', 1))),
          subtitle = 'Tempo OPERANDO',
          icon = icon('play'),
          color = "green"
        ) |> tagAppendAttributesFind(target = 1, style = 'min-height: 102px')
      ),

      # Box 2 – horas PARADO
      column(3,
        shinydashboard::valueBox(
          width = '100%',
          value = uiOutput(ns(paste0('vbParado', 1))),
          subtitle = 'Tempo PARADO',
          icon = icon('pause'),
          color = "red"
        ) |> tagAppendAttributesFind(target = 1, style = 'min-height: 102px')
      ),

      # Box 3 – horas SETUP
      column(3,
        shinydashboard::valueBox(
          width = '100%',
          value = uiOutput(ns(paste0('vbSetup',1))),
          subtitle = 'Tempo em SETUP',
          icon = icon('tools'),
          color = "yellow"
        ) |> tagAppendAttributesFind(target = 1, style = 'min-height: 102px')
      ),

      # Box 4 – utilização média
      column(3,
        shinydashboard::valueBox(
          width = '100%',
          value = uiOutput(ns(paste0('vbUtilizacao',1))),
          subtitle = 'Utilização média',
          icon = icon('chart-line'),
          color = "blue"
        ) |> tagAppendAttributesFind(target = 1, style = 'min-height: 102px')
      ),

      column(12, uiOutput(ns("dashbody")))
    ),
    br()
  )
}

# ===============================
# Server
# ===============================
#' @export
server <- function(ns, input, output, session) {

  pool      <- dbp$get_pool()
  objs_tmp  <- selectAllObjetos(pool)
  actionClickplot2 <- reactiveVal(FALSE)
  
  rv <- shiny::reactiveValues(
    setor  = NULL,
    maquina = NULL,
    dt_de  = NULL,  # NULL = sem filtro explícito
    dt_ate = NULL
  )

  observeEvent(input$bt_apply_filters, {
    .cache_obj$reset()   # ✅ força requery do banco
    rv$setor   <- if (!length(input$f_setor)) NULL else input$f_setor
    rv$maquina <- if (!length(input$f_maquina)) NULL else input$f_maquina
    
    # datas: podem ser NULL (sem filtro)
    rv$dt_de   <- input$f_de
    rv$dt_ate  <- input$f_ate
  }, ignoreInit = TRUE)
  
  # popula choices de filtros
  observe({
    ch <- fetch_choices(pool)
    updateSelectizeInput(session, "f_setor", choices = ch$setores,  selected = character(0), server = TRUE)
    updateSelectizeInput(session, "f_maquina", choices = ch$maquinas, selected = character(0), server = TRUE)
  })
  
  observeEvent(input$bt_clear_filters, {
    # limpa UI
    .cache_obj$reset()   # ✅ idem
    shiny::updateSelectizeInput(session,"f_setor",selected = character(0))
    shiny::updateSelectizeInput(session,"f_maquina",selected = character(0))
    shinyWidgets::updateAirDateInput(session,"f_de",clear = TRUE)
    shinyWidgets::updateAirDateInput(session,"f_ate",clear = TRUE)
    
    # reseta filtros aplicados
    rv$setor   <- NULL
    rv$maquina <- NULL
    rv$dt_de   <- NULL
    rv$dt_ate  <- NULL
  }, ignoreInit = TRUE)
  
  # aqui você pode ajustar setor/objeto conforme necessário
  objeto_alvo <- c("SETREMA","DOBRA")
  
  # estados da máquina que queremos monitorar
  estados_maquina <- c("OPERANDO", "PARADO", "SETUP")
  
  dados <- reactive({
    invalidateLater(60 * 5 * 1000, session)
    
    tzL <- tz_local()
    now_local <- as.POSIXct(Sys.time(), tz = tzL)
    
    dt_ate_local <- if (is.null(rv$dt_ate) || is.na(rv$dt_ate)) now_local else as.POSIXct(rv$dt_ate, tz = tzL)
    dt_de_local  <- if (is.null(rv$dt_de)  || is.na(rv$dt_de))  (dt_ate_local - 24*60*60) else as.POSIXct(rv$dt_de, tz = tzL)
    
    dt_de_utc  <- to_utc(dt_de_local, tz = tzL)
    dt_ate_utc <- to_utc(dt_ate_local, tz = tzL)
    
    # chave: filtros + intervalo (o memoise faz hash dos args)
    compute_dados_cached(
      pool = pool,
      dt_de_utc = dt_de_utc, dt_ate_utc = dt_ate_utc,
      dt_de_local = dt_de_local, dt_ate_local = dt_ate_local,
      setor = rv$setor, maquina = rv$maquina,
      tzL = tzL
    )
  })

  ep_m_reactive <- reactive({
    x <- dados()
    if (is.null(x)) return(NULL)
    x$ep_m
  })
  # -------------------------
  # Resumo por turno (para tabela executiva)
  # -------------------------
  resumo_turnos <- reactive({
    x <- dados()
    if (is.null(x)) return(NULL)
    
    ep_m <- x$ep_m
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
    
    # só componentes ".ESTADO"
    ep_m <- ep_m[grepl("\\.ESTADO$", ep_m$COMPONENTE), ]
    if (!nrow(ep_m)) return(NULL)
    
    dplyr::as_tibble(ep_m) |>
    dplyr::mutate(
      mid_time  = start_time + dur_secs / 2,
      Periodo   = as.character(classificar_turno(mid_time)),
      ESTADO    = toupper(trimws(as.character(ESTADO))),
      ESTADO    = factor(ESTADO, levels = c("OPERANDO","PARADO","SETUP")),
      SETOR     = as.character(SETOR),
      OBJETO    = as.character(OBJETO),
      mins      = dur_secs / 60
    ) |>
    dplyr::filter(!is.na(Periodo), !is.na(SETOR), !is.na(OBJETO), !is.na(ESTADO)) |>
    dplyr::group_by(Periodo, SETOR, OBJETO, ESTADO) |>
    dplyr::summarise(mins = sum(mins, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from   = ESTADO,
      values_from  = mins,
      values_fill  = list(mins = 0),
      names_expand = TRUE
    ) |>
    dplyr::mutate(
      OPERANDO = dplyr::coalesce(OPERANDO, 0),
      PARADO   = dplyr::coalesce(PARADO,   0),
      SETUP    = dplyr::coalesce(SETUP,    0),
      Total    = OPERANDO + PARADO + SETUP,
      Utilizacao = dplyr::if_else(Total > 0, OPERANDO / Total * 100, as.numeric(NA))
    ) |>
    dplyr::group_by(Periodo) |>
    dplyr::mutate(
      Ranking = dplyr::min_rank(dplyr::desc(Utilizacao))  # ranking dentro do turno
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Periodo = factor(Periodo, levels = c("Manhã","Tarde","Noite"))
    ) |>
    dplyr::arrange(Periodo, Ranking, dplyr::desc(Total))
  })
  # resumo_turnos <- resumo_turnos |>
  # bindCache(rv$setor, rv$maquina, rv$dt_de, rv$dt_ate)
  
  # resumo para os valueBoxes (reaproveitado nos 4 boxes)
  box_exec <- reactive({
    x <- dados()
    if (is.null(x) || is.null(x$ep_m) || !nrow(x$ep_m)) return(NULL)
    
    ep_m <- x$ep_m
    ep_m <- ep_m[grepl("\\.ESTADO$", ep_m$COMPONENTE), , drop = FALSE]
    if (!nrow(ep_m)) return(NULL)
    
    # -------------------------
    # 1) Escolher UM componente principal por OBJETO
    #    (evita somar estados paralelos e "duplicar horas")
    # -------------------------
    main_component_map <- c(
      "SETREMA" = "PRENSA.ESTADO",  # ajuste se necessário
      "DOBRA"   = "DOBRA.ESTADO"
    )
    
    ep_main <- dplyr::as_tibble(ep_m) |>
    dplyr::group_by(OBJETO) |>
    dplyr::group_modify(function(.x, .y) {
      
      obj   <- as.character(.y$OBJETO[1])
      comps <- unique(as.character(.x$COMPONENTE))
      
      # 1) tenta OBJETO.ESTADO
      want1 <- paste0(obj, ".ESTADO")
      if (want1 %in% comps) return(.x[.x$COMPONENTE == want1, , drop = FALSE])
      
      # 2) tenta map
      want2 <- main_component_map[obj]
      if (length(want2) && !is.na(want2) && want2 %in% comps) {
        return(.x[.x$COMPONENTE == want2, , drop = FALSE])
      }
      
      # 3) fallback: primeiro .ESTADO
      .x[.x$COMPONENTE == comps[1], , drop = FALSE]
    }) |>
    dplyr::ungroup()
    
    if (!nrow(ep_main)) return(NULL)
    
    # -------------------------
    # 2) Resumo por máquina: segundos por estado + utilização
    # -------------------------
    by_obj <- ep_main |>
    dplyr::mutate(ESTADO = toupper(trimws(as.character(ESTADO)))) |>
    dplyr::group_by(OBJETO) |>
    dplyr::summarise(
      op = sum(dur_secs[ESTADO == "OPERANDO"], na.rm = TRUE),
      pa = sum(dur_secs[ESTADO == "PARADO"],   na.rm = TRUE),
      se = sum(dur_secs[ESTADO == "SETUP"],    na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      tot  = op + pa + se,
      util = dplyr::if_else(tot > 0, op / tot * 100, as.numeric(NA))
    ) |>
    dplyr::arrange(dplyr::desc(util))
    
    nmaq <- nrow(by_obj)
    if (nmaq <= 0) return(NULL)
    
    # -------------------------
    # 3) Totais (ponderados) e médias por máquina
    # -------------------------
    op_tot  <- sum(by_obj$op, na.rm = TRUE)
    pa_tot  <- sum(by_obj$pa, na.rm = TRUE)
    se_tot  <- sum(by_obj$se, na.rm = TRUE)
    tot_tot <- op_tot + pa_tot + se_tot
    
    util_geral <- if (tot_tot > 0) op_tot / tot_tot * 100 else NA_real_
    
    # médias por máquina (em horas)
    op_mean_h <- (op_tot / 3600) / max(1, nmaq)
    pa_mean_h <- (pa_tot / 3600) / max(1, nmaq)
    se_mean_h <- (se_tot / 3600) / max(1, nmaq)
    
    # média simples de % por máquina
    util_mean <- mean(by_obj$util, na.rm = TRUE)
    
    # -------------------------
    # 4) Textos e percentuais auxiliares
    # -------------------------
    pct <- function(a,b) ifelse(is.finite(a) & is.finite(b) & b > 0, a/b*100, NA_real_)
    
    op_pct <- pct(op_tot, tot_tot)
    pa_pct <- pct(pa_tot, tot_tot)
    se_pct <- pct(se_tot, tot_tot)
    
    single_obj <- if (nmaq == 1 && nrow(by_obj)) as.character(by_obj$OBJETO[1]) else NA_character_
    
    top_txt <- if (nmaq >= 1 && is.finite(by_obj$util[1])) {
      paste0(by_obj$OBJETO[1], " (", round(by_obj$util[1], 1), "%)")
    } else {
      "—"
    }
    
    worst_txt <- {
      w <- by_obj |>
      dplyr::filter(is.finite(util)) |>
      dplyr::arrange(util) |>
      dplyr::slice(1)
      if (nrow(w)) paste0(w$OBJETO, " (", round(w$util, 1), "%)") else "—"
    }
    
    top_setup_txt <- "—"
    if (nrow(by_obj)) {
      tmp <- by_obj |>
      dplyr::mutate(setup_pct = pct(se, tot)) |>
      dplyr::arrange(dplyr::desc(setup_pct)) |>
      dplyr::slice(1)
      if (nrow(tmp) && is.finite(tmp$setup_pct)) {
        top_setup_txt <- paste0(tmp$OBJETO, " (", round(tmp$setup_pct, 1), "%)")
      }
    }
    
    per_txt <- paste0(fmt_dt(x$dt_de_local), " → ", fmt_dt(x$dt_ate_local))
    
    # -------------------------
    # 5) Saída (usada pelos 4 valueBoxes)
    # -------------------------
    list(
      # principais (média por máquina)
      op_h = op_mean_h,
      pa_h = pa_mean_h,
      se_h = se_mean_h,
      util = util_mean,
      
      # totais do período (sem duplicação)
      op_tot_h = op_tot / 3600,
      pa_tot_h = pa_tot / 3600,
      se_tot_h = se_tot / 3600,
      util_geral = util_geral,
      
      # percentuais gerais (ponderados)
      op_pct = op_pct,
      pa_pct = pa_pct,
      se_pct = se_pct,
      
      # infos auxiliares para não repetir texto quando nmaq==1
      nmaq = nmaq,
      single_obj = single_obj,
      top = top_txt,
      worst = worst_txt,
      top_setup = top_setup_txt,
      periodo = per_txt
    )
  })
  
  # box_exec <- box_exec |>
  # bindCache(rv$setor, rv$maquina, rv$dt_de, rv$dt_ate)
  
  fmt_horas <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("–")
    mins <- as.integer(round(as.numeric(x) * 60))  # x está em horas
    h <- mins %/% 60
    m <- mins %% 60
    sprintf("%d h %02d min", h, m)
  }
  
  fmt_perc  <- function(x) ifelse(is.na(x), "–", sprintf("%.1f %%", x))
  
  # -------------------------
  # ValueBoxes (sempre lendo resumo_boxes())
  # -------------------------
  output[[paste0("vbOperando",1)]] <- renderUI({
    b <- box_exec(); if (is.null(b)) return("–")
    box_value_html(
      fmt_horas(b$op_h),
      paste0("Total: ", fmt_horas(b$op_tot_h), " • ", fmt_perc(b$op_pct)),
      paste0(if (!is.na(b$single_obj)) paste0("Máquina única: ", b$single_obj) else paste0("Top utilização: ", b$top),
      " • ", b$periodo)
    )
  })
  
  output[[paste0("vbParado",1)]] <- renderUI({
    b <- box_exec(); if (is.null(b)) return("–")
    box_value_html(
      fmt_horas(b$pa_h),
      paste0("Total: ", fmt_horas(b$pa_tot_h), " • ", fmt_perc(b$pa_pct)),
      paste0(if (!is.na(b$single_obj)) paste0("Máquina única: ", b$single_obj) else paste0("Pior utilização: ", b$worst),
      " • ", b$periodo)
    )
  })
  
  output[[paste0("vbSetup",1)]] <- renderUI({
    b <- box_exec(); if (is.null(b)) return("–")
    box_value_html(
      fmt_horas(b$se_h),
      paste0("Total: ", fmt_horas(b$se_tot_h), " • ", fmt_perc(b$se_pct)),
      paste0(if (!is.na(b$single_obj)) paste0("Máquina única: ", b$single_obj) else paste0("Maior % setup: ", b$top_setup),
      " • ", b$periodo)
    )
  })
  
  output[[paste0("vbUtilizacao",1)]] <- renderUI({
    b <- box_exec(); if (is.null(b)) return("–")
    box_value_html(
      fmt_perc(b$util),
      paste0("Geral (ponderado): ", fmt_perc(b$util_geral)),
      paste0(if (!is.na(b$single_obj)) paste0("Máquina única: ", b$single_obj) else paste0("Top: ", b$top, " • Pior: ", b$worst),
      " • ", b$periodo)
    )
  })


  # =======================
  # UI dos gráficos
  # =======================
  output$dashbody <- renderUI({
    tagList(
      # Tabela executiva – melhores turnos
      box(
        id          = ns("box_turnos"),
        solidHeader = TRUE,
        collapsible = TRUE,
        width       = 12,
        title = tags$span("Resumo por período", 
        style = "font-size: 16px;"),
        div(
          style = "padding: 10px;",
          tags$p(
            "Tabela executiva destacando os turnos com melhor desempenho operacional ",
            "da máquina, considerando os estados OPERANDO, PARADO e SETUP ",
            style = "margin-bottom: 10px; font-size: 12px; color: #444;"
          ),
          DT::dataTableOutput(ns("tblTurnos")) |> shinycssloaders::withSpinner(color = 'lightblue')
        )
      ),
      insertNewPlotComponent(ns, "1) Gráfico por Setor – Operando / Parada / Setup", 1),
      insertNewPlotComponent(ns, "2) Linha do Tempo (Gantt) – Máquina",       2),
      insertNewPlotComponent(ns, "3) Heatmap – Máquinas x Horário",                 3),
      insertNewPlotComponent(ns, "4) Tendência de utilização – Fábrica",            4)
    )
  })

  # ---------------------------
  # (1) Gráfico por Setor – barras empilhadas
  # ---------------------------
  output[[paste0("plotout_",1)]] <- plotly$renderPlotly({
    ep_m <- ep_m_reactive()
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
    
    # usar o meio do episódio para classificar o turno
    ep_m$mid_time <- ep_m$start_time + ep_m$dur_secs / 2
    ep_m$turno    <- classificar_turno(ep_m$mid_time)
    
    # tira qualquer linha sem turno (por segurança)
    ep_m <- ep_m[!is.na(ep_m$turno), ]
    if (!nrow(ep_m)) return(NULL)
    
    # agrega por SETOR + TURNO + ESTADO
    df_plot <- aggregate(
      dur_secs ~ SETOR + turno + ESTADO,
      data = ep_m,
      sum,
      na.rm = TRUE
    )
    
    df_plot$horas  <- df_plot$dur_secs / 3600
    df_plot$ESTADO <- factor(df_plot$ESTADO, levels = estados_maquina)
    df_plot$turno  <- factor(df_plot$turno, levels = c("Manhã","Tarde","Noite"))
    
    g <- ggplot2$ggplot(
      df_plot,
      ggplot2$aes(x = SETOR, y = horas, fill = ESTADO)
    ) +
    ggplot2$geom_bar(stat = "identity", position = "stack") +
    ggplot2$labs(
      x     = "Setor",
      y     = "Horas no turno",
      fill  = "Estado"
    ) +
    ggplot2$facet_grid(turno ~ ., scales = "free_y",switch = "y") +
    ggplot2$scale_fill_manual(
      values = c(
        "OPERANDO" = "#00a65a",
        "PARADO"   = "tomato",
        "SETUP"    = "gold"
      )
    ) +
    themasPlotyGGplot()
    
    p <- plotly::ggplotly(g, tooltip = c("x","y","fill")) |> plotly::config(displaylogo = FALSE,displayModeBar = T)
    layoutPlotyDefault(p, legend.text = "Estado")
  })

  # ---------------------------
  # (2) Linha do Tempo (Gantt) – Máquina DOBRA
  # ---------------------------
  output[[paste0("plotout_",2)]] <- plotly$renderPlotly({
    ep_m <- ep_m_reactive()
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
    
    ep_obj <- ep_m
    if (!nrow(ep_obj)) return(NULL)
    
    ep_obj_estado <- subset(ep_obj, grepl("\\.ESTADO$", COMPONENTE))
    if (nrow(ep_obj_estado)) ep_obj <- ep_obj_estado

    if(isolate(!actionClickplot2())){
      actionClickplot2(TRUE)
    }
    
    ep_obj$hover_x  <- ep_obj$start_time + ep_obj$dur_secs / 2
    ep_obj$mid_time <- ep_obj$hover_x
    ep_obj$turno    <- classificar_turno(ep_obj$mid_time)
    
    ep_obj$hover_text <- paste0(
      "Máquina: ", ep_obj$OBJETO,
      "<br>Componente: ", ep_obj$COMPONENTE,
      "<br>Estado: ", ep_obj$ESTADO,
      "<br>Início: ", fmt_dt(ep_obj$start_time),
      "<br>Fim: ",    fmt_dt(ep_obj$end_time),
      "<br>Duração: ", round(ep_obj$dur_secs/60, 1), " min"
    )
    ep_obj$row_id <- seq_len(nrow(ep_obj))
    
    g <- ggplot2::ggplot(
      ep_obj,
      ggplot2::aes(
        x     = start_time,
        xend  = end_time,
        y     = COMPONENTE,
        yend  = COMPONENTE,
        color = ESTADO,
        text  = hover_text,
        key   = row_id
      )
    ) +
    ggplot2::geom_segment(linewidth = 6) +
    
    # ✅ ponto invisível (sem 'text' aqui pra não dar warning)
    ggplot2::geom_point(
      ggplot2::aes(x = hover_x, y = COMPONENTE, color = ESTADO),
      size = 10, alpha = 0.001, show.legend = FALSE
    ) +
    
    ggplot2::labs(x = "Tempo", y = "", color = "Estado") +
    ggplot2::facet_grid(turno ~ OBJETO, scales = "free_y", switch = "y") +
    ggplot2::scale_color_manual(values = c(
      "OPERANDO" = "#00a65a",
      "PARADO"   = "tomato",
      "SETUP"    = "gold"
    )) +
    themasPlotyGGplot(text.x.angle = 45)
    
    p <- plotly::ggplotly(g, tooltip = "text", source = "p2") |>
         plotly::config(displaylogo = FALSE, displayModeBar = TRUE,doubleClick = TRUE)
    
    p <- layoutPlotyDefault(p, legend.text = "Estado")
    plotly::event_register(p,"plotly_click")
  })

  # ---------------------------
  # (3) Heatmap – Máquinas x Horário
  # ---------------------------
  output[[paste0("plotout_",3)]] <- plotly$renderPlotly({
    ep_m <- ep_m_reactive()
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
    
    ep_hm <- ep_m
    ep_hm$start_time <- as.POSIXct(ep_hm$start_time)
    ep_hm$end_time   <- as.POSIXct(ep_hm$end_time)
    
    # Janela total (24h) em bins de 30 min
    t0 <- lubridate::floor_date(min(ep_hm$start_time, na.rm = TRUE), unit = "30 minutes")
    t1 <- lubridate::ceiling_date(max(ep_hm$end_time,   na.rm = TRUE), unit = "30 minutes")
    bin_starts <- seq(from = t0, to = t1, by = "30 min")
    
    # Expande episódios -> bins (calculando overlap real em segundos)
    expanded <- lapply(seq_len(nrow(ep_hm)), function(i) {
      st <- ep_hm$start_time[i]
      en <- ep_hm$end_time[i]
      if (is.na(st) || is.na(en) || en <= st) return(NULL)
      
      # bins que potencialmente cruzam este episódio
      bs0 <- lubridate::floor_date(st, unit = "30 minutes")
      bs1 <- lubridate::floor_date(en, unit = "30 minutes")
      bs  <- seq(from = bs0, to = bs1, by = "30 min")
      
      be  <- bs + lubridate::minutes(30)
      
      ov_start <- pmax(st, bs)
      ov_end   <- pmin(en, be)
      
      ov_secs <- as.numeric(difftime(ov_end, ov_start, units = "secs"))
      ov_secs[is.na(ov_secs) | ov_secs < 0] <- 0
      keep <- ov_secs > 0
      if (!any(keep)) return(NULL)
      
      # turno pelo meio do BIN (não pelo mid do episódio)
      mid_bin <- bs + lubridate::minutes(15)
      turno   <- classificar_turno(mid_bin)
      
      data.frame(
        OBJETO   = ep_hm$OBJETO[i],
        SETOR    = ep_hm$SETOR[i],
        ESTADO   = ep_hm$ESTADO[i],
        turno    = turno,
        time_bin = bs[keep],
        ov_secs  = ov_secs[keep],
        stringsAsFactors = FALSE
      )
    })
    
    expanded <- do.call(rbind, expanded)
    if (is.null(expanded) || !nrow(expanded)) return(NULL)
    
    # label HH:MM e níveis completos a cada 30 min
    lvl_hora <- unique(format(bin_starts, "%H:%M"))
    expanded$hora <- factor(format(expanded$time_bin, "%H:%M"), levels = lvl_hora)
    expanded$turno <- factor(expanded$turno, levels = c("Manhã","Tarde","Noite"))
    
    # agrega por OBJETO + hora + estado + turno
    hm <- aggregate(
      ov_secs ~ OBJETO + hora + ESTADO + turno,
      data = expanded,
      sum,
      na.rm = TRUE
    )
    
    # só OPERANDO
    hm_op <- subset(hm, ESTADO == "OPERANDO" & !is.na(turno) & !is.na(hora))
    if (!nrow(hm_op)) return(NULL)
    
    # ✅ MINUTOS OPERANDO
    hm_op$minutos <- hm_op$ov_secs / 60
    
    g <- ggplot2$ggplot(
      hm_op,
      ggplot2$aes(x = hora, y = OBJETO, fill = minutos)
    ) +
    ggplot2$geom_tile(color = "gray80") +
    ggplot2$facet_grid(turno ~ ., scales = "free_y", switch = "y") +
    ggplot2$labs(
      x    = "Horário (30 min)",
      y    = "Máquina / Objeto",
      fill = "Minutos OPERANDO"
    ) +
    themasPlotyGGplot(text.x.angle = 45)
    
    p <- plotly::ggplotly(g, tooltip = c("x","y","fill")) |>
    plotly::config(displaylogo = FALSE, displayModeBar = TRUE)
    
    layoutPlotyDefault(p, legend.text = "Minutos")
  })

  # ---------------------------
  # (4) Gráfico em linhas – médias móveis
  # ---------------------------
  output[[paste0("plotout_",4)]] <- plotly$renderPlotly({
    ep_m <- ep_m_reactive()
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
    
    daily <- build_daily_util(ep_m)
    if (is.null(daily) || !nrow(daily)) return(NULL)
    
    daily <- daily[order(daily$date), ]
    
    daily$ma7  <- roll_mean(daily$util,  7)
    daily$ma15 <- roll_mean(daily$util, 15)
    daily$ma30 <- roll_mean(daily$util, 30)
    
    df_long <- rbind(
      data.frame(date = daily$date, serie = "Utilização diária", valor = daily$util),
      data.frame(date = daily$date, serie = "Média 7 dias",     valor = daily$ma7),
      data.frame(date = daily$date, serie = "Média 15 dias",    valor = daily$ma15),
      data.frame(date = daily$date, serie = "Média 30 dias",    valor = daily$ma30)
    )
    
    df_long$serie <- factor(
      df_long$serie,
      levels = c("Utilização diária","Média 7 dias","Média 15 dias","Média 30 dias")
    )
    
    g <- ggplot2$ggplot(
      df_long,
      ggplot2$aes(x = date, y = valor, color = serie)
    ) +
    ggplot2$geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggplot2$labs(
      x     = "Dia",
      y     = "% tempo OPERANDO",
      color = "Série"
    ) +
    themasPlotyGGplot()
    
    p <- plotly::ggplotly(g, tooltip = c("x","y","color")) |> plotly::config(displaylogo = FALSE,displayModeBar = T)
    layoutPlotyDefault(p, legend.text = "Médias móveis")
  })
  # -------------------------
  # Tabela executiva por turno
  # -------------------------
  output$tblTurnos <- DT::renderDataTable({
    df <- resumo_turnos()
    if (is.null(df) || !nrow(df)) return(NULL)
    
    df_out <- df |>
    dplyr::transmute(
      `Período`        = as.character(Periodo),
      `Setor`          = SETOR,
      `Máquina`        = OBJETO,
      `Tempo OPERANDO` = fmt_hm(OPERANDO),
      `Tempo PARADO`   = fmt_hm(PARADO),
      `Tempo SETUP`    = fmt_hm(SETUP),
      `Tempo total`    = fmt_hm(Total),
      `Utilização (%)` = round(Utilizacao, 1),
      `Ranking`        = Ranking
    )
    
    DT::datatable(
      df_out,
      rownames = FALSE,
      extensions = c("RowGroup"),
      options = list(
        dom        = "tip",
        pageLength = 10,
        order      = list(list(0, "asc"), list(8, "asc")), # Período, Ranking
        rowGroup   = list(dataSrc = 0),                   # agrupa por Período
        language   = dt_lang_ptbr()
      ),
      class = "cell-border stripe compact"
    )
  })

  # observe tmp 
  observeEvent(actionClickplot2(),{
    
    ## observer plots
    observeEvent(plotly::event_data("plotly_click",priority = "event", source = "p2"), {

      ed <- plotly::event_data("plotly_click", source = "p2")
  
      if (is.null(ed) || is.null(ed$key)) return()
      
      actionWebUser({
        row_id <- as.integer(ed$key[1])
        x <- dados()
        if (is.null(x) || is.null(x$ep_m) || !nrow(x$ep_m)) return()
        
        ep_m <- x$ep_m
        ep_m$row_id <- seq_len(nrow(ep_m))
        linha <- ep_m[ep_m$row_id == row_id, , drop = FALSE]
        if (!nrow(linha)) return()
        
        objeto <- objs_tmp |> filter(NAME_OBJETO == linha$OBJETO & NAME_SETOR == linha$SETOR)
        # --- defina quais câmeras usar ---
        cameras_ids   <- unique(purrr::map_int(objeto$CONFIG[[1]]$COMPONENTES[[1]]$CAMERA, "CD_ID_CAMERA"))
      
        video_clip_open(
          ns, input, output, session,
          pool       = dbp$get_pool(),
          title      = paste0("Clip – ", linha$OBJETO, " / ", linha$COMPONENTE, " (", linha$ESTADO, ")"),
          time_begin = linha$start_time,
          time_end   = linha$end_time,
          camera_ids = cameras_ids,
          fps        = 5L,
          max_frames = 3000L
        )
      })
      
    }, ignoreInit = TRUE,ignoreNULL = TRUE)

  },once = TRUE,ignoreInit = TRUE)

}

# ===============================
# Helper para box/plot container
# ===============================
box_value_html <- function(main, l1 = NULL, l2 = NULL) {
  htmltools::HTML(paste0(
    "<div style='line-height:1.05'>",
      "<div style='font-size:28px;font-weight:700;'>", main, "</div>",
      "<div style='font-size:12px;opacity:.95;margin-top:6px'>", l1 %||% "", "</div>",
      "<div style='font-size:11px;opacity:.85;margin-top:2px'>", l2 %||% "", "</div>",
    "</div>"
  ))
}

cap_episodes <- function(ep, dt_de, dt_ate) {
  if (is.null(ep) || !nrow(ep)) return(ep)

  dt_de  <- as.POSIXct(dt_de,  tz = attr(ep$start_time, "tzone") %||% Sys.timezone())
  dt_ate <- as.POSIXct(dt_ate, tz = attr(ep$start_time, "tzone") %||% Sys.timezone())

  ep$start_time <- pmax(ep$start_time, dt_de)
  ep$end_time   <- pmin(ep$end_time,   dt_ate)
  ep$dur_secs   <- as.numeric(difftime(ep$end_time, ep$start_time, units = "secs"))

  ep <- ep[!is.na(ep$dur_secs) & ep$dur_secs > 0, , drop = FALSE]
  ep
}

.sql_escape <- function(x) gsub("'", "''", as.character(x), fixed = TRUE)

.sql_in <- function(vec) {
  vec <- vec[!is.na(vec) & nzchar(vec)]
  if (!length(vec)) return(NULL)
  paste0("('", paste(.sql_escape(vec), collapse = "','"), "')")
}

fmt_sql_dt <- function(x) format(as.POSIXct(x), "%Y-%m-%d %H:%M:%S")
fmt_dt <- function(x) format(as.POSIXct(x), "%d/%m/%Y %H:%M")
fmt_d  <- function(x) format(as.Date(x), "%d/%m/%Y")
fmt_hm <- function(mins) {
  if (is.null(mins)) return("–")

  x <- suppressWarnings(as.numeric(mins))
  out <- rep("–", length(x))

  ok <- !is.na(x)
  if (!any(ok)) return(out)

  mins_i <- as.integer(round(x[ok]))
  h <- mins_i %/% 60
  m <- mins_i %% 60

  out[ok] <- sprintf("%d h %02d min", h, m)
  out
}

dt_lang_ptbr <- function() {
  list(
    sEmptyTable     = "Nenhum registro encontrado",
    sInfo           = "Mostrando de _START_ até _END_ de _TOTAL_ registros",
    sInfoEmpty      = "Mostrando 0 até 0 de 0 registros",
    sInfoFiltered   = "(filtrado de _MAX_ registros no total)",
    sLengthMenu     = "Mostrar _MENU_ registros",
    sLoadingRecords = "Carregando...",
    sProcessing     = "Processando...",
    sSearch         = "Pesquisar:",
    sZeroRecords    = "Nenhum registro encontrado",
    oPaginate = list(
      sNext     = "Próximo",
      sPrevious = "Anterior",
      sFirst    = "Primeiro",
      sLast     = "Último"
    ),
    oAria = list(
      sSortAscending  = ": ativar para ordenar a coluna em ordem crescente",
      sSortDescending = ": ativar para ordenar a coluna em ordem decrescente"
    )
  )
}

insertNewPlotComponent <- function(ns, title, id){
  child <- ns(paste0('plot_', id))
  boxid <- ns(paste0('plotbox_', id))

  div(
    id = ns(paste0('child-', child)),
    box(
      id = boxid,
      solidHeader = TRUE,
      collapsible = TRUE,
      title = tags$span(title, style = 'font-size: 16px;'),
      width = 6,
      # absolutePanel(
      #   height = 45,
      #   width  = 'auto',
      #   top    = 5,
      #   right  = 35,
      #   div(
      #     actionButton(
      #       inputId = ns(paste0('bteye', id)),
      #       label   = '',
      #       icon    = icon('window-maximize')
      #     )
      #   )
      # ),
      div(
        style = 'padding: 15px; height: auto; width: 100%;',
        plotly$plotlyOutput(ns(paste0("plotout_", id))) |> shinycssloaders::withSpinner(color = 'lightblue')
      )
    )
  )
}
