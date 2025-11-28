box::use(
  shiny[...],
  shinyjs,
  ggplot2,
  plotly,
  jsonlite,
  dplyr[...],
  tidyr[unnest_wider],
  lubridate,
  shinydashboardPlus[box],
  stats[aggregate, na.omit, median],
  DBI,
  DT,
  dbp  = ../infra/db_pool,
  ./global[ actionWebUser,tagAppendAttributesFind,debugLocal]
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
# SQL – focado na máquina DOBRA
# ===============================

build_sql <- function(minutos,
                      setor  = NULL,
                      objeto = NULL) {

  base <- paste(
    "SELECT 
        oc.DATA_OC     AS ATRIBUTOS,
        f.DT_HR_LOCAL  AS DATE_TIME,
        o.NAME_OBJETO  AS OBJETO,
        s.NAME_SETOR   AS SETOR
     FROM objeto_contexto oc
     LEFT JOIN objeto o       ON o.CD_ID_OBJETO = oc.CD_ID_OBJETO
     LEFT JOIN setor s        ON s.CD_ID_SETOR  = o.CD_ID_SETOR
     LEFT JOIN frame_camera f ON f.CD_ID_FRAME  = oc.CD_ID_FRAME"
  )

  where <- c()

  if (!is.null(setor)) {
    where <- c(where, sprintf("s.NAME_SETOR  = '%s'", setor))
  }
  if (!is.null(objeto)) {
    where <- c(where, sprintf("o.NAME_OBJETO = '%s'", objeto))
  }

  # (opcional) janela de tempo em minutos – se quiser, ative:
  # where <- c(where,
  #            sprintf("f.DT_HR_LOCAL >= (UTC_TIMESTAMP() - INTERVAL %d MINUTE)", as.integer(minutos)))

  if (length(where)) {
    base <- paste0(base, "\nWHERE ", paste(where, collapse = " AND "), "\n")
  }

  base
}

run_query <- function(pool,
                      minutos,
                      setor  = NULL,
                      objeto = NULL) {
  sql <- build_sql(minutos, setor = setor, objeto = objeto)
  DBI::dbGetQuery(pool, sql)
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
    mutate(DATE_TIME = as.POSIXct(DATE_TIME, tz = Sys.timezone())) |>
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
                              nome_setup            = "SETUP") {

  if (!all(c(col_estado_maquina, col_trabalhador) %in% names(df))) {
    warning("Colunas não encontradas para SETUP: ",
            col_estado_maquina, " / ", col_trabalhador)
    return(df)
  }

  est <- df[[col_estado_maquina]]
  trab <- df[[col_trabalhador]]

  df[[col_estado_maquina]] <- dplyr::case_when(
    !is.na(est) & est == estado_parado &
      !is.na(trab) & trab == trabalhador_presente ~ nome_setup,
    TRUE ~ est
  )

  df
}

# ===============================
# Episódios de estado (run-length)
# ===============================
build_episodes <- function(df, min_secs = 60) {
  if (!nrow(df)) return(NULL)

  base_cols <- c("DATE_TIME", "OBJETO", "SETOR")
  attr_cols <- setdiff(names(df), base_cols)

  # somente colunas categóricas (character/factor)
  attr_cols <- attr_cols[vapply(df[attr_cols], function(v) is.character(v) || is.factor(v), logical(1))]
  if (!length(attr_cols)) return(NULL)

  episodes <- list()

  for (col in attr_cols) {
    sub <- df[!is.na(df[[col]]), c("DATE_TIME", "OBJETO", "SETOR", col), drop = FALSE]
    if (!nrow(sub)) next

    sub  <- sub[order(sub$OBJETO, sub$DATE_TIME), ]
    objs <- unique(sub$OBJETO)

    for (obj in objs) {
      idx <- sub$OBJETO == obj
      tmp <- sub[idx, ]
      n   <- nrow(tmp)
      if (n == 0) next

      estado_raw <- as.character(tmp[[col]])
      times      <- as.POSIXct(tmp$DATE_TIME, tz = attr(tmp$DATE_TIME, "tzone"))

      # Smoothing temporal: muda de estado em < min_secs => mantém estado anterior
      clean <- estado_raw
      if (n >= 2L) {
        prev_state <- estado_raw[1]
        for (i in 2:n) {
          dt <- as.numeric(difftime(times[i], times[i - 1], units = "secs"))
          if (!is.na(dt) && estado_raw[i] != prev_state && dt < min_secs) {
            clean[i] <- prev_state
          } else {
            prev_state <- estado_raw[i]
            clean[i]   <- prev_state
          }
        }
      }

      # Run-length: cria episódios contíguos (agora protegido para n == 1)
      start_idx <- 1L
      cur_state <- clean[1]

      if (n >= 2L) {
        for (i in 2:n) {
          # evita if(NA)
          if (!is.na(clean[i]) && clean[i] != cur_state) {
            start_time <- times[start_idx]
            end_time   <- times[i - 1L]
            dur        <- as.numeric(difftime(end_time, start_time, units = "secs"))

            episodes[[length(episodes) + 1L]] <- data.frame(
              SETOR      = tmp$SETOR[1],
              OBJETO     = obj,
              COMPONENTE = col,
              ESTADO     = cur_state,
              start_time = start_time,
              end_time   = end_time,
              dur_secs   = dur,
              stringsAsFactors = FALSE
            )

            start_idx <- i
            cur_state <- clean[i]
          }
        }
      }

      # último episódio (sempre, mesmo quando n == 1)
      start_time <- times[start_idx]
      end_time   <- times[n]
      dur        <- as.numeric(difftime(end_time, start_time, units = "secs"))

      episodes[[length(episodes) + 1L]] <- data.frame(
        SETOR      = tmp$SETOR[1],
        OBJETO     = obj,
        COMPONENTE = col,
        ESTADO     = cur_state,
        start_time = start_time,
        end_time   = end_time,
        dur_secs   = dur,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(episodes)) return(NULL)
  ep <- do.call(rbind, episodes)
  ep$SETOR      <- as.character(ep$SETOR)
  ep$OBJETO     <- as.character(ep$OBJETO)
  ep$COMPONENTE <- as.character(ep$COMPONENTE)
  ep$ESTADO     <- as.character(ep$ESTADO)
  ep
}

# ===============================
# Métricas de utilização diária
# ===============================

build_daily_util <- function(ep) {
  if (is.null(ep) || !nrow(ep)) return(NULL)

  ep$date <- as.Date(ep$start_time)

  tot <- aggregate(dur_secs ~ date, data = ep, sum, na.rm = TRUE)
  op  <- subset(ep, ESTADO == "OPERANDO")
  op  <- aggregate(dur_secs ~ date, data = op, sum, na.rm = TRUE)
  names(op)[2] <- "dur_op"

  dfu <- merge(tot, op, by = "date", all.x = TRUE)
  dfu$dur_op[is.na(dfu$dur_op)] <- 0
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

  h  <- lubridate$hour(dt)
  m  <- lubridate$minute(dt)
  mm <- h * 60 + m  # minutos desde 00:00

  turno_chr <- dplyr::case_when(
    # Amanhã: 07:30–17:18
    mm >= (7 * 60 + 30) & mm <= (17 * 60 + 18) ~ "Amanhã",

    # Tarde: 17:19–00:00
    mm >= (17 * 60 + 19) | mm == 0             ~ "Tarde",

    # Noite: 00:01–07:29
    mm >= 1 & mm <= (7 * 60 + 29)              ~ "Noite",

    TRUE                                       ~ NA_character_
  )

  factor(turno_chr, levels = c("Amanhã", "Tarde", "Noite"))
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

      # Box 1 – horas OPERANDO
      column(3,
        shinydashboard::valueBox(
          width = '100%',
          value = uiOutput(ns(paste0('vbOperando', 1))),
          subtitle = 'Tempo OPERANDO (setor DOBRA E SOLDA)',
          icon = icon('play'),
          color = "green"
        ) |> tagAppendAttributesFind(target = 1, style = 'min-height: 102px')
      ),

      # Box 2 – horas PARADO
      column(3,
        shinydashboard::valueBox(
          width = '100%',
          value = uiOutput(ns(paste0('vbParado', 1))),
          subtitle = 'Tempo PARADO (setor DOBRA E SOLDA)',
          icon = icon('pause'),
          color = "orange"
        ) |> tagAppendAttributesFind(target = 1, style = 'min-height: 102px')
      ),

      # Box 3 – horas SETUP
      column(3,
        shinydashboard::valueBox(
          width = '100%',
          value = uiOutput(ns(paste0('vbSetup',1))),
          subtitle = 'Tempo em SETUP (setor DOBRA E SOLDA)',
          icon = icon('tools'),
          color = "yellow"
        ) |> tagAppendAttributesFind(target = 1, style = 'min-height: 102px')
      ),

      # Box 4 – utilização média
      column(3,
        shinydashboard::valueBox(
          width = '100%',
          value = uiOutput(ns(paste0('vbUtilizacao',1))),
          subtitle = 'Utilização média do setor',
          icon = icon('chart-line'),
          color = "red"
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

  pool <- dbp$get_pool()

  # aqui você pode ajustar setor/objeto conforme necessário
  setor_alvo  <- "DOBRA E SOLDA"
  objeto_alvo <- "DOBRA"

  # estados da máquina que queremos monitorar
  estados_maquina <- c("OPERANDO", "PARADO", "SETUP")

  # -------------------------
  # Fonte de dados reativa (refresh a cada 1 minuto)
  # -------------------------
  ep_m_reactive <- reactive({

    # força reexecução a cada 60.000 ms
    invalidateLater(60 * 5 * 1000, session)

    # 1) busca dados no banco
    df_raw <- run_query(
      pool,
      minutos = 24 * 60,
      setor   = setor_alvo,
      objeto  = objeto_alvo
    )

    if (!nrow(df_raw)) return(NULL)

    # 2) expande atributos JSON
    df <- expand_atributos(df_raw)

    # 3) aplica regra de SETUP (PARADO + TRABALHADOR TRABALHANDO)
    df <- apply_setup_state(
      df,
      col_estado_maquina = "DOBRA.ESTADO",                 # estado da máquina
      col_trabalhador    = "AREA_DE_TRABALHO.TRABALHADOR"  # presença do trabalhador
    )

    # 4) constrói episódios de estado
    ep <- build_episodes(df, min_secs = 60)
    if (is.null(ep) || !nrow(ep)) return(NULL)
    
    # 5) filtra somente estados da máquina
    ep_m <- subset(ep, ESTADO %in% estados_maquina)
    if (!nrow(ep_m)) return(NULL)
    
    ep_m
  })
  
  # -------------------------
  # Resumo por turno (para tabela executiva)
  # -------------------------
  resumo_turnos <- reactive({
    ep_m <- ep_m_reactive()
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
    
    # meio do episódio -> turno
    ep_m$mid_time <- ep_m$start_time + ep_m$dur_secs / 2
    ep_m$turno    <- classificar_turno(ep_m$mid_time)
    ep_m          <- ep_m[!is.na(ep_m$turno), ]
    if (!nrow(ep_m)) return(NULL)
    
    # agrega duração por turno + estado
    df <- aggregate(
      dur_secs ~ turno + ESTADO,
      data = ep_m,
      sum,
      na.rm = TRUE
    )
    
    # garante ordem de turnos
    df$turno <- factor(df$turno, levels = c("Amanhã","Tarde","Noite"))
    
    turnos <- levels(df$turno)
    turnos <- turnos[turnos %in% df$turno]
    
    linhas <- lapply(turnos, function(tu) {
      d <- df[df$turno == tu, ]
      
      h_op <- sum(d$dur_secs[d$ESTADO == "OPERANDO"], na.rm = TRUE) / 3600
      h_pa <- sum(d$dur_secs[d$ESTADO == "PARADO"],   na.rm = TRUE) / 3600
      h_se <- sum(d$dur_secs[d$ESTADO == "SETUP"],    na.rm = TRUE) / 3600
      
      h_tot <- h_op + h_pa + h_se
      util  <- if (h_tot > 0) h_op / h_tot * 100 else NA_real_
      
      data.frame(
        Turno          = tu,
        Horas_OPERANDO = h_op,
        Horas_PARADO   = h_pa,
        Horas_SETUP    = h_se,
        Horas_TOTAIS   = h_tot,
        Utilizacao     = util,
        stringsAsFactors = FALSE
      )
    })
    
    out <- do.call(rbind, linhas)
    if (!nrow(out)) return(NULL)
    
    # ranking pelos melhores turnos
    out <- out[order(-out$Utilizacao), ]
    out$Ranking <- seq_len(nrow(out))
    
    # arredonda
    out$Horas_OPERANDO <- round(out$Horas_OPERANDO, 1)
    out$Horas_PARADO   <- round(out$Horas_PARADO,   1)
    out$Horas_SETUP    <- round(out$Horas_SETUP,    1)
    out$Horas_TOTAIS   <- round(out$Horas_TOTAIS,   1)
    out$Utilizacao     <- round(out$Utilizacao,     1)
    
    out
  })


  # resumo para os valueBoxes (reaproveitado nos 4 boxes)
  resumo_setor <- reactive({
    ep_m <- ep_m_reactive()
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)

    setor_state <- aggregate(dur_secs ~ SETOR + ESTADO, data = ep_m, sum, na.rm = TRUE)
    setor_state_alvo <- subset(setor_state, SETOR == setor_alvo)
    if (!nrow(setor_state_alvo)) return(NULL)

    get_horas <- function(estado) {
      x <- setor_state_alvo$dur_secs[setor_state_alvo$ESTADO == estado]
      if (!length(x)) return(0)
      as.numeric(x) / 3600
    }

    h_op  <- get_horas("OPERANDO")
    h_pa  <- get_horas("PARADO")
    h_se  <- get_horas("SETUP")
    h_tot <- h_op + h_pa + h_se
    util  <- if (h_tot > 0) (h_op / h_tot) * 100 else NA_real_

    list(
      h_op  = h_op,
      h_pa  = h_pa,
      h_se  = h_se,
      h_tot = h_tot,
      util  = util
    )
  })

fmt_horas <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("–")
  mins <- as.integer(round(as.numeric(x) * 60))
  h <- mins %/% 60
  m <- mins %% 60
  sprintf("%d:%02d h", h, m)
}
  fmt_perc  <- function(x) ifelse(is.na(x), "–", sprintf("%.1f %%", x))

  # -------------------------
  # ValueBoxes (sempre lendo resumo_setor())
  # -------------------------
  output[[paste0("vbOperando",1)]] <- renderUI({
    r <- resumo_setor()
    if (is.null(r)) return("–")
    HTML(sprintf("<b>%s</b>", fmt_horas(r$h_op)))
  })

  output[[paste0("vbParado",1)]] <- renderUI({
    r <- resumo_setor()
    if (is.null(r)) return("–")
    HTML(sprintf("<b>%s</b>", fmt_horas(r$h_pa)))
  })

  output[[paste0("vbSetup",1)]] <- renderUI({
    r <- resumo_setor()
    if (is.null(r)) return("–")
    HTML(sprintf("<b>%s</b>", fmt_horas(r$h_se)))
  })

  output[[paste0("vbUtilizacao",1)]] <- renderUI({
    r <- resumo_setor()
    if (is.null(r)) return("–")
    HTML(sprintf("<b>%s</b>", fmt_perc(r$util)))
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
        title = tags$span("Resumo por turno – últimas 24 horas (setor DOBRA E SOLDA)", 
        style = "font-size: 16px;"),
        div(
          style = "padding: 10px;",
          tags$p(
            "Tabela executiva destacando os turnos com melhor desempenho operacional ",
            "da máquina DOBRA, considerando os estados OPERANDO, PARADO e SETUP ",
            "(janela móvel de 24 horas).",
            style = "margin-bottom: 10px; font-size: 12px; color: #444;"
          ),
          DT::dataTableOutput(ns("tblTurnos")) |> shinycssloaders::withSpinner(color = 'lightblue')
        )
      ),
      insertNewPlotComponent(ns, "1) Gráfico por Setor – Operando / Parada / Setup", 1),
      insertNewPlotComponent(ns, "2) Linha do Tempo (Gantt) – Máquina DOBRA",       2),
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
    df_plot$turno  <- factor(df_plot$turno, levels = c("Amanhã","Tarde","Noite"))
    
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
        "OPERANDO" = "dodgerblue",
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
    
    obj_sel <- objeto_alvo
    ep_obj  <- subset(ep_m, OBJETO == obj_sel)
    if (!nrow(ep_obj)) return(NULL)
    
    # Preferir apenas componentes de ESTADO de máquina
    ep_obj_estado <- subset(ep_obj, grepl("\\.ESTADO$", COMPONENTE))
    if (nrow(ep_obj_estado)) ep_obj <- ep_obj_estado
    
    # classificar episódio pelo meio do intervalo
    ep_obj$mid_time <- ep_obj$start_time + ep_obj$dur_secs / 2
    ep_obj$turno    <- classificar_turno(ep_obj$mid_time)
    
    g <- ggplot2$ggplot(
      ep_obj,
      ggplot2$aes(
        x     = start_time,
        xend  = end_time,
        y     = COMPONENTE,
        yend  = COMPONENTE,
        color = ESTADO
      )
    ) +
    ggplot2$geom_segment(linewidth = 6) +
    ggplot2$labs(
      x     = "Tempo",
      y     = "",
      color = "Estado"
    ) +
    ggplot2$facet_grid(turno ~ ., scales = "free_y",switch = "y") +
    ggplot2$scale_color_manual(
      values = c(
        "OPERANDO" = "dodgerblue",
        "PARADO"   = "tomato",
        "SETUP"    = "gold"
      )
    ) +
    themasPlotyGGplot()

    p <- plotly::ggplotly(g, tooltip = c("x","xend","color")) |> plotly::config(displaylogo = FALSE,displayModeBar = T)
    layoutPlotyDefault(p, legend.text = "Estado")
  })

  # ---------------------------
  # (3) Heatmap – Máquinas x Horário
  # ---------------------------
  output[[paste0("plotout_",3)]] <- plotly$renderPlotly({
    ep_m <- ep_m_reactive()
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
    
    ep_hm <- ep_m
    
    # tempo médio do episódio
    ep_hm$mid_time <- ep_hm$start_time + ep_hm$dur_secs / 2
    ep_hm$hora     <- format(ep_hm$mid_time, "%H:%M")
    
    # classificar turno a partir do mid_time
    ep_hm$turno <- classificar_turno(ep_hm$mid_time)
    
    hm <- aggregate(
      dur_secs ~ OBJETO + hora + ESTADO + turno,
      data = ep_hm,
      sum,
      na.rm = TRUE
    )
    
    hm_op <- subset(hm, ESTADO == "OPERANDO")
    if (!nrow(hm_op)) return(NULL)
    hm_op$horas <- hm_op$dur_secs / 3600
    
    g <- ggplot2$ggplot(
      hm_op,
      ggplot2$aes(x = hora, y = OBJETO, fill = horas)
    ) +
    ggplot2$geom_tile(color = "gray80") +
    ggplot2$facet_grid(turno ~ ., scales = "free_y",switch = "y") +
    ggplot2$labs(
      x     = "Horário",
      y     = "Máquina / Objeto",
      fill  = "Horas OPERANDO"
    ) +
    themasPlotyGGplot(text.x.angle = 45)
    
    p <- plotly::ggplotly(g, tooltip = c("x","y","fill")) |> plotly::config(displaylogo = FALSE,displayModeBar = T)
    layoutPlotyDefault(p, legend.text = "Atividade")

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
    if (is.null(df)) return(NULL)
    
    # renomeia colunas para ficar legível na UI
    df_display <- df
    names(df_display) <- c(
      "Turno",
      "Horas OPERANDO",
      "Horas PARADO",
      "Horas SETUP",
      "Horas totais",
      "Utilização (%)",
      "Ranking"
    )
    
    DT::datatable(
      df_display,
      rownames = FALSE,
      options = list(
        dom        = "tip",   # apenas tabela + info + paginação
        pageLength = 5,
        order      = list(list(6, "asc")),  # ordena por Ranking (coluna 7 visual -> índice 6),
        language   = dt_lang_ptbr()
      ),
      class = "cell-border stripe compact"
    )
  })


}


# ===============================
# Helper para box/plot container
# ===============================
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
