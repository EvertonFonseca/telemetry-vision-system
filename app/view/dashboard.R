box::use(
  shiny[...],
  shinyjs,
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
                              nome_setup            = "SETUP",
                              min_parado_secs       = 60,
                              by_cols               = c("SETOR","OBJETO"),
                              max_gap_secs          = Inf) {

  stopifnot("DATE_TIME" %in% names(df))
  need <- c("DATE_TIME", col_estado_maquina, col_trabalhador, by_cols)
  miss <- setdiff(need, names(df))
  if (length(miss)) {
    warning("apply_setup_state(): colunas ausentes: ", paste(miss, collapse = ", "))
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
build_episodes <- function(df, min_secs = 60) {
  if (!nrow(df)) return(NULL)

  base_cols <- c("DATE_TIME", "OBJETO", "SETOR")
  attr_cols <- setdiff(names(df), base_cols)

  # só colunas categóricas
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

  make_ep <- function(d) {
    d <- d[order(d$DATE_TIME), ]
    n <- nrow(d)
    if (n == 0) return(NULL)

    times <- d$DATE_TIME
    state <- d$ESTADO

    # smoothing: troca com dt<min_secs => mantém anterior
    clean <- state
    if (n >= 2L) {
      prev <- state[1]
      for (i in 2:n) {
        dt <- as.numeric(difftime(times[i], times[i-1], units = "secs"))
        if (!is.na(dt) && state[i] != prev && dt < min_secs) {
          clean[i] <- prev
        } else {
          prev <- state[i]
          clean[i] <- prev
        }
      }
    }

    # end_time por amostra: vai até o próximo registro (último vai até agora)
    end_obs <- c(times[-1], Sys.time())
    end_obs[end_obs < times] <- times[end_obs < times]

    # índices de início de cada run
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
          color = "red"
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

  pool <- dbp$get_pool()

  # aqui você pode ajustar setor/objeto conforme necessário
  setor_alvo  <- c("DOBRA","ESTAMPARIA")
  objeto_alvo <- c("SETREMA","DOBRA")
  
  # estados da máquina que queremos monitorar
  estados_maquina <- c("OPERANDO", "PARADO", "SETUP")
  
  dados <- reactive({
    invalidateLater(60 * 5 * 1000, session)
    
    df_raw <- run_query(
      pool,
      minutos = 24 * 60,     # (se sua SQL NÃO filtra tempo, isso aqui não tem efeito)
      setor   = NULL,
      objeto  = NULL
    )
    if (!nrow(df_raw)) return(NULL)
    
    df <- expand_atributos(df_raw)
    
    df <- apply_setup_state(df,
      col_estado_maquina = "BOBINA.ESTADO",
      col_trabalhador    = "AREA_DE_TRABALHO_A.TRABALHADOR",  # <- confira o nome exato
      min_parado_secs    = 60,
      by_cols            = c("SETOR","OBJETO")
    )
    
    df <- apply_setup_state(df,
      col_estado_maquina = "PRENSA.ESTADO",
      col_trabalhador    = "AREA_DE_TRABALHO_B.TRABALHADOR",
      min_parado_secs    = 60,
      by_cols            = c("SETOR","OBJETO")
    )
    
    df <- apply_setup_state(df,
      col_estado_maquina = "DOBRA.ESTADO",
      col_trabalhador    = "AREA_DE_TRABALHO.TRABALHADOR",
      min_parado_secs    = 60,
      by_cols            = c("SETOR","OBJETO")
    )
    
    df <- apply_setup_state(df,
      col_estado_maquina = "SOLDA.ESTADO",
      col_trabalhador    = "AREA_DE_TRABALHO.TRABALHADOR",
      min_parado_secs    = 60,
      by_cols            = c("SETOR","OBJETO")
    )
    

    ep <- build_episodes(df, min_secs = 60)
    if (is.null(ep) || !nrow(ep)) return(NULL)
    
    ep_m <- subset(ep, ESTADO %in% estados_maquina)
    if (!nrow(ep_m)) ep_m <- ep[0, ]
    
    list(
      df_raw = df_raw,  # <- “todos os dados do SQL”
      df     = df,      # <- expandido
      ep     = ep,      # <- episódios de tudo
      ep_m   = ep_m     # <- só OPERANDO/PARADO/SETUP
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
    ep_m <- ep_m_reactive()
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
    
    # filtra DOBRA + SETREMA e apenas os componentes escolhidos
    ep_m <- subset(
      ep_m,
      OBJETO %in% c("DOBRA","SETREMA") &
      COMPONENTE %in% c("DOBRA.ESTADO","PRENSA.ESTADO")
    )
    if (!nrow(ep_m)) return(NULL)
    
    ep_m$mid_time <- ep_m$start_time + ep_m$dur_secs / 2
    ep_m$turno    <- classificar_turno(ep_m$mid_time)
    ep_m          <- ep_m[!is.na(ep_m$turno), ]
    if (!nrow(ep_m)) return(NULL)
    
    # agrega por turno + objeto + estado
    df <- aggregate(dur_secs ~ turno + OBJETO + ESTADO, data = ep_m, sum, na.rm = TRUE)
    df$turno <- factor(df$turno, levels = c("Manhã","Tarde","Noite"))
    
    # chaves (turno, objeto)
    keys <- unique(df[, c("turno","OBJETO")])
    keys <- keys[order(keys$turno, keys$OBJETO), ]
    
    linhas <- lapply(seq_len(nrow(keys)), function(i){
      tu <- keys$turno[i]
      ob <- keys$OBJETO[i]
      
      d <- df[df$turno == tu & df$OBJETO == ob, ]
      
      m_op <- sum(d$dur_secs[d$ESTADO == "OPERANDO"], na.rm = TRUE) / 60
      m_pa <- sum(d$dur_secs[d$ESTADO == "PARADO"],   na.rm = TRUE) / 60
      m_se <- sum(d$dur_secs[d$ESTADO == "SETUP"],    na.rm = TRUE) / 60
      
      m_tot <- m_op + m_pa + m_se
      util  <- if (m_tot > 0) m_op / m_tot * 100 else NA_real_
      
      data.frame(
        Turno          = as.character(tu),
        Maquina        = as.character(ob),
        Tempo_OPERANDO = fmt_hm(m_op),
        Tempo_PARADO   = fmt_hm(m_pa),
        Tempo_SETUP    = fmt_hm(m_se),
        Tempo_TOTAL    = fmt_hm(m_tot),
        Utilizacao     = round(util, 1),
        stringsAsFactors = FALSE
      )
    })
    
    out <- do.call(rbind, linhas)
    if (is.null(out) || !nrow(out)) return(NULL)
    
    out <- out[order(-out$Utilizacao), ]
    out$Ranking <- seq_len(nrow(out))
    out
  })


  # resumo para os valueBoxes (reaproveitado nos 4 boxes)
  resumo_setor <- reactive({
    ep_m <- ep_m_reactive()
    if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
    
    setor_state <- aggregate(dur_secs ~ SETOR + ESTADO, data = ep_m, sum, na.rm = TRUE)
    setor_state_alvo <- subset(setor_state, SETOR %in% setor_alvo)
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
    mins <- as.integer(round(as.numeric(x) * 60))  # x está em horas
    h <- mins %/% 60
    m <- mins %% 60
    sprintf("%d h %02d min", h, m)
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
        title = tags$span("Resumo por turno – últimas 24 horas", 
        style = "font-size: 16px;"),
        div(
          style = "padding: 10px;",
          tags$p(
            "Tabela executiva destacando os turnos com melhor desempenho operacional ",
            "da máquina, considerando os estados OPERANDO, PARADO e SETUP ",
            "(janela móvel de 24 horas).",
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
    
    obj_sel <- objeto_alvo
    ep_obj  <- subset(ep_m, OBJETO %in% obj_sel)
    if (!nrow(ep_obj)) return(NULL)
    
    # Preferir apenas componentes de ESTADO de máquina
    ep_obj_estado <- subset(ep_obj, grepl("\\.ESTADO$", COMPONENTE))
    if (nrow(ep_obj_estado)) ep_obj <- ep_obj_estado
    
    # classificar episódio pelo meio do intervalo
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
        color = ESTADO,
        text  = paste0(
          "Máquina: ", OBJETO,
          "<br>Componente: ", COMPONENTE,
          "<br>Estado: ", ESTADO,
          "<br>Início: ", fmt_dt(start_time),
          "<br>Fim: ",    fmt_dt(end_time),
          "<br>Duração: ", round(dur_secs/60, 1), " min"
        )
      )
    ) +
    ggplot2$geom_segment(linewidth = 6) +
    ggplot2$labs(
      x     = "Tempo",
      y     = "",
      color = "Estado"
    ) +
    # ✅ agora tem facet por turno (linhas) e por máquina (colunas)
    ggplot2$facet_grid(turno ~ OBJETO, scales = "free_y", switch = "y") +
    ggplot2$scale_color_manual(
      values = c(
        "OPERANDO" = "#00a65a",
        "PARADO"   = "tomato",
        "SETUP"    = "gold"
      )
    ) +
    themasPlotyGGplot(text.x.angle = 45)

    p <- plotly::ggplotly(g, tooltip = "text") |> plotly::config(displaylogo = FALSE,displayModeBar = T)
    layoutPlotyDefault(p, legend.text = "Estado")
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
    if (is.null(df)) return(NULL)
    
    names(df) <- c(
      "Período",
      "Máquina",
      "Tempo OPERANDO",
      "Tempo PARADO",
      "Tempo SETUP",
      "Tempo total",
      "Utilização (%)",
      "Ranking"
    )
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom        = "tip",
        pageLength = 5,
        order      = list(list(7, "asc")),  # Ranking (0-based na UI do DT é ok assim)
        language   = dt_lang_ptbr()
      ),
      class = "cell-border stripe compact"
    )
  })

}


# ===============================
# Helper para box/plot container
# ===============================
fmt_dt <- function(x) format(as.POSIXct(x, tz = Sys.timezone()), "%d/%m/%Y %H:%M")
fmt_d  <- function(x) format(as.Date(x), "%d/%m/%Y")
fmt_hm <- function(mins) {
  if (is.null(mins) || length(mins) == 0 || is.na(mins)) return("–")
  mins_i <- as.integer(round(as.numeric(mins)))
  h <- mins_i %/% 60
  m <- mins_i %% 60
  sprintf("%d h %02d min", h, m)
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
