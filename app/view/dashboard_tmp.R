# app/view/dashboard.R
# ===============================================================
# Dashboard (mirai async + cache de dados opcional + Flow vertical)
# - Mantém: filtros, auto-refresh, mirai, caches, valueBoxes, tabela
# - Remove: plots antigos (1/2/4/5) e substitui por Flow (flow_graphic.R)
# - Flow mostra: % por setor (todas máquinas) + mini-gantt por máquina
# - Clique no segmento do gantt (painel direito) -> abre clip
# ===============================================================

box::use(
  shiny[
    NS, tagList, div, tags, br,
    fluidRow, column,
    reactive, reactiveVal, reactiveValues,
    observeEvent, isolate, req,
    renderUI, uiOutput,
    showNotification, removeNotification,
    actionButton, icon
  ],
  shinyjs,
  shinyWidgets[airDatepickerInput, timepickerOptions, updateAirDateInput],
  shinycssloaders,
  ggplot2,
  plotly,
  jsonlite,
  dplyr[...],
  tidyr[unnest_wider],
  tibble,
  purrr,
  utils[...],
  lubridate,
  shinydashboardPlus[box],
  shinydashboard,
  stats[aggregate, na.omit, median],
  DBI,
  RMariaDB[MariaDB],
  promises,
  mirai,
  DT,
  cachem,
  digest,
  later,
  parallel,
  htmltools[HTML],
  dbp  = ../infra/db_pool,
  ./global[ actionWebUser, tagAppendAttributesFind, debugLocal, removeProgressLoader, newProgressLoader],
  ../logic/objeto_dao[selectAllObjetos],
  ./video_clip[video_clip_open],
  flow = ./flow_graphic,
  ../logic/dashboard_dao[
    run_query, fetch_choices,
    tz_local, to_utc, from_utc,
    merge_setrema_bobina_prensa
  ]
)

# ===============================================================
# ✅ Cache ON/OFF via option(USE_CACHE)
# ===============================================================
.cache_enabled <- function() {
  isTRUE(getOption("USE_CACHE", TRUE))
}

# ===============================================================
# Cache em memória (apenas para DADOS do dashboard) - opcional
# ===============================================================
.cache_data <- cachem::cache_mem(
  max_size = as.numeric(Sys.getenv("TVS_CACHE_MAX_MB", "50")) * 1024^2
)
.CACHE_NULL <- structure(list(), class = "CACHE_NULL")

.cache_get_unwrap <- function(key) {
  if (!.cache_enabled()) return(structure(list(), class = "key_missing"))
  hit <- .cache_data$get(key)
  if (inherits(hit, "key_missing")) return(structure(list(), class = "key_missing"))
  if (inherits(hit, "CACHE_NULL")) return(NULL)
  hit
}
.cache_set_wrap <- function(key, value) {
  if (!.cache_enabled()) return(invisible(FALSE))
  .cache_data$set(key, if (is.null(value)) .CACHE_NULL else value)
  invisible(TRUE)
}

# ===============================================================
# SEMÁFORO GLOBAL (limita jobs por processo)
# ===============================================================
.async_sem <- new.env(parent = emptyenv())
.async_sem$running     <- 0L
.async_sem$max_running <- as.integer(Sys.getenv("TVS_MAX_ASYNC", Sys.getenv("TVS_MAX_SYNC", "1")))
.async_sem$retry_s     <- 0.8

.sem_try_acquire <- function() {
  r <- suppressWarnings(as.integer(.async_sem$running))
  m <- suppressWarnings(as.integer(.async_sem$max_running))
  if (is.na(r) || r < 0L) r <- 0L
  if (is.na(m) || m < 1L) m <- 1L
  .async_sem$running <- r
  .async_sem$max_running <- m

  if (.async_sem$running >= .async_sem$max_running) return(FALSE)
  .async_sem$running <- .async_sem$running + 1L
  TRUE
}
.sem_release <- function() {
  r <- suppressWarnings(as.integer(.async_sem$running))
  if (is.na(r) || r < 0L) r <- 0L
  .async_sem$running <- max(0L, r - 1L)
  invisible(TRUE)
}

# ===============================================================
# mirai - tenta subir aqui se não bootou no app.R
# ===============================================================
.mirai_state <- new.env(parent = emptyenv())
.mirai_state$started   <- FALSE
.mirai_state$preloaded <- FALSE

.mirai_start <- function() {
  if (isTRUE(getOption("TVS_MIRAI_BOOTED", FALSE))) {
    .mirai_state$started <- TRUE
    return(invisible(TRUE))
  }
  if (isTRUE(.mirai_state$started)) return(invisible(TRUE))

  n <- suppressWarnings(as.integer(Sys.getenv("TVS_MIRAI_DAEMONS", "")))
  if (is.na(n) || n < 1L) {
    n <- max(parallel::detectCores(logical = TRUE) - 2L, 1L)
  }

  res <- try(mirai::daemons(n), silent = TRUE)
  if (inherits(res, "try-error")) {
    .mirai_state$started <- FALSE
    .mirai_state$n <- 0L
    return(invisible(FALSE))
  }
  .mirai_state$started <- TRUE
  .mirai_state$n <- n
  invisible(TRUE)
}

.mirai_preload <- function() {
  if (!isTRUE(.mirai_state$started)) return(invisible(FALSE))
  if (isTRUE(.mirai_state$preloaded)) return(invisible(TRUE))

  res <- try(
    mirai::everywhere({
      suppressMessages(library(DBI))
      suppressMessages(library(RMariaDB))
      suppressMessages(library(jsonlite))
      suppressMessages(library(dplyr))
      suppressMessages(library(tidyr))
      suppressMessages(library(lubridate))
      suppressMessages(library(tibble))
      suppressMessages(library(purrr))
    }, .compute = "shiny"),
    silent = TRUE
  )

  .mirai_state$preloaded <- !inherits(res, "try-error")
  invisible(.mirai_state$preloaded)
}

shiny::onStop(function() {
  if (!isTRUE(getOption("TVS_MIRAI_BOOTED", FALSE)) && isTRUE(.mirai_state$started)) {
    try(mirai::daemons(0L), silent = TRUE)
    .mirai_state$started <- FALSE
    .mirai_state$preloaded <- FALSE
  }
})

# ===============================================================
# Cache global leve: OBJETOS e CHOICES (por processo)
# ===============================================================
.obj_cache_env <- new.env(parent = emptyenv())
.obj_cache_env$objetos    <- NULL
.obj_cache_env$objetos_ts <- as.POSIXct(NA)
.obj_cache_env$choices    <- NULL
.obj_cache_env$choices_ts <- as.POSIXct(NA)

.time_since <- function(ts) {
  if (is.null(ts) || length(ts) != 1L) return(Inf)
  if (is.na(ts)) return(Inf)
  as.numeric(difftime(Sys.time(), ts, units = "secs"))
}

get_objetos_shared <- function(pool, ttl_secs = 5 * 60) {
  if (!is.null(.obj_cache_env$objetos) &&
      is.finite(.time_since(.obj_cache_env$objetos_ts)) &&
      .time_since(.obj_cache_env$objetos_ts) <= ttl_secs) {
    return(.obj_cache_env$objetos)
  }
  objs <- selectAllObjetos(pool)
  .obj_cache_env$objetos    <- objs
  .obj_cache_env$objetos_ts <- Sys.time()
  objs
}

get_choices_shared <- function(pool, ttl_secs = 5 * 60) {
  if (!is.null(.obj_cache_env$choices) &&
      is.finite(.time_since(.obj_cache_env$choices_ts)) &&
      .time_since(.obj_cache_env$choices_ts) <= ttl_secs) {
    return(.obj_cache_env$choices)
  }
  ch <- fetch_choices(pool)
  .obj_cache_env$choices    <- ch
  .obj_cache_env$choices_ts <- Sys.time()
  ch
}

# ===============================================================
# Utilidades
# ===============================================================
`%||%` <- function(x, y) if (is.null(x)) y else x

.make_cache_key_dados <- function(dt_de_utc, dt_ate_utc, dt_de_local, dt_ate_local, setor, maquina, tzL) {
  digest::digest(list(
    dt_de_utc    = as.character(dt_de_utc),
    dt_ate_utc   = as.character(dt_ate_utc),
    dt_de_local  = as.character(dt_de_local),
    dt_ate_local = as.character(dt_ate_local),
    setor        = setor,
    maquina      = maquina,
    tzL          = tzL
  ), algo = "xxhash64")
}

# ===============================================================
# ✅ UI helper: Selectize com "x" para limpar
# ===============================================================
selectize_clearable <- function(ns, id, label, choices = NULL, multiple = TRUE,
                                placeholder = NULL) {

  dom_id   <- ns(id)
  clear_id <- ns(paste0(id, "__clear"))

  div(
    class = "tvs-selectize-wrap",
    style = "position: relative;",
    shiny::selectizeInput(
      dom_id, label,
      choices = choices,
      multiple = multiple,
      options = list(
        placeholder = placeholder %||% "",
        plugins     = list("remove_button")
      )
    ),
    tags$span(
      class = "tvs-selectize-clear is-hidden",
      `data-select-id` = dom_id,
      `data-clear-id`  = clear_id,
      title = "Limpar",
      HTML("&times;")
    )
  )
}

# ===============================================================
# Proteção de memória (downsample + parse filtrado de ATRIBUTOS)
# ===============================================================
.KEEP_RE_ATR <- "(\\.ESTADO$|\\.TRABALHADOR$)"

.downsample_raw <- function(df_raw, max_rows = 180000L, bin_secs0 = 5L) {
  if (is.null(df_raw) || !nrow(df_raw)) return(df_raw)
  n <- nrow(df_raw)
  if (n <= max_rows) return(df_raw)

  bin_secs <- as.integer(bin_secs0)
  if (is.na(bin_secs) || bin_secs < 1L) bin_secs <- 5L

  tzU <- attr(df_raw$DATE_TIME, "tzone") %||% "UTC"
  dt_num <- as.numeric(as.POSIXct(df_raw$DATE_TIME, tz = tzU))

  repeat {
    time_bin <- as.POSIXct(floor(dt_num / bin_secs) * bin_secs, origin = "1970-01-01", tz = tzU)

    df2 <- df_raw |>
      dplyr::mutate(.time_bin = time_bin) |>
      dplyr::group_by(SETOR, OBJETO, .time_bin) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup() |>
      dplyr::select(-.time_bin)

    if (nrow(df2) <= max_rows || bin_secs >= 60L) return(df2)
    bin_secs <- bin_secs * 2L
  }
}

.flatten_filtered <- function(x, prefix = NULL, keep_re = .KEEP_RE_ATR) {
  out <- list()
  if (is.list(x)) {
    nm <- names(x); if (is.null(nm)) nm <- seq_along(x)
    for (i in seq_along(x)) {
      key <- as.character(nm[i])
      pfx <- if (length(prefix)) paste0(prefix, ".", key) else key
      val <- x[[i]]
      if (is.list(val)) {
        out <- c(out, .flatten_filtered(val, pfx, keep_re))
      } else {
        if (isTRUE(grepl(keep_re, pfx, perl = TRUE))) out[[pfx]] <- val
      }
    }
  } else {
    nm <- prefix %||% "value"
    if (isTRUE(grepl(keep_re, nm, perl = TRUE))) out[[nm]] <- x
  }
  out
}

safe_parse_atributos <- function(txt, keep_re = .KEEP_RE_ATR) {
  if (is.na(txt) || !nzchar(txt)) return(tibble::tibble())
  obj <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) return(tibble::tibble())
  flat <- .flatten_filtered(obj, prefix = NULL, keep_re = keep_re)
  if (!length(flat)) return(tibble::tibble())
  tibble::as_tibble(as.data.frame(flat, stringsAsFactors = FALSE))
}

expand_atributos <- function(df_raw, keep_re = .KEEP_RE_ATR) {
  if (is.null(df_raw) || !nrow(df_raw)) return(df_raw)

  base_raw <- df_raw |> dplyr::select(DATE_TIME, OBJETO, SETOR, ATRIBUTOS)

  rows <- lapply(seq_len(nrow(base_raw)), function(i) {
    extra <- safe_parse_atributos(base_raw$ATRIBUTOS[i], keep_re = keep_re)
    if (!ncol(extra)) extra <- tibble::tibble()
    dplyr::bind_cols(
      tibble::tibble(
        DATE_TIME = base_raw$DATE_TIME[i],
        OBJETO    = base_raw$OBJETO[i],
        SETOR     = base_raw$SETOR[i]
      ),
      extra
    )
  })

  dplyr::bind_rows(rows) |>
    dplyr::mutate(DATE_TIME = as.POSIXct(DATE_TIME)) |>
    dplyr::arrange(DATE_TIME)
}

# ===============================================================
# SETUP state
# ===============================================================
apply_setup_state <- function(df,
                             col_estado_maquina,
                             col_trabalhador,
                             estado_parado         = "PARADO",
                             trabalhador_presente  = "TRABALHANDO",
                             nome_setup            = "SETUP",
                             min_parado_secs       = 60,
                             by_cols               = c("SETOR","OBJETO"),
                             max_gap_secs          = Inf) {

  if (isTRUE(getOption("TVS_DISABLE_SETUP", FALSE))) return(df)

  if (!"DATE_TIME" %in% names(df)) return(df)
  need <- c("DATE_TIME", col_estado_maquina, col_trabalhador, by_cols)
  miss <- setdiff(need, names(df))
  if (length(miss)) return(df)

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
          if (is.na(dt) || dt < 0 || dt > max_gap_secs) dur_cand[i] <- 0
          else dur_cand[i] <- dur_cand[i-1] + dt
        }
      }

      is_setup <- cand & (dur_cand >= as.numeric(min_parado_secs))
      d[[col_estado_maquina]] <- ifelse(is_setup, nome_setup, st_raw)
      d
    }) |>
    dplyr::ungroup()
}

# ===============================================================
# Episódios (run-length)
# ===============================================================
build_episodes <- function(df, min_secs = 30, dt_ate = NULL, max_gap_secs = 60, tail_secs = NULL) {

  if (is.null(df) || !nrow(df)) return(NULL)

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

    dt_ate_use <- dt_ate
    if (is.null(dt_ate_use) || is.na(dt_ate_use)) dt_ate_use <- max(times, na.rm = TRUE)
    dt_ate_use <- as.POSIXct(dt_ate_use, tz = tz_use)

    tail_use <- tail_secs
    if (is.null(tail_use)) {
      dd <- diff(as.numeric(times))
      step <- suppressWarnings(stats::median(dd[dd > 0], na.rm = TRUE))
      if (!is.finite(step)) step <- 60
      tail_use <- min(max_gap_secs, as.numeric(step))
    }

    st_raw <- as.character(d$ESTADO)
    st_up  <- norm_chr(st_raw)
    clean  <- st_raw

    keep_states <- c("OPERANDO", "SETUP")

    r <- rle(st_up)
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L

    for (k in seq_along(r$values)) {
      if (r$values[k] != "PARADO") next
      i0 <- starts[k]; i1 <- ends[k]

      run_end <- if (i1 < n) times[i1 + 1L] else pmin(dt_ate_use, times[n] + tail_use)
      run_dur <- as.numeric(difftime(run_end, times[i0], units = "secs"))

      if (is.finite(run_dur) && run_dur < as.numeric(min_secs)) {
        nxt_up  <- if (i1 < n) st_up[i1 + 1L] else NA_character_
        nxt_raw <- if (i1 < n) st_raw[i1 + 1L] else NA_character_

        if (!is.na(nxt_up) && nxt_up %in% keep_states) {
          clean[i0:i1] <- nxt_raw
        } else {
          prv_raw <- if (i0 > 1L) st_raw[i0 - 1L] else NA_character_
          if (!is.na(prv_raw)) clean[i0:i1] <- prv_raw
        }
      }
    }

    end_obs <- c(times[-1], pmin(dt_ate_use, times[n] + tail_use))
    end_obs <- pmin(end_obs, times + max_gap_secs)
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

# ===============================================================
# Turnos (resumo)
# ===============================================================
classificar_turno <- function(dt) {
  if (length(dt) == 0) return(character(0))

  h  <- lubridate::hour(dt)
  m  <- lubridate::minute(dt)
  mm <- h * 60 + m

  turno_chr <- dplyr::case_when(
    mm >= 4*60  & mm <= 11*60 + 59 ~ "Manhã",
    mm >= 12*60 & mm <= 19*60 + 59 ~ "Tarde",
    mm >= 20*60 | mm <= 3*60  + 59 ~ "Noite",
    TRUE                           ~ NA_character_
  )

  factor(turno_chr, levels = c("Manhã", "Tarde", "Noite"))
}

.precompute_resumo_turnos <- function(ep_m) {
  if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
  ep_m <- ep_m[grepl("\\.ESTADO$", ep_m$COMPONENTE), , drop = FALSE]
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
    dplyr::mutate(Ranking = dplyr::min_rank(dplyr::desc(Utilizacao))) |>
    dplyr::ungroup() |>
    dplyr::mutate(Periodo = factor(Periodo, levels = c("Manhã","Tarde","Noite"))) |>
    dplyr::arrange(Periodo, Ranking, dplyr::desc(Total))
}

# ===============================================================
# Box executivo (médias e totais)
# ===============================================================
.precompute_box_exec <- function(ep_m, dt_de_local, dt_ate_local) {
  if (is.null(ep_m) || !nrow(ep_m)) return(NULL)
  ep_m <- ep_m[grepl("\\.ESTADO$", ep_m$COMPONENTE), , drop = FALSE]
  if (!nrow(ep_m)) return(NULL)

  ep_m <- ep_m |> dplyr::mutate(ESTADO = dplyr::case_when(
    ESTADO == "SETUP" ~ "PARADO",
    TRUE ~ ESTADO
  ))

  main_component_map <- c(
    "SETREMA" = "PRENSA.ESTADO",
    "DOBRA"   = "DOBRA.ESTADO"
  )

  ep_main <- dplyr::as_tibble(ep_m) |>
    dplyr::group_by(OBJETO) |>
    dplyr::group_modify(function(.x, .y) {
      obj   <- as.character(.y$OBJETO[1])
      comps <- unique(as.character(.x$COMPONENTE))

      want1 <- paste0(obj, ".ESTADO")
      if (want1 %in% comps) return(.x[.x$COMPONENTE == want1, , drop = FALSE])

      want2 <- main_component_map[obj]
      if (length(want2) && !is.na(want2) && want2 %in% comps)
        return(.x[.x$COMPONENTE == want2, , drop = FALSE])

      .x[.x$COMPONENTE == comps[1], , drop = FALSE]
    }) |>
    dplyr::ungroup()

  if (!nrow(ep_main)) return(NULL)

  by_obj <- ep_main |>
    dplyr::mutate(ESTADO = toupper(trimws(as.character(ESTADO)))) |>
    dplyr::group_by(OBJETO) |>
    dplyr::summarise(
      op = sum(dur_secs[ESTADO == "OPERANDO"], na.rm = TRUE),
      pa = sum(dur_secs[ESTADO == "PARADO"],   na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      tot  = op + pa,
      util = dplyr::if_else(tot > 0, op / tot * 100, as.numeric(NA))
    ) |>
    dplyr::arrange(dplyr::desc(util))

  nmaq <- nrow(by_obj)
  if (nmaq <= 0) return(NULL)

  op_tot  <- sum(by_obj$op, na.rm = TRUE)
  pa_tot  <- sum(by_obj$pa, na.rm = TRUE)
  tot_tot <- op_tot + pa_tot
  util_geral <- if (tot_tot > 0) op_tot / tot_tot * 100 else NA_real_

  op_mean_h <- (op_tot / 3600) / max(1, nmaq)
  pa_mean_h <- (pa_tot / 3600) / max(1, nmaq)
  util_mean <- mean(by_obj$util, na.rm = TRUE)

  pct <- function(a,b) ifelse(is.finite(a) & is.finite(b) & b > 0, a/b*100, NA_real_)
  op_pct <- pct(op_tot, tot_tot)
  pa_pct <- pct(pa_tot, tot_tot)

  single_obj <- if (nmaq == 1 && nrow(by_obj)) as.character(by_obj$OBJETO[1]) else NA_character_

  top_txt <- if (nmaq >= 1 && is.finite(by_obj$util[1])) paste0(by_obj$OBJETO[1], " (", round(by_obj$util[1], 1), "%)") else "—"
  worst_txt <- {
    w <- by_obj |> dplyr::filter(is.finite(util)) |> dplyr::arrange(util) |> dplyr::slice(1)
    if (nrow(w)) paste0(w$OBJETO, " (", round(w$util, 1), "%)") else "—"
  }

  per_txt <- paste0(format(as.POSIXct(dt_de_local), "%d/%m/%Y %H:%M"), " → ", format(as.POSIXct(dt_ate_local), "%d/%m/%Y %H:%M"))

  list(
    op_h = op_mean_h, pa_h = pa_mean_h, util = util_mean,
    op_tot_h = op_tot / 3600, pa_tot_h = pa_tot / 3600,
    util_geral = util_geral,
    op_pct = op_pct, pa_pct = pa_pct,
    nmaq = nmaq, single_obj = single_obj, top = top_txt, worst = worst_txt,
    periodo = per_txt
  )
}

# ===============================================================
# ✅ Flow episodes (o que o flow precisa)
# ===============================================================
.precompute_flow_eps <- function(ep_m) {
  if (is.null(ep_m) || !nrow(ep_m)) return(NULL)

  ep <- ep_m[grepl("\\.ESTADO$", ep_m$COMPONENTE), , drop = FALSE]
  if (!nrow(ep)) return(NULL)

  out <- dplyr::as_tibble(ep) |>
    dplyr::transmute(
      SETOR      = as.character(SETOR),
      OBJETO     = as.character(OBJETO),
      start_time = as.POSIXct(start_time),
      end_time   = as.POSIXct(end_time),
      ESTADO     = as.character(ESTADO)
    ) |>
    dplyr::filter(!is.na(start_time), !is.na(end_time), end_time > start_time) |>
    dplyr::arrange(SETOR, OBJETO, start_time)

  if (!nrow(out)) return(NULL)
  out
}

# ===============================================================
# 🔥 Processamento principal (retorna pacote leve)
# ===============================================================
process_df_raw <- function(df_raw, dt_de_local, dt_ate_local, tzL) {

  df_raw <- .downsample_raw(df_raw, max_rows = 180000L, bin_secs0 = 5L)
  df_raw$DATE_TIME <- from_utc(df_raw$DATE_TIME, tz = tzL)

  df <- expand_atributos(df_raw, keep_re = .KEEP_RE_ATR)
  rm(df_raw); gc(FALSE)

  df <- apply_setup_state(df, "BOBINA.ESTADO", "AREA_DE_TRABALHO_A.TRABALHADOR", min_parado_secs = 60, by_cols = c("SETOR","OBJETO"))
  df <- apply_setup_state(df, "PRENSA.ESTADO", "AREA_DE_TRABALHO_B.TRABALHADOR", min_parado_secs = 60, by_cols = c("SETOR","OBJETO"))
  df <- apply_setup_state(df, "DOBRA.ESTADO",  "AREA_DE_TRABALHO.TRABALHADOR",   min_parado_secs = 60, by_cols = c("SETOR","OBJETO"))
  df <- apply_setup_state(df, "SOLDA.ESTADO",  "AREA_DE_TRABALHO.TRABALHADOR",   min_parado_secs = 60, by_cols = c("SETOR","OBJETO"))

  ep <- build_episodes(df, min_secs = 30, dt_ate = dt_ate_local, max_gap_secs = 60)
  rm(df); gc(FALSE)

  ep <- cap_episodes(ep, dt_de_local, dt_ate_local)
  if (is.null(ep) || !nrow(ep)) return(NULL)

  ep_m <- ep[ep$ESTADO %in% c("OPERANDO","PARADO","SETUP"), , drop = FALSE]
  ep_m <- merge_setrema_bobina_prensa(ep_m)

  out <- list(
    dt_de_local   = dt_de_local,
    dt_ate_local  = dt_ate_local,
    resumo_turnos = .precompute_resumo_turnos(ep_m),
    box_exec      = .precompute_box_exec(ep_m, dt_de_local, dt_ate_local),
    flow_eps      = .precompute_flow_eps(ep_m)
  )

  rm(ep, ep_m); gc(FALSE)
  out
}

# ===============================================================
# UI helpers
# ===============================================================
box_value_html <- function(main, l1 = NULL, l2 = NULL) {
  htmltools::HTML(paste0(
    "<div style='line-height:1.05'>",
      "<div style='font-size:25px;font-weight:700;'>", main, "</div>",
      "<div style='font-size:11px;opacity:.95;margin-top:6px'>", l1 %||% "", "</div>",
      "<div style='font-size:10px;opacity:.85;margin-top:2px'>", l2 %||% "", "</div>",
    "</div>"
  ))
}

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

# ===============================================================
# UI
# ===============================================================
#' @export
ui <- function(ns) {
  tagList(

    # CSS + JS do "x" no canto do selectize
    tags$style(HTML("
      .tvs-selectize-clear{
        position:absolute;
        right:10px;
        top:34px;
        z-index:5;
        cursor:pointer;
        font-size:14px;
        line-height:14px;
        padding:2px 6px;
        border-radius:10px;
        opacity:.55;
        user-select:none;
      }
      .tvs-selectize-clear:hover{ opacity:.95; }
      .tvs-selectize-clear.is-hidden{ display:none; }
      .tvs-selectize-wrap .selectize-control{ padding-right:24px; }
    ")),
    tags$script(HTML("
    (function(){
      function bindOne(btn){
        var selectId = btn.getAttribute('data-select-id');
        var clearId  = btn.getAttribute('data-clear-id');

        function tryBind(){
          var el = document.getElementById(selectId);
          if(!el || !el.selectize){ setTimeout(tryBind, 200); return; }

          var s = el.selectize;

          function toggle(){
            if(s.items && s.items.length > 0) btn.classList.remove('is-hidden');
            else btn.classList.add('is-hidden');
          }

          toggle();
          s.on('change', toggle);

          btn.addEventListener('click', function(ev){
            ev.preventDefault();
            s.clear(true);
            s.close();
            toggle();
            if(window.Shiny) Shiny.setInputValue(clearId, Date.now(), {priority:'event'});
          });
        }
        tryBind();
      }

      document.addEventListener('DOMContentLoaded', function(){
        document.querySelectorAll('.tvs-selectize-clear').forEach(function(btn){
          if(btn.__tvsBound) return;
          btn.__tvsBound = true;
          bindOne(btn);
        });
      });

      if(window.Shiny){
        document.addEventListener('shiny:value', function(){
          document.querySelectorAll('.tvs-selectize-clear').forEach(function(btn){
            if(btn.__tvsBound) return;
            btn.__tvsBound = true;
            bindOne(btn);
          });
        });
      }
    })();
    ")),

    fluidRow(
      style = "margin-top: 50px;",
      column(12,
        box(
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          title = tags$span("Filtros", style = "font-size: 16px;"),
          fluidRow(
            column(3,
              selectize_clearable(
                ns, "f_setor", "Setor",
                choices = NULL, multiple = TRUE,
                placeholder = "Todos os setores"
              )
            ),
            column(3,
              selectize_clearable(
                ns, "f_maquina", "Máquina",
                choices = NULL, multiple = TRUE,
                placeholder = "Todas as máquinas"
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
      )
    ),

    column(4,
      shinydashboard::valueBox(
        width = "100%",
        value = uiOutput(ns("vbOperando1")),
        subtitle = "Tempo OPERANDO",
        icon = icon("play"),
        color = "green"
      ) |> tagAppendAttributesFind(target = 1, style = "min-height: 102px")
    ),

    column(4,
      shinydashboard::valueBox(
        width = "100%",
        value = uiOutput(ns("vbParado1")),
        subtitle = "Tempo PARADO",
        icon = icon("pause"),
        color = "red"
      ) |> tagAppendAttributesFind(target = 1, style = "min-height: 102px")
    ),

    column(4,
      shinydashboard::valueBox(
        width = "100%",
        value = uiOutput(ns("vbUtilizacao1")),
        subtitle = "Utilização média",
        icon = icon("chart-line"),
        color = "blue"
      ) |> tagAppendAttributesFind(target = 1, style = "min-height: 102px")
    ),

    column(12, uiOutput(ns("dashbody"))),
    column(12, tagList(br(), br()))
  )
}

# ===============================================================
# Server
# ===============================================================
#' @export
server <- function(ns, input, output, session) {

  pool <- dbp$get_pool()

  rv_async <- shiny::reactiveValues(
    dados   = NULL,      # mantém último dado BOM
    loading = FALSE,
    err     = NULL,
    last_ok = as.POSIXct(NA)
  )

  # NON-reactive state (SAFE p/ later)
  .st <- new.env(parent = emptyenv())
  .st$alive   <- TRUE
  .st$busy    <- FALSE
  .st$pending <- FALSE
  .st$filters <- list(setor=NULL, maquina=NULL, dt_de=NULL, dt_ate=NULL)
  .st$job_seq <- 0L

  .is_alive <- function() isTRUE(.st$alive)

  session$onSessionEnded(function() {
    .st$alive <- FALSE
  })

  # ---------------------------
  # CLIP state (per session)
  # ---------------------------
  rv_clip <- shiny::reactiveValues(
    running  = FALSE,
    notif_id = NULL,
    status   = FALSE
  )

  .clip_notif_clear <- function() {
    if (!is.null(rv_clip$notif_id)) {
      shiny::removeNotification(rv_clip$notif_id, session = session)
      rv_clip$notif_id <- NULL
    }
  }

  .clip_notif_set <- function(msg, type = c("message","warning","error"), closeButton = FALSE) {
    type <- match.arg(type)
    .clip_notif_clear()
    rv_clip$notif_id <- shiny::showNotification(
      msg,
      type = type,
      duration = NULL,
      closeButton = closeButton,
      session = session
    )
    invisible(rv_clip$notif_id)
  }

  # ---------------------------------------------------------------
  # Carrega choices de filtro após flush
  # ---------------------------------------------------------------
  .load_choices <- function() {
    if (!.is_alive()) return(invisible(NULL))
    tryCatch({
      ch <- get_choices_shared(pool)
      shiny::updateSelectizeInput(session, "f_setor",   choices = ch$setores,  selected = character(0), server = TRUE)
      shiny::updateSelectizeInput(session, "f_maquina", choices = ch$maquinas, selected = character(0), server = TRUE)
    }, error = function(e) {
      shiny::showNotification(paste0("Falha ao carregar filtros: ", conditionMessage(e)),
                              type = "error", duration = 6)
    })
    invisible(TRUE)
  }

  # ---------------------------------------------------------------
  # load_dados (ASYNC com {mirai})
  # ---------------------------------------------------------------
  .load_dados <- function() {
    if (!.is_alive()) return(invisible(NULL))

    if (isTRUE(.st$busy)) {
      .st$pending <- TRUE
      return(invisible(NULL))
    }

    if (!.sem_try_acquire()) {
      rv_async$loading <- TRUE
      later::later(function() {
        if (.is_alive()) .load_dados()
      }, .async_sem$retry_s)
      return(invisible(NULL))
    }

    .st$job_seq <- .st$job_seq + 1L
    job_seq <- .st$job_seq

    .st$busy <- TRUE
    rv_async$loading <- TRUE
    rv_async$err <- NULL

    f <- .st$filters
    tzL <- tz_local()
    now_local <- as.POSIXct(Sys.time(), tz = tzL)

    dt_ate_local <- if (is.null(f$dt_ate) || is.na(f$dt_ate)) now_local else as.POSIXct(f$dt_ate, tz = tzL)
    dt_de_local  <- if (is.null(f$dt_de)  || is.na(f$dt_de))  (dt_ate_local - 24*60*60) else as.POSIXct(f$dt_de, tz = tzL)

    dt_de_utc  <- to_utc(dt_de_local, tz = tzL)
    dt_ate_utc <- to_utc(dt_ate_local, tz = tzL)

    # ✅ Cache de dados
    key <- NULL
    if (.cache_enabled()) {
      key <- .make_cache_key_dados(
        dt_de_utc, dt_ate_utc,
        dt_de_local, dt_ate_local,
        f$setor, f$maquina,
        tzL
      )

      hit <- .cache_get_unwrap(key)
      if (!inherits(hit, "key_missing")) {
        if (.is_alive() && job_seq == .st$job_seq) {
          rv_async$dados <- hit
          rv_async$err <- NULL
          rv_async$last_ok <- Sys.time()
        }
        rv_async$loading <- FALSE
        .st$busy <- FALSE
        .sem_release()
        return(invisible(TRUE))
      }
    }

    .finish <- function() {
      if (!.is_alive()) {
        .st$busy <- FALSE
        .sem_release()
        return(invisible(NULL))
      }

      rv_async$loading <- FALSE
      .st$busy <- FALSE
      .sem_release()

      if (isTRUE(.st$pending) && .is_alive()) {
        .st$pending <- FALSE
        later::later(function() if (.is_alive()) .load_dados(), 0.05)
      }
      invisible(TRUE)
    }

    ok_mirai <- .mirai_start()
    if (isTRUE(ok_mirai)) .mirai_preload()

    # -------- fallback no main (sem mirai)
    if (!isTRUE(ok_mirai)) {
      out <- NULL
      err <- NULL

      tryCatch({
        df_raw <- run_query(pool, dt_de_utc, dt_ate_utc, f$setor, f$maquina)
        out <- if (is.null(df_raw)) NULL else process_df_raw(df_raw, dt_de_local, dt_ate_local, tzL)
        rm(df_raw); gc(FALSE)
      }, error = function(e) {
        err <<- conditionMessage(e)
      })

      if (!is.null(err)) {
        if (.is_alive() && job_seq == .st$job_seq) {
          rv_async$err <- err
        }
        .finish()
        return(invisible(NULL))
      }

      if (.is_alive() && job_seq == .st$job_seq) {
        if (.cache_enabled() && !is.null(key)) .cache_set_wrap(key, out)
        rv_async$dados <- out
        rv_async$err <- NULL
        rv_async$last_ok <- Sys.time()
      }

      .finish()
      return(invisible(TRUE))
    }

    # -------- mirai job
    job <- mirai::mirai({
      con <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        dbname   = Sys.getenv("DBNAME"),
        host     = Sys.getenv("HOST"),
        port     = Sys.getenv("PORT"),
        username = Sys.getenv("USERNAME"),
        password = Sys.getenv("PASSWORD")
      )
      on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

      df_raw <- run_query(con, dt_de_utc, dt_ate_utc, setor, maquina)
      if (is.null(df_raw)) return(NULL)

      df_raw <- .downsample_raw(df_raw, max_rows = 180000L, bin_secs0 = 5L)
      process_df_raw(df_raw, dt_de_local, dt_ate_local, tzL)
    },
    run_query       = run_query,
    process_df_raw  = process_df_raw,
    .downsample_raw = .downsample_raw,
    dt_de_utc       = dt_de_utc,
    dt_ate_utc      = dt_ate_utc,
    dt_de_local     = dt_de_local,
    dt_ate_local    = dt_ate_local,
    setor           = f$setor,
    maquina         = f$maquina,
    tzL             = tzL,
    .compute = "shiny"
    )

    p <- promises::then(job, function(out) {
      if (.is_alive() && job_seq == .st$job_seq) {
        if (.cache_enabled() && !is.null(key)) .cache_set_wrap(key, out)
        rv_async$dados <- out
        rv_async$err <- NULL
        rv_async$last_ok <- Sys.time()
      }
      removeProgressLoader(1000, session = session)
      .finish()
      out
    })

    promises::catch(p, function(e) {
      if (.is_alive() && job_seq == .st$job_seq) {
        rv_async$err <- conditionMessage(e)
      }
      .finish()
      NULL
    })

    invisible(TRUE)
  }

  # ---------------------------------------------------------------
  # Progresso Loader
  # ---------------------------------------------------------------
  observeEvent(rv_async$loading, {
    has_data <- !is.null(isolate(rv_async$dados))

    if (isTRUE(rv_async$loading) && !has_data && !isolate(rv_clip$status)) {
      newProgressLoader(session)
    } else {
      removeProgressLoader(1000, session = session)
    }
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------
  # BOTÕES
  # ---------------------------------------------------------------
  observeEvent(input$bt_apply_filters, {

    f_setor   <- input$f_setor
    f_maquina <- input$f_maquina
    f_de      <- input$f_de
    f_ate     <- input$f_ate

    .st$filters$setor   <- if (!length(f_setor)) NULL else f_setor
    .st$filters$maquina <- if (!length(f_maquina)) NULL else f_maquina
    .st$filters$dt_de   <- f_de
    .st$filters$dt_ate  <- f_ate

    actionWebUser({ .load_dados() }, auto.remove = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$bt_clear_filters, {

    shiny::updateSelectizeInput(session,"f_setor",selected = character(0))
    shiny::updateSelectizeInput(session,"f_maquina",selected = character(0))
    shinyWidgets::updateAirDateInput(session,"f_de",clear = TRUE)
    shinyWidgets::updateAirDateInput(session,"f_ate",clear = TRUE)

    .st$filters$setor   <- NULL
    .st$filters$maquina <- NULL
    .st$filters$dt_de   <- NULL
    .st$filters$dt_ate  <- NULL

    actionWebUser({ .load_dados() }, auto.remove = FALSE)
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------
  # Auto-refresh
  # ---------------------------------------------------------------
  .schedule_refresh <- function(delay_s = 5*60) {
    later::later(function() {
      if (.is_alive()) {
        .load_dados()
        .schedule_refresh(delay_s)
      }
    }, delay_s)
  }

  # ---------------------------------------------------------------
  # Início (após flush)
  # ---------------------------------------------------------------
  session$onFlushed(function() {
    later::later(function() {
      if (!.is_alive()) return()
      .load_choices()
      .load_dados()
      .schedule_refresh(5*60)
    }, 0.1)
  }, once = TRUE)

  # ---------------------------------------------------------------
  # reactive “fonte”
  # ---------------------------------------------------------------
  dados <- shiny::reactive(rv_async$dados)

  # ---------------------------------------------------------------
  # ✅ Abrir clip (usado pelo Flow)
  # ---------------------------------------------------------------
  .open_clip_from_flow <- function(objeto, setor, time_begin, time_end, estado) {
    if (isTRUE(rv_clip$running)) {
      shiny::showNotification("Já existe um clip em processamento. Aguarde finalizar para abrir outro.",
                              type = "message", duration = 4)
      return(invisible(FALSE))
    }

    objeto_row <- NULL
    tryCatch({
      objeto_row <- get_objetos_shared(pool) |>
        dplyr::filter(NAME_OBJETO == objeto & NAME_SETOR == setor)
    }, error = function(e) NULL)

    if (is.null(objeto_row) || !nrow(objeto_row)) {
      shiny::showNotification("Não encontrei a configuração da máquina para abrir o clip.",
                              type = "warning", duration = 4)
      return(invisible(FALSE))
    }

    cameras_ids <- NULL
    tryCatch({
      cameras_ids <- unique(purrr::map_int(objeto_row$CONFIG[[1]]$COMPONENTES[[1]]$CAMERA, "CD_ID_CAMERA"))
    }, error = function(e) NULL)

    if (is.null(cameras_ids) || !length(cameras_ids)) {
      shiny::showNotification("Máquina sem cameras configuradas para clip.",
                              type = "warning", duration = 4)
      return(invisible(FALSE))
    }

    title_ <- paste0("Clip – ", objeto, " (", estado, ")")
    tb_ <- as.POSIXct(time_begin)
    te_ <- as.POSIXct(time_end)

    rv_clip$status  <- TRUE
    rv_clip$running <- TRUE
    .clip_notif_set("Abrindo clip…", type = "message")

    tryCatch({
      video_clip_open(
        ns, input, output, session,
        pool       = pool,
        title      = title_,
        time_begin = tb_,
        time_end   = te_,
        camera_ids = cameras_ids,
        fps        = 5L,
        max_frames = 300L,
        callback   = function(){ rv_clip$status <- FALSE }
      )

      .clip_notif_clear()
      rv_clip$running <- FALSE

    }, error = function(e) {
      .clip_notif_clear()
      rv_clip$running <- FALSE
      rv_clip$status <- FALSE
      shiny::showNotification(
        paste0("Falha ao gerar clip: ", conditionMessage(e)),
        type = "error", duration = 6, session = session
      )
    })

    invisible(TRUE)
  }

  # ---------------------------------------------------------------
  # ✅ Liga o Flow (módulo)
  # ---------------------------------------------------------------
  flow$server(
    "flow",
    input = input, output = output, session = session,
    pool  = pool,

    # árvore respeita filtros atuais
    get_tree = function() {
      objs <- get_objetos_shared(pool)

      f <- .st$filters
      if (!is.null(f$setor) && length(f$setor)) {
        objs <- objs |> dplyr::filter(NAME_SETOR %in% f$setor)
      }
      if (!is.null(f$maquina) && length(f$maquina)) {
        objs <- objs |> dplyr::filter(NAME_OBJETO %in% f$maquina)
      }

      root <- dplyr::tibble(id = "ROOT", label = "ROOT", parent_id = NA_character_)

      sectors <- objs |>
        dplyr::distinct(NAME_SETOR) |>
        dplyr::arrange(NAME_SETOR) |>
        dplyr::mutate(
          id = as.character(NAME_SETOR),
          label = as.character(NAME_SETOR),
          parent_id = "ROOT"
        ) |>
        dplyr::select(id, label, parent_id)

      sectors_df <- dplyr::bind_rows(root, sectors)

      machines_df <- objs |>
        dplyr::distinct(NAME_SETOR, NAME_OBJETO) |>
        dplyr::mutate(
          id = paste0(NAME_SETOR, "::", NAME_OBJETO),
          label = as.character(NAME_OBJETO),
          sector_id = as.character(NAME_SETOR)
        ) |>
        dplyr::select(id, label, sector_id)

      list(sectors = sectors_df, machines = machines_df)
    },

    # episódios já filtrados pela janela do dashboard
    get_episodes = function() {
      x <- dados()
      if (is.null(x)) return(NULL)
      x$flow_eps
    },

    # janela vem do pacote “dados”
    get_time_window = function() {
      x <- dados()
      if (is.null(x)) {
        tzL <- tz_local()
        t1 <- as.POSIXct(Sys.time(), tz = tzL)
        t0 <- t1 - 24*3600
        return(list(t0 = t0, t1 = t1))
      }
      list(t0 = x$dt_de_local, t1 = x$dt_ate_local)
    },

    # clique no segmento abre clip
    on_clip = function(objeto, setor, start_time, end_time, estado) {
      .open_clip_from_flow(objeto, setor, start_time, end_time, estado)
    }
  )

  # ---------------------------------------------------------------
  # valueBoxes (pré-calculado)
  # ---------------------------------------------------------------
  fmt_horas <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return("–")
    mins <- as.integer(round(as.numeric(x) * 60))
    h <- mins %/% 60
    m <- mins %% 60
    sprintf("%d h %02d min", h, m)
  }
  fmt_perc <- function(x) ifelse(is.na(x), "–", sprintf("%.1f %%", x))

  output$vbOperando1 <- renderUI({
    x <- dados()
    if (is.null(x)) return("–")
    b <- x$box_exec
    if (is.null(b)) return("–")
    box_value_html(
      fmt_horas(b$op_h),
      paste0("Total: ", fmt_horas(b$op_tot_h), " • ", fmt_perc(b$op_pct)),
      paste0(if (!is.na(b$single_obj)) paste0("Máquina única: ", b$single_obj) else paste0("Top utilização: ", b$top),
             " • ", b$periodo)
    )
  })

  output$vbParado1 <- renderUI({
    x <- dados()
    if (is.null(x)) return("–")
    b <- x$box_exec
    if (is.null(b)) return("–")
    box_value_html(
      fmt_horas(b$pa_h),
      paste0("Total: ", fmt_horas(b$pa_tot_h), " • ", fmt_perc(b$pa_pct)),
      paste0(if (!is.na(b$single_obj)) paste0("Máquina única: ", b$single_obj) else paste0("Pior utilização: ", b$worst),
             " • ", b$periodo)
    )
  })

  output$vbUtilizacao1 <- renderUI({
    x <- dados()
    if (is.null(x)) return("–")
    b <- x$box_exec
    if (is.null(b)) return("–")
    box_value_html(
      fmt_perc(b$util),
      paste0("Geral (ponderado): ", fmt_perc(b$util_geral)),
      paste0(if (!is.na(b$single_obj)) paste0("Máquina única: ", b$single_obj) else paste0("Top: ", b$top, " • Pior: ", b$worst),
             " • ", b$periodo)
    )
  })

  # ---------------------------------------------------------------
  # dashbody (NÃO some durante refresh)
  # ---------------------------------------------------------------
  .banner_status <- function(type = c("info","danger"), title, msg) {
    type <- match.arg(type)
    cls <- if (type == "danger") "callout callout-danger" else "callout callout-info"
    tags$div(
      class = cls,
      style = "margin: 0 0 10px 0; padding: 10px 12px;",
      tags$strong(title),
      tags$span(style="margin-left:8px;", msg)
    )
  }

  output$dashbody <- renderUI({
    has_data <- !is.null(rv_async$dados)
    last_ok  <- rv_async$last_ok
    last_ok_txt <- if (!is.na(last_ok)) format(as.POSIXct(last_ok), "%d/%m/%Y %H:%M:%S") else "—"

    if (!has_data) {
      if (isTRUE(rv_async$loading)) {
        return(
          box(
            width = 12, solidHeader = TRUE, title = tags$span("Carregando...", style="font-size:16px;"),
            div(style="padding:12px; font-size:12px; color:#666;",
                paste0(
                  "Buscando dados iniciais. Jobs no processo: ",
                  .async_sem$running, "/", .async_sem$max_running,
                  " • Cache: ", if (.cache_enabled()) "ON" else "OFF", "."
                ))
          )
        )
      }
      if (!is.null(rv_async$err)) {
        return(
          box(
            width = 12, solidHeader = TRUE, title = tags$span("Erro", style="font-size:16px;"),
            div(style="padding:12px; font-size:12px; color:tomato;", rv_async$err)
          )
        )
      }
      return(NULL)
    }

    banners <- tagList()
    if (isTRUE(rv_async$loading)) {
      banners <- tagList(
        banners,
        .banner_status(
          "info",
          "Atualizando em background…",
          paste0(
            "Último update OK: ", last_ok_txt,
            " • Jobs: ", .async_sem$running, "/", .async_sem$max_running,
            " • Cache: ", if (.cache_enabled()) "ON" else "OFF", "."
          )
        )
      )
    } else if (!is.null(rv_async$err)) {
      banners <- tagList(
        banners,
        .banner_status(
          "danger",
          "Falha no refresh (mantendo dados anteriores).",
          paste0(rv_async$err, " • Último update OK: ", last_ok_txt, ".")
        )
      )
    }

    tagList(
      banners,

      box(
        id          = ns("box_turnos"),
        solidHeader = TRUE,
        collapsible = TRUE,
        width       = 12,
        title = tags$span("Resumo por período", style = "font-size: 16px;"),
        div(
          style = "padding: 10px;",
          tags$p(
            "Tabela executiva destacando os turnos com melhor desempenho operacional.",
            style = "margin-bottom: 10px; font-size: 12px; color: #444;"
          ),
          DT::dataTableOutput(ns("tblTurnos")) |> shinycssloaders::withSpinner(color = "lightblue")
        )
      ),

      # ✅ Flow logo abaixo da tabela
      flow$ui(ns("flow"))
    )
  })

  # ---------------------------------------------------------------
  # Tabela turnos
  # ---------------------------------------------------------------
  output$tblTurnos <- DT::renderDataTable({
    x <- dados()
    if (is.null(x)) return(NULL)
    df <- x$resumo_turnos
    if (is.null(df) || !nrow(df)) return(NULL)

    # mantém colunas
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
        order      = list(list(0, "asc"), list(8, "asc")),
        rowGroup   = list(dataSrc = 0),
        language   = dt_lang_ptbr()
      ),
      class = "cell-border stripe compact"
    )
  })

  invisible(TRUE)
}
