# dashboard.R  ({mirai} async: DB + pré-cálculos em daemons; later só agenda)
# ===============================================================

box::use(
  shiny[...],
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
  dbp  = ../infra/db_pool,
  ./global[ actionWebUser, tagAppendAttributesFind, debugLocal,removeProgressLoader,newProgressLoader],
  ../logic/objeto_dao[selectAllObjetos],
  ./video_clip[video_clip_open],
  ../logic/dashboard_dao[
    run_query, fetch_choices,
    tz_local, to_utc, from_utc,
    merge_setrema_bobina_prensa
  ]
)

# ===============================================================
# Cache em memória (compartilhado pelo processo)
# ===============================================================
.cache_obj  <- cachem::cache_mem(max_size = 200 * 1024^2) # 200 MB
.CACHE_NULL <- structure(list(), class = "CACHE_NULL")

.cache_get_unwrap <- function(key) {
  hit <- .cache_obj$get(key)
  if (inherits(hit, "key_missing")) return(structure(list(), class = "key_missing"))
  if (inherits(hit, "CACHE_NULL")) return(NULL)
  hit
}
.cache_set_wrap <- function(key, value) {
  .cache_obj$set(key, if (is.null(value)) .CACHE_NULL else value)
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
# mirai - NO dashboard.R:
#  - se você já bootou no app.R, não mexe.
#  - se não, tenta subir aqui (1x por processo).
# ===============================================================
.mirai_state <- new.env(parent = emptyenv())
.mirai_state$started   <- FALSE
.mirai_state$preloaded <- FALSE

.mirai_start <- function() {
  # Se o app.R já bootou, não reinicia/remeche.
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

  # carrega libs 1x nos daemons (evita overhead por job)
  res <- try(
    mirai::everywhere({
      suppressMessages(library(DBI))
      suppressMessages(library(RMariaDB))
      suppressMessages(library(jsonlite))
      suppressMessages(library(dplyr))
      suppressMessages(library(tidyr))
      suppressMessages(library(lubridate))
    }),
    silent = TRUE
  )

  .mirai_state$preloaded <- !inherits(res, "try-error")
  invisible(.mirai_state$preloaded)
}

shiny::onStop(function() {
  # Se você bootou no app.R, deixe o app gerenciar o shutdown.
  if (!isTRUE(getOption("TVS_MIRAI_BOOTED", FALSE)) && isTRUE(.mirai_state$started)) {
    try(mirai::daemons(0L), silent = TRUE)
    .mirai_state$started <- FALSE
    .mirai_state$preloaded <- FALSE
  }
})


# ===============================================================
# Cache global de OBJETOS e CHOICES (por processo)
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
# JSON / ATRIBUTOS
# ===============================================================
.flatten_once <- function(x, prefix = NULL) {
  out <- list()
  if (is.list(x)) {
    nm <- names(x)
    if (is.null(nm)) nm <- seq_along(x)
    for (i in seq_along(x)) {
      key <- as.character(nm[i])
      pfx <- if (length(prefix)) paste0(prefix, ".", key) else key
      val <- x[[i]]
      if (is.list(val)) out <- c(out, .flatten_once(val, pfx))
      else out[[pfx]] <- val
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
  tibble::as_tibble(as.data.frame(flat, stringsAsFactors = FALSE))
}

expand_atributos <- function(df_raw) {
  if (!nrow(df_raw)) return(df_raw)

  rows <- lapply(seq_len(nrow(df_raw)), function(i) {
    base  <- df_raw[i, c("DATE_TIME", "OBJETO", "SETOR"), drop = FALSE]
    extra <- safe_parse_atributos(df_raw$ATRIBUTOS[i])
    if (!ncol(extra)) extra <- tibble::tibble()
    dplyr::bind_rows(dplyr::bind_cols(base, extra))
  })

  out <- dplyr::bind_rows(rows)
  out |>
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

  stopifnot("DATE_TIME" %in% names(df))
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
    st     <- norm_chr(st_raw)
    clean  <- st_raw

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
# Daily util + rolling mean
# ===============================================================
build_daily_util <- function(ep) {
  if (is.null(ep) || !nrow(ep)) return(NULL)

  ep$start_time <- as.POSIXct(ep$start_time, tz = attr(ep$start_time, "tzone") %||% Sys.timezone())
  ep$dur_secs   <- as.numeric(ep$dur_secs)

  ep <- ep[!is.na(ep$start_time) & !is.na(ep$dur_secs) & ep$dur_secs >= 0, , drop = FALSE]
  if (!nrow(ep)) return(NULL)

  ep$date <- as.Date(ep$start_time)

  tot <- aggregate(dur_secs ~ date, data = ep, sum, na.rm = TRUE)
  if (is.null(tot) || !nrow(tot)) return(NULL)

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

# ===============================================================
# Plot themes/layout (corrigido: sem "args = NULL" quebrando theme)
# ===============================================================
themasPlotyGGplot <- function(text.x.angle = 0, ...) {
  ggplot2::theme(
    ...,
    axis.text.x   = ggplot2::element_text(angle = text.x.angle),
    legend.title  = ggplot2::element_blank(),
    axis.line     = ggplot2::element_line(colour = "gray"),
    panel.grid.major = ggplot2::element_line(
      linewidth = 0.5,
      linetype  = "solid",
      colour    = "lightgray"
    )
  )
}

layoutPlotyDefault <- function(x, legend.text = "") {
  plotly::layout(
    x,
    plot_bgcolor  = "transparent",
    paper_bgcolor = "transparent",
    showlegend = TRUE,
    modebar = list(
      bgcolor = "transparent",
      color = "lightgray",
      activecolor = "darkgray"
    ),
    legend  = list(
      title = list(text = legend.text, font = list(color = "black", size = 12)),
      font  = list(color = "black", size = 10)
    )
  )
}

# ===============================================================
# Pré-cálculos (iguais ao seu)
# ===============================================================
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

.precompute_plot1 <- function(ep_m) {
  if (is.null(ep_m) || !nrow(ep_m)) return(NULL)

  ep_m$mid_time <- ep_m$start_time + ep_m$dur_secs / 2
  ep_m$turno    <- classificar_turno(ep_m$mid_time)
  ep_m <- ep_m[!is.na(ep_m$turno), , drop = FALSE]
  if (!nrow(ep_m)) return(NULL)

  ep_m <- ep_m |> dplyr::mutate(ESTADO = dplyr::case_when(
    ESTADO == "SETUP" ~ "PARADO",
    TRUE ~ ESTADO
  ))

  df_plot <- aggregate(dur_secs ~ SETOR + turno + ESTADO, data = ep_m, sum, na.rm = TRUE)
  df_plot$horas  <- df_plot$dur_secs / 3600
  df_plot$turno  <- factor(df_plot$turno, levels = c("Manhã","Tarde","Noite"))
  df_plot$ESTADO <- factor(df_plot$ESTADO, levels = c("OPERANDO","PARADO"))
  df_plot
}

.precompute_plot2 <- function(ep_m) {
  if (is.null(ep_m) || !nrow(ep_m)) return(NULL)

  ep_m <- ep_m |> dplyr::mutate(ESTADO = dplyr::case_when(
    ESTADO == "SETUP" ~ "PARADO",
    TRUE ~ ESTADO
  ))

  ep_obj <- ep_m
  ep_obj_estado <- subset(ep_obj, grepl("\\.ESTADO$", COMPONENTE))
  if (nrow(ep_obj_estado)) ep_obj <- ep_obj_estado
  if (!nrow(ep_obj)) return(NULL)

  ep_obj$hover_x  <- ep_obj$start_time + ep_obj$dur_secs / 2
  ep_obj$mid_time <- ep_obj$hover_x
  ep_obj$turno    <- classificar_turno(ep_obj$mid_time)

  ep_obj$hover_text <- paste0(
    "Máquina: ", ep_obj$OBJETO,
    "<br>Componente: ", ep_obj$COMPONENTE,
    "<br>Estado: ", ep_obj$ESTADO,
    "<br>Início: ", format(as.POSIXct(ep_obj$start_time), "%d/%m/%Y %H:%M"),
    "<br>Fim: ",    format(as.POSIXct(ep_obj$end_time),   "%d/%m/%Y %H:%M"),
    "<br>Duração: ", round(ep_obj$dur_secs/60, 1), " min"
  )
  ep_obj$row_id <- seq_len(nrow(ep_obj))
  ep_obj
}

.precompute_plot3 <- function(ep_m) {
  if (is.null(ep_m) || !nrow(ep_m)) return(NULL)

  ep_hm <- ep_m |> dplyr::mutate(ESTADO = dplyr::case_when(
    ESTADO == "SETUP" ~ "PARADO",
    TRUE ~ ESTADO
  ))

  ep_hm$start_time <- as.POSIXct(ep_hm$start_time)
  ep_hm$end_time   <- as.POSIXct(ep_hm$end_time)

  t0 <- lubridate::floor_date(min(ep_hm$start_time, na.rm = TRUE), unit = "30 minutes")
  t1 <- lubridate::ceiling_date(max(ep_hm$end_time,   na.rm = TRUE), unit = "30 minutes")
  bin_starts <- seq(from = t0, to = t1, by = "30 min")

  expanded <- lapply(seq_len(nrow(ep_hm)), function(i) {
    st <- ep_hm$start_time[i]
    en <- ep_hm$end_time[i]
    if (is.na(st) || is.na(en) || en <= st) return(NULL)

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

  lvl_hora <- unique(format(bin_starts, "%H:%M"))
  expanded$hora  <- factor(format(expanded$time_bin, "%H:%M"), levels = lvl_hora)
  expanded$turno <- factor(expanded$turno, levels = c("Manhã","Tarde","Noite"))

  hm <- aggregate(ov_secs ~ OBJETO + hora + ESTADO + turno, data = expanded, sum, na.rm = TRUE)
  hm_op <- subset(hm, ESTADO == "OPERANDO" & !is.na(turno) & !is.na(hora))
  if (!nrow(hm_op)) return(NULL)

  hm_op$minutos <- hm_op$ov_secs / 60
  hm_op
}

.precompute_plot4 <- function(ep_m) {
  if (is.null(ep_m) || !nrow(ep_m)) return(NULL)

  ep_m <- ep_m |> dplyr::mutate(ESTADO = dplyr::case_when(
    ESTADO == "SETUP" ~ "PARADO",
    TRUE ~ ESTADO
  ))

  daily <- build_daily_util(ep_m)
  if (is.null(daily) || !nrow(daily)) return(NULL)

  daily <- daily[order(daily$date), ]
  daily$ma7  <- roll_mean(daily$util,  7)
  daily$ma15 <- roll_mean(daily$util, 15)
  daily$ma30 <- roll_mean(daily$util, 30)

  rbind(
    data.frame(date = daily$date, serie = "Utilização diária", valor = daily$util),
    data.frame(date = daily$date, serie = "Média 7 dias",     valor = daily$ma7),
    data.frame(date = daily$date, serie = "Média 15 dias",    valor = daily$ma15),
    data.frame(date = daily$date, serie = "Média 30 dias",    valor = daily$ma30)
  )
}

process_df_raw <- function(df_raw, dt_de_local, dt_ate_local, tzL) {
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
  ep_m <- merge_setrema_bobina_prensa(ep_m)

  list(
    ep           = ep,
    ep_m         = ep_m,
    dt_de_local  = dt_de_local,
    dt_ate_local = dt_ate_local,
    resumo_turnos = .precompute_resumo_turnos(ep_m),
    box_exec      = .precompute_box_exec(ep_m, dt_de_local, dt_ate_local),
    plot1_df      = .precompute_plot1(ep_m),
    plot2_df      = .precompute_plot2(ep_m),
    plot3_df      = .precompute_plot3(ep_m),
    plot4_df      = .precompute_plot4(ep_m)
  )
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

insertNewPlotComponent <- function(ns, title, id, width = 6){
  child <- ns(paste0("plot_", id))
  boxid <- ns(paste0("plotbox_", id))

  div(
    id = ns(paste0("child-", child)),
    box(
      id = boxid,
      solidHeader = TRUE,
      collapsible = TRUE,
      title = tags$span(title, style = "font-size: 16px;"),
      width = width,
      div(
        style = "padding: 15px; height: auto; width: 100%;",
        plotly$plotlyOutput(ns(paste0("plotout_", id))) |>
          shinycssloaders::withSpinner(color = "lightblue")
      )
    )
  )
}

#' @export
ui <- function(ns) {
  tagList(
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
    # column(3,
    #   shinydashboard::valueBox(
    #     width = "100%",
    #     value = uiOutput(ns("vbSetup1")),
    #     subtitle = "Tempo em SETUP",
    #     icon = icon("tools"),
    #     color = "yellow"
    #   ) |> tagAppendAttributesFind(target = 1, style = "min-height: 102px")
    # ),

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
    br()
  )
}

# ===============================================================
# Server
# ===============================================================
#' @export
server <- function(ns, input, output, session) {

  pool <- dbp$get_pool()

  rv_async <- shiny::reactiveValues(
    dados   = NULL,
    loading = FALSE,
    err     = NULL
  )

  # ---------------------------
  # NON-reactive state (SAFE p/ later)
  # ---------------------------
  .st <- new.env(parent = emptyenv())
  .st$alive   <- TRUE
  .st$busy    <- FALSE
  .st$pending <- FALSE
  .st$filters <- list(setor=NULL, maquina=NULL, dt_de=NULL, dt_ate=NULL)
  .st$job_seq <- 0L   # <- TOKEN anti-overwrite (último job vence)

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
  # Carrega choices de filtro após flush (pra não travar bootstrap)
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

    # Se já tem um job desta sessão rodando, marca pendência (último clique vence)
    if (isTRUE(.st$busy)) {
      .st$pending <- TRUE
      return(invisible(NULL))
    }

    # Semáforo global por processo
    if (!.sem_try_acquire()) {
      rv_async$loading <- TRUE
      later::later(function() {
        if (.is_alive()) .load_dados()
      }, .async_sem$retry_s)
      return(invisible(NULL))
    }

    # TOKEN do job (evita sobrescrever com retorno antigo)
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

    key <- .make_cache_key_dados(
      dt_de_utc, dt_ate_utc,
      dt_de_local, dt_ate_local,
      f$setor, f$maquina,
      tzL
    )

    # cache hit
    hit <- .cache_get_unwrap(key)
    if (!inherits(hit, "key_missing")) {
      rv_async$dados <- hit
      rv_async$err <- NULL
      rv_async$loading <- FALSE
      .st$busy <- FALSE
      .sem_release()
      return(invisible(TRUE))
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

    # garante mirai (sem mexer se app.R já bootou)
    ok_mirai <- .mirai_start()
    if (isTRUE(ok_mirai)) .mirai_preload()

    if (!isTRUE(ok_mirai)) {
      # fallback: roda no main (antigo)
      out <- NULL
      err <- NULL

      tryCatch({
        df_raw <- run_query(pool, dt_de_utc, dt_ate_utc, f$setor, f$maquina)
        out <- if (is.null(df_raw)) NULL else process_df_raw(df_raw, dt_de_local, dt_ate_local, tzL)
      }, error = function(e) {
        err <<- conditionMessage(e)
      })

      if (!is.null(err)) {
        if (.is_alive() && job_seq == .st$job_seq) {
          rv_async$err <- err
          rv_async$dados <- NULL
        }
        .finish()
        return(invisible(NULL))
      }

      if (.is_alive() && job_seq == .st$job_seq) {
        .cache_set_wrap(key, out)
        rv_async$dados <- out
        rv_async$err <- NULL
      }
      .finish()
      return(invisible(TRUE))
    }

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
      process_df_raw(df_raw, dt_de_local, dt_ate_local, tzL)
    },
    run_query      = run_query,
    process_df_raw = process_df_raw,
    dt_de_utc      = dt_de_utc,
    dt_ate_utc     = dt_ate_utc,
    dt_de_local    = dt_de_local,
    dt_ate_local   = dt_ate_local,
    setor          = f$setor,
    maquina        = f$maquina,
    tzL            = tzL)

    p <- promises::then(job, function(out) {
      if (.is_alive() && job_seq == .st$job_seq) {
        .cache_set_wrap(key, out)
        rv_async$dados <- out
        rv_async$err <- NULL
      }
      .finish()
      out
    })

    promises::catch(p, function(e) {
      if (.is_alive() && job_seq == .st$job_seq) {
        rv_async$err <- conditionMessage(e)
        rv_async$dados <- NULL
      }
      .finish()
      NULL
    })

    invisible(TRUE)
  }
  
  # ---------------------------------------------------------------
  # Progresso Bar loader
  # ---------------------------------------------------------------
  observeEvent(rv_async$loading, {
    if (isTRUE(rv_async$loading) && !isolate(rv_clip$status)) {
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
    
    .load_dados()
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

    .load_dados()
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------
  # Auto-refresh com later
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
  # Início APÓS flush
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
    b <- x$box_exec
    if (is.null(b)) return("–")
    box_value_html(
      fmt_horas(b$pa_h),
      paste0("Total: ", fmt_horas(b$pa_tot_h), " • ", fmt_perc(b$pa_pct)),
      paste0(if (!is.na(b$single_obj)) paste0("Máquina única: ", b$single_obj) else paste0("Pior utilização: ", b$worst),
             " • ", b$periodo)
    )
  })

  output$vbSetup1 <- renderUI({ "–" }) # você está tratando SETUP como PARADO

  output$vbUtilizacao1 <- renderUI({
    x <- dados()
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
  # dashbody (status)
  # ---------------------------------------------------------------
  output$dashbody <- renderUI({
    if (isTRUE(rv_async$loading)) {
      return(
        box(
          width = 12, solidHeader = TRUE, title = tags$span("Carregando...", style="font-size:16px;"),
          div(style="padding:12px; font-size:12px; color:#666;",
              paste0("Atualizando em background. ",
                     "Jobs no processo: ", .async_sem$running, "/", .async_sem$max_running, "."))
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

    tagList(
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
      insertNewPlotComponent(ns, "1) Gráfico por Setor – Operando / Parada", 1),
      insertNewPlotComponent(ns, "2) Linha do Tempo (Gantt) – Máquina", 2),
      #insertNewPlotComponent(ns, "3) Heatmap – Máquinas x Horário", 3),
      insertNewPlotComponent(ns, "3) Tendência de utilização – Fábrica", 4,width = 12)
    )
  })

  # ---------------------------------------------------------------
  # PLOTS
  # ---------------------------------------------------------------
  output[[paste0("plotout_",1)]] <- plotly::renderPlotly({
    x <- dados()
    df_plot <- x$plot1_df
    if (is.null(df_plot) || !nrow(df_plot)) return(NULL)

    g <- ggplot2::ggplot(df_plot, ggplot2::aes(x = SETOR, y = horas, fill = ESTADO)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::labs(x = "Setor", y = "Horas no turno", fill = "Estado") +
      ggplot2::facet_grid(turno ~ ., scales = "free_y", switch = "y") +
      ggplot2::scale_fill_manual(values = c("OPERANDO"="#00a65a","PARADO"="tomato")) +
      themasPlotyGGplot()

    p <- plotly::ggplotly(g, tooltip = c("x","y","fill")) |>
      plotly::config(displaylogo = FALSE, displayModeBar = TRUE)

    layoutPlotyDefault(p, legend.text = "Estado")
  })

  actionClickplot2 <- shiny::reactiveVal(FALSE)

  output[[paste0("plotout_",2)]] <- plotly::renderPlotly({
    x <- dados()
    ep_obj <- x$plot2_df
    if (is.null(ep_obj) || !nrow(ep_obj)) return(NULL)

    if (isolate(!actionClickplot2())) actionClickplot2(TRUE)

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
      ggplot2::geom_point(
        ggplot2::aes(x = hover_x, y = COMPONENTE, color = ESTADO),
        size = 10, alpha = 0.001, show.legend = FALSE
      ) +
      ggplot2::labs(x = "Tempo", y = "", color = "Estado") +
      ggplot2::facet_grid(turno ~ OBJETO, scales = "free_y", switch = "y") +
      ggplot2::scale_color_manual(values = c("OPERANDO"="#00a65a","PARADO"="tomato")) +
      themasPlotyGGplot(text.x.angle = 45)

    p <- plotly::ggplotly(g, tooltip = "text", source = "p2") |>
      plotly::config(displaylogo = FALSE, displayModeBar = TRUE, doubleClick = TRUE)

    p <- layoutPlotyDefault(p, legend.text = "Estado")
    plotly::event_register(p, "plotly_click")
  })

  # output[[paste0("plotout_",3)]] <- plotly::renderPlotly({
  #   x <- dados()
  #   hm_op <- x$plot3_df
  #   if (is.null(hm_op) || !nrow(hm_op)) return(NULL)

  #   g <- ggplot2::ggplot(hm_op, ggplot2::aes(x = hora, y = OBJETO, fill = minutos)) +
  #     ggplot2::geom_tile(color = "gray80") +
  #     ggplot2::facet_grid(turno ~ ., scales = "free_y", switch = "y") +
  #     ggplot2::labs(x="Horário (30 min)", y="Máquina / Objeto", fill="Minutos OPERANDO") +
  #     themasPlotyGGplot(text.x.angle = 45)

  #   p <- plotly::ggplotly(g, tooltip = c("x","y","fill")) |>
  #     plotly::config(displaylogo = FALSE, displayModeBar = TRUE)

  #   layoutPlotyDefault(p, legend.text = "Minutos")
  # })

  output[[paste0("plotout_",4)]] <- plotly::renderPlotly({
    x <- dados()
    df_long <- x$plot4_df
    if (is.null(df_long) || !nrow(df_long)) return(NULL)

    df_long$serie <- factor(df_long$serie, levels = c("Utilização diária","Média 7 dias","Média 15 dias","Média 30 dias"))

    g <- ggplot2::ggplot(df_long, ggplot2::aes(x = date, y = valor, color = serie)) +
      ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
      ggplot2::labs(x="Dia", y="% tempo OPERANDO", color="Série") +
      themasPlotyGGplot()

    p <- plotly::ggplotly(g, tooltip = c("x","y","color")) |>
      plotly::config(displaylogo = FALSE, displayModeBar = TRUE)

    layoutPlotyDefault(p, legend.text = "Médias móveis")
  })

  # ---------------------------------------------------------------
  # Tabela turnos
  # ---------------------------------------------------------------
  output$tblTurnos <- DT::renderDataTable({
    x <- dados()
    df <- x$resumo_turnos
    if (is.null(df) || !nrow(df)) return(NULL)

    df$SETUP <- 0

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

  # ---------------------------------------------------------------
  # Click plot2 -> clip
  # ---------------------------------------------------------------
  observeEvent(actionClickplot2(), {

    observeEvent(plotly::event_data("plotly_click", priority = "event", source = "p2"), {

      if (isTRUE(rv_clip$running)) {
        shiny::showNotification("Já existe um clip em processamento. Aguarde finalizar para abrir outro.",
                                type = "message", duration = 4)
        return()
      }

      ed <- plotly::event_data("plotly_click", source = "p2")
      if (is.null(ed) || is.null(ed$key)) return()

      row_id <- as.integer(ed$key[1])

      x <- dados()
      ep_obj <- x$plot2_df
      if (is.null(ep_obj) || !nrow(ep_obj)) return()
      
      linha <- ep_obj[ep_obj$row_id == row_id, , drop = FALSE]
      if (!nrow(linha)) return()
      
      objeto <- NULL
      tryCatch({
        objeto <- get_objetos_shared(pool) |>
        dplyr::filter(NAME_OBJETO == linha$OBJETO & NAME_SETOR == linha$SETOR)
      }, error = function(e) NULL)
      
      if (is.null(objeto) || !nrow(objeto)) return()
      
      cameras_ids <- unique(purrr::map_int(objeto$CONFIG[[1]]$COMPONENTES[[1]]$CAMERA, "CD_ID_CAMERA"))
      if (!length(cameras_ids)) return()
      
      title_ <- paste0("Clip – ", linha$OBJETO, " / ", linha$COMPONENTE, " (", linha$ESTADO, ")")
      tb_ <- linha$start_time
      te_ <- linha$end_time
      
      rv_clip$status  <- TRUE
      rv_clip$running <- TRUE
      .clip_notif_set("Abrindo clip…", type = "message")
      
      tryCatch({
        # não precisa actionWebUser aqui; você já está num observeEvent (sessão ok)
        video_clip_open(
          ns, input, output, session,
          pool       = pool,          # pode usar o pool já criado
          title      = title_,
          time_begin = tb_,
          time_end   = te_,
          camera_ids = cameras_ids,
          fps        = 5L,
          max_frames = 3000L,
          callback   = function(){
            rv_clip$status <- FALSE
          }
        )
        
        # ✅ modal abriu -> limpa notificação e libera para permitir novos clips
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

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

  }, once = TRUE, ignoreInit = TRUE)

  invisible(TRUE)
}
