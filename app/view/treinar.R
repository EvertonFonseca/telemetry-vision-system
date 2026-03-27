# treinar.R  (Leaflet + Player + Clips + DYNAMIC tracking retângulos "estrutura")
# =============================================================
# … + Player later::later (sem reentrância)
# … + Cache LRU + Prefetch por lote
# … + Múltiplos leafletOutput por Camera
# … + Handler JS 'set_frame_to' (id alvo)
# … + Timezone consistente (UTC) p/ chaves/queries
# … + Clip com duração máxima (default 5 min)
# … + Player de clip dentro do modal do clip (Preview independente)
# … + NOVO (OBJETO DINÂMICO):
#    - Retângulos vermelhos via Leaflet Draw (group "estrutura")
#    - Painel lateral (col-6) para selecionar estrutura + atributos do retângulo clicado
#    - Retângulos armazenados com ID 1..n reutilizável (menor livre)
#    - Edit/Delete atualiza estado (tracking-like)
#    - Inclui retângulos (filtrados por intervalo do clip) no output_ia no btSalvar
# =============================================================

box::use(
  shiny[...],
  shinyjs[inlineCSS, delay],
  stringi,
  jsonlite,
  . / model[...],
  . /
    global[
      dialogTitleClose,
      panelTitle,
      removeModalClear,
      newObserve,
      newProgressLoader,
      shinySetInputValue,
      play_sound,
      debugLocal,
      console,
      messageAlerta,
      actionWebUser,
      removeProgressLoader
    ],
  dplyr[...],
  tidyr[...],
  DT,
  shinyFiles[shinyDirButton, shinyDirChoose, parseDirPath],
  shinyWidgets[airDatepickerInput, timepickerOptions],
  shinycssloaders,
  .. / logic/objeto_dao[...],
  .. / logic/camera_dao[...],
  .. / logic/setor_dao[...],
  .. / logic/treinar_dao[...],
  base64enc[...],
  dbp = ../infra/db_pool,
  db  = ../infra/database,
  later,
  leaflet[...],
  leaflet.extras[...],
  purrr[map, map_df, map_chr, map_int],
  utils[...],
  stats[...]
)

# ------------------------------------------------------------
# Estado por sessão (NUNCA global)
# ------------------------------------------------------------
.get_private <- function(session, key = "setor_private") {
  stopifnot(!is.null(session))
  if (is.null(session$userData[[key]])) {
    session$userData[[key]] <- new.env(parent = emptyenv())
  }
  session$userData[[key]]
}

# Limpa somente o estado desse módulo nessa sessão
#' @export
dispose <- function(session, key = "setor_private") {
  e <- session$userData[[key]]
  if (!is.null(e)) {
    rm(list = ls(envir = e, all.names = TRUE), envir = e)
  }
  session$userData[[key]] <- NULL
  invisible(gc())
}

.register_auto_dispose <- function(session, key = "setor_private") {
  flag <- paste0(key, "_onend_registered")
  if (isTRUE(session$userData[[flag]])) return(invisible(NULL))
  session$userData[[flag]] <- TRUE
  session$onSessionEnded(function() {
    try(dispose(session, key), silent = TRUE)
  })
  invisible(NULL)
}

# ---------- Parâmetros ----------
PREFETCH_AHEAD  <- local({
  x <- suppressWarnings(as.integer(Sys.getenv("TVS_PREFETCH_AHEAD", "32")))
  if (!is.finite(x) || x < 8L) x <- 32L
  x
})
PREFETCH_EVERY_N <- local({
  x <- suppressWarnings(as.integer(Sys.getenv("TVS_PREFETCH_EVERY_N", "6")))
  if (!is.finite(x) || x < 1L) x <- 6L
  x
})
MAX_CACHE_ITEMS <- 400L

# (sync/interp) defaults
SYNC_TOL_MS_DEFAULT <- 120L
DYN_INTERPOLATE_DEFAULT <- TRUE
DYN_RECT_COLOR_DEFAULT  <- "#FF0000"
DYN_RECT_COLOR_SELECTED <- "#00A65A"
DYN_RECT_FILL_DEFAULT   <- 0.08
DYN_RECT_FILL_SELECTED  <- 0.20

# ==================================================
# Utils: MIME / Data URL / Dimensões de imagem
# ==================================================
detect_mime <- function(raw) {
  if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
  if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return("image/png")
  "application/octet-stream"
}
to_data_url <- function(raw) paste0("data:", detect_mime(raw), ";base64,", base64enc::base64encode(raw))

be32 <- function(v) sum(as.integer(v) * c(256^3, 256^2, 256, 1))
png_dims <- function(raw) {
  if (length(raw) < 24) return(c(NA_integer_, NA_integer_))
  if (!all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return(c(NA_integer_, NA_integer_))
  w <- be32(raw[17:20]); h <- be32(raw[21:24]); c(w, h)
}
jpeg_dims <- function(raw) {
  n <- length(raw); i <- 3L
  while (i + 8L <= n) {
    if (raw[i] != as.raw(0xFF)) { i <- i + 1L; next }
    marker <- as.integer(raw[i + 1L])
    if (marker == 0xD8 || marker == 0xD9) { i <- i + 2L; next }
    if (i + 3L > n) break
    len <- as.integer(raw[i + 2L]) * 256L + as.integer(raw[i + 3L])
    if (len < 2L) break
    if (marker >= 0xC0 && marker <= 0xC3) {
      if (i + 8L > n) break
      h <- as.integer(raw[i + 5L]) * 256L + as.integer(raw[i + 6L])
      w <- as.integer(raw[i + 7L]) * 256L + as.integer(raw[i + 8L])
      return(c(w, h))
    }
    i <- i + 2L + len
  }
  c(NA_integer_, NA_integer_)
}
img_dims <- function(raw) {
  mime <- detect_mime(raw)
  if (identical(mime, "image/png"))  return(png_dims(raw))
  if (identical(mime, "image/jpeg")) return(jpeg_dims(raw))
  c(NA_integer_, NA_integer_)
}

clip_static_values_complete <- function(input_source, objeto, id_clip, t0, t1) {
  if (is.null(objeto) || !nrow(objeto) || !isTRUE(objeto$cd_id_objeto_tipo == 1L)) {
    return(TRUE)
  }

  atributos <- collect_clip_attributes(input_source, objeto, id_clip, t0, t1)
  if (is.null(atributos) || !nrow(atributos)) {
    return(TRUE)
  }

  values <- as.character(atributos$VALUE)
  !any(is.na(values) | !nzchar(trimws(values)))
}

# ==================================================
# Utils: tempo/chaves/formatos
# ==================================================
key_of <- function(cam, ts_utc) sprintf(
  "%s|%s", as.character(cam), format(ts_utc, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
)
key_of_frame <- function(frame_id) sprintf("fid|%s", as.character(as.integer(frame_id)))
fmt_pt <- function(x, tz) format(x, tz = tz, format = "%d/%m/%y %H:%M:%S")

fmt_ts_utc_iso <- function(x, digits = 3L) {
  op <- options(digits.secs = as.integer(digits))
  on.exit(options(op), add = TRUE)
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%d %H:%M:%OS", tz = "UTC")
}

fmt_ts_utc_br <- function(x, digits = 3L) {
  op <- options(digits.secs = as.integer(digits))
  on.exit(options(op), add = TRUE)
  format(as.POSIXct(x, tz = "UTC"), "%d/%m/%Y %H:%M:%OS", tz = "UTC")
}

seq_time_keys <- function(ts_vec) {
  if (!length(ts_vec)) return(character(0))
  format(as.POSIXct(ts_vec, tz = "UTC"), "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
}

seq_timeline_rows <- function(seq_df) {
  if (is.null(seq_df) || !nrow(seq_df) || !"dt_hr_local" %in% names(seq_df)) {
    return(integer(0))
  }

  keys <- seq_time_keys(seq_df$dt_hr_local)
  as.integer(which(!duplicated(keys)))
}

timeline_pos_from_row <- function(timeline_rows, row_index) {
  if (!length(timeline_rows)) return(NA_integer_)

  row_index <- suppressWarnings(as.integer(row_index))
  if (!is.finite(row_index)) row_index <- timeline_rows[[1]]

  pos <- findInterval(row_index, timeline_rows)
  pos <- max(1L, min(length(timeline_rows), pos))
  as.integer(pos)
}

timeline_row_at <- function(timeline_rows, pos) {
  if (!length(timeline_rows)) return(NA_integer_)

  pos <- suppressWarnings(as.integer(pos))
  if (!is.finite(pos)) pos <- 1L
  pos <- max(1L, min(length(timeline_rows), pos))

  as.integer(timeline_rows[[pos]])
}

timeline_window_rows <- function(timeline_rows, total_rows, start_pos, end_pos) {
  if (!length(timeline_rows)) return(integer(0))

  total_rows <- suppressWarnings(as.integer(total_rows))
  if (!is.finite(total_rows) || total_rows < 1L) return(integer(0))

  start_pos <- suppressWarnings(as.integer(start_pos))
  end_pos   <- suppressWarnings(as.integer(end_pos))
  if (!is.finite(start_pos) || !is.finite(end_pos)) return(integer(0))

  n_steps <- length(timeline_rows)
  rng <- sort(c(
    max(1L, min(n_steps, start_pos)),
    max(1L, min(n_steps, end_pos))
  ))

  row_start <- timeline_row_at(timeline_rows, rng[[1]])
  row_end <- if (rng[[2]] < n_steps) {
    as.integer(timeline_rows[[rng[[2]] + 1L]] - 1L)
  } else {
    total_rows
  }

  if (!is.finite(row_start) || !is.finite(row_end) || row_start > row_end) {
    return(integer(0))
  }

  seq.int(row_start, row_end)
}

validaDateFormat <- function(text) {
  grepl(
    "^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/[0-9]{2} ([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$",
    text
  )
}

# ==================================================
# LRU Cache (env + funções puras)
# ==================================================
new_lru_cache <- function(max_items = MAX_CACHE_ITEMS) {
  cache <- new.env(parent = emptyenv())
  order <- character(0)
  list(
    get = function(k) {
      if (!exists(k, envir = cache, inherits = FALSE)) return(NULL)
      idx <- which(order == k)
      if (length(idx)) order <<- c(order[-idx], k)
      get(k, envir = cache, inherits = FALSE)
    },
    put = function(k, val) {
      assign(k, val, envir = cache)
      idx <- which(order == k)
      if (length(idx)) order <<- order[-idx]
      order <<- c(order, k)
      if (length(order) > max_items) {
        drop_keys <- order[seq_len(length(order) - max_items)]
        rm(list = drop_keys, envir = cache)
        order <<- tail(order, max_items)
      }
    },
    clear = function() {
      rm(list = ls(envir = cache), envir = cache)
      order <<- character(0)
    },
    keys = function() order
  )
}

# ==================================================
# PlayerContext (render + prefetch), independente do módulo
# ==================================================
new_player_ctx <- function(session, pool_getter, lru_cache) {
  env <- new.env(parent = emptyenv())
  env$session <- session
  env$get_pool <- if (is.function(pool_getter)) pool_getter else function() pool_getter
  env$cache   <- lru_cache

  # cache interno por sequência (para nearest rápido)
  env$seq_sig           <- NULL
  env$times_by_cam      <- NULL
  env$frame_ids_by_cam  <- NULL
  env$pos_by_cam        <- NULL
  env$last_frame_by_cam <- NULL
  env$timeline_rows     <- NULL
  env$prefetch_busy      <- FALSE
  env$prefetch_scheduled <- FALSE
  env$prefetch_next      <- NULL

  env$fetch_dataurl_by_frame <- function(frame_id, cam = NULL, ts_utc = NULL) {
    frame_id <- suppressWarnings(as.integer(frame_id))
    if (!is.finite(frame_id)) return(NULL)

    k <- key_of_frame(frame_id)
    hit <- env$cache$get(k)
    if (!is.null(hit)) {
      if (!is.null(cam) && !is.null(ts_utc)) env$cache$put(key_of(cam, ts_utc), hit)
      return(hit)
    }

    res <- db_fetch_frame_by_id(env$get_pool(), frame_id)
    if (!nrow(res) || is.null(res$data_frame[[1]])) return(NULL)

    uri <- to_data_url(res$data_frame[[1]])
    env$cache$put(k, uri)
    if (!is.null(cam) && !is.null(ts_utc)) env$cache$put(key_of(cam, ts_utc), uri)
    uri
  }

  env$fetch_dataurl_single <- function(cam, ts_utc, frame_id = NA_integer_) {
    frame_id <- suppressWarnings(as.integer(frame_id))
    if (is.finite(frame_id)) {
      return(env$fetch_dataurl_by_frame(frame_id, cam = cam, ts_utc = ts_utc))
    }

    k <- key_of(cam, ts_utc)
    hit <- env$cache$get(k)
    if (!is.null(hit)) return(hit)

    res <- db_fetch_frame_raw(env$get_pool(), cam, ts_utc)
    if (!nrow(res) || is.null(res$data_frame[[1]])) return(NULL)

    uri <- to_data_url(res$data_frame[[1]])
    env$cache$put(k, uri)
    uri
  }

  # assinatura simples pra detectar troca de seq_df
  env$seq_signature <- function(seq_df) {
    if (is.null(seq_df) || !nrow(seq_df)) return("empty")
    a <- as.POSIXct(seq_df$dt_hr_local[1], tz = "UTC")
    b <- as.POSIXct(seq_df$dt_hr_local[nrow(seq_df)], tz = "UTC")
    paste0(
      nrow(seq_df), "|",
      format(a, "%Y-%m-%d %H:%M:%OS", tz = "UTC"), "|",
      format(b, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    )
  }

  env$ensure_cam_times <- function(seq_df) {
    sig <- env$seq_signature(seq_df)
    if (!identical(sig, env$seq_sig)) {
      seq_df2 <- seq_df
      seq_df2$dt_hr_local <- as.POSIXct(seq_df2$dt_hr_local, tz = "UTC")
      seq_df2$cd_id_camera <- as.integer(seq_df2$cd_id_camera)
      seq_df2$cd_id_frame  <- as.integer(seq_df2$cd_id_frame)

      env$times_by_cam      <- split(seq_df2$dt_hr_local, seq_df2$cd_id_camera)
      env$frame_ids_by_cam  <- split(seq_df2$cd_id_frame, seq_df2$cd_id_camera)
      env$pos_by_cam        <- lapply(env$times_by_cam, function(x) 1L)
      env$last_frame_by_cam <- as.list(rep(NA_integer_, length(env$times_by_cam)))
      names(env$last_frame_by_cam) <- names(env$times_by_cam)
      env$timeline_rows <- seq_timeline_rows(seq_df2)
      env$prefetch_next <- NULL
      env$seq_sig <- sig
    }
    invisible(TRUE)
  }

  env$nearest_idx_monotonic <- function(times, ref, idx0) {
    n <- length(times)
    if (!n) return(NA_integer_)
    idx <- as.integer(idx0)
    if (!is.finite(idx) || idx < 1L) idx <- 1L
    if (idx > n) idx <- n

    d <- function(ii) abs(as.numeric(difftime(times[ii], ref, units = "secs")))

    while (idx < n) {
      if (d(idx + 1L) <= d(idx)) idx <- idx + 1L else break
    }
    while (idx > 1L) {
      if (d(idx - 1L) < d(idx)) idx <- idx - 1L else break
    }
    idx
  }

  # … Render sincronizado multi-Camera (nearest timestamp por Camera)
  env$render_current <- function(seq_df, i, w, h, id_map, fit_bounds = FALSE,
                                 tol_ms = SYNC_TOL_MS_DEFAULT) {

    if (is.null(seq_df) || !nrow(seq_df)) return(list(ok = FALSE, w = w, h = h))
    i <- max(1L, min(nrow(seq_df), as.integer(i)))
    env$ensure_cam_times(seq_df)

    ts_ref <- as.POSIXct(seq_df$dt_hr_local[i], tz = "UTC")
    ok_any <- FALSE

    tol_s <- suppressWarnings(as.numeric(tol_ms) / 1000)
    if (!is.finite(tol_s) || tol_s <= 0) tol_s <- as.numeric(SYNC_TOL_MS_DEFAULT) / 1000

    jobs <- list()
    job_n <- 0L

    for (cam_str in names(id_map)) {
      cam <- suppressWarnings(as.integer(cam_str))
      dom_id <- id_map[[cam_str]]
      if (is.null(dom_id) || is.na(dom_id) || !is.finite(cam)) next

      times <- env$times_by_cam[[cam_str]]
      if (is.null(times) || !length(times)) next
      ids <- env$frame_ids_by_cam[[cam_str]]
      if (is.null(ids) || !length(ids)) next

      idx0 <- env$pos_by_cam[[cam_str]]
      idx1 <- env$nearest_idx_monotonic(times, ts_ref, idx0)
      if (!is.finite(idx1)) next
      if (length(ids) < idx1) next
      env$pos_by_cam[[cam_str]] <- idx1

      ts_cam <- as.POSIXct(times[[idx1]], tz = "UTC")
      dist_s <- abs(as.numeric(difftime(ts_cam, ts_ref, units = "secs")))
      if (!is.finite(dist_s) || dist_s > tol_s) next

      frame_id <- suppressWarnings(as.integer(ids[[idx1]]))
      if (!is.finite(frame_id)) next

      last_id <- suppressWarnings(as.integer(env$last_frame_by_cam[[cam_str]]))
      if (!isTRUE(fit_bounds) && length(last_id) == 1L && is.finite(last_id) && last_id == frame_id) next

      job_n <- job_n + 1L
      jobs[[job_n]] <- list(
        cam = cam,
        cam_str = cam_str,
        dom_id = dom_id,
        ts_cam = ts_cam,
        frame_id = frame_id
      )
    }

    if (!job_n) return(list(ok = FALSE, w = w, h = h))

    miss_ids <- unique(vapply(jobs, function(j) as.integer(j$frame_id), integer(1)))
    miss_ids <- miss_ids[
      vapply(miss_ids, function(fid) is.null(env$cache$get(key_of_frame(fid))), logical(1L))
    ]

    if (length(miss_ids)) {
      blobs <- tryCatch(db_fetch_many_frames_by_id(env$get_pool(), miss_ids), error = function(e) NULL)
      if (!is.null(blobs) && nrow(blobs)) {
        for (j in seq_len(nrow(blobs))) {
          fid <- suppressWarnings(as.integer(blobs$cd_id_frame[[j]]))
          raw <- blobs$data_frame[[j]]
          if (is.finite(fid) && !is.null(raw)) {
            env$cache$put(key_of_frame(fid), to_data_url(raw))
          }
        }
      }
    }

    for (job in jobs) {
      uri <- env$fetch_dataurl_by_frame(job$frame_id, cam = job$cam, ts_utc = job$ts_cam)
      if (is.null(uri)) next

      env$session$sendCustomMessage("set_frame_to", list(
        id  = job$dom_id,
        url = uri,
        w   = as.integer(w),
        h   = as.integer(h),
        fit = isTRUE(fit_bounds)
      ))
      env$last_frame_by_cam[[job$cam_str]] <- job$frame_id
      ok_any <- TRUE
    }

    list(ok = ok_any, w = w, h = h)
  }

  env$prefetch_ahead_batch <- function(seq_df, i, N = PREFETCH_AHEAD) {
    if (is.null(seq_df) || !nrow(seq_df)) return(invisible(FALSE))
    n <- nrow(seq_df); i <- as.integer(i)
    if (i >= n) return(invisible(FALSE))
    env$ensure_cam_times(seq_df)

    timeline_rows <- env$timeline_rows
    if (is.null(timeline_rows) || !length(timeline_rows)) return(invisible(FALSE))

    cur_pos <- timeline_pos_from_row(timeline_rows, i)
    if (!is.finite(cur_pos)) return(invisible(FALSE))

    start_pos <- cur_pos + 1L
    end_pos   <- min(length(timeline_rows), cur_pos + as.integer(N))
    if (start_pos > end_pos) return(invisible(FALSE))

    tgt_idx <- timeline_window_rows(timeline_rows, n, start_pos, end_pos)
    if (!length(tgt_idx)) return(invisible(FALSE))
    seg <- seq_df[tgt_idx, , drop = FALSE]

    frame_ids <- suppressWarnings(as.integer(seg$cd_id_frame))
    frame_ids <- unique(frame_ids[is.finite(frame_ids)])
    if (!length(frame_ids)) return(invisible(FALSE))

    miss <- frame_ids[
      vapply(frame_ids, function(fid) is.null(env$cache$get(key_of_frame(fid))), logical(1L))
    ]
    if (!length(miss)) return(invisible(TRUE))

    blobs <- tryCatch(db_fetch_many_frames_by_id(env$get_pool(), miss), error = function(e) NULL)
    if (!is.null(blobs) && nrow(blobs)) {
      for (j in seq_len(nrow(blobs))) {
        fid <- suppressWarnings(as.integer(blobs$cd_id_frame[[j]]))
        raw <- blobs$data_frame[[j]]
        if (is.finite(fid) && !is.null(raw)) {
          env$cache$put(key_of_frame(fid), to_data_url(raw))
        }
      }
    }

    miss2 <- miss[
      vapply(miss, function(fid) is.null(env$cache$get(key_of_frame(fid))), logical(1L))
    ]
    if (length(miss2)) {
      for (fid in miss2) {
        invisible(env$fetch_dataurl_by_frame(fid))
      }
    }

    invisible(TRUE)
  }

  env$prefetch_request <- function(seq_df, i, N = PREFETCH_AHEAD) {
    if (is.null(seq_df) || !nrow(seq_df)) return(invisible(FALSE))

    i <- suppressWarnings(as.integer(i))
    if (!is.finite(i)) return(invisible(FALSE))

    N <- suppressWarnings(as.integer(N))
    if (!is.finite(N) || N < 1L) N <- PREFETCH_AHEAD

    env$prefetch_next <- list(seq_df = seq_df, i = i, N = N)

    if (isTRUE(env$prefetch_scheduled) || isTRUE(env$prefetch_busy)) {
      return(invisible(TRUE))
    }

    env$prefetch_scheduled <- TRUE
    later::later(function() {
      env$prefetch_scheduled <- FALSE

      task <- env$prefetch_next
      env$prefetch_next <- NULL
      if (is.null(task)) return(invisible(FALSE))

      env$prefetch_busy <- TRUE
      on.exit({
        env$prefetch_busy <- FALSE
        if (!is.null(env$prefetch_next) && !isTRUE(env$prefetch_scheduled)) {
          env$prefetch_request(env$prefetch_next$seq_df, env$prefetch_next$i, env$prefetch_next$N)
        }
      }, add = TRUE)

      try(env$prefetch_ahead_batch(task$seq_df, task$i, task$N), silent = TRUE)
      invisible(TRUE)
    }, 0)

    invisible(TRUE)
  }

  env
}

# ==================================================
# Clip Manager util
# ==================================================
clips_update_title <- function(df, id, title) {
  if (!nrow(df)) return(NULL)
  idx <- which(df$id == id)
  if (length(idx)) { df$title[idx] <- title; df } else df
}

clips_update_date_time_t0 <- function(df, id, time) {
  if (!nrow(df)) return(NULL)
  idx <- which(df$id == id)
  new_time <- as.POSIXct(as.POSIXct(time, format = "%d/%m/%y %H:%M:%S"), tz = "UTC")
  if (length(idx)) { df$t0[idx] <- new_time; df } else df
}
clips_update_date_time_t1 <- function(df, id, time) {
  if (!nrow(df)) return(NULL)
  idx <- which(df$id == id)
  new_time <- as.POSIXct(as.POSIXct(time, format = "%d/%m/%y %H:%M:%S"), tz = "UTC")
  if (length(idx)) { df$t1[idx] <- new_time; df } else df
}

clip_start <- function(rv) {
  ts0 <- rv_get_current_ts(rv)
  if (is.null(ts0)) return(FALSE)
  rv$clip_active <- TRUE
  rv$clip_t0     <- ts0
  rv$clip_i0     <- rv_get_current_step(rv)
  TRUE
}
clip_limit_exceeded <- function(rv, max_min, now_ts = NULL) {
  if (!isTRUE(rv$clip_active)) return(FALSE)
  if (is.null(rv$clip_t0))     return(FALSE)
  if (is.null(now_ts)) now_ts <- rv_get_current_ts(rv)
  if (is.null(now_ts)) return(FALSE)
  max_sec <- (if (is.finite(max_min) && max_min > 0) max_min else 5) * 60
  elapsed <- as.numeric(difftime(now_ts, rv$clip_t0, units = "secs"))
  is.finite(elapsed) && elapsed >= max_sec
}

# ==================================================
# NOVO: Helpers para retângulos dinâmicos (tracking-like)
# ==================================================
.drop_dup_last <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  if (nrow(df) >= 2) {
    last <- df[nrow(df), , drop = FALSE]
    first <- df[1, , drop = FALSE]
    if (isTRUE(all.equal(as.numeric(last$x), as.numeric(first$x))) &&
        isTRUE(all.equal(as.numeric(last$y), as.numeric(first$y)))) {
      df <- df[-nrow(df), , drop = FALSE]
    }
  }
  df
}

poly_to_box <- function(poly) {
  if (is.null(poly) || !nrow(poly)) return(NULL)
  x <- suppressWarnings(as.numeric(poly$x))
  y <- suppressWarnings(as.numeric(poly$y))
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (!length(x) || !length(y)) return(NULL)
  list(
    x_min = min(x),
    y_min = min(y),
    x_max = max(x),
    y_max = max(y)
  )
}

poly_from_feature <- function(feat) {
  if (is.null(feat$geometry$type) || !(feat$geometry$type %in% c("Polygon","MultiPolygon"))) return(NULL)
  coords <- feat$geometry$coordinates[[1]]
  if (is.null(coords) || length(coords) < 3) return(NULL)
  lng <- vapply(coords, function(x) x[[1]], numeric(1))
  lat <- vapply(coords, function(x) x[[2]], numeric(1))
  .drop_dup_last(tibble::tibble(x = lng, y = lat))
}

dynrect_next_free_id <- function(used_ids) {
  used_ids <- as.integer(used_ids)
  used_ids <- used_ids[is.finite(used_ids) & used_ids > 0L]
  if (!length(used_ids)) return(1L)
  used_ids <- sort(unique(used_ids))
  cand <- 1L
  for (u in used_ids) {
    if (u == cand) cand <- cand + 1L
    if (u > cand) break
  }
  cand
}

dynrect_empty_df <- function() {
  tibble::tibble(
    rect_id    = integer(0),
    leaflet_id = character(0),
    cam_id     = integer(0),
    created_ts_utc = as.POSIXct(character(0), tz = "UTC"),
    last_ts_utc    = as.POSIXct(character(0), tz = "UTC"),
    last_poly      = list(),
    tracking_active = logical(0),
    estrutura_id   = integer(0),
    estrutura_nome = character(0),
    attrs      = list()
  )
}

dyntrack_empty_df <- function() {
  tibble::tibble(
    rect_id    = integer(0),
    leaflet_id = character(0),
    cam_id     = integer(0),
    ts_utc     = as.POSIXct(character(0), tz = "UTC"),
    poly       = list(),
    box        = list(),
    estrutura_id   = integer(0),
    estrutura_nome = character(0),
    attrs      = list()
  )
}

dynrect_structures_df <- function(objeto) {
  comps <- objeto$config[[1]]$componentes[[1]]
  if (is.null(comps) || !nrow(comps)) return(tibble::tibble(cd_id_estrutura = integer(0), name_estrutura = character(0), atributos = list()))
  estrs <- comps$estrutura
  if (is.null(estrs) || !length(estrs)) return(tibble::tibble(cd_id_estrutura = integer(0), name_estrutura = character(0), atributos = list()))

  estr_df <- purrr::map_df(estrs, function(e){
    if (is.null(e) || !length(e)) return(NULL)
    att <- NULL
    if (!is.null(e$configs) && length(e$configs) && !is.null(e$configs[[1]]$atributos)) {
      att <- e$configs[[1]]$atributos[[1]]
    }
    tibble::tibble(
      cd_id_estrutura = as.integer(e$cd_id_estrutura[[1]]),
      name_estrutura  = as.character(e$name_estrutura[[1]]),
      atributos       = list(att)
    )
  }) |>
    dplyr::distinct(.data$cd_id_estrutura, .keep_all = TRUE)

  estr_df
}

dynrect_attrs_ui <- function(ns, attrs_df, values = NULL, locked = FALSE) {
  if (is.null(attrs_df) || !nrow(attrs_df)) return(tags$em("Sem atributos para esta estrutura."))
  out <- tagList()
  for (k in seq_len(nrow(attrs_df))) {
    att <- attrs_df[k, ]
    att_id   <- as.integer(att$cd_id_atributo[[1]])
    att_name <- as.character(att$name_atributo[[1]])
    att_type <- as.character(att$name_data[[1]])
    att_vals <- if ("value_atributo" %in% names(att)) as.character(att$value_atributo[[1]]) else ""
    cur <- NULL
    if (!is.null(values) && length(values)) {
      if (!is.null(values[[att_name]])) {
        cur <- values[[att_name]]
      } else if (!is.null(values[[toupper(att_name)]])) {
        cur <- values[[toupper(att_name)]]
      } else if (!is.null(values[[as.character(att_id)]])) {
        cur <- values[[as.character(att_id)]]
      }
    }

    if (identical(att_type, "QUALITATIVE")) {
      classes <- stringr::str_split(att_vals, ",")[[1]]
      classes <- trimws(classes)
      out <- tagAppendChildren(out,
        tagAppendAttributes(
          selectizeInput(
            ns(paste0("dynrect_att_", att_id)),
            label = att_name,
            choices = classes,
            selected = if (!is.null(cur) && nzchar(cur)) cur else NULL,
            options = list(dropdownParent = 'body', openOnFocus = TRUE, closeAfterSelect = TRUE)
          ),
          disabled = if (isTRUE(locked)) "disabled" else NULL
        )
      )
    } else {
      out <- tagAppendChildren(out,
        tagAppendAttributes(
          numericInput(
            ns(paste0("dynrect_att_", att_id)),
            label = att_name,
            value = suppressWarnings(as.numeric(cur)),
            step  = 1
          ),
          disabled = if (isTRUE(locked)) "disabled" else NULL
        )
      )
    }
  }
  out
}

# ==================================================
# Overlay resumo do clip + PLAYER DO CLIP
# ==================================================
clip_summary_overlay <- function(ns, session, input, objeto, id_clip, ts_start, ts_end, n_frames, tz_local = Sys.timezone()) {
  if (is.null(ts_start) || is.null(ts_end)) return(invisible())

  codigo_date <- paste0(id_clip, "_")
  overlay_id  <- ns("clip_summary_overlay")
  parent_sel  <- paste0("#parent", ns("dialogTrain"), " .modal-content")

  try(removeUI(selector = paste0("#", overlay_id), multiple = TRUE, immediate = TRUE), silent = TRUE)

  camera_ids    <- NULL
  camera_names  <- NULL

  atributos_mem <- collect_clip_attributes(input, objeto, id_clip, ts_start, ts_end)
  componentes   <- objeto$config[[1]]$componentes[[1]]
  divLista      <- fluidRow()

  for (i in seq_len(nrow(componentes))) {
    comp          <- componentes[i, ]
    id_comp       <- comp$cd_id_componente
    estrutura     <- comp$estrutura[[1]]
    atributos     <- estrutura$configs[[1]]$atributos[[1]]
    camera_ids    <- c(camera_ids, comp$cd_id_camera)
    listAtributos <- tagList()

    for (k in seq_len(nrow(atributos))) {
      atributo  <- atributos[k, ]
      id_html   <- ns(paste0(codigo_date, comp$cd_id_componente, "_", atributo$cd_id_atributo, "_", k))
      att_tmp   <- atributos_mem |>
        dplyr::filter(cd_id_componente == id_comp) |>
        dplyr::filter(cd_id_atributo == atributo$cd_id_atributo)

      value <- att_tmp$VALUE
      if (any(is.na(value))) value <- NULL

      if (atributo$name_data == "QUALITATIVE") {
        classes       <- stringr::str_split(atributo$value_atributo, ",")[[1]]
        listAtributos <- tagAppendChildren(
          listAtributos,
          selectizeInput(
            id_html,
            label   = atributo$name_atributo,
            choices = classes,
            selected = value,
            options = list(dropdownParent = 'body', openOnFocus = TRUE, closeAfterSelect = TRUE)
          )
        )
      } else {
        listAtributos <- tagAppendChildren(
          listAtributos,
          numericInput(id_html, label = atributo$name_atributo, value = value)
        )
      }
    }

    divLista <- tagAppendChildren(
      divLista,
      panelTitle(
        title = comp$name_componente,
        background.color.title = "white",
        title.color  = "black",
        border.color = "lightgray",
        children = div(style = "padding: 10px;", listAtributos)
      ),
      br()
    )
  }

  camera_ids <- as.integer(unique(camera_ids))
  if (is.null(camera_names)) {
    cam_tbl <- purrr::map_df(componentes$camera, ~ .x) |>
      dplyr::distinct(cd_id_camera, name_camera)
    camera_names <- vapply(camera_ids, function(cid) {
      nm <- cam_tbl$name_camera[cam_tbl$cd_id_camera == cid]
      if (length(nm)) nm[[1]] else paste("Camera", cid)
    }, character(1))
  }

  img_cards <- tagList()
  n <- length(camera_ids)
  for (k in seq_along(camera_ids)) {
    cid  <- camera_ids[k]
    name <- camera_names[k]
    img_cards <- tagAppendChildren(
      img_cards,
      column(
        width = ifelse(n > 1, 6, 12),
        panelTitle(
          title = paste0("Preview – ", name, " (", cid, ")"),
          background.color.title = "white",
          title.color  = "black",
          border.color = "lightgray",
          children = div(
            style = "padding: 10px; text-align:center;",
            tags$img(
              id = ns(paste0("clipOverlayImg_", cid)),
              style = "max-width:100%; max-height:300px; border:1px solid #ccc; border-radius:6px; background:#000;"
            )
          )
        )
      )
    )
  }

  controls_block <- div(
    style = "padding: 10px; text-align:center;",
    splitLayout(
      cellWidths = c("auto", "auto", "auto", "auto", "auto", "120px"),
      tags$button(
        class = "btn btn-outline-secondary btn-sm",
        title = "Reverse",
        onclick = sprintf(
          "Shiny.setInputValue('%s',{action:'reverse',nonce:Math.random()},{priority:'event'})",
          ns("clipOverlay_action")
        ),
        shiny::icon("backward"), " Reverse"
      ),
      tags$button(
        class = "btn btn-outline-secondary btn-sm",
        title = "Prev",
        onclick = sprintf(
          "Shiny.setInputValue('%s',{action:'prev',nonce:Math.random()},{priority:'event'})",
          ns("clipOverlay_action")
        ),
        shiny::icon("step-backward"), " Prev"
      ),
      tags$button(
        class = "btn btn-outline-secondary btn-sm",
        title = "Play",
        onclick = sprintf(
          "Shiny.setInputValue('%s',{action:'play',nonce:Math.random()},{priority:'event'})",
          ns("clipOverlay_action")
        ),
        shiny::icon("play"), " Play"
      ),
      tags$button(
        class = "btn btn-outline-secondary btn-sm",
        title = "Pause",
        onclick = sprintf(
          "Shiny.setInputValue('%s',{action:'pause',nonce:Math.random()},{priority:'event'})",
          ns("clipOverlay_action")
        ),
        shiny::icon("pause"), " Pause"
      ),
      tags$button(
        class = "btn btn-outline-secondary btn-sm",
        title = "Next",
        onclick = sprintf(
          "Shiny.setInputValue('%s',{action:'next',nonce:Math.random()},{priority:'event'})",
          ns("clipOverlay_action")
        ),
        shiny::icon("step-forward"), " Next"
      ),
      numericInput(ns("clipOverlay_step_ms"), label = "FPS", value = 25, min = 1, max = 60, step = 1, width = "120px") |>
        tagAppendAttributes(style = ";margin-top: -25px;")
    )
  )

  insertUI(
    selector = parent_sel,
    where    = "beforeEnd",
    ui = div(
      id = overlay_id,
      style = paste(
        "position: fixed; inset: 0; background: rgba(0,0,0,.5); z-index: 1060;",
        "display: flex; align-items: center; justify-content: center;"
      ),
      div(
        style = paste(
          "background:#fff; border-radius:10px; width:min(1100px,96%);",
          "height:92vh; min-height:420px; box-shadow:0 12px 30px rgba(0,0,0,.25);",
          "display:flex; flex-direction:column;"
        ),
        div(
          style = "padding:16px 18px; border-bottom:1px solid #eee; flex:0 0 auto;",
          tags$h4("Clip selecionado", style = "margin:0;")
        ),
        div(
          style = "padding:10px; flex:1 1 auto; min-height:0; overflow-y:auto; overflow-x:hidden;",
          panelTitle(
            title = "Preview do Clip (multi-Camera)",
            background.color.title = "white",
            title.color  = "black",
            border.color = "lightgray",
            children = tagList(controls_block, br(), fluidRow(img_cards))
          ),
          br(),
          panelTitle(
            title = "Componentes",
            background.color.title = "white",
            title.color  = "black",
            border.color = "lightgray",
            children = div(style = "padding: 20px;", divLista)
          )
        ),
        div(
          style = "padding:12px 18px; border-top:1px solid #eee; text-align:right; flex:0 0 auto;",
          actionButton(ns("clipCloseVideo"), "Ok", class = "btn btn-primary btn-sm", width = "80px", height = "34px")
        )
      )
    )
  )
}

# ==================================================
# UI Builders (JS handlers, Header form, Lista de Cameras, Controles)
# ==================================================
uiClipsPanel <- function(ns) {
  panelTitle(
    title = "Clips",
    background.color.title = "white",
    title.color  = "black",
    border.color = "lightgray",
    children = div(
      style = "padding: 10px;",
      tags$script(HTML("
      (function(){
        const reDateTime = /^(0[1-9]|[12][0-9]|3[01])\\/(0[1-9]|1[0-2])\\/[0-9]{2}\\s([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$/;
        $(document).on('input', 'input.shiny-datetime-mask', function(e){
          let v = $(this).val();
          v = v.replace(/[^0-9\\/ :]/g, '');
          let digits = v.replace(/[^0-9]/g,'').substring(0,12);
          let out = '';
          if(digits.length > 0){ out += digits.substring(0,2); }
          if(digits.length >= 3){ out += '/' + digits.substring(2,4); }
          if(digits.length >= 5){ out += '/' + digits.substring(4,6); }
          if(digits.length >= 7){ out += ' ' + digits.substring(6,8); }
          if(digits.length >= 9){ out += ':' + digits.substring(8,10); }
          if(digits.length >= 11){ out += ':' + digits.substring(10,12); }
          $(this).val(out);

          if(reDateTime.test(out)){
            $(this).css('border-color', '');
          } else {
            $(this).css('border-color', 'red');
          }
          Shiny.setInputValue($(this).attr('id'), out, {priority:'event'});
        });

        $(document).on('blur', 'input.shiny-datetime-mask', function(e){
          const v = $(this).val();
          if(!reDateTime.test(v)){
            $(this).css('border-color','red');
          } else {
            $(this).css('border-color','');
          }
        });
      })();
    ")),
      DT::DTOutput(ns("clipsTable"))
    )
  )
}

ui_js_handlers <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        tr.tvs-hover td { background: rgba(255, 235, 59, 0.25) !important; }
      "))
    ),
    tags$script(HTML("
      (function(){
        const overlays = {}; const boundsOf = {}; const maps = {}; const lastUrl = {};

        function fitWholeImage(map, w, h){
          const size = map.getSize();
          const scale = Math.min(size.x / w, size.y / h);
          let z = Math.log2(scale); if (!isFinite(z)) z = 0;
          const center = [h/2, w/2];
          map.setView(center, z, {animate:false});
          const b = [[0,0],[h,w]];
          map.setMaxBounds(b); map.options.maxBoundsViscosity = 1.0; map.setMinZoom(z);
          setTimeout(function(){ map.invalidateSize(); }, 0);
          return b;
        }

        Shiny.addCustomMessageHandler('set_frame_to', function(msg){
          const widget = HTMLWidgets.find('#' + msg.id); if (!widget) return;
          const map = widget.getMap(); if (!map) return;
          const w = msg.w || 512, h = msg.h || 512; const b = [[0,0],[h,w]];
          const needRecreate = (!overlays[msg.id]) || (!maps[msg.id]) || (maps[msg.id] !== map);
          if (needRecreate) {
            try { if (overlays[msg.id]) overlays[msg.id].remove(); } catch(e){}
            overlays[msg.id] = L.imageOverlay(msg.url, b, {opacity:1}).addTo(map);
            maps[msg.id]     = map;
            lastUrl[msg.id]  = msg.url;
            boundsOf[msg.id] = fitWholeImage(map, w, h);
            return;
          }
          const changed = !boundsOf[msg.id] || boundsOf[msg.id][1][0] !== h || boundsOf[msg.id][1][1] !== w;
          if (changed){
            overlays[msg.id].setBounds(b);
            boundsOf[msg.id] = b;
            if (msg.fit) boundsOf[msg.id] = fitWholeImage(map, w, h);
          }
          if (lastUrl[msg.id] !== msg.url){
            overlays[msg.id].setUrl(msg.url);
            lastUrl[msg.id] = msg.url;
          }
        });

        Shiny.addCustomMessageHandler('reset_overlays', function(ids){
          (ids || []).forEach(function(id){
            try { if (overlays[id]) overlays[id].remove(); } catch(e){}
            delete overlays[id]; delete boundsOf[id]; delete maps[id]; delete lastUrl[id];
          });
        });

        function getWidgetById(mapId){
          var el = document.getElementById(mapId);
          if(!el) return null;
          return HTMLWidgets.find('#' + mapId) || HTMLWidgets.find(el) || null;
        }

        function findLayerByLeafletId(map, lid){
          if(!map) return null;
          if(lid === null || lid === undefined) return null;
          var sid = String(lid);

          if(map._layers){
            if(map._layers[sid]) return map._layers[sid];
            var nid = Number(sid);
            if(isFinite(nid) && map._layers[nid]) return map._layers[nid];

            for(var k in map._layers){
              if(!Object.prototype.hasOwnProperty.call(map._layers, k)) continue;
              var lyr = map._layers[k];
              if(!lyr) continue;
              if(lyr._leaflet_id !== undefined && String(lyr._leaflet_id) === sid) return lyr;
              if(lyr.options && lyr.options.layerId !== undefined && String(lyr.options.layerId) === sid) return lyr;
            }
          }
          return null;
        }

        Shiny.addCustomMessageHandler('set_draw_style', function(msg){
          try{
            var widget = getWidgetById(msg.map_id);
            if(!widget || !widget.getMap) return;
            var map = widget.getMap();
            var lid = msg.leaflet_id;
            if(lid === null || lid === undefined) return;
            var layer = findLayerByLeafletId(map, lid);
            if(layer && layer.setStyle){
              var style = {
                color: msg.color || 'red',
                fillColor: msg.fillColor || msg.color || 'red',
                weight: msg.weight || 3,
                opacity: (msg.opacity === null || msg.opacity === undefined) ? 1 : msg.opacity,
                fillOpacity: (msg.fillOpacity === null || msg.fillOpacity === undefined) ? 0.1 : msg.fillOpacity
              };
              layer.setStyle(style);
              if(layer.bringToFront) layer.bringToFront();
              if(layer.redraw) layer.redraw();
              setTimeout(function(){
                try{
                  layer.setStyle(style);
                  if(layer.bringToFront) layer.bringToFront();
                  if(layer.redraw) layer.redraw();
                } catch(err){}
              }, 0);
            }
          } catch(e){}
        });

        Shiny.addCustomMessageHandler('set_draw_bounds', function(msg){
          try{
            var widget = getWidgetById(msg.map_id);
            if(!widget || !widget.getMap) return;
            var map = widget.getMap();
            if(!map) return;

            var lid = msg.leaflet_id;
            if(lid === null || lid === undefined) return;
            var layer = findLayerByLeafletId(map, lid);
            if(!layer) return;

            var x0 = Number(msg.x_min), y0 = Number(msg.y_min), x1 = Number(msg.x_max), y1 = Number(msg.y_max);
            if(!isFinite(x0)||!isFinite(y0)||!isFinite(x1)||!isFinite(y1)) return;

            var b = [[y0, x0],[y1, x1]];
            if(layer.setBounds){
              layer.setBounds(b, {animate:false});
            } else if(layer.setLatLngs){
              layer.setLatLngs([[ [y0,x0],[y0,x1],[y1,x1],[y1,x0] ]]);
            }
          } catch(e){}
        });

        Shiny.addCustomMessageHandler('remove_draw_shape', function(msg){
          try{
            var widget = getWidgetById(msg.map_id);
            if(!widget || !widget.getMap) return;
            var map = widget.getMap();
            if(!map) return;

            var lid = msg.leaflet_id;
            if(lid === null || lid === undefined) return;
            var layer = findLayerByLeafletId(map, lid);
            if(!layer) return;

            try { if(layer.closeTooltip) layer.closeTooltip(); } catch(e){}
            try { if(layer.unbindTooltip) layer.unbindTooltip(); } catch(e){}

            map.eachLayer(function(l){
              try{
                if(l && l.removeLayer && l.hasLayer && l.hasLayer(layer)) {
                  l.removeLayer(layer);
                }
              } catch(err){}
            });

            try { if(map.hasLayer && map.hasLayer(layer)) map.removeLayer(layer); } catch(e){}
            try { if(layer.remove) layer.remove(); } catch(e){}
          } catch(e){}
        });

        Shiny.addCustomMessageHandler('set_draw_tooltip', function(msg){
          try{
            var widget = getWidgetById(msg.map_id);
            if(!widget || !widget.getMap) return;
            var map = widget.getMap();
            if(!map) return;

            var lid = msg.leaflet_id;
            var layer = findLayerByLeafletId(map, lid);
            if(!layer) return;

            var text = msg.text || '';
            if(!text) return;

            try { if(layer.unbindTooltip) layer.unbindTooltip(); } catch(e){}
            if(layer.bindTooltip){
              layer.bindTooltip(text, { sticky:true, direction:'top', opacity:0.95 });
            }
          } catch(e){}
        });

        Shiny.addCustomMessageHandler('dynrect_hover_row', function(msg){
          try{
            var rootId = msg.table_id;
            var rid = String(msg.rect_id || '');
            var on  = !!msg.on;

            var $root = $('#' + rootId);
            if(!$root.length) return;

            var $table = $root.is('table') ? $root : $root.find('table').first();
            if(!$table.length) return;

            var dt = $table.DataTable();
            dt.rows().nodes().to$().removeClass('tvs-hover');
            if(!on) return;

            dt.rows().every(function(){
              var data = this.data();
              var idCell = (data && data[0] !== undefined) ? String(data[0]) : '';
              if(idCell === rid){
                $(this.node()).addClass('tvs-hover');
              }
            });
          } catch(e){}
        });

        Shiny.addCustomMessageHandler('dynrect_select_row', function(msg){
          try{
            var rootId = msg.table_id;
            var rid = String(msg.rect_id || '');
            var rowIdx = Number(msg.row_index);

            var $root = $('#' + rootId);
            if(!$root.length) return;

            var $table = $root.is('table') ? $root : $root.find('table').first();
            if(!$table.length) return;
            if(!$.fn.DataTable || !$.fn.DataTable.isDataTable($table[0])) return;

            var dt = $table.DataTable();
            var targetNode = null;

            if(isFinite(rowIdx) && rowIdx >= 1){
              var n = dt.row(rowIdx - 1).node();
              if(n) targetNode = n;
            }

            if(!targetNode && rid){
              dt.rows().every(function(){
                if(targetNode) return;
                var data = this.data();
                var idCell = (data && data[0] !== undefined) ? String(data[0]) : '';
                if(idCell === rid){
                  targetNode = this.node();
                }
              });
            }

            dt.rows().nodes().to$().removeClass('selected');
            if(!targetNode) return;

            var rowApi = dt.row(targetNode);
            var usedSelectApi = false;
            try{
              if(dt.rows && typeof dt.rows === 'function'){
                var selectedApi = dt.rows({selected:true});
                if(selectedApi && typeof selectedApi.deselect === 'function'){
                  selectedApi.deselect();
                }
              }
              if(rowApi && typeof rowApi.select === 'function'){
                rowApi.select();
                usedSelectApi = true;
              }
            } catch(err){}
            if(!usedSelectApi){
              $(targetNode).addClass('selected');
            }

            var idx0 = dt.row(targetNode).index();
            if(window.Shiny && idx0 !== null && idx0 !== undefined && isFinite(idx0)){
              Shiny.setInputValue(rootId + '_rows_selected', [idx0 + 1], {priority:'event'});
            }

            try { targetNode.scrollIntoView({block:'nearest'}); } catch(err){}
          } catch(e){}
        });

        Shiny.addCustomMessageHandler('set_clip_overlay_frame', function(msg){
          var imgEl = document.getElementById(msg.img_id);
          if(!imgEl) return;
          imgEl.src = msg.url || '';
        });

      })();
    "))
  )
}

uiMain <- function(ns, setores) {
  div(
    ui_js_handlers(),
    inlineCSS(paste0("#", ns("textNameTreino"), " {text-transform: uppercase;}")),
    panelTitle(
      title = "Configuração",
      background.color.title = "white",
      title.color  = "black",
      border.color = "lightgray",
      children = fluidRow(
        style = "padding-top: 10px; padding-left: 15px; padding-right: 15px;",
        column(3, selectizeInput(ns("comboSetor"),  label = "Setor",  choices = setores$name_setor)),
        column(3, selectizeInput(ns("comboObjeto"), label = "Objeto", choices = NA)),
        column(6, splitLayout(
          cellWidths = c("45%", "45%", "10%"),
          airDatepickerInput(
            inputId = ns("datetimeBegin"), label = "Data e Hora De:",
            language = "pt-BR", timepicker = TRUE, dateFormat = "dd/MM/yyyy",
            timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 1),
            update_on  = "change",
            readonly   = TRUE,
            onkeydown  = "if(event.key==='Enter'){this.blur();}",
            width = "98%", placeholder = "Escolha uma data e hora"
          ),
          airDatepickerInput(
            inputId = ns("datetimeEnd"), label = "Data e Hora Até:",
            language = "pt-BR", timepicker = TRUE, dateFormat = "dd/MM/yyyy",
            timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 1),
            update_on  = "change",
            readonly   = TRUE,
            onkeydown  = "if(event.key==='Enter'){this.blur();}",
            width = "98%", placeholder = "Escolha uma data e hora"
          ),
          actionButton(ns("btBuscar"), label = "", icon = icon("search"),
            style = "margin-top: 25px; margin-left: -5px;"
          )
        ))
      )
    ),
    br(),
    uiOutput(ns("uiCamerasFrames"))
  )
}

# ===== NOVO: Painel lateral para retângulos dinâmicos =====
uiDynRectPanel <- function(ns) {
  panelTitle(
    title = "Rastreamento - Estruturas (Retangulos)",
    background.color.title = "white",
    title.color  = "black",
    border.color = "lightgray",
    children = div(
      style = "padding: 10px;",
      tags$div(
        style = "margin-bottom:8px; color:#666; font-size:13px;",
        tags$b("Como usar:"), " ative um ", tags$b("Clip"), " e desenhe retangulos vermelhos. Clique na linha da tabela para selecionar e ele ficara na ", tags$b("cor escolhida"), ". ",
        "Ajuste o retangulo no Leaflet e clique em ", tags$b("Aplicar"), " para salvar o tracking (posicoes ao longo do tempo)."
      ),
      tags$div(
        style = "display:flex; align-items:center; gap:8px; margin-bottom:8px;",
        tags$label(`for` = ns("dynrect_selected_color"), style = "margin:0; color:#555; font-weight:600;", "Cor da selecao:"),
        tags$input(
          id = ns("dynrect_selected_color"),
          type = "color",
          value = DYN_RECT_COLOR_SELECTED,
          style = "width:44px; height:30px; padding:0; border:none; background:transparent; cursor:pointer;"
        ),
        tags$span(style = "font-size:12px; color:#777;", "aplicada em tempo real ao retangulo ativo.")
      ),
      DT::DTOutput(ns("dynrectTable")),
      br(),
      uiOutput(ns("dynrectEditor"))
    )
  )
}

uiCamerasComponentes <- function(ns, input, output, objeto, componentes) {

  cameras <- purrr::map_df(componentes$camera, ~ .x) |>
    dplyr::distinct(cd_id_camera, name_camera)

  id_by_cam <- list()
  divLista  <- fluidRow()
  n_cam     <- nrow(cameras)

  for (i in seq_len(n_cam)) {
    local({
      cam_id       <- cameras$cd_id_camera[i]
      comps_by_cam <- componentes[componentes$cd_id_camera == cam_id, ]

      cam_name <- cameras$name_camera[i]
      out_id   <- paste0("map_", cam_id)
      dom_id   <- ns(out_id)
      id_by_cam[[as.character(cam_id)]] <<- dom_id

      output[[out_id]] <- renderLeaflet({
        map_cam <- leaflet(options = leafletOptions(
          crs = leafletCRS(crsClass = "L.CRS.Simple"),
          zoomSnap  = 0, zoomDelta = 0.25
        ))
        
        # objeto dinâmico: draw retângulo no group "estrutura" (vermelho) + edit/remove
        if (objeto$cd_id_objeto_tipo == 2L) {
          map_cam <- map_cam |>
                      addDrawToolbar(
                        targetGroup          = "draw",
                        polylineOptions      = FALSE,
                        circleMarkerOptions  = FALSE,
                        markerOptions        = FALSE,
                        polygonOptions       = FALSE,
                        circleOptions        = FALSE,
                        rectangleOptions = drawRectangleOptions(
                          shapeOptions = drawShapeOptions(
                            color = DYN_RECT_COLOR_DEFAULT,
                            weight = 2,
                            opacity = 1,
                            fill = TRUE,
                            fillColor = DYN_RECT_COLOR_DEFAULT,
                            fillOpacity = DYN_RECT_FILL_SELECTED
                          ),
                          showArea = FALSE
                        ),
                        editOptions = editToolbarOptions(
                          edit = TRUE,
                          remove = TRUE,
                          selectedPathOptions = selectedPathOptions(
                            maintainColor = TRUE,
                            color = DYN_RECT_COLOR_DEFAULT,
                            fillColor = DYN_RECT_COLOR_DEFAULT,
                            fillOpacity = DYN_RECT_FILL_SELECTED
                          )
                        )
                      )
        }

        # … (NOVO) Clique em retângulos desenhados (Leaflet.Draw):
        # - Dispara input '<mapId>_shape_draw_click' com o leaflet_id do layer clicado.
        # - Isso permite selecionar a linha correspondente no DT (via dyn_select_by_leaflet()).
        map_cam <- map_cam |>
          htmlwidgets::onRender("
            function(el, x){
              function bindLayer(layer){
                if(!layer || !layer.on) return;
                if(layer.__tvs_bound_click) return;
                layer.__tvs_bound_click = true;
                layer.on('click', function(e){
                  try{
                    if(!window.Shiny) return;
                    if(!layer._leaflet_id) return;
                    Shiny.setInputValue(el.id + '_shape_draw_click', {id: String(layer._leaflet_id)}, {priority: 'event'});
                  } catch(err){}
                });

                layer.on('mouseover', function(e){
                  try{
                    if(!window.Shiny) return;
                    if(!layer._leaflet_id) return;
                    Shiny.setInputValue(el.id + '_shape_draw_hover', {id: String(layer._leaflet_id), nonce: Math.random()}, {priority:'event'});
                    if(layer.openTooltip) layer.openTooltip();
                  } catch(err){}
                });

                layer.on('mouseout', function(e){
                  try{
                    if(!window.Shiny) return;
                    if(!layer._leaflet_id) return;
                    Shiny.setInputValue(el.id + '_shape_draw_out', {id: String(layer._leaflet_id), nonce: Math.random()}, {priority:'event'});
                    if(layer.closeTooltip) layer.closeTooltip();
                  } catch(err){}
                });
              }
              try{
                var widget = HTMLWidgets.find(el);
                if(!widget || !widget.getMap) return;
                var map = widget.getMap();
                if(!map) return;

                map.eachLayer(function(l){ bindLayer(l); });

                map.on('layeradd', function(e){
                  if(e && e.layer) bindLayer(e.layer);
                });
              } catch(err){}
            }
          ")

        # componentes (NÃO editáveis)
        for (k in seq_len(nrow(comps_by_cam))) {
          comp      <- comps_by_cam[k, ]
          poligno   <- comp$poligno_componente[[1]]
          estrutura <- comp$estrutura[[1]]
          label <- HTML(paste0(
            "<strong>COMPONENTE:</strong> ", comp$name_componente,
            "<br><strong>estrutura:</strong> ", estrutura$name_estrutura
          ))
          
          map_cam <- map_cam |>
          addPolygons(
            group   = "comp",
            lng = poligno$x, lat = poligno$y,
            layerId = comp$cd_id_componente, weight = 2, fillOpacity = 0.1, label = label,
            options = leaflet::pathOptions(clickable = objeto$cd_id_objeto_tipo != 2L)
          )
        }

        map_cam
      })

      cameraElement <- panelTitle(
        title = paste0("Camera: ", cam_name),
        background.color.title = "white",
        title.color  = "black",
        border.color = "lightgray",
        children = div(style = "padding: 10px;", leafletOutput(dom_id, height = "350px", width = "100%"))
      )

      col_w <- if (i == n_cam && (n_cam %% 2L == 1L)) 12L else 6L
      if (objeto$cd_id_objeto_tipo == 2L) col_w <- 6L
      divLista <<- tagAppendChildren(divLista, column(col_w, cameraElement))
    })
  }

  if (objeto$cd_id_objeto_tipo == 2L) {
    ui <- tagAppendChildren(divLista, column(6L,uiDynRectPanel(ns)))
  } else {
    ui <- divLista
  }

  list(ui = ui, id_by_cam = id_by_cam, cameras = cameras)
}

uiComponenteVideo <- function(ns) {
  splitLayout(
    cellWidths = c("auto","auto","auto","auto","auto","110px","auto","120px","auto"),
    actionButton(ns("backPlay"),  label = "Reverse", icon = icon("backward"),
      class = "btn btn-outline-secondary", title = "Reproduzir para trás"
    ),
    actionButton(ns("prevFrame"), label = "Prev",    icon = icon("step-backward"),
      class = "btn btn-outline-secondary", title = "Frame anterior"
    ),
    actionButton(ns("play"),      label = "Play",    icon = icon("play"),
      class = "btn btn-outline-secondary", title = "Reproduzir"
    ),
    actionButton(ns("pause"),     label = "Pause",   icon = icon("pause"),
      class = "btn btn-outline-secondary", title = "Pausar"
    ),
    actionButton(ns("nextFrame"), label = "Next",    icon = icon("step-forward"),
      class = "btn btn-outline-secondary", title = "Próximo frame"
    ),
    numericInput(ns("step_ms"), "FPS", value = 25, min = 1, max = 60, step = 1, width = "110px") |>
      tagAppendAttributes(style = ";margin-top: -25px;"),
    actionButton(ns("clipToggle"), label = "Start Clip", icon = icon("scissors"),
      class = "btn btn-outline-primary", title = "Iniciar/encerrar clip (máx. duração)"
    ),
    numericInput(ns("clip_max_min"), "Máx (min)", value = 1, min = 1, max = 60, step = 1, width = "120px") |>
      tagAppendAttributes(style = ";margin-top: -25px;"),
    textOutput(ns("titleClock"))
  )
}

# ==================================================
# Helpers públicos/compartilhados
# ==================================================
rv_get_current_ts <- function(rv) {
  isolate({
    if (is.null(rv$seq) || !nrow(rv$seq)) return(NULL)
    i <- max(1L, min(nrow(rv$seq), rv$i))
    rv$seq$dt_hr_local[i]
  })
}

get_current_frame_ts <- function(rv, tz = "UTC", fmt = NULL) {
  ts <- rv_get_current_ts(rv)
  if (is.null(fmt)) return(ts)
  format(ts, tz = tz, usetz = FALSE, format = fmt)
}

rv_get_current_step <- function(rv) {
  isolate({
    pos <- suppressWarnings(as.integer(rv$timeline_pos))
    if (is.finite(pos) && pos >= 1L) return(pos)

    i <- suppressWarnings(as.integer(rv$i))
    if (is.finite(i) && i >= 1L) return(i)

    NA_integer_
  })
}

# ==================================================
# Módulo principal (server)
# ==================================================
#' @export
uiNewTreinar <- function(ns, input, output, session, callback, dialogTitle = "Novo Pacote") {

  .register_auto_dispose(session)
  e <- .get_private(session)

  clockupdate <- reactiveVal()
  obs         <- newObserve()
  setores     <- selectAllSetors(dbp$get_pool())
  objetos     <- selectAllObjetos(dbp$get_pool())
  tiposPacotes <- selectAllTypesPacote(dbp$get_pool())
  objeto      <- NULL

  if (nrow(objetos) == 0) {
    obs$destroy()
    showNotification("Nenhum registro de objeto foi encontrado!", type = "error")
    callback()
  }

  rv <- reactiveValues(
    seq = NULL, i = 1L, timeline_rows = integer(0), timeline_pos = NA_integer_,
    w = 512L, h = 512L,
    id_by_cam = NULL,
    clip_active = FALSE, clip_t0 = NULL, clip_i0 = NA_integer_
  )

  playing   <- reactiveVal(FALSE)
  loop_on   <- FALSE
  play_dir  <- reactiveVal(+1L)

  clips <- reactiveVal(
    data.frame(
      id = integer(0),
      title = character(0),
      t0 = as.POSIXct(character(0), tz = "UTC"),
      t1 = as.POSIXct(character(0), tz = "UTC"),
      i0 = integer(0),
      i1 = integer(0),
      estrutura_ok = logical(0),
      stringsAsFactors = FALSE
    )
  )
  next_clip_id <- reactiveVal(1L)
  current_clip_view_id <- reactiveVal(NA_integer_)

  updateClipsTable <- function(df) {
    if (!"estrutura_ok" %in% names(df)) {
      df$estrutura_ok <- FALSE
    }
    n <- nrow(df)
    if (!n) return(df)
    for (i in 1:nrow(df)) {
      id          <- df$id[i]
      df$title[i] <- isolate(input[[paste0("clip_title_", id)]])
      df          <- clips_update_date_time_t0(df, id, isolate(input[[paste0("clip_t0_", id)]]))
      df          <- clips_update_date_time_t1(df, id, isolate(input[[paste0("clip_t1_", id)]]))
    }
    df
  }

  update_clip_structure_status <- function(id_clip, input_source = input) {
    if (!isTRUE(!is.null(objeto) && objeto$cd_id_objeto_tipo == 1L)) return(invisible(FALSE))

    df <- updateClipsTable(clips())
    idx <- which(df$id == as.integer(id_clip))
    if (!length(idx)) return(invisible(FALSE))

    if (!"estrutura_ok" %in% names(df)) {
      df$estrutura_ok <- FALSE
    }

    df$estrutura_ok[idx[1]] <- clip_static_values_complete(
      input_source = input_source,
      objeto = objeto,
      id_clip = df$id[idx[1]],
      t0 = df$t0[idx[1]],
      t1 = df$t1[idx[1]]
    )

    clips(df)
    invisible(TRUE)
  }

  clips_add <- function(t0, t1, i0, i1, title = "") {
    df <- updateClipsTable(clips())
    id <- next_clip_id()
    row <- data.frame(
      id    = id,
      title = if (nzchar(title)) title else sprintf("Clip %d", id),
      t0 = as.POSIXct(t0, tz = "UTC"),
      t1 = as.POSIXct(t1, tz = "UTC"),
      i0 = as.integer(i0),
      i1 = as.integer(i1),
      estrutura_ok = if (isTRUE(!is.null(objeto) && objeto$cd_id_objeto_tipo == 1L)) {
        clip_static_values_complete(input, objeto, id, as.POSIXct(t0, tz = "UTC"), as.POSIXct(t1, tz = "UTC"))
      } else {
        TRUE
      },
      stringsAsFactors = FALSE
    )
    clips(rbind(df, row))
    next_clip_id(id + 1L)
  }

  clips_remove <- function(id) {
    df <- updateClipsTable(clips())
    if (!nrow(df)) return(invisible())
    clips(df[df$id != id, , drop = FALSE])
  }

  if (is.null(session$userData$lru_cache))  session$userData$lru_cache  <- new_lru_cache(MAX_CACHE_ITEMS)
  if (is.null(session$userData$player_ctx)) session$userData$player_ctx <- new_player_ctx(session, dbp$get_pool, session$userData$lru_cache)
  ctx <- session$userData$player_ctx

  clipOverlayPlayer <- reactiveValues(
    seq = NULL,
    timeline_rows = integer(0),
    timeline_pos = NA_integer_,
    i   = 1L,
    dir = +1L,
    playing = FALSE
  )

  render_clip_overlay_frame <- function() {
    isolate({
      seq_df <- clipOverlayPlayer$seq
      if (is.null(seq_df) || !nrow(seq_df)) return(invisible())

      seq_df$dt_hr_local <- as.POSIXct(seq_df$dt_hr_local, tz = "UTC")
      seq_df$cd_id_camera <- suppressWarnings(as.integer(seq_df$cd_id_camera))
      seq_df$cd_id_frame  <- suppressWarnings(as.integer(seq_df$cd_id_frame))

      j <- max(1L, min(nrow(seq_df), as.integer(clipOverlayPlayer$i)))
      ts_ref <- as.POSIXct(seq_df$dt_hr_local[j], tz = "UTC")

      cams <- unique(seq_df$cd_id_camera[is.finite(seq_df$cd_id_camera)])
      if (!length(cams)) return(invisible())

      idx_pick <- vapply(cams, function(cam_id) {
        idx_cam <- which(seq_df$cd_id_camera == cam_id & !is.na(seq_df$dt_hr_local))
        if (!length(idx_cam)) return(NA_integer_)
        dt <- abs(as.numeric(difftime(seq_df$dt_hr_local[idx_cam], ts_ref, units = "secs")))
        idx_cam[[which.min(dt)]]
      }, integer(1))
      idx_pick <- unique(as.integer(idx_pick[is.finite(idx_pick)]))
      if (!length(idx_pick)) return(invisible())

      by_cam <- seq_df[idx_pick, , drop = FALSE]

      fids <- suppressWarnings(as.integer(by_cam$cd_id_frame))
      fids <- unique(fids[is.finite(fids)])
      if (length(fids)) {
        miss <- fids[vapply(fids, function(fid) is.null(ctx$cache$get(key_of_frame(fid))), logical(1L))]
        if (length(miss)) {
          blobs <- tryCatch(db_fetch_many_frames_by_id(ctx$get_pool(), miss), error = function(e) NULL)
          if (!is.null(blobs) && nrow(blobs)) {
            for (jj in seq_len(nrow(blobs))) {
              fid_j <- suppressWarnings(as.integer(blobs$cd_id_frame[[jj]]))
              raw_j <- blobs$data_frame[[jj]]
              if (is.finite(fid_j) && !is.null(raw_j)) {
                ctx$cache$put(key_of_frame(fid_j), to_data_url(raw_j))
              }
            }
          }
        }
      }

      for (r in seq_len(nrow(by_cam))) {
        cam <- as.integer(by_cam$cd_id_camera[r])
        fid <- suppressWarnings(as.integer(by_cam$cd_id_frame[r]))
        ts_cam <- as.POSIXct(by_cam$dt_hr_local[r], tz = "UTC")
        uri <- ctx$fetch_dataurl_single(cam, ts_cam, frame_id = fid)
        if (!is.null(uri)) {
          session$sendCustomMessage("set_clip_overlay_frame", list(
            img_id = ns(paste0("clipOverlayImg_", cam)),
            url    = uri
          ))
        }
      }
    })
    invisible()
  }

  # Primeiro draw do overlay pode acontecer antes do DOM do modal estar pronto.
  # Faz algumas tentativas curtas para garantir que as imagens aparecam.
  render_clip_overlay_first_frame <- function(attempt = 1L, max_attempts = 6L) {
    has_seq <- shiny::isolate({
      !is.null(clipOverlayPlayer$seq) && nrow(clipOverlayPlayer$seq) > 0
    })
    if (!isTRUE(has_seq)) {
      return(invisible(FALSE))
    }

    shiny::withReactiveDomain(session, {
      render_clip_overlay_frame()
    })

    if (as.integer(attempt) < as.integer(max_attempts)) {
      later::later(function() {
        shiny::withReactiveDomain(session, {
          render_clip_overlay_first_frame(
            attempt = as.integer(attempt) + 1L,
            max_attempts = as.integer(max_attempts)
          )
        })
      }, 0.16)
    }

    invisible(TRUE)
  }

  overlay_play_step <- function() {
    withReactiveDomain(session, {
      isolate({
        if (!isTRUE(clipOverlayPlayer$playing)) return(invisible())
        if (is.null(clipOverlayPlayer$seq) || !nrow(clipOverlayPlayer$seq)) {
          clipOverlayPlayer$playing <- FALSE
          return(invisible())
        }

        timeline_rows <- isolate(clipOverlayPlayer$timeline_rows)
        n_steps <- length(timeline_rows)
        pos <- isolate({
          cur <- suppressWarnings(as.integer(clipOverlayPlayer$timeline_pos))
          if (is.finite(cur) && cur >= 1L) cur else timeline_pos_from_row(timeline_rows, clipOverlayPlayer$i)
        })
        dir <- clipOverlayPlayer$dir
        if (!n_steps || !is.finite(pos) ||
            (dir > 0L && pos >= n_steps) ||
            (dir < 0L && pos <= 1L)) {
          clipOverlayPlayer$playing <- FALSE
          return(invisible())
        }

        clipOverlayPlayer$timeline_pos <- pos + dir
        clipOverlayPlayer$i <- timeline_row_at(timeline_rows, clipOverlayPlayer$timeline_pos)
        render_clip_overlay_frame()

        fps <- suppressWarnings(as.numeric(input$clipOverlay_step_ms))
        if (!is.finite(fps)) fps <- 25
        fps <- max(1, min(60, fps))
        delay <- 1 / fps
        later::later(function() { overlay_play_step() }, delay)
      })
    })
  }

  dyn <- reactiveValues(
    rects  = dynrect_empty_df(),
    tracks = dyntrack_empty_df(),
    pending = new.env(parent = emptyenv()),
    selected_leaflet_id    = NA_character_,
    selected_rect_id       = NA_integer_,
    highlighted_leaflet_id = NA_character_,
    deleting = FALSE,
    editing  = FALSE,
    selecting_from_map = FALSE,
    editor_refresh = 0L
  )
  dyn_editor_last_rect <- NA_integer_
  dyn_editor_last_touch <- NA_integer_

  dyn_track_cache <- new.env(parent = emptyenv())
  dyn_track_cache$rev       <- 0L
  dyn_track_cache$built_rev <- -1L
  dyn_track_cache$index     <- list()
  dyn_track_cache$cursor    <- new.env(parent = emptyenv())
  dyn_track_cache$last_box_sig <- new.env(parent = emptyenv())

  dyn_clear_env <- function(env_obj) {
    if (!is.environment(env_obj)) return(invisible(FALSE))
    nm <- ls(envir = env_obj, all.names = TRUE)
    if (length(nm)) rm(list = nm, envir = env_obj)
    invisible(TRUE)
  }

  dyn_track_key <- function(rect_id, cam_id) {
    paste0(as.integer(rect_id), "|", as.integer(cam_id))
  }

  dyn_cache_clear_boxes <- function() {
    dyn_clear_env(dyn_track_cache$last_box_sig)
  }

  dyn_cache_invalidate_tracks <- function() {
    rev <- suppressWarnings(as.integer(dyn_track_cache$rev))
    if (!is.finite(rev)) rev <- 0L
    dyn_track_cache$rev <- rev + 1L
    dyn_track_cache$built_rev <- -1L
    dyn_track_cache$index <- list()
    dyn_clear_env(dyn_track_cache$cursor)
    dyn_cache_clear_boxes()
    invisible(TRUE)
  }

  dyn_get_track_index <- function() {
    if (identical(dyn_track_cache$built_rev, dyn_track_cache$rev)) {
      return(dyn_track_cache$index)
    }

    tr <- dyn$tracks
    if (is.null(tr) || !nrow(tr)) {
      dyn_track_cache$index <- list()
      dyn_track_cache$built_rev <- dyn_track_cache$rev
      dyn_clear_env(dyn_track_cache$cursor)
      return(dyn_track_cache$index)
    }

    tr2 <- tr
    tr2$rect_id <- suppressWarnings(as.integer(tr2$rect_id))
    tr2$cam_id  <- suppressWarnings(as.integer(tr2$cam_id))
    tr2$ts_utc  <- as.POSIXct(tr2$ts_utc, tz = "UTC")

    ok <- is.finite(tr2$rect_id) & is.finite(tr2$cam_id) & !is.na(tr2$ts_utc)
    if (!any(ok)) {
      dyn_track_cache$index <- list()
      dyn_track_cache$built_rev <- dyn_track_cache$rev
      dyn_clear_env(dyn_track_cache$cursor)
      return(dyn_track_cache$index)
    }
    tr2 <- tr2[ok, , drop = FALSE]

    keys <- paste0(tr2$rect_id, "|", tr2$cam_id)
    grp <- split(seq_len(nrow(tr2)), keys)

    out <- vector("list", length(grp))
    names(out) <- names(grp)

    for (k in names(grp)) {
      ii <- grp[[k]]
      ii <- ii[order(tr2$ts_utc[ii])]

      boxes <- vector("list", length(ii))
      ts_num <- as.numeric(tr2$ts_utc[ii])

      for (j in seq_along(ii)) {
        row_id <- ii[[j]]
        b <- NULL
        if ("box" %in% names(tr2)) b <- tr2$box[[row_id]]
        if (is.null(b) && "poly" %in% names(tr2)) b <- poly_to_box(tr2$poly[[row_id]])
        if (!is.null(b)) {
          vals <- suppressWarnings(as.numeric(c(b$x_min, b$y_min, b$x_max, b$y_max)))
          if (!all(is.finite(vals))) {
            b <- NULL
          } else {
            b <- list(x_min = vals[1], y_min = vals[2], x_max = vals[3], y_max = vals[4])
          }
        }
        boxes[[j]] <- b
      }

      out[[k]] <- list(ts = ts_num, box = boxes)
    }

    dyn_track_cache$index <- out
    dyn_track_cache$built_rev <- dyn_track_cache$rev
    dyn_clear_env(dyn_track_cache$cursor)
    out
  }

  dyn_box_signature <- function(box) {
    if (is.null(box)) return(NULL)
    vals <- suppressWarnings(as.numeric(c(box$x_min, box$y_min, box$x_max, box$y_max)))
    if (!all(is.finite(vals))) return(NULL)
    paste(format(round(vals, 3), nsmall = 3, trim = TRUE), collapse = "|")
  }

  dyn_has_leaflet_id <- function(leaflet_id) {
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(FALSE)
    any(as.character(df$leaflet_id) == as.character(leaflet_id))
  }

  dyn_get_by_leaflet <- function(leaflet_id) {
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(NULL)
    idx <- which(as.character(df$leaflet_id) == as.character(leaflet_id))
    if (!length(idx)) return(NULL)
    df[idx[1], , drop = FALSE]
  }

  dyn_get_by_rect <- function(rect_id) {
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(NULL)
    idx <- which(as.integer(df$rect_id) == as.integer(rect_id))
    if (!length(idx)) return(NULL)
    df[idx[1], , drop = FALSE]
  }

  dyn_tracking_active_vec <- function(df) {
    if (is.null(df) || !nrow(df)) return(logical(0))
    if (!"tracking_active" %in% names(df)) return(rep(TRUE, nrow(df)))
    out <- as.logical(df$tracking_active)
    out[is.na(out)] <- TRUE
    out
  }

  dyn_is_row_tracking_active <- function(row) {
    vals <- dyn_tracking_active_vec(row)
    if (!length(vals)) return(TRUE)
    isTRUE(vals[[1]])
  }

  dyn_tracks_by_rect <- function(rect_id) {
    tr <- dyn$tracks
    if (is.null(tr) || !nrow(tr)) return(NULL)
    out <- tr |>
      dplyr::filter(as.integer(.data$rect_id) == as.integer(.env$rect_id)) |>
      dplyr::arrange(as.POSIXct(.data$ts_utc, tz = "UTC"))
    if (!nrow(out)) return(NULL)
    out
  }

  dyn_first_kf <- function(rect_id) {
    tr <- dyn_tracks_by_rect(rect_id)
    if (is.null(tr) || !nrow(tr)) return(NULL)
    tr[1, , drop = FALSE]
  }

  dyn_last_kf <- function(rect_id) {
    tr <- dyn_tracks_by_rect(rect_id)
    if (is.null(tr) || !nrow(tr)) return(NULL)
    tr[nrow(tr), , drop = FALSE]
  }

  dyn_has_keyframe <- function(rect_id) {
    tr <- dyn_tracks_by_rect(rect_id)
    !is.null(tr) && nrow(tr) > 0
  }

  dyn_sync_rect_from_keyframes <- function(rect_id) {
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.integer(df$rect_id) == as.integer(rect_id))
    if (!length(idx)) return(invisible(FALSE))
    i <- idx[1]

    first_kf <- dyn_first_kf(rect_id)
    last_kf  <- dyn_last_kf(rect_id)
    changed <- FALSE

    if (!is.null(first_kf) && nrow(first_kf)) {
      sid <- suppressWarnings(as.integer(first_kf$estrutura_id[[1]]))
      snm <- as.character(first_kf$estrutura_nome[[1]])
      if (is.finite(sid) && !identical(suppressWarnings(as.integer(df$estrutura_id[[i]])), sid)) {
        df$estrutura_id[[i]] <- sid
        changed <- TRUE
      }
      if (nzchar(snm) && !identical(as.character(df$estrutura_nome[[i]]), snm)) {
        df$estrutura_nome[[i]] <- snm
        changed <- TRUE
      }
    }

    if (!is.null(last_kf) && nrow(last_kf)) {
      last_attrs <- last_kf$attrs[[1]]
      if (is.null(last_attrs) || !is.list(last_attrs)) last_attrs <- list()
      if (!identical(df$attrs[[i]], last_attrs)) {
        df$attrs[[i]] <- last_attrs
        changed <- TRUE
      }

      last_ts <- as.POSIXct(last_kf$ts_utc[[1]], tz = "UTC")
      if (!is.na(last_ts) && !identical(as.POSIXct(df$last_ts_utc[[i]], tz = "UTC"), last_ts)) {
        df$last_ts_utc[[i]] <- last_ts
        changed <- TRUE
      }

      last_poly <- NULL
      if ("poly" %in% names(last_kf)) last_poly <- last_kf$poly[[1]]
      if (!is.null(last_poly) && !identical(df$last_poly[[i]], last_poly)) {
        df$last_poly[[i]] <- last_poly
        changed <- TRUE
      }
    }

    if (isTRUE(changed)) dyn$rects <- df
    TRUE
  }

  dyn_reset_pending <- function() {
    dyn$pending <- new.env(parent = emptyenv())
  }

  dyn_reset <- function() {
    dyn$rects  <- dynrect_empty_df()
    dyn$tracks <- dyntrack_empty_df()
    dyn_cache_invalidate_tracks()
    dyn_reset_pending()
    dyn$selected_leaflet_id    <- NA_character_
    dyn$selected_rect_id       <- NA_integer_
    dyn$highlighted_leaflet_id <- NA_character_
    dyn$deleting <- FALSE
    dyn$editing  <- FALSE
    dyn$selecting_from_map <- FALSE
    dyn$editor_refresh <- 0L
    dyn_editor_last_rect <<- NA_integer_
    dyn_editor_last_touch <<- NA_integer_
  }

  dyn_touch_editor <- function() {
    v <- suppressWarnings(as.integer(dyn$editor_refresh))
    if (!is.finite(v)) v <- 0L
    dyn$editor_refresh <- v + 1L
    TRUE
  }

  dyn_dt_proxy <- function() {
    DT::dataTableProxy("dynrectTable", session = session)
  }

  dyn_dt_select_by_leaflet <- function(leaflet_id) {
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.character(df$leaflet_id) == as.character(leaflet_id))
    if (!length(idx)) {
      dyn$selecting_from_map <- FALSE
      return(invisible(FALSE))
    }

    dyn$selecting_from_map <- TRUE
    idx_row <- as.integer(idx[1])
    rid <- suppressWarnings(as.integer(df$rect_id[idx_row]))
    proxy <- dyn_dt_proxy()

    DT::selectRows(proxy, NULL)
    DT::selectRows(proxy, idx_row)

    session$sendCustomMessage("dynrect_select_row", list(
      table_id = ns("dynrectTable"),
      rect_id = if (is.finite(rid)) rid else NA_integer_,
      row_index = idx_row
    ))

    session$onFlushed(function() {
      try({
        proxy2 <- dyn_dt_proxy()
        DT::selectRows(proxy2, NULL)
        DT::selectRows(proxy2, idx_row)
        session$sendCustomMessage("dynrect_select_row", list(
          table_id = ns("dynrectTable"),
          rect_id = if (is.finite(rid)) rid else NA_integer_,
          row_index = idx_row
        ))
      }, silent = TRUE)
      dyn$selecting_from_map <- FALSE
    }, once = TRUE)

    TRUE
  }
  
  dyn_send_style <- function(cam_id, leaflet_id,
    color = DYN_RECT_COLOR_DEFAULT,
    fill_opacity = DYN_RECT_FILL_DEFAULT,
    weight = 3
  ) {
    if (is.null(cam_id) || !is.finite(cam_id) || is.null(leaflet_id) || !nzchar(leaflet_id)) return(invisible(FALSE))
    fill_opacity <- suppressWarnings(as.numeric(fill_opacity))
    if (!is.finite(fill_opacity)) fill_opacity <- DYN_RECT_FILL_DEFAULT
    weight <- suppressWarnings(as.numeric(weight))
    if (!is.finite(weight)) weight <- 3
    session$sendCustomMessage("set_draw_style", list(
      map_id = ns(paste0("map_", as.integer(cam_id))),
      leaflet_id = as.character(leaflet_id),
      color = color,
      fillColor = color,
      weight = weight,
      fillOpacity = fill_opacity
    ))
    TRUE
  }

  dyn_remove_shape <- function(cam_id, leaflet_id) {
    if (is.null(cam_id) || !is.finite(cam_id) || is.null(leaflet_id) || !nzchar(leaflet_id)) {
      return(invisible(FALSE))
    }

    map_dom_id <- ns(paste0("map_", as.integer(cam_id)))
    session$sendCustomMessage("remove_draw_shape", list(
      map_id = map_dom_id,
      leaflet_id = as.character(leaflet_id)
    ))
    try(
      leaflet::leafletProxy(map_dom_id) |>
        leaflet::removeShape(layerId = as.character(leaflet_id)),
      silent = TRUE
    )
    TRUE
  }

  dyn_get_selected_color <- function() {
    col <- tryCatch(as.character(input[["dynrect_selected_color"]]), error = function(e) NA_character_)
    if (length(col) != 1L || is.na(col) || !nzchar(col)) return(DYN_RECT_COLOR_SELECTED)
    if (!grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", col)) return(DYN_RECT_COLOR_SELECTED)
    toupper(col)
  }

  dyn_send_bounds <- function(cam_id, leaflet_id, box) {
    if (is.null(cam_id) || !is.finite(cam_id) || is.null(leaflet_id) || !nzchar(leaflet_id)) return(invisible(FALSE))
    if (is.null(box)) return(invisible(FALSE))
    session$sendCustomMessage("set_draw_bounds", list(
      map_id = ns(paste0("map_", as.integer(cam_id))),
      leaflet_id = as.character(leaflet_id),
      x_min = as.numeric(box$x_min),
      y_min = as.numeric(box$y_min),
      x_max = as.numeric(box$x_max),
      y_max = as.numeric(box$y_max)
    ))
    TRUE
  }

  dyn_get_box_at_ts <- function(rect_id, cam_id, ts_utc, interpolate = DYN_INTERPOLATE_DEFAULT) {
    idx_all <- dyn_get_track_index()
    key <- dyn_track_key(rect_id, cam_id)
    idx <- idx_all[[key]]
    if (is.null(idx) || !length(idx$ts)) return(NULL)

    t_now <- suppressWarnings(as.numeric(as.POSIXct(ts_utc, tz = "UTC")))
    if (!is.finite(t_now)) return(NULL)

    ts_vec <- idx$ts
    n <- length(ts_vec)

    pos <- 1L
    if (exists(key, envir = dyn_track_cache$cursor, inherits = FALSE)) {
      pos <- suppressWarnings(as.integer(get(key, envir = dyn_track_cache$cursor, inherits = FALSE)))
    }
    if (!is.finite(pos) || pos < 1L) pos <- 1L
    if (pos > n) pos <- n

    while (pos < n && ts_vec[pos + 1L] <= t_now) pos <- pos + 1L
    while (pos > 1L && ts_vec[pos] > t_now) pos <- pos - 1L
    assign(key, pos, envir = dyn_track_cache$cursor)

    if (ts_vec[pos] > t_now) return(NULL)

    b0 <- idx$box[[pos]]
    if (is.null(b0)) return(NULL)

    if (!isTRUE(interpolate)) return(b0)

    nxt <- pos + 1L
    while (nxt <= n && ts_vec[nxt] <= t_now) nxt <- nxt + 1L
    if (nxt > n) return(b0)

    b1 <- idx$box[[nxt]]
    if (is.null(b1)) return(b0)

    t0 <- ts_vec[pos]
    t1 <- ts_vec[nxt]
    dt <- t1 - t0
    if (!is.finite(dt) || dt <= 0) return(b0)

    a <- (t_now - t0) / dt
    if (!is.finite(a)) a <- 0
    a <- max(0, min(1, a))

    list(
      x_min = (1 - a) * b0$x_min + a * b1$x_min,
      y_min = (1 - a) * b0$y_min + a * b1$y_min,
      x_max = (1 - a) * b0$x_max + a * b1$x_max,
      y_max = (1 - a) * b0$y_max + a * b1$y_max
    )
  }

  dyn_update_view_on_ts <- function(ts_utc) {
    if (is.null(objeto) || !isTRUE(objeto$cd_id_objeto_tipo == 2L)) return(invisible(FALSE))
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    ts_utc <- as.POSIXct(ts_utc, tz = "UTC")
    if (is.na(ts_utc)) return(invisible(FALSE))

    live_lids <- character(0)
    tracking_active <- dyn_tracking_active_vec(df)

    for (k in seq_len(nrow(df))) {
      lid <- as.character(df$leaflet_id[[k]])
      cam <- as.integer(df$cam_id[[k]])
      rid <- as.integer(df$rect_id[[k]])
      if (!tracking_active[[k]]) next
      if (!nzchar(lid) || !is.finite(cam) || !is.finite(rid)) next
      live_lids <- c(live_lids, lid)

      box <- dyn_get_box_at_ts(rid, cam, ts_utc, interpolate = DYN_INTERPOLATE_DEFAULT)
      if (is.null(box)) next

      sig <- dyn_box_signature(box)
      if (is.null(sig)) next

      prev_sig <- NULL
      if (exists(lid, envir = dyn_track_cache$last_box_sig, inherits = FALSE)) {
        prev_sig <- get(lid, envir = dyn_track_cache$last_box_sig, inherits = FALSE)
      }
      if (identical(prev_sig, sig)) next

      assign(lid, sig, envir = dyn_track_cache$last_box_sig)
      dyn_send_bounds(cam, lid, box)
    }

    if (is.environment(dyn_track_cache$last_box_sig)) {
      old <- ls(envir = dyn_track_cache$last_box_sig, all.names = TRUE)
      stale <- setdiff(old, unique(live_lids))
      if (length(stale)) rm(list = stale, envir = dyn_track_cache$last_box_sig)
    }

    TRUE
  }

  dyn_highlight_selected <- function(force = FALSE) {
    cur <- dyn$selected_leaflet_id
    if (is.null(cur) || !nzchar(cur)) return(invisible(FALSE))

    prev <- dyn$highlighted_leaflet_id
    if (!is.null(prev) && nzchar(prev) && !identical(prev, cur)) {
      prev_row <- dyn_get_by_leaflet(prev)
      if (!is.null(prev_row) && nrow(prev_row)) {
        dyn_send_style(
          prev_row$cam_id[[1]], prev,
          color = DYN_RECT_COLOR_DEFAULT,
          fill_opacity = DYN_RECT_FILL_DEFAULT,
          weight = 3
        )
      }
    }

    if (!isTRUE(force) && identical(prev, cur)) return(invisible(TRUE))

    row <- dyn_get_by_leaflet(cur)
    if (!is.null(row) && nrow(row) && isTRUE(dyn_is_row_tracking_active(row))) {
      selected_color <- dyn_get_selected_color()
      dyn_send_style(
        row$cam_id[[1]], cur,
        color = selected_color,
        fill_opacity = DYN_RECT_FILL_SELECTED,
        weight = 4
      )
      dyn$highlighted_leaflet_id <- cur
    } else {
      dyn$highlighted_leaflet_id <- NA_character_
    }
    TRUE
  }

  dyn_select_by_leaflet <- function(leaflet_id) {
    if (is.null(leaflet_id) || !nzchar(leaflet_id)) return(invisible(FALSE))
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.character(df$leaflet_id) == as.character(leaflet_id))
    if (!length(idx)) return(invisible(FALSE))
    rid <- as.integer(df$rect_id[idx[1]])
    if (is.finite(rid)) dyn_sync_rect_from_keyframes(rid)
    dyn$selected_leaflet_id <- as.character(df$leaflet_id[idx[1]])
    dyn$selected_rect_id    <- rid
    dyn_touch_editor()
    dyn_dt_select_by_leaflet(leaflet_id)
    dyn_highlight_selected(force = TRUE)
    TRUE
  }
  
  dyn_select_by_rect <- function(rect_id) {
    if (is.null(rect_id) || !is.finite(rect_id)) return(invisible(FALSE))
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.integer(df$rect_id) == as.integer(rect_id))
    if (!length(idx)) return(invisible(FALSE))
    row <- df[idx[1], , drop = FALSE]
    rid <- as.integer(row$rect_id[[1]])
    if (is.finite(rid)) dyn_sync_rect_from_keyframes(rid)
    dyn$selected_leaflet_id <- if (isTRUE(dyn_is_row_tracking_active(row))) as.character(row$leaflet_id[[1]]) else NA_character_
    dyn$selected_rect_id    <- rid
    dyn_touch_editor()
    dyn_highlight_selected(force = TRUE)
    TRUE
  }
  
  dyn_track_upsert <- function(rect_id, leaflet_id, cam_id, ts_utc, poly,
    box = NULL,
    estrutura_id = NA_integer_,
    estrutura_nome = "",
    attrs = NULL) {
      
      ts_utc <- as.POSIXct(ts_utc, tz = "UTC")
      if (is.na(ts_utc)) return(invisible(FALSE))
      
      if (is.null(box)) box <- poly_to_box(poly)
      if (is.null(box)) return(invisible(FALSE))
      
      estrutura_id <- suppressWarnings(as.integer(estrutura_id))
      if (!is.finite(estrutura_id)) estrutura_id <- NA_integer_
      estrutura_nome <- if (is.null(estrutura_nome)) "" else as.character(estrutura_nome)
      
      if (is.null(attrs)) attrs <- list()
      if (!is.list(attrs)) attrs <- as.list(attrs)
      
      if (length(attrs)) {
        for (nm in names(attrs)) {
          v <- attrs[[nm]]
          if (is.null(v) || !length(v) || any(is.na(v))) attrs[[nm]] <- ""
          if (isTRUE(is.numeric(attrs[[nm]]))) attrs[[nm]] <- as.character(attrs[[nm]])
        }
      }
      
      tr <- dyn$tracks

      if (!is.null(tr) && nrow(tr)) {
        idx_rect <- which(as.integer(tr$rect_id) == as.integer(rect_id))
        if (length(idx_rect)) {
          ord <- idx_rect[order(as.POSIXct(tr$ts_utc[idx_rect], tz = "UTC"))]
          lock_id <- suppressWarnings(as.integer(tr$estrutura_id[ord[1]]))
          lock_nm <- as.character(tr$estrutura_nome[ord[1]])
          if (is.finite(lock_id) && nzchar(lock_nm)) {
            estrutura_id <- lock_id
            estrutura_nome <- lock_nm
          }
        }
      }
      
      if (!is.null(tr) && nrow(tr)) {
        if (!"attrs" %in% names(tr)) tr$attrs <- replicate(nrow(tr), list(list()))
        if (!"box" %in% names(tr)) tr$box <- replicate(nrow(tr), list(NULL))
        if (!"estrutura_id" %in% names(tr)) tr$estrutura_id <- rep(NA_integer_, nrow(tr))
        if (!"estrutura_nome" %in% names(tr)) tr$estrutura_nome <- rep("", nrow(tr))
      }
      
      if (is.null(tr) || !nrow(tr)) {
        dyn$tracks <- tibble::tibble(
          rect_id = as.integer(rect_id),
          leaflet_id = as.character(leaflet_id),
          cam_id = as.integer(cam_id),
          ts_utc = ts_utc,
          poly = list(poly),
          box  = list(box),
          estrutura_id   = estrutura_id,
          estrutura_nome = estrutura_nome,
          attrs = list(attrs)
        )
        dyn_cache_invalidate_tracks()
        return(TRUE)
      }
      
      idx <- which(
        as.integer(tr$rect_id) == as.integer(rect_id) &
        as.POSIXct(tr$ts_utc, tz = "UTC") == ts_utc
      )
      
      if (length(idx)) {
        j <- idx[1]
        tr$poly[[j]]  <- poly
        tr$box[[j]]   <- box
        tr$estrutura_id[[j]]   <- estrutura_id
        tr$estrutura_nome[[j]] <- estrutura_nome
        tr$attrs[[j]] <- attrs
      } else {
        tr <- dplyr::bind_rows(tr, tibble::tibble(
          rect_id = as.integer(rect_id),
          leaflet_id = as.character(leaflet_id),
          cam_id = as.integer(cam_id),
          ts_utc = ts_utc,
          poly = list(poly),
          box  = list(box),
          estrutura_id   = estrutura_id,
          estrutura_nome = estrutura_nome,
          attrs = list(attrs)
        ))
      }
      
      dyn$tracks <- tr
      dyn_cache_invalidate_tracks()
      TRUE
    }

  dyn_set_pending_poly <- function(leaflet_id, ts_utc, poly) {
    if (is.null(leaflet_id) || !nzchar(leaflet_id)) return(invisible(FALSE))
    dyn$pending[[as.character(leaflet_id)]] <- list(
      ts_utc = as.POSIXct(ts_utc, tz = "UTC"),
      poly   = poly
    )
    TRUE
  }

  dyn_get_pending_poly <- function(leaflet_id) {
    if (is.null(leaflet_id) || !nzchar(leaflet_id)) return(NULL)
    dyn$pending[[as.character(leaflet_id)]]
  }

  dyn_clear_pending <- function(leaflet_id) {
    if (is.null(leaflet_id) || !nzchar(leaflet_id)) return(invisible(FALSE))
    if (exists(as.character(leaflet_id), envir = dyn$pending, inherits = FALSE)) {
      rm(list = as.character(leaflet_id), envir = dyn$pending)
    }
    TRUE
  }

  dyn_add_rect <- function(leaflet_id, cam_id, ts_utc, poly) {
    used   <- dyn$rects$rect_id
    new_id <- dynrect_next_free_id(used)

    ts_utc <- as.POSIXct(ts_utc, tz = "UTC")

    new_row <- tibble::tibble(
      rect_id = as.integer(new_id),
      leaflet_id = as.character(leaflet_id),
      cam_id = as.integer(cam_id),
      created_ts_utc = ts_utc,
      last_ts_utc    = ts_utc,
      last_poly      = list(poly),
      tracking_active = TRUE,
      estrutura_id   = as.integer(NA),
      estrutura_nome = "",
      attrs          = list(list())
    )

    dyn$rects <- dplyr::bind_rows(dyn$rects, new_row)

    dyn$selected_leaflet_id <- as.character(leaflet_id)
    dyn$selected_rect_id    <- as.integer(new_id)

    dyn_set_pending_poly(leaflet_id, ts_utc, poly)

    dyn_highlight_selected()
    session$sendCustomMessage("set_draw_tooltip", list(
      map_id     = ns(paste0("map_", as.integer(cam_id))),
      leaflet_id = as.character(leaflet_id),
      text       = paste0("Retângulo #", as.integer(new_id))
    ))

    session$onFlushed(function() {
      try(dyn_dt_select_by_leaflet(as.character(leaflet_id)), silent = TRUE)
    }, once = TRUE)

    new_id
  }
  
  dyn_update_poly <- function(leaflet_id, ts_utc, poly) {
    if (!isTRUE(rv$clip_active)) return(invisible(FALSE))
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.character(df$leaflet_id) == as.character(leaflet_id))
    if (!length(idx)) return(invisible(FALSE))
    if (!dyn_tracking_active_vec(df)[[idx[1]]]) return(invisible(FALSE))
    dyn_set_pending_poly(leaflet_id, ts_utc, poly)
    dyn_highlight_selected(force = TRUE)
    TRUE
  }
  
  dyn_apply_tracking <- function(rv) {
    rid <- dyn$selected_rect_id
    lid <- dyn$selected_leaflet_id
    if (is.null(rid) || !is.finite(rid) || is.null(lid) || !nzchar(lid)) return(invisible(FALSE))
    
    if (!isTRUE(rv$clip_active)) {
      showNotification("Ative um Clip para aplicar tracking.", type = "error")
      return(invisible(FALSE))
    }
    
    now_ts <- rv_get_current_ts(rv)
    if (is.null(now_ts)) now_ts <- as.POSIXct(Sys.time(), tz = "UTC")
    
    base_ts <- as.POSIXct(now_ts, tz = "UTC")
    if (is.na(base_ts)) base_ts <- as.POSIXct(Sys.time(), tz = "UTC")
    ts_final <- base_ts
    
    tr0 <- dyn$tracks
    if (!is.null(tr0) && nrow(tr0)) {
      hits <- which(as.integer(tr0$rect_id) == as.integer(rid) &
      as.POSIXct(tr0$ts_utc, tz = "UTC") == base_ts)
      if (length(hits)) ts_final <- base_ts + (0.001 * length(hits))
    }
    
    pending <- dyn_get_pending_poly(lid)
    poly <- NULL
    if (!is.null(pending) && !is.null(pending$poly) && nrow(pending$poly)) {
      poly <- pending$poly
    } else {
      row <- dyn_get_by_leaflet(lid)
      if (!is.null(row) && nrow(row)) poly <- row$last_poly[[1]]
    }
    if (is.null(poly) || !nrow(poly)) return(invisible(FALSE))
    
    box <- poly_to_box(poly)
    if (is.null(box)) {
      showNotification("Não foi possível calcular o BOX.", type = "error")
      return(invisible(FALSE))
    }
    
    df <- dyn$rects
    idx <- which(as.integer(df$rect_id) == as.integer(rid))
    if (!length(idx)) return(invisible(FALSE))
    if (!dyn_tracking_active_vec(df)[[idx[1]]]) {
      showNotification("O rastreamento deste retângulo já foi parado.", type = "warning")
      return(invisible(FALSE))
    }
    
    estr_id   <- suppressWarnings(as.integer(df$estrutura_id[idx[1]]))
    estr_nome <- as.character(df$estrutura_nome[idx[1]])
    attrs_snapshot <- df$attrs[[idx[1]]]
    if (is.null(attrs_snapshot) || !is.list(attrs_snapshot)) attrs_snapshot <- list()
    
    attrs_ok <- length(attrs_snapshot) > 0 &&
    all(vapply(attrs_snapshot, function(v) !is.null(v) && length(v) && !any(is.na(v)) && nzchar(as.character(v)), logical(1)))
    
    if (!is.finite(estr_id) || !nzchar(estr_nome) || !attrs_ok) {
      showNotification("Selecione a Estrutura e preencha TODOS os atributos (sem vazio) antes de salvar keyframe.", type = "error")
      return(invisible(FALSE))
    }
    
    df$last_ts_utc[idx[1]] <- as.POSIXct(ts_final, tz = "UTC")
    df$last_poly[[idx[1]]] <- poly
    dyn$rects <- df
    
    dyn_track_upsert(
      rid, lid, df$cam_id[idx[1]], ts_final,
      poly,
      box = box,
      estrutura_id = estr_id,
      estrutura_nome = estr_nome,
      attrs = attrs_snapshot
    )
    
    dyn_clear_pending(lid)
    TRUE
  }

  dyn_stop_tracking_selected <- function() {
    rid <- suppressWarnings(as.integer(dyn$selected_rect_id))
    if (!is.finite(rid)) return(invisible(FALSE))

    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))

    idx <- which(as.integer(df$rect_id) == rid)
    if (!length(idx)) return(invisible(FALSE))
    i <- idx[1]

    if (!dyn_tracking_active_vec(df)[[i]]) return(invisible(FALSE))

    cam_id <- suppressWarnings(as.integer(df$cam_id[[i]]))
    leaflet_id <- as.character(df$leaflet_id[[i]])

    df$tracking_active[[i]] <- FALSE
    dyn$rects <- df

    dyn_clear_pending(leaflet_id)
    dyn_remove_shape(cam_id, leaflet_id)
    dyn$selected_leaflet_id <- NA_character_

    if (identical(dyn$highlighted_leaflet_id, leaflet_id)) {
      dyn$highlighted_leaflet_id <- NA_character_
    }

    dyn_cache_clear_boxes()
    dyn_touch_editor()
    TRUE
  }

  dyn_delete_selected_record <- function() {
    rid <- suppressWarnings(as.integer(dyn$selected_rect_id))
    if (!is.finite(rid)) return(invisible(FALSE))

    row <- dyn_get_by_rect(rid)
    if (is.null(row) || !nrow(row)) return(invisible(FALSE))

    leaflet_id <- as.character(row$leaflet_id[[1]])
    cam_id <- suppressWarnings(as.integer(row$cam_id[[1]]))
    if (isTRUE(dyn_is_row_tracking_active(row))) {
      dyn_remove_shape(cam_id, leaflet_id)
    }

    dyn_delete_leaflet_ids(leaflet_id)
    TRUE
  }
  
  dyn_delete_leaflet_ids <- function(leaflet_ids) {
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    removed_rect_ids <- suppressWarnings(as.integer(df$rect_id[as.character(df$leaflet_id) %in% as.character(leaflet_ids)]))
    leaf_keep <- !as.character(df$leaflet_id) %in% as.character(leaflet_ids)
    df2 <- df[leaf_keep, , drop = FALSE]
    dyn$rects <- df2
    
    tr <- dyn$tracks
    if (!is.null(tr) && nrow(tr)) {
      tr_keep <- !as.character(tr$leaflet_id) %in% as.character(leaflet_ids)
      dyn$tracks <- tr[tr_keep, , drop = FALSE]
    }
    
    for (lid in as.character(leaflet_ids)) {
      dyn_clear_pending(lid)
    }
    
    if (!nrow(df2)) {
      dyn$selected_leaflet_id    <- NA_character_
      dyn$selected_rect_id       <- NA_integer_
      dyn$highlighted_leaflet_id <- NA_character_
      dyn_editor_last_rect <<- NA_integer_
      dyn_editor_last_touch <<- NA_integer_
    } else {
      selected_rect_id <- suppressWarnings(as.integer(dyn$selected_rect_id))
      if (dyn$selected_leaflet_id %in% as.character(leaflet_ids) || selected_rect_id %in% removed_rect_ids) {
        dyn$selected_leaflet_id    <- NA_character_
        dyn$selected_rect_id       <- NA_integer_
        dyn$highlighted_leaflet_id <- NA_character_
        dyn_editor_last_rect <<- NA_integer_
        dyn_editor_last_touch <<- NA_integer_
      }
    }
    dyn_cache_invalidate_tracks()
    TRUE
  }
  
  dyn_apply_meta <- function(struct_df) {
    rid <- dyn$selected_rect_id
    if (is.null(rid) || !is.finite(rid)) return(invisible(FALSE))
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.integer(df$rect_id) == as.integer(rid))
    if (!length(idx)) return(invisible(FALSE))
    
    first_kf <- dyn_first_kf(rid)
    locked_estr_id <- NA_integer_
    locked_estr_nome <- ""
    if (!is.null(first_kf) && nrow(first_kf)) {
      locked_estr_id <- suppressWarnings(as.integer(first_kf$estrutura_id[[1]]))
      locked_estr_nome <- as.character(first_kf$estrutura_nome[[1]])
    }

    input_estr_id <- suppressWarnings(as.integer(input[["dynrect_structure"]]))
    has_locked_structure <- is.finite(locked_estr_id)

    if (isTRUE(has_locked_structure)) {
      estr_id <- as.integer(locked_estr_id)
      estr_nome <- as.character(locked_estr_nome)
      if (!nzchar(estr_nome)) {
        estr_row_tmp <- struct_df |> dplyr::filter(.data$cd_id_estrutura == estr_id)
        if (nrow(estr_row_tmp)) estr_nome <- as.character(estr_row_tmp$name_estrutura[[1]])
      }
      if (is.finite(input_estr_id) && as.integer(input_estr_id) != as.integer(estr_id)) {
        showNotification("Estrutura bloqueada pelo primeiro keyframe. Apenas atributos podem ser alterados.", type = "warning")
      }
      try(updateSelectInput(session, "dynrect_structure", selected = as.character(estr_id)), silent = TRUE)
    } else {
      estr_id <- input_estr_id
      if (!is.finite(estr_id)) {
        showNotification("Selecione uma estrutura.", type = "error")
        return(invisible(FALSE))
      }
      estr_row_tmp <- struct_df |> dplyr::filter(.data$cd_id_estrutura == estr_id)
      estr_nome <- if (nrow(estr_row_tmp)) as.character(estr_row_tmp$name_estrutura[[1]]) else ""
    }

    estr_row <- struct_df |> dplyr::filter(.data$cd_id_estrutura == as.integer(estr_id))
    if (!nrow(estr_row) || !nzchar(estr_nome)) {
      showNotification("Estrutura sem nome (inválida).", type = "error")
      return(invisible(FALSE))
    }
    
    attrs_df <- NULL
    if (nrow(estr_row) && "atributos" %in% names(estr_row)) attrs_df <- estr_row$atributos[[1]]
    
    vals <- list()
    missing <- character(0)
    
    if (!is.null(attrs_df) && nrow(attrs_df)) {
      for (k in seq_len(nrow(attrs_df))) {
        att <- attrs_df[k, ]
        att_id   <- as.integer(att$cd_id_atributo[[1]])
        att_name <- as.character(att$name_atributo[[1]])
        att_type <- as.character(att$name_data[[1]])
        
        inp_id <- paste0("dynrect_att_", att_id)
        v <- input[[inp_id]]
        
        key <- trimws(att_name)
        if (!nzchar(key)) key <- as.character(att_id)
        if (!is.null(vals[[key]])) key <- paste0(key, "__", att_id)
        
        ok <- TRUE
        if (identical(att_type, "QUALITATIVE")) {
          if (is.null(v) || is.na(v) || !nzchar(as.character(v))) ok <- FALSE
          if (!ok) missing <- c(missing, att_name)
          v <- if (!ok) "" else as.character(v)
        } else {
          vnum <- suppressWarnings(as.numeric(v))
          if (is.null(v) || is.na(vnum) || !is.finite(vnum)) ok <- FALSE
          if (!ok) missing <- c(missing, att_name)
          v <- if (!ok) "" else as.character(vnum)
        }
        
        vals[[key]] <- v
      }
    }
    
    if (length(missing)) {
      showNotification(
        paste0("Preencha TODOS os atributos (sem vazio): ", paste(unique(missing), collapse = ", ")),
        type = "error"
      )
      return(invisible(FALSE))
    }
    
    df$estrutura_id[idx[1]]   <- estr_id
    df$estrutura_nome[idx[1]] <- estr_nome
    df$attrs[[idx[1]]]        <- vals
    dyn$rects <- df
    TRUE
  }

  id       <- ns("dialogTrain")
  cssStyle <- list()
  cssStyle[[paste0(" #parent", id, " .modal-dialog")]]  <- "width: 95% !important; height: 90% !important;"
  cssStyle[[paste0(" #parent", id, " .modal-content")]] <- "width: 100% !important; height: 100% !important;"
  cssStyle[[paste0(" #parent", id, " .modal-body")]]    <- "width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto; overflow-x: hidden;"

  showModal(
    session = session,
    div(
      id = paste0("parent", id),
      style = "height: 80%; overflow: hidden;",
      inlineCSS(cssStyle),
      dialogModal(
        title = dialogTitle,
        size  = "m",
        uiMain(ns, setores),
        footer = uiOutput(ns("uiFooter"))
      )
    )
  )
  output$uiFooter <- renderUI(tagList(
    actionButton(ns("btSair"),  label = "Sair",    icon = icon("arrow-left")),
    actionButton(ns("btSalvar"), class = "btn-primary", label = "Salvar", icon = icon("save"))
  ))

  obs$add(observeEvent(input$comboSetor, {
    setor <- setores |> dplyr::filter(name_setor == input$comboSetor)
    if (!nrow(setor)) return()
    objs_setor <- objetos |> dplyr::filter(cd_id_setor == setor$cd_id_setor)
    updateSelectizeInput(session, "comboObjeto", choices = objs_setor$name_objeto)

    output$uiCamerasFrames <- NULL
    playing(FALSE); loop_on <<- FALSE
    session$userData$lru_cache$clear()
    dyn_reset()
    rv$timeline_rows <- integer(0)
    rv$timeline_pos <- NA_integer_

    if (isTRUE(rv$clip_active)) {
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }
  }, ignoreNULL = TRUE))

  obs$add(observeEvent(input$btBuscar, {
    loader_started <- FALSE
    loader_managed_async <- FALSE
    on.exit({
      if (isTRUE(loader_started) && !isTRUE(loader_managed_async)) {
        removeProgressLoader(session = session)
      }
    }, add = TRUE)

    objeto <<- objetos |> dplyr::filter(name_objeto == isolate(input$comboObjeto))
    if (!nrow(objeto)) {
      showNotification("Selecione um objeto válido.", type = "warning")
      return(invisible())
    }

    time_begin <- isolate(input$datetimeBegin)
    time_end   <- isolate(input$datetimeEnd)

    if (is.null(time_begin)) {
      showNotification("'Data e Hora De' está com campo vazio!", type = "warning")
      return(invisible())
    } else if (is.null(time_end)) {
      showNotification("'Data e Hora Até' está com campo vazio!", type = "warning")
      return(invisible())
    }

    time_begin  <- as.POSIXct(time_begin, tz = "UTC")
    time_end    <- as.POSIXct(time_end, tz = "UTC")
    componentes <- objeto$config[[1]]$componentes[[1]]
    cameras_ids <- unique(purrr::map_int(componentes$camera, "cd_id_camera"))

    newProgressLoader(session)
    loader_started <- TRUE

    frames_idx <- fetch_frames(
      dbp$get_pool(),
      time_begin    = time_begin,
      time_end      = time_end,
      camera_id_vec = cameras_ids
    )
    if (!nrow(frames_idx)) {
      showNotification("Nenhum frame no intervalo/Cameras selecionados.", type = "warning")
      return(invisible())
    }

    res_cam <- uiCamerasComponentes(ns, input, output, objeto, componentes)
    rv$id_by_cam <- res_cam$id_by_cam
    dyn_reset()

    output$uiCamerasFrames <- renderUI({
      output$titleClock <- renderText({
        clockupdate()
        get_current_frame_ts(rv, tz = Sys.timezone(), fmt = "%d/%m/%y %H:%M:%S")
      })
      tagList(
        br(),
        uiComponenteVideo(ns),
        uiClipsPanel(ns),
        br(),
        res_cam$ui
      )
    })

    session$userData$lru_cache$clear()
    if (isTRUE(rv$clip_active)) {
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }
    timeline_rows <- seq_timeline_rows(frames_idx)
    rv$seq <- frames_idx
    rv$timeline_rows <- timeline_rows
    rv$timeline_pos <- if (length(timeline_rows)) 1L else NA_integer_
    rv$i <- if (length(timeline_rows)) timeline_row_at(timeline_rows, 1L) else 1L
    rv$w <- 512L
    rv$h <- 512L
    playing(FALSE); loop_on <<- FALSE

    first <- frames_idx[rv$i, , drop = FALSE]
    fid1 <- suppressWarnings(as.integer(first$cd_id_frame[[1]]))
    if (is.finite(fid1)) {
      res1 <- db_fetch_frame_by_id(dbp$get_pool(), fid1)
    } else {
      res1 <- db_fetch_frame_raw(dbp$get_pool(), first$cd_id_camera, first$dt_hr_local)
    }
    if (nrow(res1) && !is.null(res1$data_frame[[1]])) {
      wh <- img_dims(res1$data_frame[[1]])
      rv$w <- if (is.finite(wh[1])) as.integer(wh[1]) else 512L
      rv$h <- if (is.finite(wh[2])) as.integer(wh[2]) else 350L
    }

    id_map <- rv$id_by_cam; seq_df <- rv$seq; i0 <- rv$i; w0 <- rv$w; h0 <- rv$h
    loader_managed_async <- TRUE
    session$onFlushed(function() {
      # Tolerancia larga somente no bootstrap inicial para desenhar
      # frame em todos os Leaflets das cameras carregadas.
      initial_tol_ms <- 7 * 24 * 60 * 60 * 1000

      # Render inicial robusto: tenta algumas vezes para cobrir o tempo
      # de inicializacao dos widgets Leaflet apos o renderUI.
      render_first_frames <- function(attempt = 1L, max_attempts = 6L) {
        st <- ctx$render_current(
          seq_df = seq_df,
          i = i0,
          w = w0,
          h = h0,
          id_map = id_map,
          fit_bounds = TRUE,
          tol_ms = initial_tol_ms
        )
        isolate({
          if (isTRUE(st$ok)) {
            rv$w <- st$w
            rv$h <- st$h
          }
        })
        shiny::isolate({
          dyn_update_view_on_ts(rv_get_current_ts(rv))
        })

        if (attempt < max_attempts) {
          later::later(function() {
            render_first_frames(attempt = attempt + 1L, max_attempts = max_attempts)
          }, 0.18)
        }
        invisible(TRUE)
      }

      session$sendCustomMessage("reset_overlays", unname(id_map))
      render_first_frames()
      ctx$prefetch_request(seq_df = seq_df, i = i0, N = PREFETCH_AHEAD)

      removeProgressLoader(session = session, callback = function() {
        render_first_frames(attempt = 1L, max_attempts = 3L)
      })
    }, once = TRUE)

    if (isTRUE(objeto$cd_id_objeto_tipo == 2L)) {

      if (is.null(e$dyn_map_registered)) e$dyn_map_registered <- character(0)

      cams_tbl <- res_cam$cameras
      cam_ids  <- as.integer(cams_tbl$cd_id_camera)

      for (cid in cam_ids) {
        map_out_id <- paste0("map_", cid)
        map_dom_id <- ns(map_out_id)

        if (map_dom_id %in% e$dyn_map_registered) next
        e$dyn_map_registered <- c(e$dyn_map_registered, map_dom_id)
        
        obs$add(observeEvent(input[[paste0(map_out_id, "_draw_new_feature")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))
          feat <- input[[paste0(map_out_id, "_draw_new_feature")]]
          
          if (is.null(feat)) return()
          poly <- poly_from_feature(feat)
          if (is.null(poly) || !nrow(poly)) return()
          
          leaflet_id <- feat$properties$`_leaflet_id`
          if (!is.null(feat$properties$layerId)) leaflet_id <- feat$properties$layerId
          leaflet_id <- as.character(leaflet_id)

          if (!isTRUE(rv$clip_active)) {
            showNotification("Ative um Clip antes de desenhar retângulos (tracking é sempre dentro do clip).", type = "warning")
            try(leaflet::leafletProxy(map_dom_id) |> leaflet::removeShape(layerId = leaflet_id), silent = TRUE)
            return()
          }
          
          ts <- rv_get_current_ts(rv)
          if (is.null(ts)) ts <- as.POSIXct(Sys.time(), tz = "UTC")
          
          new_id <- dyn_add_rect(leaflet_id = leaflet_id, cam_id = cid, ts_utc = ts, poly = poly)
          showNotification(paste0("Retângulo #", new_id, " criado. Clique e aplique Estrutura/Atributos."), type = "message")
        }, ignoreInit = TRUE))
        
        obs$add(observeEvent(input[[paste0(map_out_id, "_draw_edited_features")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))

          if (!isTRUE(rv$clip_active)) return()
          feats <- input[[paste0(map_out_id, "_draw_edited_features")]]
          
          if (is.null(feats$features) || length(feats$features) == 0) return()
          ts <- rv_get_current_ts(rv)
          if (is.null(ts)) ts <- as.POSIXct(Sys.time(), tz = "UTC")
          
          for (feat in feats$features) {
            poly <- poly_from_feature(feat)
            if (is.null(poly) || !nrow(poly)) next
            leaflet_id <- feat$properties$`_leaflet_id`
            if (!is.null(feat$properties$layerId)) leaflet_id <- feat$properties$layerId
            leaflet_id <- as.character(leaflet_id)
            
            dyn_update_poly(leaflet_id = leaflet_id, ts_utc = ts, poly = poly)
          }
        }, ignoreInit = TRUE))
        
        obs$add(observeEvent(input[[paste0(map_out_id, "_draw_deleted_features")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))
          feats <- input[[paste0(map_out_id, "_draw_deleted_features")]]
          if (is.null(feats$features) || length(feats$features) == 0) return()
          
          ids <- character(0)
          for (feat in feats$features) {
            leaflet_id <- feat$properties$`_leaflet_id`
            if (!is.null(feat$properties$layerId)) leaflet_id <- feat$properties$layerId
            ids <- c(ids, as.character(leaflet_id))
          }
          ids <- unique(ids)
          if (length(ids)) dyn_delete_leaflet_ids(ids)
        }, ignoreInit = TRUE))
        
        obs$add(observeEvent(input[[paste0(map_out_id, "_draw_deletestart")]], {
          dyn$deleting <- TRUE
        }, ignoreInit = TRUE))
        obs$add(observeEvent(input[[paste0(map_out_id, "_draw_deletestop")]], {
          dyn$deleting <- FALSE
        }, ignoreInit = TRUE))
        obs$add(observeEvent(input[[paste0(map_out_id, "_draw_editstart")]], {
          dyn$editing <- TRUE
        }, ignoreInit = TRUE))
        obs$add(observeEvent(input[[paste0(map_out_id, "_draw_editstop")]], {
          dyn$editing <- FALSE
        }, ignoreInit = TRUE))
        
        obs$add(observeEvent(input[[paste0(map_out_id, "_shape_click")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))
          
          if (isTRUE(dyn$deleting) || isTRUE(dyn$editing)) return()
          
          ev <- input[[paste0(map_out_id, "_shape_click")]]
          if (is.null(ev) || is.null(ev$id)) return()
          
          lid <- as.character(ev$id)
          
          if (!dyn_has_leaflet_id(lid)) return()
          
          dyn_select_by_leaflet(lid)
        }, ignoreInit = TRUE, ignoreNULL = TRUE))
        
        obs$add(observeEvent(input[[paste0(map_out_id, "_shape_draw_click")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))
          
          if (isTRUE(dyn$deleting) || isTRUE(dyn$editing)) return()
          
          ev <- input[[paste0(map_out_id, "_shape_draw_click")]]
          if (is.null(ev) || is.null(ev$id)) return()
          
          lid <- as.character(ev$id)
          
          if (!dyn_has_leaflet_id(lid)) return()
          
          dyn_select_by_leaflet(lid)
        }, ignoreInit = TRUE, ignoreNULL = TRUE))

        obs$add(observeEvent(input[[paste0(map_out_id, "_shape_draw_out")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))
          ev <- input[[paste0(map_out_id, "_shape_draw_out")]]
          if (is.null(ev) || is.null(ev$id)) return()
          lid <- as.character(ev$id)
          if (!dyn_has_leaflet_id(lid)) return()

          session$sendCustomMessage("dynrect_hover_row", list(
            table_id = ns("dynrectTable"),
            rect_id  = 0,
            on       = FALSE
          ))
        }, ignoreInit = TRUE, ignoreNULL = TRUE))

        obs$add(observeEvent(input[[paste0(map_out_id, "_shape_draw_hover")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))
          if (isTRUE(dyn$deleting) || isTRUE(dyn$editing)) return()

          ev <- input[[paste0(map_out_id, "_shape_draw_hover")]]
          if (is.null(ev) || is.null(ev$id)) return()
          lid <- as.character(ev$id)
          if (!dyn_has_leaflet_id(lid)) return()

          row <- dyn_get_by_leaflet(lid)
          if (is.null(row) || !nrow(row)) return()
          rid <- as.integer(row$rect_id[[1]])

          session$sendCustomMessage("dynrect_hover_row", list(
            table_id = ns("dynrectTable"),
            rect_id  = rid,
            on       = TRUE
          ))
        }, ignoreInit = TRUE, ignoreNULL = TRUE))

      }
    }
  }, ignoreInit = TRUE))

  step_frame <- function(delta) {
    req(rv$seq, nrow(rv$seq) > 0)
    timeline_rows <- isolate(rv$timeline_rows)
    n_steps <- length(timeline_rows)
    if (!n_steps) return(invisible())

    cur_pos <- isolate({
      pos <- suppressWarnings(as.integer(rv$timeline_pos))
      if (is.finite(pos) && pos >= 1L) pos else timeline_pos_from_row(timeline_rows, rv$i)
    })
    new_pos <- max(1L, min(n_steps, cur_pos + as.integer(delta)))
    if (new_pos == cur_pos) return(invisible())

    rv$timeline_pos <- new_pos
    rv$i <- timeline_row_at(timeline_rows, new_pos)
    st <- ctx$render_current(rv$seq, rv$i, rv$w, rv$h, rv$id_by_cam, fit_bounds = FALSE)
    if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }
    ctx$prefetch_request(rv$seq, rv$i, PREFETCH_AHEAD)
    dyn_update_view_on_ts(rv_get_current_ts(rv))

    if (clip_limit_exceeded(rv, suppressWarnings(as.numeric(input$clip_max_min)))) {
      playing(FALSE); loop_on <<- FALSE
      ts_start <- rv$clip_t0; i0 <- rv$clip_i0
      ts_end   <- rv_get_current_ts(rv); i1 <- rv_get_current_step(rv)
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
      clips_add(ts_start, ts_end, i0, i1)
    }
  }
  obs$add(observeEvent(input$prevFrame, { step_frame(-1L) }, ignoreInit = TRUE))
  obs$add(observeEvent(input$nextFrame, { step_frame(+1L) }, ignoreInit = TRUE))
  
  play_step <- function() {
    withReactiveDomain(session, {
      isolate({
        if (!isTRUE(playing())) { loop_on <<- FALSE; return(invisible()) }
        if (is.null(rv$seq) || nrow(rv$seq) == 0L) { playing(FALSE); loop_on <<- FALSE; return(invisible()) }
        
        t0 <- proc.time()[3]
        
        timeline_rows <- isolate(rv$timeline_rows)
        n_steps <- length(timeline_rows)
        pos <- isolate({
          cur <- suppressWarnings(as.integer(rv$timeline_pos))
          if (is.finite(cur) && cur >= 1L) cur else timeline_pos_from_row(timeline_rows, rv$i)
        })
        dir <- play_dir()
        if (!n_steps || !is.finite(pos) ||
            (dir > 0L && pos >= n_steps) ||
            (dir < 0L && pos <= 1L)) {
          playing(FALSE); loop_on <<- FALSE; return(invisible())
        }
        
        rv$timeline_pos <- pos + dir
        rv$i <- timeline_row_at(timeline_rows, rv$timeline_pos)
        st <- ctx$render_current(rv$seq, rv$i, rv$w, rv$h, rv$id_by_cam, fit_bounds = FALSE)
        if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }
        dyn_update_view_on_ts(rv_get_current_ts(rv))
        
        if ((rv$i %% PREFETCH_EVERY_N) == 0L) {
          ctx$prefetch_request(rv$seq, rv$i, PREFETCH_AHEAD)
        }
        clockupdate(Sys.time())

        if (clip_limit_exceeded(rv, suppressWarnings(as.numeric(input$clip_max_min)))) {
          playing(FALSE); loop_on <<- FALSE
          ts_start <- rv$clip_t0; i0 <- rv$clip_i0
          ts_end   <- rv_get_current_ts(rv); i1 <- rv_get_current_step(rv)
          rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
          updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
          clips_add(ts_start, ts_end, i0, i1)
          return(invisible())
        }
        
        fps <- suppressWarnings(as.numeric(input$step_ms))
        if (!is.finite(fps)) fps <- 25
        fps <- max(1, min(60, fps))
        target <- 1 / fps
        
        elapsed <- proc.time()[3] - t0
        delay <- max(0.001, target - elapsed)
        
        later::later(function() { play_step() }, delay)
      })
    })
  }

  obs$add(observeEvent(input$play, {
    play_dir(+1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  obs$add(observeEvent(input$backPlay, {
    play_dir(-1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  obs$add(observeEvent(input$pause, { playing(FALSE) }, ignoreInit = TRUE))

  obs$add(observeEvent(input$clipToggle, {
    if (!isTRUE(rv$clip_active)) {
      if (!clip_start(rv)) {
        showNotification("Nenhum frame carregado para iniciar o clip.", type = "warning")
        return(invisible())
      }
      updateActionButton(session, "clipToggle", label = "Stop Clip", icon = icon("square"))
      showNotification("Clip iniciado.", type = "message")
      play_dir(+1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
    } else {
      playing(FALSE); loop_on <<- FALSE
      ts_start <- rv$clip_t0; i0 <- rv$clip_i0
      ts_end   <- rv_get_current_ts(rv); i1 <- rv_get_current_step(rv)
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
      clips_add(ts_start, ts_end, i0, i1)
    }
  }, ignoreInit = TRUE))

  output$dynrectTable <- DT::renderDT({
    req(!is.null(objeto))
    if (!isTRUE(objeto$cd_id_objeto_tipo == 2L)) {
      return(DT::datatable(data.frame(), options = list(dom = "t")))
    }
    df <- dyn$rects
    rid_sel <- suppressWarnings(as.integer(dyn$selected_rect_id))
    if (is.null(df) || !nrow(df)) {
      return(DT::datatable(
        data.frame(
          ID = integer(0),
          Status = character(0),
          Camera = integer(0),
          Estrutura = character(0),
          UltimoFrame = character(0),
          stringsAsFactors = FALSE
        ),
        selection = "single",
        options = list(dom = "t", paging = FALSE, ordering = FALSE,
          language = list(emptyTable = "Nenhum retangulo criado ainda.")
        )
      ))
    }

    is_pending <- vapply(df$leaflet_id, function(lid) {
      lid <- as.character(lid)
      if (!nzchar(lid)) return(FALSE)
      exists(lid, envir = dyn$pending, inherits = FALSE)
    }, logical(1L))
    tracking_active <- dyn_tracking_active_vec(df)

    selected_row <- integer(0)
    if (is.finite(rid_sel)) {
      idx_sel <- which(as.integer(df$rect_id) == as.integer(rid_sel))
      if (length(idx_sel)) selected_row <- as.integer(idx_sel[1])
    }

    out <- df |>
      dplyr::mutate(
        ID = .data$rect_id,
        Status = dplyr::case_when(
          !tracking_active ~ "PARADO",
          is_pending ~ "PENDENTE",
          TRUE ~ "OK"
        ),
        Camera = .data$cam_id,
        Estrutura = ifelse(nzchar(.data$estrutura_nome), .data$estrutura_nome, "(nao definido)"),
        UltimoFrame = format(.data$last_ts_utc, tz = Sys.timezone(), format = "%d/%m/%y %H:%M:%S")
      ) |>
      dplyr::select(.data$ID, .data$Status, .data$`Camera`, .data$Estrutura, .data$UltimoFrame)

    DT::datatable(
      out,
      rownames = FALSE,
      selection = list(mode = "single", selected = selected_row),
      escape = TRUE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE
      )
    )
  })

  obs$add(observeEvent(input$dynrectTable_rows_selected, {
    req(!is.null(objeto), isTRUE(objeto$cd_id_objeto_tipo == 2L))

    if (isTRUE(dyn$selecting_from_map)) {
      dyn$selecting_from_map <- FALSE
      rid_cur <- suppressWarnings(as.integer(dyn$selected_rect_id))
      if (is.finite(rid_cur)) dyn_sync_rect_from_keyframes(rid_cur)
      dyn_touch_editor()
      dyn_highlight_selected(force = TRUE)
      return(invisible())
    }

    sel <- input$dynrectTable_rows_selected
    if (is.null(sel) || !length(sel)) return(invisible())

    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible())

    rid <- suppressWarnings(as.integer(df$rect_id[sel[1]]))
    if (!is.finite(rid)) return(invisible())

    if (isTRUE(!is.null(dyn$selected_rect_id)) && isTRUE(as.integer(dyn$selected_rect_id) == rid)) {
      dyn_sync_rect_from_keyframes(rid)
      dyn_touch_editor()
      dyn_highlight_selected(force = TRUE)
      return(invisible())
    }

    dyn_select_by_rect(rid)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$dynrect_selected_color, {
    req(!is.null(objeto), isTRUE(objeto$cd_id_objeto_tipo == 2L))
    dyn_highlight_selected(force = TRUE)
  }, ignoreInit = TRUE))

  output$dynrectEditor <- renderUI({
    req(!is.null(objeto))
    if (!isTRUE(objeto$cd_id_objeto_tipo == 2L)) return(NULL)
    editor_touch <- suppressWarnings(as.integer(dyn$editor_refresh))
    if (!is.finite(editor_touch)) editor_touch <- 0L

    df <- dyn$rects
    if (is.null(df) || !nrow(df)) {
      return(div(tags$em("Crie um retângulo vermelho em qualquer Camera para começar.")))
    }

    rid <- dyn$selected_rect_id
    if (is.null(rid) || !isTRUE(is.finite(as.numeric(rid)))) {
      return(div(tags$em("Clique em um retângulo vermelho para editar Estrutura e Atributos.")))
    }

    idx <- which(as.integer(df$rect_id) == as.integer(rid))
    if (!length(idx)) return(div(tags$em("Clique em um retângulo vermelho para editar.")))
    row <- df[idx[1], , drop = FALSE]
    tracking_active <- dyn_is_row_tracking_active(row)

    struct_df <- dynrect_structures_df(objeto)
    if (is.null(struct_df) || !nrow(struct_df)) {
      return(div(tags$em("Nenhuma estrutura encontrada para este objeto.")))
    }

    choices <- setNames(struct_df$cd_id_estrutura, struct_df$name_estrutura)

    is_ok_int <- function(x) {
      x <- suppressWarnings(as.integer(x))
      isTRUE(is.finite(x)) && !is.na(x)
    }

    first_kf <- dyn_first_kf(rid)
    last_kf  <- dyn_last_kf(rid)
    prev_editor_rect <- suppressWarnings(as.integer(dyn_editor_last_rect))
    rect_changed <- !is_ok_int(prev_editor_rect) || as.integer(prev_editor_rect) != as.integer(rid)
    dyn_editor_last_rect <<- as.integer(rid)
    prev_touch <- suppressWarnings(as.integer(dyn_editor_last_touch))
    touch_changed <- !is_ok_int(prev_touch) || as.integer(prev_touch) != as.integer(editor_touch)
    dyn_editor_last_touch <<- as.integer(editor_touch)

    locked_estr_id <- suppressWarnings(as.integer(NA))
    if (!is.null(first_kf) && nrow(first_kf)) {
      locked_estr_id <- suppressWarnings(as.integer(first_kf$estrutura_id[[1]]))
    }
    structure_locked <- is_ok_int(locked_estr_id)

    cur_estr_saved <- if (isTRUE(structure_locked)) locked_estr_id else suppressWarnings(as.integer(row$estrutura_id[[1]]))
    if (!is_ok_int(cur_estr_saved)) cur_estr_saved <- suppressWarnings(as.integer(row$estrutura_id[[1]]))
    if (!is_ok_int(cur_estr_saved)) cur_estr_saved <- suppressWarnings(as.integer(struct_df$cd_id_estrutura[[1]]))

    cur_estr_ui <- suppressWarnings(as.integer(input$dynrect_structure))
    force_restore <- isTRUE(rect_changed) || isTRUE(touch_changed)
    if (isTRUE(structure_locked)) {
      cur_estr <- cur_estr_saved
    } else if (isTRUE(force_restore)) {
      cur_estr <- cur_estr_saved
    } else {
      if (!is_ok_int(cur_estr_ui)) cur_estr_ui <- cur_estr_saved
      cur_estr <- cur_estr_ui
    }

    if (isTRUE(structure_locked) || (isTRUE(force_restore) && is_ok_int(cur_estr_saved))) {
      session$onFlushed(function() {
        try(updateSelectInput(session, "dynrect_structure", selected = as.character(cur_estr_saved)), silent = TRUE)
      }, once = TRUE)
    }

    attrs_df <- NULL
    if (nrow(struct_df) > 0 && is_ok_int(cur_estr)) {
      sr <- struct_df |> dplyr::filter(.data$cd_id_estrutura == cur_estr)
      if (nrow(sr) > 0 && "atributos" %in% names(sr)) attrs_df <- sr$atributos[[1]]
    }

    cur_vals <- row$attrs[[1]]
    if (!is.null(last_kf) && nrow(last_kf) && "attrs" %in% names(last_kf)) {
      cur_vals <- last_kf$attrs[[1]]
    }
    if (is.null(cur_vals) || !is.list(cur_vals)) cur_vals <- list()

    div(
      panelTitle(
        title = paste0("Retângulo #", row$rect_id, " (Camera ", row$cam_id, ")"),
        background.color.title = "white",
        title.color  = "black",
        border.color = "lightgray",
        children = div(
          style = "padding:10px;",
          tagAppendAttributes(
            selectInput(
              ns("dynrect_structure"),
              label = "Estrutura",
              choices = choices,
              selected = cur_estr,
              width = "100%"
            ),
            disabled = if (isTRUE(structure_locked) || !isTRUE(tracking_active)) "disabled" else NULL
          ),
          if (isTRUE(structure_locked)) {
            tags$div(
              style = "margin-top:-8px; margin-bottom:8px; color:#777; font-size:12px;",
              "Estrutura bloqueada pelo 1º keyframe. Apenas atributos podem ser alterados."
            )
          },
          if (!isTRUE(tracking_active)) {
            tags$div(
              style = "margin-top:-8px; margin-bottom:8px; color:#777; font-size:12px;",
              "Rastreamento parado. Os keyframes já salvos permanecem no clip, mas este retângulo não volta mais para o mapa."
            )
          },
          br(),
          dynrect_attrs_ui(ns, attrs_df, values = cur_vals, locked = !isTRUE(tracking_active)),
          br(),
          div(
            style = "display:flex; gap:8px; flex-wrap:wrap;",
            if (isTRUE(tracking_active)) {
              actionButton(ns("dynrect_apply"), "Aplicar", class = "btn btn-primary btn-sm")
            } else {
              tags$button(
                type = "button",
                class = "btn btn-primary btn-sm",
                disabled = "disabled",
                "Aplicar"
              )
            },
            if (isTRUE(tracking_active)) {
              actionButton(ns("dynrect_stop"), "Parar de rastrear", class = "btn btn-outline-warning btn-sm")
            } else {
              actionButton(ns("dynrect_delete_record"), "Excluir registro e keyframes", class = "btn btn-danger btn-sm")
            }
          ),
          tags$hr(style = "margin:12px 0;"),
          tags$h5(style = "margin:0 0 8px 0;", paste0("Keyframes do Retângulo #", as.integer(rid))),
          DT::DTOutput(ns("dynrectKfTable")),
          if (isTRUE(tracking_active)) {
            div(
              style = "display:flex; gap:8px; margin-top:8px; flex-wrap:wrap;",
              actionButton(ns("dynrectKf_delete"), "Excluir selecionado(s)", class = "btn btn-outline-danger btn-sm"),
              actionButton(ns("dynrectKf_clear"), "Limpar tudo", class = "btn btn-outline-secondary btn-sm")
            )
          },
          tags$div(
            style = "margin-top:6px; color:#777; font-size:12px;",
            if (isTRUE(tracking_active)) {
              "Selecione um ou mais keyframes na tabela para excluir."
            } else {
              "Registro travado. A única ação disponível agora é excluir o registro inteiro com todos os keyframes."
            }
          )
        )
      )
    )
  })

  output$dynrectKfTable <- DT::renderDT({
    req(!is.null(objeto))
    if (!isTRUE(objeto$cd_id_objeto_tipo == 2L)) {
      return(DT::datatable(data.frame(), options = list(dom = "t")))
    }

    rid <- dyn$selected_rect_id
    if (is.null(rid) || !is.finite(rid)) {
      return(DT::datatable(data.frame(), options = list(dom = "t")))
    }

    tr <- dyn$tracks
    if (is.null(tr) || !nrow(tr)) {
      return(DT::datatable(
        data.frame(`Tempo (UTC)` = character(0), `Camera` = integer(0)),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }

    kf <- tr |>
      dplyr::filter(as.integer(rect_id) == as.integer(rid)) |>
      dplyr::arrange(as.POSIXct(ts_utc, tz = "UTC"))

    if (!nrow(kf)) {
      return(DT::datatable(
        data.frame(`Tempo (UTC)` = character(0), `Camera` = integer(0)),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }

    df <- kf |>
      dplyr::mutate(
        ts_raw = fmt_ts_utc_iso(as.POSIXct(ts_utc, tz = "UTC"), digits = 3L),
        `Tempo (UTC)` = fmt_ts_utc_br(as.POSIXct(ts_utc, tz = "UTC"), digits = 3L),
        `Camera` = as.integer(cam_id),
        `Atributos` = if ("attrs" %in% names(kf)) vapply(attrs, function(a) {
          jsonlite::toJSON(if (is.null(a)) list() else a, auto_unbox = TRUE, null = "null")
        }, character(1)) else ""
      ) |>
      dplyr::select(ts_raw, `Tempo (UTC)`, `Camera`, `Atributos`)

    DT::datatable(
      df,
      rownames = FALSE,
      selection = list(mode = "multiple"),
      options = list(
        dom = "tip",
        pageLength = 6,
        order = list(list(1, "asc")),
        columnDefs = list(
          list(targets = 0, visible = FALSE, searchable = FALSE)
        )
      )
    )
  })

  obs$add(observeEvent(input$dynrectKf_delete, {
    req(!is.null(objeto), isTRUE(objeto$cd_id_objeto_tipo == 2L))

    rid <- dyn$selected_rect_id
    if (is.null(rid) || !is.finite(rid)) return(invisible())
    row <- dyn_get_by_rect(rid)
    if (is.null(row) || !nrow(row)) return(invisible())
    if (!isTRUE(dyn_is_row_tracking_active(row))) {
      showNotification("Registro travado. Exclua o registro inteiro para remover seus keyframes.", type = "warning")
      return(invisible())
    }

    sel <- input$dynrectKfTable_rows_selected
    if (is.null(sel) || !length(sel)) {
      showNotification("Selecione um ou mais keyframes para excluir.", type = "warning")
      return(invisible())
    }

    tr <- dyn$tracks
    if (is.null(tr) || !nrow(tr)) return(invisible())

    kf <- tr |>
      dplyr::filter(as.integer(rect_id) == as.integer(rid)) |>
      dplyr::arrange(as.POSIXct(ts_utc, tz = "UTC")) |>
      dplyr::mutate(ts_raw = fmt_ts_utc_iso(as.POSIXct(ts_utc, tz = "UTC"), digits = 3L))

    if (!nrow(kf)) return(invisible())

    ts_del <- kf$ts_raw[sel]
    ts_del <- ts_del[!is.na(ts_del) & nzchar(ts_del)]
    if (!length(ts_del)) return(invisible())

    tr2 <- tr |>
      dplyr::mutate(ts_raw = fmt_ts_utc_iso(as.POSIXct(ts_utc, tz = "UTC"), digits = 3L)) |>
      dplyr::filter(!(as.integer(rect_id) == as.integer(rid) & ts_raw %in% ts_del)) |>
      dplyr::select(-ts_raw)

    dyn$tracks <- tr2
    dyn_cache_invalidate_tracks()

    showNotification(
      paste0("Excluído(s) ", length(ts_del), " keyframe(s) do Retângulo #", as.integer(rid), "."),
      type = "message"
    )
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$dynrectKf_clear, {
    req(!is.null(objeto), isTRUE(objeto$cd_id_objeto_tipo == 2L))

    rid <- dyn$selected_rect_id
    if (is.null(rid) || !is.finite(rid)) return(invisible())
    row <- dyn_get_by_rect(rid)
    if (is.null(row) || !nrow(row)) return(invisible())
    if (!isTRUE(dyn_is_row_tracking_active(row))) {
      showNotification("Registro travado. Exclua o registro inteiro para remover seus keyframes.", type = "warning")
      return(invisible())
    }

    tr <- dyn$tracks
    if (is.null(tr) || !nrow(tr)) return(invisible())

    dyn$tracks <- tr |>
      dplyr::filter(as.integer(rect_id) != as.integer(rid))
    dyn_cache_invalidate_tracks()

    showNotification(
      paste0("Todos os keyframes do Retângulo #", as.integer(rid), " foram removidos."),
      type = "message"
    )
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$dynrect_apply, {
    req(!is.null(objeto), isTRUE(objeto$cd_id_objeto_tipo == 2L))

    row <- dyn_get_by_rect(dyn$selected_rect_id)
    if (is.null(row) || !nrow(row)) return(invisible())
    if (!isTRUE(dyn_is_row_tracking_active(row))) {
      showNotification("O rastreamento deste retângulo foi parado. Os keyframes já salvos foram mantidos.", type = "warning")
      return(invisible())
    }

    struct_df <- dynrect_structures_df(objeto)

    ok_meta <- dyn_apply_meta(struct_df)
    if (!isTRUE(ok_meta)) {
      return(invisible())
    }

    ok_trk <- dyn_apply_tracking(rv)

    if (isTRUE(ok_trk)) {
      showNotification("Tracking aplicado e salvo (posição + atributos).", type = "message")
    } else {
      showNotification("Tracking não foi salvo — verifique Clip ativo / retângulo selecionado.", type = "warning")
    }
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$dynrect_stop, {
    req(!is.null(objeto), isTRUE(objeto$cd_id_objeto_tipo == 2L))

    ok <- dyn_stop_tracking_selected()
    if (!isTRUE(ok)) {
      showNotification("Selecione um retângulo ativo para parar o rastreamento.", type = "warning")
      return(invisible())
    }

    showNotification("Rastreamento parado. O retângulo foi removido do mapa e os keyframes já salvos foram mantidos no clip.", type = "message")
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$dynrect_delete_record, {
    req(!is.null(objeto), isTRUE(objeto$cd_id_objeto_tipo == 2L))

    ok <- dyn_delete_selected_record()
    if (!isTRUE(ok)) {
      showNotification("Selecione um registro válido para excluir.", type = "warning")
      return(invisible())
    }

    showNotification("Registro e keyframes excluídos.", type = "message")
  }, ignoreInit = TRUE))

  output$clipsTable <- DT::renderDT({

    df <- clips()
    if (!"estrutura_ok" %in% names(df)) {
      df$estrutura_ok <- FALSE
    }
    show_clip_view <- !is.null(objeto) && !isTRUE(objeto$cd_id_objeto_tipo == 2L)
    if (!nrow(df)) {
      empty_df <- data.frame(
        Linha  = integer(0),
        Titulo = character(0),
        Tipo   = character(0),
        De = character(0),
        Ate = character(0),
        Excluir = character(0),
        stringsAsFactors = FALSE
      )
      if (isTRUE(show_clip_view)) {
        empty_df$Visualizar <- character(0)
        empty_df$Status <- character(0)
        empty_df <- empty_df[, c("Linha", "Titulo", "Tipo", "De", "Ate", "Visualizar", "Status", "Excluir")]
      }
      return(DT::datatable(
        empty_df,
        escape = FALSE, selection = "none",
        options = list(
          dom = "t", paging = FALSE, ordering = FALSE,
          language = list(
            emptyTable   = "Nenhum clip disponível na tabela",
            zeroRecords  = "Nenhum registro encontrado",
            infoEmpty    = "",
            info         = ""
          )
        )
      ))
    }

    rownum      <- seq_len(nrow(df))
    tiposPacote <- tiposPacotes$name_tipo_pacote

    titulo <- vapply(df$id, function(id) {
      as.character(
        textInput(
          inputId = ns(paste0("clip_title_", id)),
          label = NULL, value = df$title[df$id == id],
          width = "100%", placeholder = "Nome do clip"
        ) |>
          tagAppendAttributes(style = ";margin-top: 10px;")
      )
    }, character(1))

    tipo_cb <- vapply(df$id, function(id) {
      idx <- which(df$id == id)
      cur <- if ("tipo" %in% names(df)) as.character(df$tipo[idx]) else NA_character_
      if (is.na(cur) || !nzchar(cur) || !(cur %in% tiposPacote)) cur <- tiposPacote[[1]]

      as.character(
        selectInput(
          inputId = ns(paste0("clip_tipo_", id)),
          label   = NULL,
          choices = tiposPacote,
          selected = cur,
          width   = "100%"
        ) |>
          tagAppendAttributes(style = ";margin-top: 10px;")
      )
    }, character(1))

    t0_txt <- vapply(df$id, function(id) {
      idx <- which(df$id == id)
      val <- fmt_pt(df$t0[idx], Sys.timezone())
      as.character(
        textInput(
          inputId = ns(paste0("clip_t0_", id)),
          label = NULL, value = val, width = "100%",
          placeholder = "dd/mm/aa HH:MM:SS"
        ) |>
          tagAppendAttributes(style = ";margin-top: 10px;")
      )
    }, character(1))

    t1_txt <- vapply(df$id, function(id) {
      idx <- which(df$id == id)
      val <- fmt_pt(df$t1[idx], Sys.timezone())
      as.character(
        textInput(
          inputId = ns(paste0("clip_t1_", id)),
          label = NULL, value = val, width = "100%",
          placeholder = "dd/mm/aa HH:MM:SS"
        ) |>
          tagAppendAttributes(style = ";margin-top: 10px;")
      )
    }, character(1))

    btn_view <- NULL
    clip_status <- NULL
    if (isTRUE(show_clip_view)) {
      btn_view <- vapply(df$id, function(id) {
        sprintf(
          "<button class='btn btn-sm btn-outline-primary' onclick=\"Shiny.setInputValue('%s', {action:'view', id:%d, nonce:Math.random()}, {priority:'event'})\">
           <i class='fa fa-eye'></i>
         </button>", ns("clip_action"), id
        )
      }, character(1))

      clip_status <- vapply(seq_len(nrow(df)), function(i) {
        ok <- isTRUE(df$estrutura_ok[i])
        if (isTRUE(ok)) {
          "<span title='Valores da estrutura definidos' style='color:#1f8b4c; font-size:18px;'><i class='fa-solid fa-circle-check'></i></span>"
        } else {
          "<span title='Valores da estrutura pendentes' style='color:#d97706; font-size:18px;'><i class='fa-solid fa-triangle-exclamation'></i></span>"
        }
      }, character(1))
    }

    btn_del <- vapply(df$id, function(id) {
      sprintf(
        "<button class='btn btn-sm btn-danger' onclick=\"Shiny.setInputValue('%s', {action:'del', id:%d, nonce:Math.random()}, {priority:'event'})\">
         <i class='fa fa-trash'></i>
       </button>", ns("clip_action"), id
      )
    }, character(1))

    out <- data.frame(
      Linha          = rownum,
      Titulo         = titulo,
      Tipo           = tipo_cb,
      De             = t0_txt,
      Ate            = t1_txt,
      Excluir        = btn_del,
      stringsAsFactors = FALSE
    )
    if (isTRUE(show_clip_view)) {
      out$Visualizar <- btn_view
      out$Status <- clip_status
      out <- out[, c("Linha", "Titulo", "Tipo", "De", "Ate", "Visualizar", "Status", "Excluir")]
    }

    DT::datatable(
      out, escape = FALSE, selection = "none",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        preDrawCallback = DT::JS(
          "function(settings){ Shiny.unbindAll(this.api().table().node()); }"
        ),
        drawCallback = DT::JS(
          "function(settings){ Shiny.bindAll(this.api().table().node()); }"
        ),
        columnDefs = list(
          list(visible = FALSE, targets = 0),
          list(className = "dt-center", targets = "_all"),
          list(width = "75px", targets = if (isTRUE(show_clip_view)) c(1, 5, 6, 7) else c(1, 6))
        )
      ),
      callback = DT::JS(
        "table.on('click keydown', 'input, textarea, select', function(e){ e.stopPropagation(); });"
      )
    ) |>
      DT::formatStyle(names(out), cursor = "pointer")
  })

  obs$add(observeEvent(input$clip_action, {
    req(input$clip_action$id, input$clip_action$action)
    df <- isolate(clips())
    if (!nrow(df)) return(invisible())
    id_sel <- as.integer(input$clip_action$id)
    act    <- as.character(input$clip_action$action)
    row    <- df[df$id == id_sel, , drop = FALSE]
    if (!nrow(row)) return(invisible())

    if (act == "view") {
      n_frames <- abs(row$i1 - row$i0) + 1L

      if (!is.null(rv$seq) && nrow(rv$seq) > 0 && length(rv$timeline_rows)) {
        rows_sel <- timeline_window_rows(
          timeline_rows = rv$timeline_rows,
          total_rows = nrow(rv$seq),
          start_pos = row$i0[1],
          end_pos = row$i1[1]
        )
        sub_seq <- rv$seq[rows_sel, , drop = FALSE]
      } else {
        sub_seq <- NULL
      }

      clip_summary_overlay(
        ns, session, input, objeto, id_clip = id_sel,
        ts_start = row$t0, ts_end = row$t1,
        n_frames = n_frames
      )
      current_clip_view_id(id_sel)

      clip_timeline_rows <- seq_timeline_rows(sub_seq)
      clipOverlayPlayer$seq     <- sub_seq
      clipOverlayPlayer$timeline_rows <- clip_timeline_rows
      clipOverlayPlayer$timeline_pos  <- if (length(clip_timeline_rows)) 1L else NA_integer_
      clipOverlayPlayer$i       <- if (length(clip_timeline_rows)) timeline_row_at(clip_timeline_rows, 1L) else 1L
      clipOverlayPlayer$dir     <- +1L
      clipOverlayPlayer$playing <- FALSE
      render_clip_overlay_first_frame(attempt = 1L, max_attempts = 3L)
      session$onFlushed(function() {
        render_clip_overlay_first_frame(attempt = 1L, max_attempts = 8L)
      }, once = TRUE)

    } else if (act == "del") {
      clips_remove(id_sel)
      if (isTRUE(identical(as.integer(isolate(current_clip_view_id())), as.integer(id_sel)))) {
        current_clip_view_id(NA_integer_)
      }
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
      clipOverlayPlayer$playing <- FALSE
      clipOverlayPlayer$seq     <- NULL
      clipOverlayPlayer$timeline_rows <- integer(0)
      clipOverlayPlayer$timeline_pos  <- NA_integer_
    }
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$clipOverlay_action, {
    req(input$clipOverlay_action$action)
    act <- as.character(input$clipOverlay_action$action)

    if (is.null(clipOverlayPlayer$seq) || !nrow(clipOverlayPlayer$seq)) return(invisible())

    if (act == "reverse") {
      clipOverlayPlayer$dir     <- -1L
      clipOverlayPlayer$playing <- TRUE
      overlay_play_step()

    } else if (act == "play") {
      clipOverlayPlayer$dir     <- +1L
      clipOverlayPlayer$playing <- TRUE
      overlay_play_step()

    } else if (act == "pause") {
      clipOverlayPlayer$playing <- FALSE

    } else if (act == "prev") {
      clipOverlayPlayer$playing <- FALSE
      timeline_rows <- isolate(clipOverlayPlayer$timeline_rows)
      n_steps <- length(timeline_rows)
      if (!n_steps) return(invisible())
      cur_pos <- isolate({
        cur <- suppressWarnings(as.integer(clipOverlayPlayer$timeline_pos))
        if (is.finite(cur) && cur >= 1L) cur else timeline_pos_from_row(timeline_rows, clipOverlayPlayer$i)
      })
      clipOverlayPlayer$timeline_pos <- max(1L, cur_pos - 1L)
      clipOverlayPlayer$i <- timeline_row_at(timeline_rows, clipOverlayPlayer$timeline_pos)
      render_clip_overlay_frame()

    } else if (act == "next") {
      clipOverlayPlayer$playing <- FALSE
      timeline_rows <- isolate(clipOverlayPlayer$timeline_rows)
      n_steps <- length(timeline_rows)
      if (!n_steps) return(invisible())
      cur_pos <- isolate({
        cur <- suppressWarnings(as.integer(clipOverlayPlayer$timeline_pos))
        if (is.finite(cur) && cur >= 1L) cur else timeline_pos_from_row(timeline_rows, clipOverlayPlayer$i)
      })
      clipOverlayPlayer$timeline_pos <- min(n_steps, cur_pos + 1L)
      clipOverlayPlayer$i <- timeline_row_at(timeline_rows, clipOverlayPlayer$timeline_pos)
      render_clip_overlay_frame()
    }
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btSair, {
    obs$destroy()
    output$uiCamerasFrames <- NULL
    playing(FALSE); loop_on <<- FALSE
    session$userData$lru_cache$clear()
    dyn_reset()
    rv$timeline_rows <- integer(0)
    rv$timeline_pos <- NA_integer_

    if (isTRUE(rv$clip_active)) {
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }

    clipOverlayPlayer$playing <- FALSE
    clipOverlayPlayer$seq     <- NULL
    clipOverlayPlayer$timeline_rows <- integer(0)
    clipOverlayPlayer$timeline_pos  <- NA_integer_
    current_clip_view_id(NA_integer_)

    removeModal(session); callback()
  }, ignoreInit = TRUE, ignoreNULL = TRUE))

  obs$add(observeEvent(input$btSalvar, {

    df <- isolate(clips())
    if (!nrow(df)) {
      showNotification("Não foi possivel salvar o pacote de treino, nenhum clip foi encontrado!", type = "error")
      return(invisible())
    }

    input_snapshot <- isolate(shiny::reactiveValuesToList(input))
    dyn_rects_snapshot <- isolate(list(rects = dyn$rects, tracks = dyn$tracks))

    if (isTRUE(!is.null(objeto) && objeto$cd_id_objeto_tipo == 1L)) {
      df_status <- updateClipsTable(df)
      df_status$estrutura_ok <- vapply(seq_len(nrow(df_status)), function(i) {
        clip_static_values_complete(
          input_source = input_snapshot,
          objeto = objeto,
          id_clip = df_status$id[i],
          t0 = df_status$t0[i],
          t1 = df_status$t1[i]
        )
      }, logical(1))
      clips(df_status)
      df <- df_status
    }

    actionWebUser({
      info <- tryCatch(
        build_objeto_descricao(input_snapshot, df, objeto, tiposPacotes, dyn_rects = dyn_rects_snapshot),
        error = function(e) {
          showNotification(
            paste0("Falha ao preparar o pacote antes de salvar: ", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )

      if (is.null(info)) {
        return(invisible(FALSE))
      }

      if (!info$status) {
        showNotification(info$message, type = "error")
        return(invisible(FALSE))
      }

      if (!length(info$datas)) {
        showNotification("Não foi possivel montar um pacote válido para salvar.", type = "error")
        return(invisible(FALSE))
      }
  
      save_result <- db$tryTransaction(function(conn) {
 
        for (i in seq_along(info$datas)) {
          descricao <- info$datas[[i]]
          tipo_pacote <- suppressWarnings(as.integer(descricao$tipoPacote))
          dt_begin <- as.POSIXct(descricao$begin, tz = "UTC")
          dt_end   <- as.POSIXct(descricao$end, tz = "UTC")

          if (!is.finite(tipo_pacote)) {
            stop(sprintf("Tipo de pacote invalido no clip %d.", i))
          }
          if (is.na(dt_begin) || is.na(dt_end)) {
            stop(sprintf("Intervalo invalido no clip %d.", i))
          }

          objPacote <- list()
          objPacote$cd_id_objeto       <- objeto$cd_id_objeto
          objPacote$titulo_ia          <- as.character(descricao$titulo)
          objPacote$input_ia           <- as.character(descricao$input)
          objPacote$cd_id_tipo_pacote  <- tipo_pacote
          objPacote$output_ia          <- as.character(descricao$output)
          objPacote$dt_hr_local_begin  <- dt_begin
          objPacote$dt_hr_local_end    <- dt_end

          db$insertTable(conn, "pacote_ia", objPacote)
        }
      })

      if (!isTRUE(save_result)) {
        save_err <- attr(save_result, "error_message", exact = TRUE)
        msg <- "Não foi possivel salvar o pacote de treino, durante o processo houve falha!"
        if (!is.null(save_err) && nzchar(save_err)) {
          msg <- paste0(msg, " ", save_err)
        }
        showNotification(msg, type = "error")
        return(invisible(FALSE))
      }

      dialogConfirm(
        session = session,
        id    = ns("dialogConfirm"),
        title = "Pacote criado com sucesso!",
        text  = "Deseja criar novamente um novo Pacote?"
      )

      observeEvent(input$dialogConfirm, {

        status <- input$dialogConfirm

        playing(FALSE); loop_on <<- FALSE
        session$userData$lru_cache$clear()
        dyn_reset()

        if (isTRUE(rv$clip_active)) {
          rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
          try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
        }

        clips(data.frame(
          id = integer(0),
          title = character(0),
          t0 = as.POSIXct(character(0), tz = "UTC"),
          t1 = as.POSIXct(character(0), tz = "UTC"),
          i0 = integer(0),
          i1 = integer(0),
          estrutura_ok = logical(0),
          stringsAsFactors = FALSE
        ))
        next_clip_id(1L)
        current_clip_view_id(NA_integer_)

        clipOverlayPlayer$playing <- FALSE
        clipOverlayPlayer$seq     <- NULL

        if (!status) {
          obs$destroy()
          output$uiCamerasFrames <- NULL
          removeModal(session); callback()
        }
      }, ignoreInit = TRUE, once = TRUE)

    }, delay = 0, lock_id = "treinar_pacote_save")
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$clipCloseVideo, {
    id_clip <- suppressWarnings(as.integer(isolate(current_clip_view_id())))
    if (is.finite(id_clip)) {
      update_clip_structure_status(id_clip, input_source = input)
    }
    current_clip_view_id(NA_integer_)
    clipOverlayPlayer$playing <- FALSE
    clipOverlayPlayer$seq     <- NULL
    try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
  }, ignoreInit = TRUE))
}

# ==================================================
# Coleta dos valores dos atributos no overlay de clip
# ==================================================
collect_clip_attributes <- function(input, objeto, id_clip, t0, t1) {
  stopifnot(!is.null(objeto), nrow(objeto) >= 1)
  componentes <- objeto$config[[1]]$componentes[[1]]
  if (is.null(componentes) || !nrow(componentes)) {
    return(data.frame(
      cd_id_componente = integer(0),
      name_componente  = character(0),
      name_atributo    = character(0),
      name_data        = character(0),
      VALUE            = character(0),
      stringsAsFactors = FALSE
    ))
  }

  normalize_id_piece <- function(x) {
    x <- as.character(x)
    x <- gsub("\\s+", "_", x)
    x <- gsub("[^A-Za-z0-9_\\-]", "_", x)
    x
  }

  codigo_date <- paste0(id_clip, "_")

  make_ids <- function(comp_name, attr_name, k) {
    raw_id  <- paste0(codigo_date, comp_name, "_", attr_name, "_", k)
    norm_id <- paste0(codigo_date, normalize_id_piece(comp_name), "_", normalize_id_piece(attr_name), "_", k)
    list(raw = raw_id, norm = norm_id)
  }

  read_input <- function(id_candidates) {
    cand <- unlist(id_candidates, use.names = FALSE)
    for (cid in cand) {
      if (cid %in% names(input)) return(input[[cid]])
    }
    NA
  }

  rows <- list()

  for (i in seq_len(nrow(componentes))) {
    comp      <- componentes[i, ]
    estrutura <- comp$estrutura[[1]]
    atributos <- estrutura$configs[[1]]$atributos[[1]]
    if (is.null(atributos) || !nrow(atributos)) next

    comp_id   <- as.integer(comp$cd_id_componente[[1]])
    comp_name <- as.character(comp$name_componente[[1]])

    for (k in seq_len(nrow(atributos))) {
      atributo  <- atributos[k, ]
      attr_id   <- atributo$cd_id_atributo[[1]]
      attr_name <- as.character(atributo$name_atributo[[1]])
      attr_type <- as.character(atributo$name_data[[1]])

      ids   <- make_ids(comp_id, attr_id, k)
      value <- read_input(ids)

      if (isTRUE(is.numeric(value))) {
        value <- as.character(value)
      } else if (isTRUE(is.logical(value))) {
        value <- ifelse(is.na(value), NA_character_, ifelse(value, "TRUE", "FALSE"))
      } else {
        value <- if (is.null(value)) NA_character_ else as.character(value)
      }

      rows[[length(rows) + 1L]] <- data.frame(
        cd_id_componente = comp_id,
        cd_id_atributo   = attr_id,
        name_componente  = comp_name,
        name_atributo    = attr_name,
        name_data        = attr_type,
        VALUE            = value,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) {
    return(data.frame(
      cd_id_componente = integer(0),
      name_componente  = character(0),
      name_atributo    = character(0),
      name_data        = character(0),
      VALUE            = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

# ==================================================
# NOVO: Serialização dos retângulos dinâmicos para o output_ia
# ==================================================
dynrects_to_json <- function(dyn_rects, dyn_tracks, t0, t1) {
  if (is.null(dyn_rects) || !nrow(dyn_rects)) return(NULL)
  if (is.null(dyn_tracks) || !nrow(dyn_tracks)) return(NULL)

  t0 <- as.POSIXct(t0, tz = "UTC"); t1 <- as.POSIXct(t1, tz = "UTC")
  if (is.na(t0) || is.na(t1)) return(NULL)

  sanitize_attrs <- function(a) {
    if (is.null(a)) return(list())
    if (!is.list(a)) a <- as.list(a)
    if (!length(a)) return(list())
    out <- a
    for (nm in names(out)) {
      v <- out[[nm]]
      if (is.null(v) || !length(v) || any(is.na(v))) out[[nm]] <- ""
      if (isTRUE(is.numeric(out[[nm]]))) out[[nm]] <- as.character(out[[nm]])
    }
    out
  }

  sanitize_box <- function(b) {
    if (is.null(b)) return(NULL)
    x_min <- suppressWarnings(as.numeric(b$x_min))
    y_min <- suppressWarnings(as.numeric(b$y_min))
    x_max <- suppressWarnings(as.numeric(b$x_max))
    y_max <- suppressWarnings(as.numeric(b$y_max))
    if (!all(is.finite(c(x_min, y_min, x_max, y_max)))) return(NULL)
    list(x_min = x_min, y_min = y_min, x_max = x_max, y_max = y_max)
  }

  tracks_all <- dyn_tracks |>
    dplyr::mutate(
      rect_id = as.integer(.data$rect_id),
      ts_utc  = as.POSIXct(.data$ts_utc, tz = "UTC")
    )

  in_rng <- tracks_all |>
    dplyr::filter(.data$ts_utc >= t0, .data$ts_utc <= t1)

  if (!nrow(in_rng)) return(NULL)

  rect_ids_in <- unique(in_rng$rect_id)

  last_before <- tracks_all |>
    dplyr::filter(.data$ts_utc < t0, .data$rect_id %in% rect_ids_in) |>
    dplyr::group_by(.data$rect_id) |>
    dplyr::slice_max(.data$ts_utc, with_ties = FALSE) |>
    dplyr::ungroup()

  if (nrow(last_before)) in_rng <- dplyr::bind_rows(last_before, in_rng)

  in_rng <- in_rng |>
    dplyr::arrange(.data$rect_id, .data$ts_utc) |>
    dplyr::distinct(.data$rect_id, .data$ts_utc, .keep_all = TRUE)

  rect_ids <- unique(in_rng$rect_id)

  items <- lapply(rect_ids, function(rid) {
    meta <- dyn_rects |>
      dplyr::filter(as.integer(.data$rect_id) == as.integer(rid)) |>
      dplyr::slice(1)

    if (!nrow(meta)) return(NULL)

    kf <- in_rng |>
      dplyr::filter(.data$rect_id == rid) |>
      dplyr::arrange(.data$ts_utc)

    if (!nrow(kf)) return(NULL)

    meta_attrs <- sanitize_attrs(meta$attrs[[1]])

    keyframes <- lapply(seq_len(nrow(kf)), function(i) {
      r <- kf[i, ]

      b <- NULL
      if ("box" %in% names(r)) b <- r$box[[1]]
      b <- sanitize_box(b)

      if (is.null(b)) {
        poly <- NULL
        if ("poly" %in% names(r)) poly <- r$poly[[1]]
        b <- sanitize_box(poly_to_box(poly))
      }

      list(
        TS_UTC = fmt_ts_utc_iso(r$ts_utc[[1]], digits = 3L),
        BOX    = b,
        atributos = sanitize_attrs(
          if ("attrs" %in% names(r)) r$attrs[[1]] else meta_attrs
        )
      )
    })

    list(
      RECT_ID   = as.integer(meta$rect_id[[1]]),
      CAMERA_ID = as.integer(meta$cam_id[[1]]),
      cd_id_estrutura = ifelse(is.finite(meta$estrutura_id[[1]]), as.integer(meta$estrutura_id[[1]]), NA_integer_),
      name_estrutura  = as.character(meta$estrutura_nome[[1]]),
      atributos = meta_attrs,
      KEYFRAMES = keyframes
    )
  })

  items <- Filter(Negate(is.null), items)
  if (!length(items)) return(NULL)

  jsonlite::toJSON(items, auto_unbox = TRUE, null = "null")
}

build_objeto_descricao <- function(input, df, objeto, tiposPacotes, dyn_rects = NULL) {

  listas  <- list()
  status  <- TRUE
  message <- ""

  for (i in 1:nrow(df)) {
    df_tmp <- df[i, ]
    id     <- df_tmp$id
    df_tmp$title <- input[[paste0("clip_title_", id)]]
    t0           <- input[[paste0("clip_t0_", id)]]
    t1           <- input[[paste0("clip_t1_", id)]]
    tipoPacote   <- tiposPacotes |> dplyr::filter(name_tipo_pacote == input[[paste0("clip_tipo_", id)]])

    if (!validaDateFormat(t0) || !validaDateFormat(t1)) {
      status  <- FALSE
      message <- "✕ formato inválido na tabela clips (use dd/mm/aa HH:MM:SS)!"
      break
    }

    df_tmp <- clips_update_date_time_t0(df_tmp, id, t0)
    df_tmp <- clips_update_date_time_t1(df_tmp, id, t1)
    
    atributos    <- NULL
    output_parts <- NULL
    if(objeto$cd_id_objeto_tipo == 1L){
      atributos <- collect_clip_attributes(input, objeto, id, df_tmp$t0, df_tmp$t1)
      if (!isTRUE(clip_static_values_complete(input, objeto, id, df_tmp$t0, df_tmp$t1))) {
        clip_label <- if (!is.null(df_tmp$title) && nzchar(as.character(df_tmp$title))) {
          as.character(df_tmp$title)
        } else {
          paste0("Clip ", id)
        }
        status  <- FALSE
        message <- paste0("Defina todos os valores da estrutura para o clip '", clip_label, "' antes de salvar o pacote.")
        break
      }
      atributos <- atributos |>
      group_by(name_componente) |>
      nest() |>
      ungroup() |>
      mutate(output = purrr::map2_chr(name_componente, data, function(nome, dados) {
        x <- dados |> select(cd_id_componente, name_atributo, VALUE, name_data)
        idc <- x$cd_id_componente
        texto <- ""
        for (k in 1:nrow(x)) {
          if (k > 1) texto <- paste0(texto, ",")
          att <- x[k, ]
          if (att$name_data == "QUALITATIVE") {
            texto <- paste0(texto, paste0("\"", att$name_atributo, "\": \"", att$VALUE, "\""))
          } else {
            texto <- paste0(texto, paste0("\"", att$name_atributo, "\": ", att$VALUE))
          }
        }
        paste0("{\"", nome, "\": {", texto, "}, \"ID\": ", idc, "}")
      }))
      output_parts <- atributos$output
      output_parts <- output_parts[!is.na(output_parts) & nzchar(output_parts)]
    }
   
    if (isTRUE(objeto$cd_id_objeto_tipo == 2L) && is.list(dyn_rects) &&
        !is.null(dyn_rects$rects) && !is.null(dyn_rects$tracks)) {
      dyn_json <- dynrects_to_json(dyn_rects$rects, dyn_rects$tracks, df_tmp$t0, df_tmp$t1)
      if (!is.null(dyn_json) && nzchar(dyn_json)) {
        output_parts <- c(output_parts, paste0("{\"DYN_RECTS\": ", dyn_json, "}"))
      }
    }

    output_ <- paste0("[", paste0(output_parts, collapse = ","), "]")

    objeto_nome <- ""
    if ("name_objeto" %in% names(objeto) && length(objeto$name_objeto) >= 1L) {
      objeto_nome <- as.character(objeto$name_objeto[[1]])
    }

    objeto_tipo_nome <- ""
    if ("name_objeto_tipo" %in% names(objeto) && length(objeto$name_objeto_tipo) >= 1L) {
      objeto_tipo_nome <- as.character(objeto$name_objeto_tipo[[1]])
    }

    input_  <- paste0("OBJETO: ", objeto_nome, " TIPO: ", objeto_tipo_nome)

    listas[[i]] <- list(
      titulo = df_tmp$title,
      input  = input_,
      output = output_,
      tipoPacote = tipoPacote$cd_id_tipo_pacote,
      begin  = df_tmp$t0,
      end    = df_tmp$t1
    )
  }

  list(datas = listas, status = status, message = message)
}

#' @export
uiBuildTreinar <- function(ns, input, output, session, callback) {

  .register_auto_dispose(session)
  e <- .get_private(session)

  obs <- newObserve()

  empty_packages <- function() {
    data.frame(
      cd_id_ia = integer(0),
      cd_id_objeto = integer(0),
      titulo_ia = character(0),
      name_tipo_pacote = character(0),
      name_setor = character(0),
      name_objeto = character(0),
      dt_hr_local_begin = as.POSIXct(character(0), tz = "UTC"),
      dt_hr_local_end = as.POSIXct(character(0), tz = "UTC"),
      arquivo_rds = character(0),
      stringsAsFactors = FALSE
    )
  }

  build_packages <- reactiveVal(empty_packages())
  checked_package_ids <- reactiveVal(integer(0))
  check_flag_active   <- reactiveVal(FALSE)
  build_logs <- reactiveVal("Marque os pacotes desejados e clique em Compactar para gerar o arquivo .rds.")
  build_notice <- reactiveVal(NULL)
  build_running <- reactiveVal(FALSE)
  show_output_widget <- reactiveVal(FALSE)
  output_dir_value <- reactiveVal("train")
  file_name_value <- reactiveVal("")
  file_name_touched <- reactiveVal(FALSE)
  build_limit_value <- reactiveVal(150L)
  build_dir_roots <- local({
    project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

    if (.Platform$OS.type == "windows") {
      drive_paths <- paste0(LETTERS, ":/")
      drive_paths <- drive_paths[dir.exists(drive_paths)]
      drive_roots <- normalizePath(drive_paths, winslash = "/", mustWork = TRUE)
      names(drive_roots) <- sub("/$", "", drive_paths)
      return(c(project = project_root, drive_roots))
    }

    c(
      project = project_root,
      home = normalizePath("~", winslash = "/", mustWork = FALSE)
    )
  })

  build_ctx <- reactiveValues(
    rows = NULL,
    index = 0L,
    built = list(),
    skipped = 0L,
    output_dir = NULL,
    file_name = NULL,
    limit = 150L,
    summary_row = NULL
  )

  append_log <- function(text) {
    text <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", text)
    current_logs <- isolate(build_logs())
    build_logs(paste(current_logs, text, sep = "\n"))
  }

  sync_build_checkbox_ui <- function(ids = integer(0)) {
    session$sendCustomMessage(
      "tvs-build-sync-checks",
      list(
        tableId = ns("buildPacotesTable"),
        ids = as.integer(ids)
      )
    )
    invisible(TRUE)
  }

  enqueue_build_notice <- function(message, type = "message", duration = NULL) {
    build_notice(list(
      message = as.character(message)[1],
      type = as.character(type)[1],
      duration = duration,
      nonce = as.numeric(Sys.time())
    ))
    invisible(TRUE)
  }

  resolve_output_dir <- function(path) {
    path <- trimws(as.character(path)[1])
    if (!nzchar(path)) path <- "train"

    is_abs <- grepl("^[A-Za-z]:[\\\\/]|^\\\\\\\\|^/", path)
    full_path <- if (is_abs) path else file.path(getwd(), path)

    normalizePath(full_path, winslash = "/", mustWork = FALSE)
  }

  normalize_file_name <- function(name) {
    name <- trimws(as.character(name)[1])
    if (!nzchar(name)) stop("Informe um nome para o arquivo .rds.")

    name <- gsub("[<>:\"/\\\\|?*]", "_", name)
    if (!grepl("\\.rds$", name, ignore.case = TRUE)) {
      name <- paste0(name, ".rds")
    }

    name
  }

  normalize_build_packages <- function(df) {
    if (is.null(df) || !nrow(df)) return(empty_packages())

    if ("dt_hr_local_begin" %in% names(df)) {
      df$dt_hr_local_begin <- as.POSIXct(df$dt_hr_local_begin, tz = "UTC")
    }
    if ("dt_hr_local_end" %in% names(df)) {
      df$dt_hr_local_end <- as.POSIXct(df$dt_hr_local_end, tz = "UTC")
    }

    if (!"name_tipo_pacote" %in% names(df)) df$name_tipo_pacote <- "-"
    df$name_tipo_pacote[is.na(df$name_tipo_pacote) | !nzchar(trimws(df$name_tipo_pacote))] <- "-"
    df$arquivo_rds <- mapply(
      buildTreinoFileName,
      df$name_setor,
      df$name_objeto,
      USE.NAMES = FALSE
    )

    df
  }

  prepare_build_table_df <- function(df, checked_ids) {
    table_df <- df

    table_df$selecionar <- vapply(table_df$cd_id_ia, function(id) {
      checked_attr <- if (as.integer(id) %in% as.integer(checked_ids)) "checked" else ""
      sprintf(
        "<input type='checkbox' class='build-check-pacote' data-pacote-id='%d' style='cursor:pointer;' %s onclick=\"Shiny.setInputValue('%s',{id:%d,checked:this.checked,nonce:Math.random()},{priority:'event'})\">",
        as.integer(id),
        checked_attr,
        ns("buildCheckPacote"),
        as.integer(id)
      )
    }, character(1))

    table_df$excluir <- vapply(table_df$cd_id_ia, function(id) {
      sprintf(
        "<button class='btn btn-danger btn-xs' onclick=\"Shiny.setInputValue('%s',{id:%d,nonce:Math.random()},{priority:'event'})\"><i class='fa fa-trash'></i></button>",
        ns("buildDeletePacote"),
        as.integer(id)
      )
    }, character(1))

    table_df$dt_hr_local_begin <- format(as.POSIXct(table_df$dt_hr_local_begin, tz = Sys.timezone()), "%d/%m/%Y %H:%M:%S")
    table_df$dt_hr_local_end <- format(as.POSIXct(table_df$dt_hr_local_end, tz = Sys.timezone()), "%d/%m/%Y %H:%M:%S")

    table_df <- table_df[, c(
      "selecionar",
      "excluir",
      "cd_id_ia",
      "cd_id_objeto",
      "titulo_ia",
      "name_tipo_pacote",
      "name_setor",
      "name_objeto",
      "dt_hr_local_begin",
      "dt_hr_local_end",
      "arquivo_rds"
    )]

    names(table_df) <- c(
      "Build",
      "Excluir",
      "ID Pacote",
      "ID Objeto",
      "Titulo",
      "Tipo",
      "Setor",
      "Objeto",
      "Inicio",
      "Fim",
      "Arquivo Padrao"
    )

    table_df
  }

  suggest_file_name_for_packages <- function(df) {
    if (is.null(df) || !nrow(df)) return("dataset.rds")

    objetos <- unique(as.integer(df$cd_id_objeto))
    objetos <- objetos[is.finite(objetos)]

    if (length(objetos) == 1L) {
      return(buildTreinoFileName(df$name_setor[[1]], df$name_objeto[[1]]))
    }

    "dataset_multi_objetos.rds"
  }

  apply_suggested_file_name <- function(df, force = FALSE) {
    if (isTRUE(force) || !isTRUE(isolate(file_name_touched()))) {
      file_name_touched(FALSE)
      file_name_value(suggest_file_name_for_packages(df))
    }
    invisible(TRUE)
  }

  get_checked_build_packages <- function(isolate_values = TRUE) {
    ids <- if (isTRUE(isolate_values)) isolate(checked_package_ids()) else checked_package_ids()
    df <- if (isTRUE(isolate_values)) isolate(build_packages()) else build_packages()

    if (!length(ids) || !nrow(df)) return(empty_packages())

    df[df$cd_id_ia %in% as.integer(ids), , drop = FALSE]
  }

  refresh_build_packages <- function(preserve_checks = TRUE) {
    keep_ids <- if (isTRUE(preserve_checks)) isolate(checked_package_ids()) else integer(0)

    df <- tryCatch(
      selectBuildTreinoPacotes(dbp$get_pool()),
      error = function(e) {
        enqueue_build_notice(
          paste0("Nao foi possivel carregar os pacotes de build: ", conditionMessage(e)),
          type = "error"
        )
        empty_packages()
      }
    )

    df <- normalize_build_packages(df)
    build_packages(df)

    if (!nrow(df)) {
      checked_package_ids(integer(0))
      apply_suggested_file_name(df, force = TRUE)
      return(invisible(df))
    }

    keep_ids <- intersect(as.integer(keep_ids), as.integer(df$cd_id_ia))
    checked_package_ids(keep_ids)
    apply_suggested_file_name(df[df$cd_id_ia %in% keep_ids, , drop = FALSE], force = FALSE)
    invisible(df)
  }

  process_next_build <- NULL

  finish_build <- function(success = TRUE, notify_type = "message") {
    build_running(FALSE)
    build_ctx$rows <- NULL
    build_ctx$index <- 0L
    build_ctx$built <- list()

    if (isTRUE(success)) {
      refresh_build_packages(preserve_checks = TRUE)
      enqueue_build_notice("Arquivo .rds gerado com sucesso!", type = notify_type)
    } else {
      enqueue_build_notice("Falha ao gerar o arquivo .rds. Veja os logs do processo.", type = "error")
    }
  }

  process_next_build <- function() {
    if (!isTRUE(isolate(build_running()))) return(invisible(FALSE))

    rows <- isolate(build_ctx$rows)
    total <- length(rows)
    next_idx <- isolate(build_ctx$index) + 1L

    if (!total) {
      append_log("Nenhum pacote encontrado para o build.")
      finish_build(success = FALSE)
      return(invisible(FALSE))
    }

    if (next_idx > total) {
      dataset <- dplyr::bind_rows(isolate(build_ctx$built))

      if (!nrow(dataset)) {
        append_log("Nenhum pacote valido foi convertido em dataset.")
        finish_build(success = FALSE)
        return(invisible(FALSE))
      }

      dir.create(isolate(build_ctx$output_dir), recursive = TRUE, showWarnings = FALSE)
      file_path <- file.path(isolate(build_ctx$output_dir), isolate(build_ctx$file_name))

      if (file.exists(file_path)) {
        append_log(paste0("Arquivo existente sera sobrescrito: ", file_path))
      }

      save_ok <- tryCatch({
        saveRDS(dataset, file_path)
        TRUE
      }, error = function(e) {
        append_log(sprintf("Falha ao salvar o arquivo .rds: %s", conditionMessage(e)))
        FALSE
      })

      if (!isTRUE(save_ok)) {
        finish_build(success = FALSE)
        return(invisible(FALSE))
      }

      append_log(sprintf("Dataset final com %d pacote(s) valido(s).", nrow(dataset)))
      append_log(sprintf("Pacotes ignorados: %d", isolate(build_ctx$skipped)))
      append_log(sprintf("Arquivo salvo em: %s", normalizePath(file_path, winslash = "/", mustWork = FALSE)))
      finish_build(success = TRUE)
      return(invisible(TRUE))
    }

    build_ctx$index <- next_idx
    row <- rows[[next_idx]]

    append_log(sprintf("[%d/%d] Processando pacote %s...", next_idx, total, row$cd_id_ia[[1]]))

    result <- tryCatch(
      buildTreinoDatasetRow(
        dbp$get_pool(),
        pacote_row = row,
        limit = isolate(build_ctx$limit),
        logger = append_log
      ),
      error = function(e) {
        append_log(sprintf("Erro ao processar pacote %s: %s", row$cd_id_ia[[1]], conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(result) && nrow(result)) {
      built_items <- isolate(build_ctx$built)
      built_items[[length(built_items) + 1L]] <- result
      build_ctx$built <- built_items
      append_log(sprintf("Pacote %s adicionado ao dataset.", row$cd_id_ia[[1]]))
    } else {
      build_ctx$skipped <- isolate(build_ctx$skipped) + 1L
      append_log(sprintf("Pacote %s foi ignorado.", row$cd_id_ia[[1]]))
    }

    later::later(function() {
      shiny::withReactiveDomain(session, {
        process_next_build()
      })
    }, 0.02)
    invisible(TRUE)
  }

  id <- ns("dialogBuildTrain")
  cssStyle <- list()
  cssStyle[[paste0(" #parent", id, " .modal-dialog")]] <- "width: 92% !important; height: 88% !important;"
  cssStyle[[paste0(" #parent", id, " .modal-content")]] <- "width: 100% !important; height: 100% !important;"
  cssStyle[[paste0(" #parent", id, " .modal-body")]] <- "width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto; overflow-x: hidden;"

  showModal(
    session = session,
    div(
      id = paste0("parent", id),
      style = "height: 80%; overflow: hidden;",
      inlineCSS(cssStyle),
      dialogModal(
        title = "Build de Dataset",
        size = "m",
        div(
          singleton(tags$script(HTML(
            "window.tvsBuildSyncCheckboxes = window.tvsBuildSyncCheckboxes || function(tableId) {
               var selected = (window.tvsBuildSelected && window.tvsBuildSelected[tableId]) || [];
               var selectedMap = {};
               for (var i = 0; i < selected.length; i++) {
                 selectedMap[String(selected[i])] = true;
               }
               var $table = $('#' + tableId);
               if (!$table.length || !$.fn.dataTable.isDataTable($table)) return;
               var table = $table.DataTable();
               $(table.rows({page:'current'}).nodes()).find('input.build-check-pacote').each(function() {
                 var pacoteId = String($(this).attr('data-pacote-id'));
                 this.checked = !!selectedMap[pacoteId];
               });
             };
             if (!window.tvsBuildSyncHandlerInstalled) {
               window.tvsBuildSyncHandlerInstalled = true;
               Shiny.addCustomMessageHandler('tvs-build-sync-checks', function(message) {
                 window.tvsBuildSelected = window.tvsBuildSelected || {};
                 window.tvsBuildSelected[message.tableId] = (message.ids || []).map(function(x) {
                   return parseInt(x, 10);
                 }).filter(function(x) {
                   return !isNaN(x);
                 });
                 window.tvsBuildSyncCheckboxes(message.tableId);
               });
             }"
          ))),
          panelTitle(
            title = "Pacotes Disponiveis",
            background.color.title = "white",
            title.color = "black",
            border.color = "lightgray",
            children = div(
              style = "padding: 10px;",
              tags$p(
                style = "color:#666; margin-bottom:10px;",
                "Marque os pacotes desejados. O Build vai gerar o .rds usando todos os itens que estiverem checados."
              ),
              uiOutput(ns("buildSelectionActions")),
              DT::DTOutput(ns("buildPacotesTable"))
            )
          ),
          br(),
          fluidRow(
            column(
              12,
              panelTitle(
                title = "Saida",
                background.color.title = "white",
                title.color = "black",
                border.color = "lightgray",
                children = div(
                  style = "padding: 15px;",
                  uiOutput(ns("buildOutputToggle")),
                  br(),
                  uiOutput(ns("buildPathPreview")),
                  br(),
                  uiOutput(ns("buildOutputWidget")),
                  tags$div(
                    style = "color:#666; font-size:12px;",
                    textOutput(ns("buildSelectedInfo"))
                  )
                )
              )
            ),
            column(
              12,
              panelTitle(
                title = "Logs do Processo",
                background.color.title = "white",
                title.color = "black",
                border.color = "lightgray",
                children = div(
                  style = "padding: 10px;",
                  tags$pre(
                    style = paste(
                      "height: 240px; overflow-y: auto; background: #111;",
                      "color: #d9f3da; padding: 12px; border-radius: 6px;",
                      "border: 1px solid #2d2d2d; margin: 0;"
                    ),
                    textOutput(ns("buildLogs"))
                  )
                )
              )
            )
          )
        ),
        footer = uiOutput(ns("uiBuildFooter"))
      )
    )
  )

  shinyDirChoose(
    input,
    "buildOutputDirPicker",
    roots = build_dir_roots,
    session = session
  )

  output$uiBuildFooter <- renderUI({
    compact_btn <- actionButton(
      ns("btCompactarBuild"),
      label = if (isTRUE(build_running())) "Processando..." else "Compactar",
      icon = icon(if (isTRUE(build_running())) "spinner" else "archive"),
      class = "btn-primary"
    )

    if (isTRUE(build_running())) {
      compact_btn <- tagAppendAttributes(compact_btn, disabled = "disabled")
    }

    refresh_btn <- actionButton(
      ns("btAtualizarBuild"),
      label = "Atualizar",
      icon = icon("refresh")
    )

    if (isTRUE(build_running())) {
      refresh_btn <- tagAppendAttributes(refresh_btn, disabled = "disabled")
    }

    tagList(
      actionButton(ns("btSairBuild"), label = "Sair", icon = icon("arrow-left")),
      refresh_btn,
      compact_btn
    )
  })

  output$buildLogs <- renderText({
    build_logs()
  })

  output$buildSelectionActions <- renderUI({
    df <- build_packages()
    ids <- checked_package_ids()
    ids <- intersect(as.integer(ids), as.integer(df$cd_id_ia))
    has_checked <- length(ids) > 0L

    actions <- list(
      actionButton(
        ns("btCheckAllBuild"),
        label = "Checar todos",
        icon = icon("square-check"),
        class = "btn btn-default btn-sm"
      ),
      actionButton(
        ns("btUncheckAllBuild"),
        label = "Desmarcar todos",
        icon = icon("square"),
        class = "btn btn-default btn-sm"
      )
    )

    if (isTRUE(has_checked)) {
      actions[[length(actions) + 1L]] <- actionButton(
        ns("btClearAllBuild"),
        label = "Limpar",
        icon = icon("trash"),
        class = "btn btn-danger btn-sm"
      )
    }

    div(
      style = "display:flex; gap:8px; margin-bottom:10px;",
      tagList(actions)
    )
  })

  obs$add(observeEvent(build_notice(), {
    info <- build_notice()
    if (is.null(info)) return(invisible())

    showNotification(
      ui = info$message,
      type = info$type,
      duration = info$duration
    )

    build_notice(NULL)
  }, ignoreInit = TRUE, ignoreNULL = TRUE))

  output$buildOutputToggle <- renderUI({
    actionButton(
      ns("btMostrarDestinoBuild"),
      label = if (isTRUE(show_output_widget())) "Ocultar destino" else "Mostrar destino",
      icon = icon(if (isTRUE(show_output_widget())) "eye-slash" else "folder-open"),
      class = "btn btn-default"
    )
  })

  output$buildOutputWidget <- renderUI({
    if (!isTRUE(show_output_widget())) {
      return(
        tags$div(
          style = "padding: 10px 12px; background: #f8f8f8; border: 1px dashed #d0d0d0; border-radius: 6px; color: #666;",
          "Clique em 'Mostrar destino' para editar a pasta, o nome do arquivo e os parametros do build."
        )
      )
    }

    tagList(
      splitLayout(
        cellWidths = c("78%", "22%"),
        textInput(
          ns("buildOutputDir"),
          "Pasta de saida",
          value = output_dir_value(),
          width = "100%"
        ),
        shinyDirButton(
          ns("buildOutputDirPicker"),
          "Selecionar...",
          "Escolha a pasta de destino",
          buttonType = "default",
          icon = icon("folder-open"),
          style = "margin-top: 25px; width: 100%;"
        )
      ),
      textInput(
        ns("buildFileName"),
        "Nome do arquivo .rds",
        value = file_name_value(),
        width = "100%"
      ),
      numericInput(
        ns("buildLimit"),
        "Limite de frames por camera",
        value = build_limit_value(),
        min = 1L,
        step = 1L,
        width = "100%"
      )
    )
  })

  output$buildPathPreview <- renderUI({
    checked_df <- get_checked_build_packages(isolate_values = FALSE)
    default_file <- suggest_file_name_for_packages(checked_df)

    dir_value <- output_dir_value()
    file_value <- file_name_value()
    if (!nzchar(trimws(file_value))) file_value <- default_file

    full_path <- tryCatch(
      file.path(resolve_output_dir(dir_value), normalize_file_name(file_value)),
      error = function(e) file.path(resolve_output_dir(dir_value), default_file)
    )

    tags$div(
      style = "padding: 12px; background: #f3f6f9; border: 1px solid #d9e1e8; border-radius: 6px;",
      tags$div(style = "font-weight: 600; color: #37474f; margin-bottom: 6px;", "Destino atual"),
      tags$div(style = "font-size: 12px; color: #5f6b72; margin-bottom: 4px;", paste0("Pasta: ", resolve_output_dir(dir_value))),
      tags$div(style = "font-size: 12px; color: #5f6b72; margin-bottom: 4px;", paste0("Arquivo: ", file_value)),
      tags$div(style = "font-size: 12px; color: #1f2d3d;", paste0("Caminho final: ", full_path))
    )
  })

  output$buildSelectedInfo <- renderText({
    df <- get_checked_build_packages(isolate_values = FALSE)
    if (!nrow(df)) return("Nenhum pacote marcado para build.")

    objetos <- unique(df$name_objeto)
    tipos <- unique(df$name_tipo_pacote)

    sprintf(
      "Marcados: %d pacote(s) | Objetos: %d | Tipos: %s",
      nrow(df),
      length(objetos),
      paste(tipos, collapse = ", ")
    )
  })

  output$buildPacotesTable <- DT::renderDT({

    check_flag_active()
    df <- build_packages()

    if (!nrow(df)) {
      return(
        DT::datatable(
          data.frame(Mensagem = "Nenhum pacote disponivel para build.", stringsAsFactors = FALSE),
          rownames = FALSE,
          options = list(dom = "t", paging = FALSE, ordering = FALSE)
        )
      )
    }
    
    table_df <- prepare_build_table_df(df, isolate(checked_package_ids()))

    DT::datatable(
      table_df,
      escape = FALSE,
      filter = "top",
      rownames = FALSE,
      selection = "none",
      class = "cell-border stripe compact",
      options = list(
        dom = "ftip",
        pageLength = 8,
        lengthChange = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        ordering = FALSE,
        searching = TRUE,
        destroy = TRUE,
        language = list(
          emptyTable = "Nenhum pacote disponivel para build.",
          zeroRecords = "Nenhum pacote encontrado."
        ),
        drawCallback = DT::JS(
          sprintf(
            "function(settings){ if (window.tvsBuildSyncCheckboxes) window.tvsBuildSyncCheckboxes('%s'); }",
            ns("buildPacotesTable")
          )
        ),
        columnDefs = list(
          list(searchable = FALSE, targets = c(0, 1)),
          list(width = "40px", targets = c(0, 1)),
          list(className = "dt-center", targets = c(0, 1, 2, 3))
        )
      )
    )
  }, server = FALSE)

  obs$add(observeEvent(checked_package_ids(), {
    df <- isolate(build_packages())
    if (!nrow(df)) return(invisible())
    sync_build_checkbox_ui(checked_package_ids())
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$buildCheckPacote, {
    info <- input$buildCheckPacote
    pacote_id <- suppressWarnings(as.integer(info$id))
    if (!is.finite(pacote_id)) return(invisible())

    ids <- as.integer(checked_package_ids())
    if (isTRUE(info$checked)) {
      ids <- union(ids, pacote_id)
    } else {
      ids <- setdiff(ids, pacote_id)
    }

    checked_package_ids(sort(unique(ids)))
    apply_suggested_file_name(get_checked_build_packages(), force = FALSE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btCheckAllBuild, {
    if (isTRUE(build_running())) {
      showNotification("Aguarde o build terminar para alterar a selecao dos pacotes.", type = "warning")
      return(invisible())
    }

    df <- isolate(build_packages())
    if (!nrow(df)) return(invisible())

    checked_package_ids(sort(unique(as.integer(df$cd_id_ia))))
    apply_suggested_file_name(get_checked_build_packages(), force = FALSE)
    append_log(sprintf("Todos os pacotes foram marcados (%d item(ns)).", nrow(df)))
    check_flag_active(TRUE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btUncheckAllBuild, {
    if (isTRUE(build_running())) {
      showNotification("Aguarde o build terminar para alterar a selecao dos pacotes.", type = "warning")
      return(invisible())
    }

    checked_package_ids(integer(0))
    apply_suggested_file_name(get_checked_build_packages(), force = FALSE)
    append_log("Todos os pacotes foram desmarcados.")
    check_flag_active(FALSE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btClearAllBuild, {
    if (isTRUE(build_running())) {
      showNotification("Aguarde o build terminar para limpar os pacotes.", type = "warning")
      return(invisible())
    }

    df <- isolate(build_packages())
    ids <- intersect(as.integer(isolate(checked_package_ids())), as.integer(df$cd_id_ia))
    if (!length(ids)) return(invisible())

    dialogConfirm(
      session = session,
      id = ns("dialogDeleteAllBuild"),
      title = "Limpar pacotes marcados?",
      text = paste0("Deseja excluir ", length(ids), " pacote(s) marcado(s) do banco de dados?")
    )

    obs$add(observeEvent(input$dialogDeleteAllBuild, {
      status <- input$dialogDeleteAllBuild
      if (!isTRUE(status)) return(invisible())

      delete_result <- db$tryTransaction(function(conn) {
        for (pacote_id in ids) {
          db$deleteTable(
            conn,
            "pacote_ia",
            where_cols = c("cd_id_ia"),
            where_vals = list(as.integer(pacote_id))
          )
        }
      })

      if (!isTRUE(delete_result)) {
        delete_err <- attr(delete_result, "error_message", exact = TRUE)
        msg <- "Nao foi possivel limpar os pacotes marcados."
        if (!is.null(delete_err) && nzchar(delete_err)) {
          msg <- paste0(msg, " ", delete_err)
        }
        showNotification(msg, type = "error")
        return(invisible())
      }

      checked_package_ids(integer(0))
      check_flag_active(FALSE)
      refresh_build_packages(preserve_checks = FALSE)
      append_log(sprintf("%d pacote(s) marcado(s) foram excluidos do banco de dados.", length(ids)))
      showNotification("Pacotes marcados removidos com sucesso.", type = "message")
    }, ignoreInit = TRUE, once = TRUE))
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$buildDeletePacote, {
    if (isTRUE(build_running())) {
      showNotification("Aguarde o build terminar para excluir pacotes.", type = "warning")
      return(invisible())
    }

    pacote_id <- suppressWarnings(as.integer(input$buildDeletePacote$id))
    if (!is.finite(pacote_id)) return(invisible())

    df <- build_packages()
    pacote <- df[df$cd_id_ia == pacote_id, , drop = FALSE]
    if (!nrow(pacote)) return(invisible())

    dialogConfirm(
      session = session,
      id = ns("dialogDeleteBuild"),
      title = "Excluir pacote?",
      text = paste0("Deseja excluir o pacote ", pacote_id, " do banco de dados?")
    )

    obs$add(observeEvent(input$dialogDeleteBuild, {
      status <- input$dialogDeleteBuild
      if (!isTRUE(status)) return(invisible())

      delete_result <- db$tryTransaction(function(conn) {
        db$deleteTable(
          conn,
          "pacote_ia",
          where_cols = c("cd_id_ia"),
          where_vals = list(as.integer(pacote_id))
        )
      })

      if (!isTRUE(delete_result)) {
        delete_err <- attr(delete_result, "error_message", exact = TRUE)
        msg <- "Nao foi possivel excluir o pacote selecionado."
        if (!is.null(delete_err) && nzchar(delete_err)) {
          msg <- paste0(msg, " ", delete_err)
        }
        showNotification(msg, type = "error")
        return(invisible())
      }

      checked_package_ids(setdiff(as.integer(checked_package_ids()), pacote_id))
      refresh_build_packages(preserve_checks = TRUE)
      append_log(sprintf("Pacote %s excluido do banco de dados.", pacote_id))
      showNotification("Pacote excluido com sucesso.", type = "message")
    }, ignoreInit = TRUE, once = TRUE))
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btMostrarDestinoBuild, {
    show_output_widget(!isTRUE(show_output_widget()))
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$buildOutputDir, {
    val <- trimws(as.character(input$buildOutputDir)[1])
    output_dir_value(if (nzchar(val)) val else "train")
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$buildOutputDirPicker, {
    selected_dir <- tryCatch(
      parseDirPath(build_dir_roots, input$buildOutputDirPicker),
      error = function(e) character(0)
    )

    if (!length(selected_dir)) return(invisible())

    selected_dir <- normalizePath(as.character(selected_dir)[1], winslash = "/", mustWork = FALSE)
    output_dir_value(selected_dir)
    updateTextInput(session, "buildOutputDir", value = selected_dir)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$buildFileName, {
    file_name_value(as.character(input$buildFileName)[1])
    file_name_touched(TRUE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$buildLimit, {
    val <- suppressWarnings(as.integer(input$buildLimit))
    if (is.finite(val) && val >= 1L) build_limit_value(val)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btAtualizarBuild, {
    refresh_build_packages(preserve_checks = TRUE)
    append_log("Lista de pacotes atualizada.")
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btCompactarBuild, {
    if (isTRUE(build_running())) {
      showNotification("Ja existe um build em andamento.", type = "warning")
      return(invisible())
    }

    checked_df <- get_checked_build_packages()
    if (!nrow(checked_df)) {
      showNotification("Marque pelo menos um pacote na lista para compactar.", type = "warning")
      return(invisible())
    }

    output_dir <- tryCatch(
      resolve_output_dir(output_dir_value()),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    if (is.null(output_dir)) return(invisible())

    file_name <- tryCatch(
      normalize_file_name(file_name_value()),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    if (is.null(file_name)) return(invisible())

    limit_frames <- suppressWarnings(as.integer(build_limit_value()))
    if (!is.finite(limit_frames) || limit_frames < 1L) {
      showNotification("Informe um limite de frames valido.", type = "warning")
      return(invisible())
    }

    build_logs("Preparando build para os pacotes marcados.")
    append_log(sprintf("Pacotes marcados: %d", nrow(checked_df)))
    append_log(sprintf("Arquivo de saida: %s", file.path(output_dir, file_name)))
    append_log(sprintf("Limite por camera: %d frame(s)", limit_frames))

    build_ctx$rows <- split(checked_df, seq_len(nrow(checked_df)))
    build_ctx$index <- 0L
    build_ctx$built <- list()
    build_ctx$skipped <- 0L
    build_ctx$output_dir <- output_dir
    build_ctx$file_name <- file_name
    build_ctx$limit <- limit_frames
    build_ctx$summary_row <- checked_df

    build_running(TRUE)
    later::later(function() {
      shiny::withReactiveDomain(session, {
        process_next_build()
      })
    }, 0.02)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btSairBuild, {
    build_running(FALSE)
    build_ctx$rows <- NULL
    obs$destroy()
    removeModal(session)
    callback()
  }, ignoreInit = TRUE, ignoreNULL = TRUE))

  refresh_build_packages(preserve_checks = FALSE)
}
