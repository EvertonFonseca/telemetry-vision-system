# treinar.R  (Leaflet + Player + Clips + DYNAMIC tracking retângulos "estrutura")
# =============================================================
# ✅ + Player later::later (sem reentrância)
# ✅ + Cache LRU + Prefetch por lote
# ✅ + Múltiplos leafletOutput por câmera
# ✅ + Handler JS 'set_frame_to' (id alvo)
# ✅ + Timezone consistente (UTC) p/ chaves/queries
# ✅ + Clip com duração máxima (default 5 min)
# ✅ + Player de clip dentro do modal do clip (Preview independente)
# ✅ + NOVO (OBJETO DINÂMICO):
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
PREFETCH_AHEAD  <- 16L
MAX_CACHE_ITEMS <- 400L

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

# ==================================================
# Utils: tempo/chaves/formatos
# ==================================================
key_of <- function(cam, ts_utc) sprintf(
  "%s|%s", as.character(cam), format(ts_utc, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
)
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
new_player_ctx <- function(session, pool, lru_cache) {
  env <- new.env(parent = emptyenv())
  env$session <- session
  env$pool    <- pool
  env$cache   <- lru_cache

  env$fetch_dataurl_single <- function(cam, ts_utc) {
    k <- key_of(cam, ts_utc)
    hit <- env$cache$get(k)
    if (!is.null(hit)) return(hit)
    res <- db_fetch_frame_raw(env$pool, cam, ts_utc)
    if (!nrow(res) || is.null(res$data_frame[[1]])) return(NULL)
    raw <- res$data_frame[[1]]
    uri <- to_data_url(raw)
    env$cache$put(k, uri)
    uri
  }

  env$render_current <- function(seq_df, i, w, h, id_map, fit_bounds = FALSE) {
    if (is.null(seq_df) || !nrow(seq_df)) return(list(ok = FALSE, w = w, h = h))
    i <- max(1L, min(nrow(seq_df), as.integer(i)))

    ts_ref <- as.POSIXct(seq_df$dt_hr_local[i], tz = "UTC")
    same_ts <- seq_df[seq_df$dt_hr_local == ts_ref, , drop = FALSE]
    if (!nrow(same_ts)) return(list(ok = FALSE, w = w, h = h))

    ok_any <- FALSE

    for (row in seq_len(nrow(same_ts))) {
      cam    <- as.integer(same_ts$cd_id_camera[row])
      dom_id <- id_map[[as.character(cam)]]
      if (is.null(dom_id) || is.na(dom_id)) next

      uri <- env$fetch_dataurl_single(cam, ts_ref)
      if (is.null(uri)) next

      env$session$sendCustomMessage("set_frame_to", list(
        id  = dom_id,
        url = uri,
        w   = as.integer(w),
        h   = as.integer(h),
        fit = isTRUE(fit_bounds)
      ))
      ok_any <- TRUE
    }

    list(ok = ok_any, w = w, h = h)
  }

  env$prefetch_ahead_batch <- function(seq_df, i, N = PREFETCH_AHEAD) {
    if (is.null(seq_df) || !nrow(seq_df)) return(invisible(FALSE))
    n <- nrow(seq_df); i <- as.integer(i)
    if (i >= n) return(invisible(FALSE))

    tgt_idx <- seq.int(i + 1L, min(n, i + N))
    if (!length(tgt_idx)) return(invisible(FALSE))
    seg <- seq_df[tgt_idx, , drop = FALSE]
    seg$k <- mapply(key_of, seg$cd_id_camera, seg$dt_hr_local)
    seg <- seg[!vapply(seg$k, function(z) !is.null(env$cache$get(z)), logical(1L)), , drop = FALSE]
    if (!nrow(seg)) return(invisible(TRUE))

    groups <- split(seg, seg$cd_id_camera)
    for (cam_str in names(groups)) {
      g      <- groups[[cam_str]]
      cam    <- as.integer(cam_str)
      ts_vec <- as.POSIXct(g$dt_hr_local, tz = "UTC")

      res <- tryCatch(db_fetch_many_frames(env$pool, cam, ts_vec), error = function(e) NULL)
      if (!is.null(res) && nrow(res)) {
        res$dt_hr_local <- as.POSIXct(res$dt_hr_local, tz = "UTC")
        idx_map <- match(ts_vec, res$dt_hr_local)
        for (j in seq_along(ts_vec)) {
          if (!is.na(idx_map[j])) {
            raw <- res$data_frame[[idx_map[j]]]
            if (!is.null(raw)) env$cache$put(key_of(cam, ts_vec[j]), to_data_url(raw))
          }
        }
      }
      for (j in seq_along(ts_vec)) {
        if (is.null(env$cache$get(key_of(cam, ts_vec[j])))) {
          invisible(env$fetch_dataurl_single(cam, ts_vec[j]))
        }
      }
    }

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
  rv$clip_i0     <- rv$i
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
    rect_id    = integer(0),         # 1..n (reutilizável)
    leaflet_id = character(0),       # id do layer no leaflet (string)
    cam_id     = integer(0),
    created_ts_utc = as.POSIXct(character(0), tz = "UTC"),
    last_ts_utc    = as.POSIXct(character(0), tz = "UTC"),  # último APPLY de tracking
    last_poly      = list(),                                 # poly do último APPLY
    estrutura_id   = integer(0),
    estrutura_nome = character(0),
    attrs      = list()              # named list (valores)
  )
}

dyntrack_empty_df <- function() {
  tibble::tibble(
    rect_id    = integer(0),
    leaflet_id = character(0),
    cam_id     = integer(0),
    ts_utc     = as.POSIXct(character(0), tz = "UTC"),
    poly       = list(),              # tibble x,y (keyframes)
    box        = list(),              # list(x_min,y_min,x_max,y_max)
    estrutura_id   = integer(0),
    estrutura_nome = character(0),
    attrs      = list()               # snapshot de atributos por keyframe
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

dynrect_attrs_ui <- function(ns, attrs_df, values = NULL) {
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
      # ✅ novo formato (por NOME)
      if (!is.null(values[[att_name]])) {
        cur <- values[[att_name]]
      } else if (!is.null(values[[toupper(att_name)]])) {
        # opcional: se você decidiu salvar com toupper()
        cur <- values[[toupper(att_name)]]
      } else if (!is.null(values[[as.character(att_id)]])) {
        # compat: formato antigo (por ID)
        cur <- values[[as.character(att_id)]]
      }
    }

    if (identical(att_type, "QUALITATIVE")) {
      classes <- stringr::str_split(att_vals, ",")[[1]]
      classes <- trimws(classes)
      out <- tagAppendChildren(out,
        selectizeInput(
          ns(paste0("dynrect_att_", att_id)),
          label = att_name,
          choices = classes,
          selected = if (!is.null(cur) && nzchar(cur)) cur else NULL,
          options = list(dropdownParent = 'body', openOnFocus = TRUE, closeAfterSelect = TRUE)
        )
      )
    } else {
      out <- tagAppendChildren(out,
        numericInput(
          ns(paste0("dynrect_att_", att_id)),
          label = att_name,
          value = suppressWarnings(as.numeric(cur)),
          step  = 1
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
      if (length(nm)) nm[[1]] else paste("Câmera", cid)
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
      numericInput(ns("clipOverlay_step_ms"), label = "Intervalo (ms)", value = 50, min = 1, step = 1, width = "120px") |>
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
            title = "Preview do Clip (multi-câmera)",
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
# UI Builders (JS handlers, Header form, Lista de câmeras, Controles)
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
  tags$script(HTML("
    (function(){
      const overlays = {}; const boundsOf = {}; const maps = {};
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
          boundsOf[msg.id] = fitWholeImage(map, w, h);
          return;
        }
        const changed = !boundsOf[msg.id] || boundsOf[msg.id][1][0] !== h || boundsOf[msg.id][1][1] !== w;
        if (changed){
          overlays[msg.id].setBounds(b);
          boundsOf[msg.id] = b;
          if (msg.fit) boundsOf[msg.id] = fitWholeImage(map, w, h);
        }
        overlays[msg.id].setUrl(msg.url);
      });
      Shiny.addCustomMessageHandler('reset_overlays', function(ids){
        (ids || []).forEach(function(id){
          try { if (overlays[id]) overlays[id].remove(); } catch(e){}
          delete overlays[id]; delete boundsOf[id]; delete maps[id];
        });
      });

            Shiny.addCustomMessageHandler('set_draw_style', function(msg){
        try{
          var el = document.getElementById(msg.map_id);
          if(!el) return;
          var widget = HTMLWidgets.find(el);
          if(!widget || !widget.getMap) return;
          var map = widget.getMap();
          var lid = msg.leaflet_id;
          if(lid === null || lid === undefined) return;
          var layer = map._layers[lid];
          if(layer && layer.setStyle){
            layer.setStyle({
              color: msg.color || 'red',
              fillColor: msg.fillColor || msg.color || 'red',
              weight: msg.weight || 3,
              opacity: (msg.opacity === null || msg.opacity === undefined) ? 1 : msg.opacity,
              fillOpacity: (msg.fillOpacity === null || msg.fillOpacity === undefined) ? 0.1 : msg.fillOpacity
            });
          }
        } catch(e){}
      });

Shiny.addCustomMessageHandler('set_clip_overlay_frame', function(msg){
        var imgEl = document.getElementById(msg.img_id);
        if(!imgEl) return;
        imgEl.src = msg.url || '';
      });

    })();
  "))
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
    title = "Rastreamento – Estruturas (Retângulos)",
    background.color.title = "white",
    title.color  = "black",
    border.color = "lightgray",
    children = div(
      style = "padding: 10px;",
      tags$div(
        style = "margin-bottom:8px; color:#666; font-size:13px;",
        tags$b("Como usar:"), " ative um ", tags$b("Clip"), " e desenhe retângulos vermelhos. Clique na linha da tabela para selecionar e ele ficará ", tags$b("VERDE"), ". ",
        "Ajuste o retângulo no Leaflet e clique em ", tags$b("Aplicar"), " para salvar o tracking (posições ao longo do tempo)."
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
                            color = "#FF0000",
                            weight = 2,
                            opacity = 1,
                            fill = TRUE,
                            fillColor = "#FF0000",
                            fillOpacity = 0.20
                          ),
                          showArea = FALSE
                        ),
                        editOptions = editToolbarOptions(
                          edit = TRUE,
                          remove = TRUE,
                          selectedPathOptions = selectedPathOptions(
                            maintainColor = TRUE,
                            color = "#FF0000",
                            fillColor = "#FF0000",
                            fillOpacity = 0.20
                          )
                        )
                      )
        }

        # ✅ (NOVO) Clique em retângulos desenhados (Leaflet.Draw):
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
              }
              try{
                var widget = HTMLWidgets.find(el);
                if(!widget || !widget.getMap) return;
                var map = widget.getMap();
                if(!map) return;

                // bind existentes
                map.eachLayer(function(l){ bindLayer(l); });

                // bind futuros (ex.: draw_new_feature / edições)
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
        title = paste0("Câmera: ", cam_name),
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

  # layout final:
  # - estático: só câmeras
  # - dinâmico: col-6 câmeras + col-6 painel de retângulos
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
    numericInput(ns("step_ms"), "Intervalo (ms)", value = 50, min = 1, step = 1, width = "110px") |>
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

# ==================================================
# Módulo principal (server)
# ==================================================
#' @export
uiNewTreinar <- function(ns, input, output, session, callback) {

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

  # ---------- Estado player principal ----------
  rv <- reactiveValues(
    seq = NULL, i = 1L, w = 512L, h = 512L,
    id_by_cam = NULL,
    clip_active = FALSE, clip_t0 = NULL, clip_i0 = NA_integer_
  )

  playing   <- reactiveVal(FALSE)
  loop_on   <- FALSE
  play_dir  <- reactiveVal(+1L)

  # ---------- Store de Clips ----------
  clips <- reactiveVal(
    data.frame(
      id = integer(0),
      title = character(0),
      t0 = as.POSIXct(character(0), tz = "UTC"),
      t1 = as.POSIXct(character(0), tz = "UTC"),
      i0 = integer(0),
      i1 = integer(0),
      stringsAsFactors = FALSE
    )
  )
  next_clip_id <- reactiveVal(1L)

  updateClipsTable <- function(df) {
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

  # ---------- Cache + PlayerContext ----------
  if (is.null(session$userData$lru_cache))  session$userData$lru_cache  <- new_lru_cache(MAX_CACHE_ITEMS)
  if (is.null(session$userData$player_ctx)) session$userData$player_ctx <- new_player_ctx(session, dbp$get_pool(), session$userData$lru_cache)
  ctx <- session$userData$player_ctx

  # ---------- Player do CLIP no modal (preview independente) ----------
  clipOverlayPlayer <- reactiveValues(
    seq = NULL,
    i   = 1L,
    dir = +1L,
    playing = FALSE
  )

  render_clip_overlay_frame <- function() {
    isolate({
      if (is.null(clipOverlayPlayer$seq) || !nrow(clipOverlayPlayer$seq)) return(invisible())
      j  <- max(1L, min(nrow(clipOverlayPlayer$seq), as.integer(clipOverlayPlayer$i)))
      ts <- as.POSIXct(clipOverlayPlayer$seq$dt_hr_local[j], tz = "UTC")

      same_ts <- clipOverlayPlayer$seq[clipOverlayPlayer$seq$dt_hr_local == ts, , drop = FALSE]
      if (!nrow(same_ts)) return(invisible())

      for (r in seq_len(nrow(same_ts))) {
        cam <- as.integer(same_ts$cd_id_camera[r])
        uri <- ctx$fetch_dataurl_single(cam, ts)
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

  overlay_play_step <- function() {
    withReactiveDomain(session, {
      isolate({
        if (!isTRUE(clipOverlayPlayer$playing)) return(invisible())
        if (is.null(clipOverlayPlayer$seq) || !nrow(clipOverlayPlayer$seq)) {
          clipOverlayPlayer$playing <- FALSE
          return(invisible())
        }

        n   <- nrow(clipOverlayPlayer$seq)
        dir <- clipOverlayPlayer$dir
        if ((dir > 0L && clipOverlayPlayer$i >= n) ||
            (dir < 0L && clipOverlayPlayer$i <= 1L)) {
          clipOverlayPlayer$playing <- FALSE
          return(invisible())
        }

        clipOverlayPlayer$i <- clipOverlayPlayer$i + dir
        render_clip_overlay_frame()

        delay <- suppressWarnings(as.numeric(input$clipOverlay_step_ms) / 1000)
        if (!is.finite(delay) || delay <= 0) delay <- 0.001
        later::later(function() { overlay_play_step() }, delay)
      })
    })
  }

  # ---------- NOVO: Estado dos retângulos dinâmicos ----------
  dyn <- reactiveValues(
    rects  = dynrect_empty_df(),
    tracks = dyntrack_empty_df(),
    pending = new.env(parent = emptyenv()),   # pending[[leaflet_id]] = list(ts_utc=..., poly=...)
    selected_leaflet_id    = NA_character_,
    selected_rect_id       = NA_integer_,
    highlighted_leaflet_id = NA_character_,
    deleting = FALSE,
    editing  = FALSE,
    selecting_from_map = FALSE
  )

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

  dyn_reset_pending <- function() {
    dyn$pending <- new.env(parent = emptyenv())
  }

  dyn_reset <- function() {
    dyn$rects  <- dynrect_empty_df()
    dyn$tracks <- dyntrack_empty_df()
    dyn_reset_pending()
    dyn$selected_leaflet_id    <- NA_character_
    dyn$selected_rect_id       <- NA_integer_
    dyn$highlighted_leaflet_id <- NA_character_
    dyn$deleting <- FALSE
    dyn$editing  <- FALSE
    dyn$selecting_from_map <- FALSE
  }

    dyn_dt_proxy <- function() {
      # ✅ em módulo: passe o ID sem ns() (session já sabe o namespace)
      DT::dataTableProxy("dynrectTable", session = session)
    }

   dyn_dt_select_by_leaflet <- function(leaflet_id) {
  df <- dyn$rects
  if (is.null(df) || !nrow(df)) return(invisible(FALSE))
  idx <- which(as.character(df$leaflet_id) == as.character(leaflet_id))
  if (!length(idx)) return(invisible(FALSE))

  dyn$selecting_from_map <- TRUE
  proxy <- dyn_dt_proxy()

  # força seleção mesmo se já estava selecionado
  DT::selectRows(proxy, NULL)
  DT::selectRows(proxy, idx[1])

  TRUE
} 

  dyn_send_style <- function(cam_id, leaflet_id, color = "red") {
    if (is.null(cam_id) || !is.finite(cam_id) || is.null(leaflet_id) || !nzchar(leaflet_id)) return(invisible(FALSE))
    session$sendCustomMessage("set_draw_style", list(
      map_id = ns(paste0("map_", as.integer(cam_id))),
      leaflet_id = as.character(leaflet_id),
      color = color,
      fillColor = color,
      weight = 3,
      fillOpacity = 0.08
    ))
    TRUE
  }

  dyn_highlight_selected <- function() {
    cur <- dyn$selected_leaflet_id
    if (is.null(cur) || !nzchar(cur)) return(invisible(FALSE))

    prev <- dyn$highlighted_leaflet_id
    if (!is.null(prev) && nzchar(prev) && !identical(prev, cur)) {
      prev_row <- dyn_get_by_leaflet(prev)
      if (!is.null(prev_row) && nrow(prev_row)) {
        dyn_send_style(prev_row$cam_id[[1]], prev, color = "red")
      }
    }

    row <- dyn_get_by_leaflet(cur)
    if (!is.null(row) && nrow(row)) {
      dyn_send_style(row$cam_id[[1]], cur, color = "green")
      dyn$highlighted_leaflet_id <- cur
    }
    TRUE
  }

  dyn_select_by_leaflet <- function(leaflet_id) {
    if (is.null(leaflet_id) || !nzchar(leaflet_id)) return(invisible(FALSE))
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.character(df$leaflet_id) == as.character(leaflet_id))
    if (!length(idx)) return(invisible(FALSE))
    dyn$selected_leaflet_id <- as.character(df$leaflet_id[idx[1]])
    dyn$selected_rect_id    <- as.integer(df$rect_id[idx[1]])
    dyn_dt_select_by_leaflet(leaflet_id)
    dyn_highlight_selected()
    TRUE
  }
  
  dyn_select_by_rect <- function(rect_id) {
    if (is.null(rect_id) || !is.finite(rect_id)) return(invisible(FALSE))
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.integer(df$rect_id) == as.integer(rect_id))
    if (!length(idx)) return(invisible(FALSE))
    dyn$selected_leaflet_id <- as.character(df$leaflet_id[idx[1]])
    dyn$selected_rect_id    <- as.integer(df$rect_id[idx[1]])
    dyn_highlight_selected()
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
      
      # sanitiza attrs (nunca NULL/NA -> "")
      if (length(attrs)) {
        for (nm in names(attrs)) {
          v <- attrs[[nm]]
          if (is.null(v) || !length(v) || any(is.na(v))) attrs[[nm]] <- ""
          if (isTRUE(is.numeric(attrs[[nm]]))) attrs[[nm]] <- as.character(attrs[[nm]])
        }
      }
      
      tr <- dyn$tracks
      
      # compat: sessões antigas
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
    estrutura_id   = as.integer(NA),
    estrutura_nome = "",
    attrs          = list(list())
  )

  dyn$rects <- dplyr::bind_rows(dyn$rects, new_row)

  # ✅ este é o retângulo ativo
  dyn$selected_leaflet_id <- as.character(leaflet_id)
  dyn$selected_rect_id    <- as.integer(new_id)

  # ✅ após criar, marcamos como PENDING para o 1º APPLY gerar o 1º keyframe
  dyn_set_pending_poly(leaflet_id, ts_utc, poly)

  dyn_highlight_selected()

  # ✅ MUITO IMPORTANTE:
  # o DT re-renderiza depois que dyn$rects muda.
  # então selecionamos a linha após o flush, para não voltar pro retângulo #1.
  session$onFlushed(function() {
    try(dyn_dt_select_by_leaflet(as.character(leaflet_id)), silent = TRUE)
  }, once = TRUE)

  new_id
}
  
  # compat: edição do retângulo atualiza PENDING (só salva no APPLY)
  dyn_update_poly <- function(leaflet_id, ts_utc, poly) {
    if (!isTRUE(rv$clip_active)) return(invisible(FALSE))  # ✅ só registra edição com Clip ativo
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.character(df$leaflet_id) == as.character(leaflet_id))
    if (!length(idx)) return(invisible(FALSE))
    dyn_set_pending_poly(leaflet_id, ts_utc, poly)
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
    
    # ✅ valida meta e attrs (NUNCA salvar keyframe com null/vazio)
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
    
    # atualiza last_*
    df$last_ts_utc[idx[1]] <- as.POSIXct(ts_final, tz = "UTC")
    df$last_poly[[idx[1]]] <- poly
    dyn$rects <- df
    
    # ✅ salva keyframe completo
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
  
  dyn_delete_leaflet_ids <- function(leaflet_ids) {
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
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
    } else {
      if (dyn$selected_leaflet_id %in% as.character(leaflet_ids)) {
        dyn$selected_leaflet_id    <- NA_character_
        dyn$selected_rect_id       <- NA_integer_
        dyn$highlighted_leaflet_id <- NA_character_
      }
    }
    TRUE
  }
  
  dyn_apply_meta <- function(struct_df) {
    rid <- dyn$selected_rect_id
    if (is.null(rid) || !is.finite(rid)) return(invisible(FALSE))
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible(FALSE))
    idx <- which(as.integer(df$rect_id) == as.integer(rid))
    if (!length(idx)) return(invisible(FALSE))
    
    estr_id <- suppressWarnings(as.integer(input[["dynrect_structure"]]))
    if (!is.finite(estr_id)) {
      showNotification("Selecione uma estrutura.", type = "error")
      return(invisible(FALSE))
    }
    
    estr_row <- struct_df |> dplyr::filter(.data$cd_id_estrutura == estr_id)
    estr_nome <- if (nrow(estr_row)) as.character(estr_row$name_estrutura[[1]]) else ""
    if (!nzchar(estr_nome)) {
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
        if (!is.null(vals[[key]])) key <- paste0(key, "__", att_id)  # evita colisão
        
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

  # ---------- Modal/UI ----------
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
        title = "Novo Pacote",
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

  # ---------- Reagir troca de setor -> objetos ----------
  obs$add(observeEvent(input$comboSetor, {
    setor <- setores |> dplyr::filter(name_setor == input$comboSetor)
    if (!nrow(setor)) return()
    objs_setor <- objetos |> dplyr::filter(cd_id_setor == setor$cd_id_setor)
    updateSelectizeInput(session, "comboObjeto", choices = objs_setor$name_objeto)

    output$uiCamerasFrames <- NULL
    playing(FALSE); loop_on <<- FALSE
    session$userData$lru_cache$clear()
    dyn_reset()

    if (isTRUE(rv$clip_active)) {
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }
  }, ignoreNULL = TRUE))

  # ---------- Botão Buscar ----------
  obs$add(observeEvent(input$btBuscar, {

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

    frames_idx <- fetch_frames(
      dbp$get_pool(),
      time_begin    = time_begin,
      time_end      = time_end,
      camera_id_vec = cameras_ids
    )
    if (!nrow(frames_idx)) {
      removeProgressLoader()
      showNotification("Nenhum frame no intervalo/câmeras selecionados.", type = "warning")
      return(invisible())
    }

    # monta UI das câmeras + (se dinâmico) painel lateral
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
    rv$seq <- frames_idx; rv$i <- 1L; rv$w <- 512L; rv$h <- 512L
    playing(FALSE); loop_on <<- FALSE

    # Descobre w/h do primeiro frame
    first <- frames_idx[1, ]
    res1  <- db_fetch_frame_raw(dbp$get_pool(), first$cd_id_camera, first$dt_hr_local)
    if (nrow(res1) && !is.null(res1$data_frame[[1]])) {
      wh <- img_dims(res1$data_frame[[1]])
      rv$w <- if (is.finite(wh[1])) as.integer(wh[1]) else 512L
      rv$h <- if (is.finite(wh[2])) as.integer(wh[2]) else 350L
    }

    id_map <- rv$id_by_cam; seq_df <- rv$seq; i0 <- rv$i; w0 <- rv$w; h0 <- rv$h
    session$onFlushed(function() {
      session$sendCustomMessage("reset_overlays", unname(id_map))
      st <- ctx$render_current(seq_df = seq_df, i = i0, w = w0, h = h0, id_map = id_map, fit_bounds = TRUE)
      isolate({ if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h } })
      ctx$prefetch_ahead_batch(seq_df = seq_df, i = i0, N = PREFETCH_AHEAD)

      removeProgressLoader(callback = function() {
        step_frame(+1L)
      })
    }, once = TRUE)

    # -------- NOVO: registra observers do Leaflet Draw/Click para retângulos (se dinâmico) --------
    if (isTRUE(objeto$cd_id_objeto_tipo == 2L)) {

      # evita duplicar observers ao clicar "Buscar" repetidas vezes no mesmo conjunto
      if (is.null(e$dyn_map_registered)) e$dyn_map_registered <- character(0)

      cams_tbl <- res_cam$cameras
      cam_ids  <- as.integer(cams_tbl$cd_id_camera)

      for (cid in cam_ids) {
        map_out_id <- paste0("map_", cid)
        map_dom_id <- ns(map_out_id)

        if (map_dom_id %in% e$dyn_map_registered) next
        e$dyn_map_registered <- c(e$dyn_map_registered, map_dom_id)
        
        # ---- draw new ----
        obs$add(observeEvent(input[[paste0(map_out_id, "_draw_new_feature")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))
          feat <- input[[paste0(map_out_id, "_draw_new_feature")]]
          print("Novo")
          if (is.null(feat)) return()
          poly <- poly_from_feature(feat)
          if (is.null(poly) || !nrow(poly)) return()
          
          leaflet_id <- feat$properties$`_leaflet_id`
          if (!is.null(feat$properties$layerId)) leaflet_id <- feat$properties$layerId
          leaflet_id <- as.character(leaflet_id)


          # ✅ regra: não pode existir retângulo sem Clip
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
        
        # ---- draw edited ----
        obs$add(observeEvent(input[[paste0(map_out_id, "_draw_edited_features")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))

          # ✅ só permite editar/memorizar pending quando Clip está ativo
          if (!isTRUE(rv$clip_active)) return()
          feats <- input[[paste0(map_out_id, "_draw_edited_features")]]
          print("Editado")

          if (is.null(feats$features) || length(feats$features) == 0) return()
          ts <- rv_get_current_ts(rv)
          if (is.null(ts)) ts <- as.POSIXct(Sys.time(), tz = "UTC")
          
          for (feat in feats$features) {
            poly <- poly_from_feature(feat)
            if (is.null(poly) || !nrow(poly)) next
            leaflet_id <- feat$properties$`_leaflet_id`
            if (!is.null(feat$properties$layerId)) leaflet_id <- feat$properties$layerId
            leaflet_id <- as.character(leaflet_id)
            
            # atualiza poly do retângulo existente
            dyn_update_poly(leaflet_id = leaflet_id, ts_utc = ts, poly = poly)
          }
        }, ignoreInit = TRUE))
        
        # ---- draw deleted ----
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
        
        # ---- estados de edit/delete (para bloquear clique) ----
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
        
        # ---- clique no shape (tentamos 2 nomes comuns) ----
        obs$add(observeEvent(input[[paste0(map_out_id, "_shape_click")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))
          
          
          
          if (isTRUE(dyn$deleting) || isTRUE(dyn$editing)) return()
          
          ev <- input[[paste0(map_out_id, "_shape_click")]]
          if (is.null(ev) || is.null(ev$id)) return()
          
          lid <- as.character(ev$id)
          
          # ✅ só retângulos vermelhos (os que você registrou no dyn)
          if (!dyn_has_leaflet_id(lid)) return()
          
          dyn_select_by_leaflet(lid)
        }, ignoreInit = TRUE, ignoreNULL = TRUE))
        
        obs$add(observeEvent(input[[paste0(map_out_id, "_shape_draw_click")]], {
          req(isTRUE(objeto$cd_id_objeto_tipo == 2L))
          
          
          
          if (isTRUE(dyn$deleting) || isTRUE(dyn$editing)) return()
          
          ev <- input[[paste0(map_out_id, "_shape_draw_click")]]
          if (is.null(ev) || is.null(ev$id)) return()
          
          lid <- as.character(ev$id)
          
          # ✅ só retângulos vermelhos (os que você registrou no dyn)
          if (!dyn_has_leaflet_id(lid)) return()
          
          dyn_select_by_leaflet(lid)
        }, ignoreInit = TRUE, ignoreNULL = TRUE))

      } # for cams
    }

  }, ignoreInit = TRUE))

  # ---------- Navegação frame a frame (player principal) ----------
  step_frame <- function(delta) {
    req(rv$seq, nrow(rv$seq) > 0)
    new_i <- max(1L, min(nrow(rv$seq), rv$i + delta))
    if (new_i == rv$i) return(invisible())
    rv$i <- new_i
    st <- ctx$render_current(rv$seq, rv$i, rv$w, rv$h, rv$id_by_cam, fit_bounds = FALSE)
    if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }
    ctx$prefetch_ahead_batch(rv$seq, rv$i, PREFETCH_AHEAD)

    if (clip_limit_exceeded(rv, suppressWarnings(as.numeric(input$clip_max_min)))) {
      playing(FALSE); loop_on <<- FALSE
      ts_start <- rv$clip_t0; i0 <- rv$clip_i0
      ts_end   <- rv_get_current_ts(rv); i1 <- rv$i
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
      clips_add(ts_start, ts_end, i0, i1)
    }
  }
  obs$add(observeEvent(input$prevFrame, { step_frame(-1L) }, ignoreInit = TRUE))
  obs$add(observeEvent(input$nextFrame, { step_frame(+1L) }, ignoreInit = TRUE))
  
  # ---------- Loop Play principal ----------
  # ---------- Loop Play principal ----------
  play_step <- function() {
    withReactiveDomain(session, {
      isolate({
        if (!isTRUE(playing())) { loop_on <<- FALSE; return(invisible()) }
        if (is.null(rv$seq) || nrow(rv$seq) == 0L) { playing(FALSE); loop_on <<- FALSE; return(invisible()) }
        
        t0 <- proc.time()[3]  # <- tempo inicio do tick
        
        n   <- nrow(rv$seq)
        dir <- play_dir()
        if ((dir > 0L && rv$i >= n) || (dir < 0L && rv$i <= 1L)) {
          playing(FALSE); loop_on <<- FALSE; return(invisible())
        }
        
        rv$i <- rv$i + dir
        st <- ctx$render_current(rv$seq, rv$i, rv$w, rv$h, rv$id_by_cam, fit_bounds = FALSE)
        if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }
        
        # ✅ prefetch não precisa rodar TODO frame (ver item 3 abaixo)
        if ((rv$i %% 4L) == 0L) {
          ctx$prefetch_ahead_batch(rv$seq, rv$i, PREFETCH_AHEAD)
        }
        
        clockupdate(Sys.time())
        
        if (clip_limit_exceeded(rv, suppressWarnings(as.numeric(input$clip_max_min)))) {
          playing(FALSE); loop_on <<- FALSE
          ts_start <- rv$clip_t0; i0 <- rv$clip_i0
          ts_end   <- rv_get_current_ts(rv); i1 <- rv$i
          rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
          updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
          clips_add(ts_start, ts_end, i0, i1)
          return(invisible())
        }
        
        target <- suppressWarnings(as.numeric(input$step_ms) / 1000)
        if (!is.finite(target) || target <= 0) target <- 0.05
        
        elapsed <- proc.time()[3] - t0
        delay <- max(0.001, target - elapsed)   # ✅ não “pune” com delay extra
        
        later::later(function() { play_step() }, delay)
      })
    })
  }

  # ---------- Botões play/pause/reverse PRINCIPAL ----------
  obs$add(observeEvent(input$play, {
    play_dir(+1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  obs$add(observeEvent(input$backPlay, {
    play_dir(-1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  obs$add(observeEvent(input$pause, { playing(FALSE) }, ignoreInit = TRUE))

  # ---------- Clip toggle PRINCIPAL ----------
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
      ts_end   <- rv_get_current_ts(rv); i1 <- rv$i
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
      clips_add(ts_start, ts_end, i0, i1)
    }
  }, ignoreInit = TRUE))

  # ==========================================================
  # NOVO: Render/Editor do painel de retângulos dinâmicos
  # ==========================================================
  output$dynrectTable <- DT::renderDT({
    req(!is.null(objeto))
    if (!isTRUE(objeto$cd_id_objeto_tipo == 2L)) {
      return(DT::datatable(data.frame(), options = list(dom = "t")))
    }
    df <- dyn$rects
    if (is.null(df) || !nrow(df)) {
      return(DT::datatable(
        data.frame(
          ID = integer(0),
          Câmera = integer(0),
          Estrutura = character(0),
          ÚltimoFrame = character(0),
          stringsAsFactors = FALSE
        ),
        selection = "single",
        options = list(dom = "t", paging = FALSE, ordering = FALSE,
          language = list(emptyTable = "Nenhum retângulo criado ainda.")
        )
      ))
    }

    out <- df |>
      dplyr::mutate(
        ID = .data$rect_id,
        Câmera = .data$cam_id,
        Estrutura = ifelse(nzchar(.data$estrutura_nome), .data$estrutura_nome, "(não definido)"),
        ÚltimoFrame = format(.data$last_ts_utc, tz = Sys.timezone(), format = "%d/%m/%y %H:%M:%S")
      ) |>
      dplyr::select(.data$ID, .data$`Câmera`, .data$Estrutura, .data$ÚltimoFrame)

    DT::datatable(
      out,
      rownames = FALSE,
      selection = "single",
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

    # seleção programática (vinda do clique no retângulo / criação)
    if (isTRUE(dyn$selecting_from_map)) {
      dyn$selecting_from_map <- FALSE
      return(invisible())
    }

    sel <- input$dynrectTable_rows_selected
    if (is.null(sel) || !length(sel)) return(invisible())

    df <- dyn$rects
    if (is.null(df) || !nrow(df)) return(invisible())

    rid <- suppressWarnings(as.integer(df$rect_id[sel[1]]))
    if (!is.finite(rid)) return(invisible())

    # ✅ não sobrescreve se já é o mesmo
    if (isTRUE(!is.null(dyn$selected_rect_id)) && isTRUE(as.integer(dyn$selected_rect_id) == rid)) {
      return(invisible())
    }

    dyn_select_by_rect(rid)
  }, ignoreInit = TRUE))

  output$dynrectEditor <- renderUI({
  req(!is.null(objeto))
  if (!isTRUE(objeto$cd_id_objeto_tipo == 2L)) return(NULL)

  df <- dyn$rects
  if (is.null(df) || !nrow(df)) {
    return(div(tags$em("Crie um retângulo vermelho em qualquer câmera para começar.")))
  }

  rid <- dyn$selected_rect_id
  if (is.null(rid) || !isTRUE(is.finite(as.numeric(rid)))) {
    return(div(tags$em("Clique em um retângulo vermelho para editar Estrutura e Atributos.")))
  }

  idx <- which(as.integer(df$rect_id) == as.integer(rid))
  if (!length(idx)) return(div(tags$em("Clique em um retângulo vermelho para editar.")))
  row <- df[idx[1], , drop = FALSE]

  struct_df <- dynrect_structures_df(objeto)
  if (is.null(struct_df) || !nrow(struct_df)) {
    return(div(tags$em("Nenhuma estrutura encontrada para este objeto.")))
  }

  choices <- setNames(struct_df$cd_id_estrutura, struct_df$name_estrutura)

  # helper: valida inteiro finito SEM gerar NA no if()
  is_ok_int <- function(x) {
    x <- suppressWarnings(as.integer(x))
    isTRUE(is.finite(x)) && !is.na(x)
  }

  # estrutura salva (fallback)
  cur_estr_saved <- suppressWarnings(as.integer(row$estrutura_id[[1]]))
  if (!is_ok_int(cur_estr_saved)) cur_estr_saved <- suppressWarnings(as.integer(struct_df$cd_id_estrutura[[1]]))

  # ✅ estrutura do UI (depende do input -> força re-render quando muda o select)
  cur_estr_ui <- suppressWarnings(as.integer(input$dynrect_structure))
  if (!is_ok_int(cur_estr_ui)) cur_estr_ui <- cur_estr_saved
  cur_estr <- cur_estr_ui

  # attrs da estrutura do UI (sem if com NA)
  attrs_df <- NULL
  if (nrow(struct_df) > 0 && is_ok_int(cur_estr)) {
    sr <- struct_df |> dplyr::filter(.data$cd_id_estrutura == cur_estr)
    if (nrow(sr) > 0 && "atributos" %in% names(sr)) attrs_df <- sr$atributos[[1]]
  }

  cur_vals <- row$attrs[[1]]
  if (is.null(cur_vals) || !is.list(cur_vals)) cur_vals <- list()

  div(
    panelTitle(
      title = paste0("Retângulo #", row$rect_id, " (câmera ", row$cam_id, ")"),
      background.color.title = "white",
      title.color  = "black",
      border.color = "lightgray",
      children = div(
        style = "padding:10px;",
        selectInput(
          ns("dynrect_structure"),
          label = "Estrutura",
          choices = choices,
          selected = cur_estr,         # ✅ mantém a escolha atual do usuário
          width = "100%"
        ),
        br(),
        dynrect_attrs_ui(ns, attrs_df, values = cur_vals),
        br(),
        tagAppendAttributes(
          actionButton(ns("dynrect_apply"), "Aplicar", class = "btn btn-primary btn-sm"),
          disabled = if (!isTRUE(rv$clip_active)) "disabled" else NULL
        ),
        tags$hr(style = "margin:12px 0;"),
        tags$h5(style = "margin:0 0 8px 0;", paste0("Keyframes do Retângulo #", as.integer(rid))),
        DT::DTOutput(ns("dynrectKfTable")),
        div(
          style = "display:flex; gap:8px; margin-top:8px; flex-wrap:wrap;",
          actionButton(ns("dynrectKf_delete"), "Excluir selecionado(s)", class = "btn btn-outline-danger btn-sm"),
          actionButton(ns("dynrectKf_clear"), "Limpar tudo", class = "btn btn-outline-secondary btn-sm")
        ),
        tags$div(
          style = "margin-top:6px; color:#777; font-size:12px;",
          "Selecione um ou mais keyframes na tabela para excluir."
        )
      )
    )
  )
})

  # ==========================================================
  # NOVO: Tabela de keyframes do retângulo selecionado (por estrutura)
  # - excluir keyframe(s)
  # - limpar todos
  # ==========================================================
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
        data.frame(`Tempo (UTC)` = character(0), `Câmera` = integer(0)),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }

    kf <- tr |>
      dplyr::filter(as.integer(rect_id) == as.integer(rid)) |>
      dplyr::arrange(as.POSIXct(ts_utc, tz = "UTC"))

    if (!nrow(kf)) {
      return(DT::datatable(
        data.frame(`Tempo (UTC)` = character(0), `Câmera` = integer(0)),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }

    df <- kf |>
      dplyr::mutate(
        ts_raw = fmt_ts_utc_iso(as.POSIXct(ts_utc, tz = "UTC"), digits = 3L),
        `Tempo (UTC)` = fmt_ts_utc_br(as.POSIXct(ts_utc, tz = "UTC"), digits = 3L),
        `Câmera` = as.integer(cam_id),
        `Atributos` = if ("attrs" %in% names(kf)) vapply(attrs, function(a) {
          jsonlite::toJSON(if (is.null(a)) list() else a, auto_unbox = TRUE, null = "null")
        }, character(1)) else ""
      ) |>
      dplyr::select(ts_raw, `Tempo (UTC)`, `Câmera`, `Atributos`)

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

    showNotification(
      paste0("Excluído(s) ", length(ts_del), " keyframe(s) do Retângulo #", as.integer(rid), "."),
      type = "message"
    )
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$dynrectKf_clear, {
    req(!is.null(objeto), isTRUE(objeto$cd_id_objeto_tipo == 2L))

    rid <- dyn$selected_rect_id
    if (is.null(rid) || !is.finite(rid)) return(invisible())

    tr <- dyn$tracks
    if (is.null(tr) || !nrow(tr)) return(invisible())

    dyn$tracks <- tr |>
      dplyr::filter(as.integer(rect_id) != as.integer(rid))

    showNotification(
      paste0("Todos os keyframes do Retângulo #", as.integer(rid), " foram removidos."),
      type = "message"
    )
  }, ignoreInit = TRUE))


  obs$add(observeEvent(input$dynrect_apply, {
    req(!is.null(objeto), isTRUE(objeto$cd_id_objeto_tipo == 2L))

    struct_df <- dynrect_structures_df(objeto)

    ok_meta <- dyn_apply_meta(struct_df)
    if (!isTRUE(ok_meta)) {
      # ✅ se não aplicou meta, nem tenta salvar keyframe
      return(invisible())
    }

    ok_trk <- dyn_apply_tracking(rv)

    if (isTRUE(ok_trk)) {
      showNotification("Tracking aplicado e salvo (posição + atributos).", type = "message")
    } else {
      showNotification("Tracking não foi salvo — verifique Clip ativo / retângulo selecionado.", type = "warning")
    }
  }, ignoreInit = TRUE))

  # ==========================================================
  # Render da tabela de clips
  # ==========================================================
  output$clipsTable <- DT::renderDT({

    df <- clips()
    if (!nrow(df)) {
      return(DT::datatable(
        data.frame(
          Linha  = integer(0),
          Título = character(0),
          Tipo   = character(0),
          De = character(0),
          Ate = character(0),
          Visualizar = character(0),
          Excluir = character(0)
        ),
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
    tiposPacote <- tiposPacotes$NAME_TIPO_PACOTE

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

    btn_view <- vapply(df$id, function(id) {
      sprintf(
        "<button class='btn btn-sm btn-outline-primary' onclick=\"Shiny.setInputValue('%s', {action:'view', id:%d, nonce:Math.random()}, {priority:'event'})\">
         <i class='fa fa-eye'></i>
       </button>", ns("clip_action"), id
      )
    }, character(1))

    btn_del <- vapply(df$id, function(id) {
      sprintf(
        "<button class='btn btn-sm btn-danger' onclick=\"Shiny.setInputValue('%s', {action:'del', id:%d, nonce:Math.random()}, {priority:'event'})\">
         <i class='fa fa-trash'></i>
       </button>", ns("clip_action"), id
      )
    }, character(1))

    out <- data.frame(
      Linha          = rownum,
      Título         = titulo,
      Tipo           = tipo_cb,
      De             = t0_txt,
      Ate            = t1_txt,
      Visualizar     = btn_view,
      Excluir        = btn_del,
      stringsAsFactors = FALSE
    )

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
          list(width = "75px", targets = c(1, 6, 7))
        )
      ),
      callback = DT::JS(
        "table.on('click keydown', 'input, textarea, select', function(e){ e.stopPropagation(); });"
      )
    ) |>
      DT::formatStyle(names(out), cursor = "pointer")
  })

  # ---------- Visualizar / Excluir clip ----------
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

      if (!is.null(rv$seq) && nrow(rv$seq) > 0) {
        i0 <- max(1L, min(nrow(rv$seq), row$i0[1]))
        i1 <- max(1L, min(nrow(rv$seq), row$i1[1]))
        rng <- sort(c(i0, i1))
        sub_seq <- rv$seq[seq.int(rng[1], rng[2]), , drop = FALSE]
      } else {
        sub_seq <- NULL
      }

      clip_summary_overlay(
        ns, session, input, objeto, id_clip = id_sel,
        ts_start = row$t0, ts_end = row$t1,
        n_frames = n_frames
      )

      clipOverlayPlayer$seq     <- sub_seq
      clipOverlayPlayer$i       <- 1L
      clipOverlayPlayer$dir     <- +1L
      clipOverlayPlayer$playing <- FALSE
      render_clip_overlay_frame()

    } else if (act == "del") {
      clips_remove(id_sel)
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
      clipOverlayPlayer$playing <- FALSE
      clipOverlayPlayer$seq     <- NULL
    }
  }, ignoreInit = TRUE))

  # ---------- Controles do player dentro do modal ----------
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
      clipOverlayPlayer$i <- max(1L, clipOverlayPlayer$i - 1L)
      render_clip_overlay_frame()

    } else if (act == "next") {
      clipOverlayPlayer$playing <- FALSE
      clipOverlayPlayer$i <- min(nrow(clipOverlayPlayer$seq), clipOverlayPlayer$i + 1L)
      render_clip_overlay_frame()
    }
  }, ignoreInit = TRUE))

  # ---------- Rodapé ----------
  obs$add(observeEvent(input$btSair, {
    obs$destroy()
    output$uiCamerasFrames <- NULL
    playing(FALSE); loop_on <<- FALSE
    session$userData$lru_cache$clear()
    dyn_reset()

    if (isTRUE(rv$clip_active)) {
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }

    clipOverlayPlayer$playing <- FALSE
    clipOverlayPlayer$seq     <- NULL

    removeModal(session); callback()
  }, ignoreInit = TRUE, ignoreNULL = TRUE))

  # ---------- Salvar ----------
  obs$add(observeEvent(input$btSalvar, {

    df <- isolate(clips())
    if (!nrow(df)) {
      showNotification("Não foi possivel salvar o pacote de treino, nenhum clip foi encontrado!", type = "error")
      return(invisible())
    }

    dyn_rects_snapshot <- isolate(list(rects = dyn$rects, tracks = dyn$tracks))

    if (!db$tryTransaction(function(conn) {

      info <- build_objeto_descricao(input, df, objeto, tiposPacotes, dyn_rects = dyn_rects_snapshot)
 
      if (!info$status) {
        showNotification(info$message, type = "error")
        return(invisible(NULL))
      }

      for (i in seq_along(info$datas)) {
        descricao <- info$datas[[i]]
        objPacote <- list()
        objPacote$cd_id_objeto       <- objeto$cd_id_objeto
        objPacote$titulo_ia          <- descricao$titulo
        objPacote$input_ia           <- descricao$input
        objPacote$cd_id_tipo_pacote  <- descricao$tipoPacote
        objPacote$output_ia          <- descricao$output
        objPacote$dt_hr_local_begin  <- descricao$begin
        objPacote$dt_hr_local_end    <- descricao$end
        db$insertTable(conn, "pacote_ia", objPacote)
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
          stringsAsFactors = FALSE
        ))
        next_clip_id(1L)

        clipOverlayPlayer$playing <- FALSE
        clipOverlayPlayer$seq     <- NULL

        if (!status) {
          obs$destroy()
          output$uiCamerasFrames <- NULL
          removeModal(session); callback()
        }
      }, ignoreInit = TRUE, once = TRUE)

    })) {
      showNotification("Não foi possivel salvar o pacote de treino, durante o processo houve falha!", type = "error")
    }

  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$clipCloseVideo, {
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

      # fallback: se não tiver box salvo, calcula do poly
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
    tipoPacote   <- tiposPacotes |> dplyr::filter(NAME_TIPO_PACOTE == input[[paste0("clip_tipo_", id)]])

    if (!validaDateFormat(t0) || !validaDateFormat(t1)) {
      status  <- FALSE
      message <- "❌ formato inválido na tabela clips (use dd/mm/aa HH:MM:SS)!"
      break
    }

    df_tmp <- clips_update_date_time_t0(df_tmp, id, t0)
    df_tmp <- clips_update_date_time_t1(df_tmp, id, t1)
    
    atributos    <- NULL
    output_parts <- NULL
    if(objeto$cd_id_objeto_tipo == 1L){ # tipo de objeto estatico
      atributos <- collect_clip_attributes(input, objeto, id, df_tmp$t0, df_tmp$t1)
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
      # JSON base (componentes)
      output_parts <- atributos$output
      output_parts <- output_parts[!is.na(output_parts) & nzchar(output_parts)]
      
    }
   
    # ✅ NOVO: adiciona retângulos dinâmicos no intervalo do clip (se objeto dinâmico)
    if (isTRUE(objeto$cd_id_objeto_tipo == 2L) && is.list(dyn_rects) &&
        !is.null(dyn_rects$rects) && !is.null(dyn_rects$tracks)) {
      dyn_json <- dynrects_to_json(dyn_rects$rects, dyn_rects$tracks, df_tmp$t0, df_tmp$t1)
      if (!is.null(dyn_json) && nzchar(dyn_json)) {
        output_parts <- c(output_parts, paste0("{\"DYN_RECTS\": ", dyn_json, "}"))
      }
    }

    output_ <- paste0("[", paste0(output_parts, collapse = ","), "]")
    input_  <- paste0("OBJETO: ", objeto$name_objeto, " TIPO: ", objeto$name_objeto_TIPO)

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
