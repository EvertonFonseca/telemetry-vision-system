# ============================================
# Novo Treinamento (refatorado e organizado)
# - Player com later::later (sem reentrância)
# - Cache LRU + Prefetch por lote
# - Múltiplos leafletOutput por câmera
# - Handler JS 'set_frame_to' (id alvo)
# - Timezone consistente (UTC) p/ chaves/queries
# - Clip com duração máxima (default 5 min)
# ============================================

box::use(
  shiny[...],
  shinyjs[inlineCSS, delay],
  stringi,
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
  purrr[map, map_df],
  utils[...]
)

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

# ==================================================
# LRU Cache (env + funções puras)
# ==================================================
new_lru_cache <- function(max_items = MAX_CACHE_ITEMS) {
  cache <- new.env(parent = emptyenv())
  order <- character(0)
  list(
    get = function(k) {
      if (!exists(k, envir = cache, inherits = FALSE)) return(NULL)
      idx <- which(order == k); if (length(idx)) order <<- c(order[-idx], k)
      get(k, envir = cache, inherits = FALSE)
    },
    put = function(k, val) {
      assign(k, val, envir = cache)
      idx <- which(order == k); if (length(idx)) order <<- order[-idx]
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
# DB fetchers (frame único e lote para prefetch)
# ==================================================
db_fetch_frame_raw <- function(pool, camera_id, ts_utc) {
  DBI::dbGetQuery(
    pool,
    "SELECT DATA_FRAME FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL = ? LIMIT 1",
    params = list(as.integer(camera_id), as.POSIXct(ts_utc, tz = "UTC"))
  )
}
db_fetch_many_frames <- function(pool, camera_id, ts_vec_utc) {
  placeholders <- paste(rep("?", length(ts_vec_utc)), collapse = ",")
  sql <- paste0(
    "SELECT DT_HR_LOCAL, DATA_FRAME
       FROM FRAME_CAMERA
      WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL IN (", placeholders, ")"
  )
  DBI::dbGetQuery(pool, sql, params = c(list(as.integer(camera_id)), as.list(as.POSIXct(ts_vec_utc, tz = "UTC"))))
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
    if (!nrow(res) || is.null(res$DATA_FRAME[[1]])) return(NULL)
    raw <- res$DATA_FRAME[[1]]
    uri <- to_data_url(raw)
    env$cache$put(k, uri)
    uri
  }

  env$render_current <- function(seq_df, i, w, h, id_map, fit_bounds = FALSE) {
    if (is.null(seq_df) || !nrow(seq_df)) return(list(ok = FALSE, w = w, h = h))
    i <- max(1L, min(nrow(seq_df), as.integer(i)))
    cur <- seq_df[i, ]
    uri <- env$fetch_dataurl_single(cur$CD_ID_CAMERA, cur$DT_HR_LOCAL)
    if (is.null(uri)) return(list(ok = FALSE, w = w, h = h))
    dom_id <- id_map[[as.character(cur$CD_ID_CAMERA)]]
    if (is.null(dom_id) || is.na(dom_id)) return(list(ok = FALSE, w = w, h = h))

    env$session$sendCustomMessage("set_frame_to", list(
      id = dom_id, url = uri, w = as.integer(w), h = as.integer(h), fit = isTRUE(fit_bounds)
    ))
    list(ok = TRUE, w = w, h = h)
  }

  env$prefetch_ahead_batch <- function(seq_df, i, N = PREFETCH_AHEAD) {
    if (is.null(seq_df) || !nrow(seq_df)) return(invisible(FALSE))
    n <- nrow(seq_df); i <- as.integer(i)
    if (i >= n) return(invisible(FALSE))
    tgt_idx <- seq.int(i + 1L, min(n, i + N))
    if (!length(tgt_idx)) return(invisible(FALSE))

    seg <- seq_df[tgt_idx, , drop = FALSE]
    seg$k <- mapply(key_of, seg$CD_ID_CAMERA, seg$DT_HR_LOCAL)
    seg <- seg[!vapply(seg$k, function(z) !is.null(env$cache$get(z)), logical(1L)), , drop = FALSE]
    if (!nrow(seg)) return(invisible(TRUE))

    # Busca em lote por câmera
    groups <- split(seg, seg$CD_ID_CAMERA)
    for (cam_str in names(groups)) {
      g      <- groups[[cam_str]]
      cam    <- as.integer(cam_str)
      ts_vec <- as.POSIXct(g$DT_HR_LOCAL, tz = "UTC")

      res <- tryCatch(db_fetch_many_frames(env$pool, cam, ts_vec), error = function(e) NULL)
      if (!is.null(res) && nrow(res)) {
        res$DT_HR_LOCAL <- as.POSIXct(res$DT_HR_LOCAL, tz = "UTC")
        idx_map <- match(ts_vec, res$DT_HR_LOCAL)
        for (j in seq_along(ts_vec)) {
          if (!is.na(idx_map[j])) {
            raw <- res$DATA_FRAME[[idx_map[j]]]
            if (!is.null(raw)) env$cache$put(key_of(cam, ts_vec[j]), to_data_url(raw))
          }
        }
      }

      # Falhas de cache: fallback single
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
# Clip Manager (puro: funções acessadas pelo módulo)
# ==================================================
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

clip_summary_overlay <- function(ns, session, objeto, ts_start, ts_end, n_frames,
                                 tz_local = Sys.timezone()) {
  
  if (is.null(ts_start) || is.null(ts_end)) return(invisible())

  dur_sec   <- as.numeric(difftime(ts_end, ts_start, units = "secs"))
  dur_txt   <- sprintf("%02d:%02d", floor(dur_sec / 60), round(dur_sec %% 60))
  start_utc <- fmt_pt(ts_start, "UTC"); end_utc <- fmt_pt(ts_end, "UTC")
  start_loc <- fmt_pt(ts_start, tz_local); end_loc <- fmt_pt(ts_end, tz_local)

  codigo_date <- paste0(as.integer(ts_start),"_",as.integer(ts_end),"_")
  overlay_id <- ns("clip_summary_overlay")
  parent_sel <- paste0("#parent", ns("dialogObj"), " .modal-content")
  try(removeUI(selector = paste0("#", overlay_id), multiple = TRUE, immediate = TRUE), silent = TRUE)

  # Atributos por componente (replica seu construtor)
  componentes <- objeto$CONFIG[[1]]$COMPONENTES[[1]]
  divLista    <- fluidRow()
  for (i in seq_len(nrow(componentes))) {
    comp          <- componentes[i,]
    estrutura     <- comp$ESTRUTURA[[1]]
    atributos     <- estrutura$CONFIGS[[1]]$ATRIBUTOS[[1]]
    listAtributos <- tagList()
    for (k in seq_len(nrow(atributos))) {
      atributo  <- atributos[k,]
      id_html   <- ns(paste0(codigo_date,comp$NAME_COMPONENTE, "_", atributo$NAME_ATRIBUTO, "_", k))
      if (atributo$NAME_DATA == "QUALITATIVE") {
        classes <- stringr::str_split(atributo$CLASSE_ATRIBUTO, ",")[[1]]
        listAtributos <- tagAppendChildren(
          listAtributos,
          selectizeInput(id_html, label = atributo$NAME_ATRIBUTO, choices = classes,
                         options = list(dropdownParent = 'body', openOnFocus = TRUE, closeAfterSelect = TRUE))
        )
      } else {
        listAtributos <- tagAppendChildren(listAtributos,numericInput(id_html, label = atributo$NAME_ATRIBUTO))
      }
    }
    divLista <- tagAppendChildren(
      divLista,
      panelTitle(
        title = comp$NAME_COMPONENTE,
        background.color.title = "white",
        title.color  = "black",
        border.color = "lightgray",
        children = div(style = "padding: 10px;", listAtributos)
      ),
      br()
    )
  }

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
          "background:#fff; border-radius:10px; width:min(560px,92%);",
          "height:90vh; min-height:350px; box-shadow:0 12px 30px rgba(0,0,0,.25);",
          "display:flex; flex-direction:column;"
        ),
        div(
          style = "padding:16px 18px; border-bottom:1px solid #eee; flex:0 0 auto;",
          tags$h4("Comportamento do objeto", style = "margin:0;")
        ),
        div(
          style = paste("padding:10px;", "flex:1 1 auto;", "min-height:0;", "overflow-y:auto; overflow-x:hidden;"),
          panelTitle(
            title = "Atributos",
            background.color.title = "white",
            title.color  = "black",
            border.color = "lightgray",
            children = div(style = "padding: 20px;", divLista)
          ),
          # br(),
          # panelTitle(
          #   title = "Descrição",
          #   background.color.title = "white",
          #   title.color  = "black",
          #   border.color = "lightgray",
          #   children = div(
          #     style = "padding: 10px;",
          #     tags$p(tags$b("Janela do clip (UTC):")),
          #     tags$p(sprintf("De: %s  —  Até: %s", start_utc, end_utc)),
          #     tags$hr(),
          #     tags$p(tags$b(sprintf("Duração: %s (%.1f s)", dur_txt, dur_sec))),
          #     tags$p(tags$b(sprintf("Frames no intervalo: %s", ifelse(is.na(n_frames), "-", as.character(n_frames))))),
          #     tags$hr(),
          #     tags$p(tags$b("Conversão p/ fuso local")),
          #     tags$p(sprintf("Início: %s  —  Fim: %s  (%s)", start_loc, end_loc, tz_local))
          #   )
          # )
        ),
        div(
          style = "padding:12px 18px; border-top:1px solid #eee; text-align:right; flex:0 0 auto;",
          actionButton(ns("clipCloseVideo"), "OK", class = "btn btn-primary btn-sm")
        )
      )
    )
  )
}

# ==================================================
# UI Builders (JS handlers, Header form, Lista de câmeras, Controles)
# ==================================================
uiClipsPanel <- function(ns){
  panelTitle(
    title = "Clips",
    background.color.title = "white",
    title.color  = "black",
    border.color = "lightgray",
    children = div(
      style = "padding: 10px;",
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
    })();
  "))
}

uiMain <- function(ns, setores) {
  div(
    ui_js_handlers(),
    inlineCSS(paste0("#", ns("textNameTreino"), " {text-transform: uppercase;}")),
    panelTitle(
      title = "Configuração",
      background.color.title = 'white',
      title.color  = 'black',
      border.color = 'lightgray',
      children = fluidRow(
        style = 'padding-top: 10px; padding-left: 15px; padding-right: 15px;',
        column(3, selectizeInput(ns('comboSetor'),  label = 'Setor',  choices = setores$NAME_SETOR)),
        column(3, selectizeInput(ns('comboObjeto'), label = 'Objeto', choices = NA)),
        column(6, splitLayout(
          cellWidths = c("45%", "45%", "10%"),
          airDatepickerInput(
            inputId = ns("datetimeBegin"), label = "Data e Hora De:",
            language = "pt-BR", timepicker = TRUE, dateFormat = "dd/MM/yyyy",
            timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 5),
            width ="98%", placeholder = "Escolha uma data e hora"
          ),
          airDatepickerInput(
            inputId = ns("datetimeEnd"), label = "Data e Hora Até:",
            language = "pt-BR", timepicker = TRUE, dateFormat = "dd/MM/yyyy",
            timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 5),
            width ="98%", placeholder = "Escolha uma data e hora"
          ),
          actionButton(ns('btBuscar'), label = '', icon = icon("search"),
                       style = "margin-top: 25px; margin-left: -5px;")
        ))
      )
    ),
    br(),
    uiOutput(ns('uiCamerasFrames'))
  )
}

uiCamerasComponentes <- function(ns, input, output, componentes) {
  cameras  <- purrr::map_df(componentes$CAMERA, ~ .x) |>
              dplyr::distinct(CD_ID_CAMERA, NAME_CAMERA)

  id_by_cam <- list()
  divLista  <- fluidRow()
  n_cam     <- nrow(cameras)

  for (i in seq_len(n_cam)) {
    local({
      cam_id       <- cameras$CD_ID_CAMERA[i]
      comps_by_cam <- componentes[componentes$CD_ID_CAMERA == cam_id,]

      cam_name <- cameras$NAME_CAMERA[i]
      out_id   <- paste0("map_", cam_id)
      dom_id   <- ns(out_id)
      id_by_cam[[as.character(cam_id)]] <<- dom_id

      output[[out_id]] <- renderLeaflet({
        map_cam <- leaflet(options = leafletOptions(
          crs = leafletCRS(crsClass = "L.CRS.Simple"),
          zoomSnap  = 0, zoomDelta = 0.25
        ))
        for (k in seq_len(nrow(comps_by_cam))) {
          comp      <- comps_by_cam[k,]
          poligno   <- comp$POLIGNO_COMPONENTE[[1]]
          estrutura <- comp$ESTRUTURA[[1]]
          label <- HTML(paste0("<strong>COMPONENTE:</strong> ", comp$NAME_COMPONENTE,
                               "<br><strong>ESTRUTURA:</strong> ", estrutura$NAME_ESTRUTURA))
          map_cam <- map_cam |>
            addPolygons(
              group = "comp",
              lng = poligno$x, lat = poligno$y,
              layerId = comp$CD_ID_COMPONENTE, weight = 2, fillOpacity = 0.1, label = label
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
      divLista <<- tagAppendChildren(divLista, column(col_w, cameraElement))
    })
  }
  list(ui = divLista, id_by_cam = id_by_cam)
}

uiComponenteVideo <- function(ns){
  splitLayout(
    cellWidths = c("auto","auto","auto","auto","auto","110px","auto","120px","auto"),
    actionButton(ns("backPlay"),  label = "Reverse", icon = icon("backward"),
                 class = "btn btn-outline-secondary", title = "Reproduzir para trás"),
    actionButton(ns("prevFrame"), label = "Prev",    icon = icon("step-backward"),
                 class = "btn btn-outline-secondary", title = "Frame anterior"),
    actionButton(ns("play"),      label = "Play",    icon = icon("play"),
                 class = "btn btn-outline-secondary", title = "Reproduzir"),
    actionButton(ns("pause"),     label = "Pause",   icon = icon("pause"),
                 class = "btn btn-outline-secondary", title = "Pausar"),
    actionButton(ns("nextFrame"), label = "Next",    icon = icon("step-forward"),
                 class = "btn btn-outline-secondary", title = "Próximo frame"),
    numericInput(ns("step_ms"),"Intervalo (ms)", value = 50, min = 1, step = 1, width = "110px") |>
      tagAppendAttributes(style = ';margin-top: -25px;'),
    actionButton(ns("clipToggle"), label = "Start Clip", icon = icon("scissors"),
                 class = "btn btn-outline-primary", title = "Iniciar/encerrar clip (máx. duração)"),
    numericInput(ns("clip_max_min"), "Máx (min)", value = 5, min = 1, max = 60, step = 1, width = "120px") |>
      tagAppendAttributes(style = ';margin-top: -25px;'),
    textOutput(ns("titleClock"))
  )
}

# ==================================================
# Helpers públicos/compartilhados
# ==================================================
rv_get_current_ts <- function(rv) {
  isolate({
    if (is.null(rv$seq) || !nrow(rv$seq)) return(NULL)
    i  <- max(1L, min(nrow(rv$seq), rv$i))
    rv$seq$DT_HR_LOCAL[i]  # UTC no pipeline
  })
}
get_current_frame_ts <- function(rv, tz = "UTC", fmt = NULL) {
  ts <- rv_get_current_ts(rv)
  if (is.null(fmt)) return(ts)
  format(ts, tz = tz, usetz = FALSE, format = fmt)
}

# ==================================================
# Módulo principal (server) — “magro”
# ==================================================
#' @export
uiNewTreinar <- function(ns, input, output, session, callback){

  clockupdate <- reactiveVal()
  obs         <- newObserve()
  setores     <- selectAllSetors(dbp$get_pool())
  objetos     <- selectAllObjetos(dbp$get_pool())
  objeto      <- NULL

  # ---------- Estado ----------
  rv <- reactiveValues(
    seq = NULL, i = 1L, w = 512L, h = 512L,
    id_by_cam = NULL,
    clip_active = FALSE, clip_t0 = NULL, clip_i0 = NA_integer_
  )
  # ---------- Store de Clips (por sessão) ----------
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
  clips_observed_titles <- reactiveVal(integer(0))  # ids já “observados” p/ título
  
  clips_add <- function(t0, t1, i0, i1, title = ""){
    df <- clips()
    id <- next_clip_id()
    n  <- nrow(df)
    row <- data.frame(
      id = id,
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
  
  clips_remove <- function(id){
    df <- clips()
    if (!nrow(df)) return(invisible())
    clips(df[df$id != id, , drop = FALSE])
  }
  
  clips_update_title <- function(id, title){
    df <- clips()
    if (!nrow(df)) return(invisible())
    idx <- which(df$id == id)
    if (length(idx)) { df$title[idx] <- title; clips(df) }
  }

  playing <- reactiveVal(FALSE)
  loop_on <- FALSE
  play_dir <- reactiveVal(+1L)

  # ---------- Cache + PlayerContext (escopo de sessão) ----------
  if (is.null(session$userData$lru_cache))   session$userData$lru_cache   <- new_lru_cache(MAX_CACHE_ITEMS)
  if (is.null(session$userData$player_ctx))  session$userData$player_ctx  <- new_player_ctx(session, dbp$get_pool(), session$userData$lru_cache)
  ctx <- session$userData$player_ctx

  # ---------- Modal/UI ----------
  id       <- ns('dialogObj')
  cssStyle <- list()
  cssStyle[[paste0(' #parent',id,' .modal-dialog')]]  <- 'width: 95% !important; height: 90% !important;'
  cssStyle[[paste0(' #parent',id,' .modal-content')]] <- 'width: 100% !important; height: 100% !important;'
  cssStyle[[paste0(' #parent',id,' .modal-body')]]    <- 'width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto; overflow-x: hidden;'

  showModal(
    session = session,
    div(
      id = paste0('parent', id),
      style = "height: 80%; overflow: hidden;",
      inlineCSS(cssStyle),
      dialogModal(
        title = "Novo treinamento",
        size  = 'm',
        uiMain(ns, setores),
        footer = uiOutput(ns('uiFooter'))
      )
    )
  )
  output$uiFooter <- renderUI(tagList(
    actionButton(ns("btSair"),  label = "Voltar",    icon = icon("arrow-left")),
    actionButton(ns("btSalvar"),class = "btn-warning", label = "Atualizar", icon = icon("save"))
  ))

  # ---------- Reagir troca de setor -> objetos ----------
  obs$add(observeEvent(input$comboSetor, {
    setor <- setores |> dplyr::filter(NAME_SETOR == input$comboSetor)
    if (!nrow(setor)) return()
    objs_setor <- objetos |> dplyr::filter(CD_ID_SETOR == setor$CD_ID_SETOR)
    updateSelectizeInput(session, "comboObjeto", choices = objs_setor$NAME_OBJETO)

    output$uiCamerasFrames <- NULL
    playing(FALSE); loop_on <<- FALSE
    session$userData$lru_cache$clear()
    if (isTRUE(rv$clip_active)) {
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }
  }, ignoreNULL = TRUE))

  # ---------- Botão Buscar ----------
  obs$add(observeEvent(input$btBuscar, {
    actionWebUser({
      objeto <<- objetos |> dplyr::filter(NAME_OBJETO == isolate(input$comboObjeto))
      if (!nrow(objeto)) {
        showNotification("Selecione um objeto válido.", type = "warning")
        removeProgressLoader()
        return(invisible())
      }
      time_begin  <- as.POSIXct(isolate(input$datetimeBegin), tz = "UTC")
      time_end    <- as.POSIXct(isolate(input$datetimeEnd),   tz = "UTC")
      componentes <- objeto$CONFIG[[1]]$COMPONENTES[[1]]
      cameras_ids <- unique(purrr::map_int(componentes$CAMERA, "CD_ID_CAMERA"))

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

      res_cam <- uiCamerasComponentes(ns, input, output, componentes)
      rv$id_by_cam <- res_cam$id_by_cam
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
      res1  <- db_fetch_frame_raw(dbp$get_pool(), first$CD_ID_CAMERA, first$DT_HR_LOCAL)
      if (nrow(res1) && !is.null(res1$DATA_FRAME[[1]])) {
        wh <- img_dims(res1$DATA_FRAME[[1]])
        rv$w <- if (is.finite(wh[1])) as.integer(wh[1]) else 512L
        rv$h <- if (is.finite(wh[2])) as.integer(wh[2]) else 350L
      }

      # snapshot p/ render inicial (fora do reativo)
      id_map <- rv$id_by_cam; seq_df <- rv$seq; i0 <- rv$i; w0 <- rv$w; h0 <- rv$h
      session$onFlushed(function(){
        session$sendCustomMessage("reset_overlays", unname(id_map))
        st <- ctx$render_current(seq_df = seq_df, i = i0, w = w0, h = h0, id_map = id_map, fit_bounds = TRUE)
        isolate({ if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h } })
        ctx$prefetch_ahead_batch(seq_df = seq_df, i = i0, N = PREFETCH_AHEAD)
      }, once = TRUE)

      removeProgressLoader(callback = function(){step_frame(+1L)})
    }, auto.remove = FALSE)
  }, ignoreInit = TRUE))

  # ---------- Navegação por frame ----------
  step_frame <- function(delta) {
    req(rv$seq, nrow(rv$seq) > 0)
    new_i <- max(1L, min(nrow(rv$seq), rv$i + delta))
    if (new_i == rv$i) return(invisible())
    rv$i <- new_i
    st <- ctx$render_current(rv$seq, rv$i, rv$w, rv$h, rv$id_by_cam, fit_bounds = FALSE)
    if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }
    ctx$prefetch_ahead_batch(rv$seq, rv$i, PREFETCH_AHEAD)
    # clip limite
    if (clip_limit_exceeded(rv, suppressWarnings(as.numeric(input$clip_max_min)))) {
      # encerra clip
      playing(FALSE); loop_on <<- FALSE
      ts_start <- rv$clip_t0; i0 <- rv$clip_i0
      ts_end   <- rv_get_current_ts(rv); i1 <- rv$i
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
      clips_add(ts_start, ts_end, i0, i1)
      clip_summary_overlay(ns, session, objeto, ts_start, ts_end, n_frames = abs(i1 - i0) + 1L)
    }
  }
  obs$add(observeEvent(input$prevFrame, { step_frame(-1L) }, ignoreInit = TRUE))
  obs$add(observeEvent(input$nextFrame, { step_frame(+1L) }, ignoreInit = TRUE))

  # ---------- Loop de Play (later) ----------
  play_step <- function() {
    withReactiveDomain(session, {
      isolate({
        if (!isTRUE(playing())) { loop_on <<- FALSE; return(invisible()) }
        if (is.null(rv$seq) || nrow(rv$seq) == 0L) { playing(FALSE); loop_on <<- FALSE; return(invisible()) }

        n   <- nrow(rv$seq)
        dir <- play_dir()
        if ((dir > 0L && rv$i >= n) || (dir < 0L && rv$i <= 1L)) {
          playing(FALSE); loop_on <<- FALSE; return(invisible())
        }

        rv$i <- rv$i + dir

        st <- ctx$render_current(rv$seq, rv$i, rv$w, rv$h, rv$id_by_cam, fit_bounds = FALSE)
        if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }
        ctx$prefetch_ahead_batch(rv$seq, rv$i, PREFETCH_AHEAD)

        clockupdate(Sys.time())
        # checa clip
        if (clip_limit_exceeded(rv, suppressWarnings(as.numeric(input$clip_max_min)))) {
          playing(FALSE); loop_on <<- FALSE
          ts_start <- rv$clip_t0; i0 <- rv$clip_i0
          ts_end   <- rv_get_current_ts(rv); i1 <- rv$i
          rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
          updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
          clips_add(ts_start, ts_end, i0, i1)
          clip_summary_overlay(ns, session, objeto, ts_start, ts_end, n_frames = abs(i1 - i0) + 1L)
          return(invisible())
        }

        delay <- suppressWarnings(as.numeric(input$step_ms) / 1000)
        if (!is.finite(delay) || delay <= 0) delay <- 0.001
        later::later(function(){ play_step() }, delay)
      })
    })
  }

  # ---------- Botões play/pause/reverse ----------
  obs$add(observeEvent(input$play, {
    play_dir(+1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  obs$add(observeEvent(input$backPlay, {
    play_dir(-1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  obs$add(observeEvent(input$pause, { playing(FALSE) }, ignoreInit = TRUE))
  
  # ---------- Clip toggle ----------
  obs$add(observeEvent(input$clipToggle, {
    if (!isTRUE(rv$clip_active)) {
      if (!clip_start(rv)) { showNotification("Nenhum frame carregado para iniciar o clip.", type = "warning"); return(invisible()) }
      updateActionButton(session, "clipToggle", label = "Stop Clip", icon = icon("square"))
      showNotification("Clip iniciado.", type = "message")
      play_dir(+1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
    } else {
      # Encerramento manual
      playing(FALSE); loop_on <<- FALSE
      ts_start <- rv$clip_t0; i0 <- rv$clip_i0
      ts_end   <- rv_get_current_ts(rv); i1 <- rv$i
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
      clips_add(ts_start, ts_end, i0, i1)
      clip_summary_overlay(ns, session, objeto, ts_start, ts_end, n_frames = abs(i1 - i0) + 1L)
    }
  }, ignoreInit = TRUE))
  
  # ---------- Render da tabela de clips ----------
  output$clipsTable <- DT::renderDT({
    df <- clips()
    if (!nrow(df)) {
      return(DT::datatable(
        data.frame(
          Linha  = integer(0),
          Título = character(0),
          `Data Hora De` = character(0),
          `Data Hora Até` = character(0),
          Visualizar = character(0),
          Excluir = character(0)
        ),
        escape = FALSE, selection = "none",
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      ))
    }
    
    # monta colunas com HTML (inputs e botões)
    rownum <- seq_len(nrow(df))
    titulo <- vapply(df$id, function(id){
      # inputId = "clip_title_<id>" (sem ns no server)
       as.character(
        textInput(
          inputId = paste0("clip_title_", id),
          label = NULL, value = df$title[df$id == id],
          width = "100%", placeholder = "Nome do clip"
        ) |> tagAppendAttributes(style = ';margin-top: 10px;')
      )
    }, character(1))
    
    btn_view <- vapply(df$id, function(id){
      sprintf(
        "<button class='btn btn-sm btn-outline-primary' onclick=\"Shiny.setInputValue('%s', {action:'view', id:%d, nonce:Math.random()}, {priority:'event'})\">
         <i class='fa fa-eye'></i>
       </button>", ns("clip_action"), id
      )
    }, character(1))
    
    btn_del <- vapply(df$id, function(id){
      sprintf(
        "<button class='btn btn-sm btn-danger' onclick=\"Shiny.setInputValue('%s', {action:'del', id:%d, nonce:Math.random()}, {priority:'event'})\">
         <i class='fa fa-trash'></i>
       </button>", ns("clip_action"), id
      )
    }, character(1))
    
    fmt <- function(x) format(x, tz = "UTC", format = "%d/%m/%y %H:%M:%S")
    
    out <- data.frame(
      `Linha`     = rownum,
      Título     = titulo,
      `Data Hora De` = fmt_pt(df$t0,Sys.timezone()),
      `Data Hora Até`= fmt_pt(df$t1,Sys.timezone()),
      Visualizar = btn_view,
      Excluir    = btn_del,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      out, escape = FALSE, selection = "none",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = 0),                 # oculta ID
          list(className = 'dt-center', targets = "_all"),
          list(width = '75px', targets = c(1, 5, 6))          # #, Visualizar, Excluir
          # Deixe Título (2) e Datas (3,4) sem width -> auto
          # Se quiser explicitar: list(width = 'auto', targets = c(2, 3, 4))
          )
      ) 
    ) |> DT$formatStyle(names(out), cursor = 'pointer')
  })
  
  # Observa cliques de "visualizar" e "excluir"
  obs$add(observeEvent(input$clip_action, {
    req(input$clip_action$id, input$clip_action$action)
    df <- clips(); if (!nrow(df)) return(invisible())
    id <- as.integer(input$clip_action$id)
    act <- as.character(input$clip_action$action)
    row <- df[df$id == id, , drop = FALSE]; if (!nrow(row)) return(invisible())
    
    if (act == "view") {
      n_frames <- abs(row$i1 - row$i0) + 1L
      # reabre o overlay
      clip_summary_overlay(ns, session, objeto, row$t0, row$t1, n_frames = n_frames)
    } else if (act == "del") {
      clips_remove(id)
      # remove overlay se estiver aberto (não sabemos qual estava aberto; é seguro tentar)
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }
  }, ignoreInit = TRUE))
  
  
  # ---------- Rodapé ----------
  obs$add(observeEvent(input$btSair, {
    obs$destroy()
    output$uiCamerasFrames <- NULL
    playing(FALSE); loop_on <<- FALSE
    session$userData$lru_cache$clear()
    if (isTRUE(rv$clip_active)) {
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }
    removeModal(session); callback()
  }, ignoreInit = TRUE, ignoreNULL = TRUE))

  obs$add(observeEvent(input$btSalvar, {
    showNotification("Atualizado!", type = "message")
    print(as.POSIXct(get_current_frame_ts(rv), tz = Sys.timezone()))
    
    df <- isolate(clips());
    debugLocal(function(x){ df })
    if (!nrow(df)) return(invisible())
   
    dados <- collect_clip_attributes(input,objeto,df$t0,df$t1)
 
  }, ignoreInit = TRUE))
  
  obs$add(observeEvent(input$clipCloseVideo, {
    try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
  }, ignoreInit = TRUE))
  
  # Cria observers para inputs de título recém-criados (evita duplicar)
  obs$add(observe({
    df <- clips(); if (!nrow(df)) return(invisible())
    already <- clips_observed_titles()
    new_ids <- setdiff(df$id, already)
    if (!length(new_ids)) return(invisible())
    
    for (id in new_ids) {
      local({
        clip_id <- id
        input_id <- paste0("clip_title_", clip_id)
        obs$add(observeEvent(input[[input_id]], {
          val <- input[[input_id]]
          clips_update_title(clip_id, if (is.null(val)) "" else as.character(val))
        }, ignoreInit = TRUE))
      })
    }
    clips_observed_titles(c(already, new_ids))
  }))
}

# ==================================================
# Coleta dos valores dos atributos no overlay de clip
# ==================================================
# Retorna um data.frame com:
# CD_ID_COMPONENTE, NAME_COMPONENTE, NAME_ATRIBUTO, NAME_DATA, VALUE
collect_clip_attributes <- function(input, objeto,t0,t1) {
  stopifnot(!is.null(objeto), nrow(objeto) >= 1)
  componentes <- objeto$CONFIG[[1]]$COMPONENTES[[1]]
  if (is.null(componentes) || !nrow(componentes)) {
    return(data.frame(
      CD_ID_COMPONENTE = integer(0),
      NAME_COMPONENTE  = character(0),
      NAME_ATRIBUTO    = character(0),
      NAME_DATA        = character(0),
      VALUE            = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # helper: normaliza pedaços para um id seguro (caso haja espaços/acentos)
  normalize_id_piece <- function(x) {
    x <- as.character(x)
    x <- gsub("\\s+", "_", x)             # espaços -> _
    x <- gsub("[^A-Za-z0-9_\\-]", "_", x) # demais -> _
    x
  }

 codigo_date <- paste0(as.integer(t0),"_",as.integer(t1),"_")

  # gera id completo exatamente como no builder (com namespace)
  make_ids <- function(comp_name, attr_name, k) {
    raw_id   <- paste0(codigo_date,comp_name, "_", attr_name, "_", k)
    norm_id  <- paste0(codigo_date,normalize_id_piece(comp_name), "_",normalize_id_piece(attr_name), "_", k)
    # com namespace (ns é uma função)
    list(
      raw  = raw_id,
      norm = norm_id
    )
  }

  # tenta ler um input considerando id "raw" e "norm"
  read_input <- function(id_candidates) {
    cand <- unlist(id_candidates, use.names = FALSE)
    for (cid in cand) {
      if (cid %in% names(input)) {
        return(input[[cid]])
      }
    }
    NA
  }

  rows <- list()

  for (i in seq_len(nrow(componentes))) {
    comp          <- componentes[i, ]
    estrutura     <- comp$ESTRUTURA[[1]]
    atributos     <- estrutura$CONFIGS[[1]]$ATRIBUTOS[[1]]
    if (is.null(atributos) || !nrow(atributos)) next

    comp_id   <- as.integer(comp$CD_ID_COMPONENTE[[1]])
    comp_name <- as.character(comp$NAME_COMPONENTE[[1]])

    for (k in seq_len(nrow(atributos))) {
      atributo   <- atributos[k, ]
      attr_name  <- as.character(atributo$NAME_ATRIBUTO[[1]])
      attr_type  <- as.character(atributo$NAME_DATA[[1]])      # "QUALITATIVE" | outro

      ids   <- make_ids(comp_name, attr_name, k)
      value <- read_input(ids)

      # normaliza saída: sempre como character, preservando NA
      if (isTRUE(is.numeric(value))) {
        value <- as.character(value)
      } else if (isTRUE(is.logical(value))) {
        value <- ifelse(is.na(value), NA_character_, ifelse(value, "TRUE", "FALSE"))
      } else {
        value <- if (is.null(value)) NA_character_ else as.character(value)
      }

      rows[[length(rows) + 1L]] <- data.frame(
        CD_ID_COMPONENTE = comp_id,
        NAME_COMPONENTE  = comp_name,
        NAME_ATRIBUTO    = attr_name,
        NAME_DATA        = attr_type,
        VALUE            = value,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) {
    return(data.frame(
      CD_ID_COMPONENTE = integer(0),
      NAME_COMPONENTE  = character(0),
      NAME_ATRIBUTO    = character(0),
      NAME_DATA        = character(0),
      VALUE            = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}
