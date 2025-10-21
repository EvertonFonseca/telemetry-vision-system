# ============================================
# Módulo: Novo Treinamento (Leaflet + Player multi-câmera)
# - Player com later::later (sem reentrância)
# - Cache LRU + Prefetch por lote
# - Um leafletOutput por câmera (map_<CD_ID_CAMERA>)
# - Handler JS 'set_frame_to' que recebe o id do widget alvo
# - Timezone consistente (UTC) p/ chave do cache e queries
# - Suporte a "Clip" com duração máxima (default 5 min)
# ============================================
box::use(
  shiny[...],
  shinyjs[inlineCSS,delay],
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
  shinyWidgets[airDatepickerInput,timepickerOptions],
  shinycssloaders,
  .. / logic/objeto_dao[...],
  .. / logic/camera_dao[...],
  .. / logic/setor_dao[...],
  .. / logic/treinar_dao[...],
  base64enc[...],
  dbp  = ../infra/db_pool,
  db   = ../infra/database,
  later,
  leaflet[...],
  purrr[map,map_df]
)

# ---------- Parâmetros de cache/prefetch ----------
PREFETCH_AHEAD   <- 16L   # quantos frames antecipar
MAX_CACHE_ITEMS  <- 400L  # limite LRU de itens (dataURLs) na sessão

# ==================================================
# Helpers: MIME / data URL / W×H do BLOB / IMG DIMS
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
# UI principal do modal (config + botões + área de câmeras)
# ==================================================
uiMain <- function(ns, setores, objetos, valueComboSetor = NULL, valueObjeto = NULL){
  div(
    # Handler JS: atualiza um widget Leaflet específico via id
    tags$script(HTML("
    (function(){
      const overlays = {};
      const boundsOf = {};
      const maps     = {}; // guarda a instância do mapa por id

      function fitWholeImage(map, w, h){
        const size  = map.getSize();
        const scaleX = size.x / w;
        const scaleY = size.y / h;
        const scale  = Math.min(scaleX, scaleY);
        let z = Math.log2(scale);
        if (!isFinite(z)) z = 0;
        const center = [h/2, w/2];
        map.setView(center, z, {animate:false});
        const b = [[0,0],[h,w]];
        map.setMaxBounds(b);
        map.options.maxBoundsViscosity = 1.0;
        map.setMinZoom(z);
        setTimeout(function(){ map.invalidateSize(); }, 0);
        return b;
      }

      // msg: { id, url, w, h, fit }
      Shiny.addCustomMessageHandler('set_frame_to', function(msg){
        const sel = '#' + msg.id;
        const widget = HTMLWidgets.find(sel);
        if (!widget) return;
        const map = widget.getMap();
        if (!map) return;

        const w = msg.w || 512;
        const h = msg.h || 512;
        const b = [[0,0],[h,w]];

        // RECRIA overlay se: não existe OU o mapa mudou
        const needRecreate = (!overlays[msg.id]) || (!maps[msg.id]) || (maps[msg.id] !== map);
        if (needRecreate) {
          try { if (overlays[msg.id]) overlays[msg.id].remove(); } catch(e){}
          overlays[msg.id] = L.imageOverlay(msg.url, b, {opacity:1}).addTo(map);
          maps[msg.id]     = map;
          boundsOf[msg.id] = fitWholeImage(map, w, h);
          return; // já atualizamos; próxima chamada só fará setUrl
        }

        // Se dimensões mudaram, ajusta bounds e (opcional) refit
        const changed = !boundsOf[msg.id] ||
                        boundsOf[msg.id][1][0] !== h ||
                        boundsOf[msg.id][1][1] !== w;
        if (changed){
          overlays[msg.id].setBounds(b);
          boundsOf[msg.id] = b;
          if (msg.fit) boundsOf[msg.id] = fitWholeImage(map, w, h);
        }
        overlays[msg.id].setUrl(msg.url);
      });

      // Reset seletivo do cache JS
      Shiny.addCustomMessageHandler('reset_overlays', function(ids){
        (ids || []).forEach(function(id){
          try { if (overlays[id]) overlays[id].remove(); } catch(e){}
          delete overlays[id];
          delete boundsOf[id];
          delete maps[id];
        });
      });
    })();
    ")),
    inlineCSS(paste0("#",ns("textNameTreino")," {text-transform: uppercase;}")),
    panelTitle(
      title = "Configuração",
      background.color.title = 'white',
      title.color  = 'black',
      border.color = 'lightgray',
      children = fluidRow(
        style = 'padding-top: 10px; padding-left: 15px; padding-right: 15px;',
        column(3, selectizeInput(ns('comboSetor'), label = 'Setor', choices = setores$NAME_SETOR, selected = valueComboSetor)),
        column(3, selectizeInput(ns('comboObjeto'), label = 'Objeto', choices = NA)),
        column(6, splitLayout(
          cellWidths = c("45%", "45%","10%"),
          airDatepickerInput(
            inputId = ns("datetimeBegin"),
            label = "Data e Hora De:",
            language = "pt-BR",
            timepicker = TRUE,
            dateFormat = "dd/MM/yyyy",
            timepickerOpts = timepickerOptions(
              hoursStep = 1,
              minutesStep = 5
            ),
            width ="98%",
            placeholder = "Escolha uma data e hora"
          ),
          airDatepickerInput(
            inputId = ns("datetimeEnd"),
            label = "Data e Hora Até:",
            language = "pt-BR",
            timepicker = TRUE,
            dateFormat = "dd/MM/yyyy",
            timepickerOpts = timepickerOptions(
              hoursStep = 1,
              minutesStep = 5
            ),
            width = "98%",
            placeholder = "Escolha uma data e hora"
          ),
          actionButton(ns('btBuscar'), label = '', icon = icon("search"), style = "margin-top: 25px; margin-left: -5px;")
        ))
      )
    ),
    br(),
    uiOutput(ns('uiCamerasFrames'))
  )
}

# ==================================================
# Gera UI das câmeras + retorna mapa cam_id -> DOM id
# ==================================================
uiCamerasComponentes <- function(ns, input, output, componentes){

  cameras  <- purrr::map_df(componentes$CAMERA, ~ .x) |>
              dplyr::distinct(CD_ID_CAMERA, NAME_CAMERA)

  id_by_cam <- list()   # precisa ser lista
  divLista  <- fluidRow()

  for (i in seq_len(nrow(cameras))) {
    local({
      cam_id       <- cameras$CD_ID_CAMERA[i]
      comps_by_cam <- componentes[which(componentes$CD_ID_CAMERA == cam_id),]
    
      cam_name  <- cameras$NAME_CAMERA[i]
      out_id    <- paste0("map_", cam_id)   # id interno (sem ns)
      dom_id    <- ns(out_id)               # id com namespace
      id_by_cam[[as.character(cam_id)]] <<- dom_id

      output[[out_id]] <- renderLeaflet({
        map_cam <- leaflet(options = leafletOptions(
          crs = leafletCRS(crsClass = "L.CRS.Simple"),
          zoomSnap  = 0,
          zoomDelta = 0.25
        )) 
        
        for(k in 1:nrow(comps_by_cam)){
          comp      <- comps_by_cam[k,]
          poligno   <- comp$POLIGNO_COMPONENTE[[1]]
          estrutura <- comp$ESTRUTURA[[1]]
          label <-  HTML(paste0("<strong>COMPONENTE:</strong> ", comp$NAME_COMPONENTE, "<br><strong>ESTRUTURA:</strong> ", estrutura$NAME_ESTRUTURA))
          
          map_cam <- map_cam |> addPolygons(
            group   = "comp",
            lng = poligno$x,
            lat = poligno$y,
            layerId = comp$CD_ID_COMPONENTE,
            weight  = 2,
            fillOpacity = 0.1,
            label = label
          )
        }
        map_cam
      })

      cameraElement <- panelTitle(
        title = paste0("Câmera: ", cam_name),
        background.color.title = "white",
        title.color  = "black",
        border.color = "lightgray",
        children = div(
          style = "padding: 10px;",
          leafletOutput(dom_id, height = "256px", width = "100%")
        )
      )
      divLista <<- tagAppendChildren(divLista,column(6,cameraElement))
    })
  }
  list(ui = divLista, id_by_cam = id_by_cam)
}

# ==================================================
# Contexto do player (render/prefetch compartilhado na sessão)
# ==================================================
new_player_ctx <- function(session, cache_get, cache_put, cache_trim, key_of, fetch_dataurl_single) {
  env <- new.env(parent = emptyenv())
  env$session       <- session
  env$cache_get     <- cache_get
  env$cache_put     <- cache_put
  env$cache_trim    <- cache_trim
  env$key_of        <- key_of
  env$fetch_single  <- fetch_dataurl_single

  env$render_current <- function(seq_df, i, w, h, id_map, fit_bounds = FALSE) {
    if (is.null(seq_df) || !nrow(seq_df)) return(list(w = w, h = h, ok = FALSE))
    i <- max(1L, min(nrow(seq_df), as.integer(i)))
    cur <- seq_df[i, ]
    uri <- env$fetch_single(cur$CD_ID_CAMERA, cur$DT_HR_LOCAL, update_wh_if_first = (i == 1L))
    if (is.null(uri)) return(list(w = w, h = h, ok = FALSE))

    dom_id <- id_map[[ as.character(cur$CD_ID_CAMERA) ]]
    if (is.null(dom_id) || is.na(dom_id)) return(list(w = w, h = h, ok = FALSE))

    env$session$sendCustomMessage("set_frame_to", list(
      id  = dom_id,
      url = uri,
      w   = as.integer(w),
      h   = as.integer(h),
      fit = isTRUE(fit_bounds)
    ))
    list(w = w, h = h, ok = TRUE)
  }

  env$prefetch_ahead_batch <- function(seq_df, i, N = PREFETCH_AHEAD) {
    if (is.null(seq_df) || !nrow(seq_df)) return(invisible(FALSE))
    n <- nrow(seq_df)
    i <- as.integer(i)
    if (i >= n) return(invisible(FALSE))

    tgt_idx <- seq.int(i + 1L, min(n, i + N))
    if (!length(tgt_idx)) return(invisible(FALSE))

    seg <- seq_df[tgt_idx, , drop = FALSE]
    seg$k <- mapply(env$key_of, seg$CD_ID_CAMERA, seg$DT_HR_LOCAL)
    seg <- seg[!vapply(seg$k, function(z) !is.null(env$cache_get(z)), logical(1L)), , drop = FALSE]
    if (!nrow(seg)) return(invisible(TRUE))

    groups <- split(seg, seg$CD_ID_CAMERA)
    for (cam_str in names(groups)) {
      g      <- groups[[cam_str]]
      cam    <- as.integer(cam_str)
      ts_vec <- as.POSIXct(g$DT_HR_LOCAL, tz = "UTC")

      placeholders <- paste(rep("?", length(ts_vec)), collapse = ",")
      sql <- paste0(
        "SELECT DT_HR_LOCAL, DATA_FRAME
           FROM FRAME_CAMERA
          WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL IN (", placeholders, ")"
      )

      res <- tryCatch(DBI::dbGetQuery(dbp$get_pool(), sql, params = c(list(cam), as.list(ts_vec))), error = function(e) NULL)

      if (!is.null(res) && nrow(res)) {
        res$DT_HR_LOCAL <- as.POSIXct(res$DT_HR_LOCAL, tz = "UTC")
        idx_map <- match(ts_vec, res$DT_HR_LOCAL)
        for (j in seq_along(ts_vec)) {
          if (!is.na(idx_map[j])) {
            raw <- res$DATA_FRAME[[ idx_map[j] ]]
            if (!is.null(raw)) {
              uri <- to_data_url(raw)
              env$cache_put(env$key_of(cam, ts_vec[j]), uri)
            }
          }
        }
      }

      for (j in seq_along(ts_vec)) {
        if (is.null(env$cache_get(env$key_of(cam, ts_vec[j])))) {
          invisible(env$fetch_single(cam, ts_vec[j], update_wh_if_first = FALSE))
        }
      }
    }

    env$cache_trim()
    invisible(TRUE)
  }

  env
}

# ==================================================
# MÓDULO: uiNewTreinar
# ==================================================
#' @export
uiNewTreinar <- function(ns, input, output, session, callback){

  obs       <- newObserve()
  setores   <- selectAllSetors(dbp$get_pool())
  objetos   <- selectAllObjetos(dbp$get_pool())
  objeto    <- NULL

  # ---------- Estado do player ----------
  rv <- reactiveValues(
    seq = NULL,        # data.frame: DT_HR_LOCAL, CD_ID_CAMERA
    i   = 1L,          # índice corrente
    w = 512L, h = 512L,# dimensões atuais do overlay
    id_by_cam = NULL,  # mapeamento cam_id (chr) -> DOM id (ns)
    # >>> CLIP:
    clip_active = FALSE,
    clip_t0     = NULL,
    clip_i0     = NA_integer_
  )
  playing <- reactiveVal(FALSE)  # controla play/pause
  loop_on <- FALSE               # flag interna p/ evitar múltiplos loops

  # ---------- Cache LRU (dataURL por chave "cam|ts") ----------
  cache <- new.env(parent = emptyenv())
  cache_order <- character(0)  # LRU: chave mais antiga no início
  key_of <- function(cam, ts) sprintf("%s|%s", as.character(cam), format(ts, "%Y-%m-%d %H:%M:%OS", tz = "UTC"))

  cache_get <- function(k) {
    if (exists(k, envir = cache, inherits = FALSE)) {
      idx <- which(cache_order == k)
      if (length(idx)) cache_order <<- c(cache_order[-idx], k)
      return(get(k, envir = cache, inherits = FALSE))
    }
    NULL
  }
  cache_put <- function(k, val) {
    assign(k, val, envir = cache)
    idx <- which(cache_order == k)
    if (length(idx)) cache_order <<- cache_order[-idx]
    cache_order <<- c(cache_order, k)
    cache_trim()
  }
  cache_trim <- function() {
    overflow <- length(cache_order) - MAX_CACHE_ITEMS
    if (overflow > 0) {
      drop_keys <- cache_order[seq_len(overflow)]
      rm(list = drop_keys, envir = cache)
      cache_order <<- cache_order[-seq_len(overflow)]
    }
  }
  cache_clear <- function() {
    rm(list = ls(envir = cache), envir = cache)
    cache_order <<- character(0)
  }

  # ---------- Modal (layout principal) ----------
  id       <- ns('dialogObj')
  cssStyle <- list()
  cssStyle[[paste0(' #parent',id,' .modal-dialog')]]  <- paste0('width: 95% !important; height: 90% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-content')]] <- paste0('width: 100% !important; height: 100% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-body')]]    <- paste0('width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto; overflow-x: hidden;')

  showModal(
    session = session,
    div(
      id = paste0('parent', id),
      style = "height: 80%; overflow: hidden;",
      inlineCSS(cssStyle),
      dialogModal(
        title = "Novo treinamento",
        size  = 'm',
        uiMain(ns, setores, objetos),
        footer = uiOutput(ns('uiFooter'))
      )
    )
  )

  output$uiFooter <- renderUI({
    tagList(
      actionButton(ns("btSair"),  label = "Voltar",   icon = icon("arrow-left")),
      actionButton(ns("btSalvar"),class = "btn-warning", label = "Atualizar", icon = icon("save"))
    )
  })

  # ---------- CLIP: funções internas (closure com rv/session/input) ----------
   show_clip_summary <- function(ts_start, ts_end, i0, i1, tz_local = Sys.timezone()){
    if (is.null(ts_start) || is.null(ts_end)) return(invisible())

    dur_sec <- as.numeric(difftime(ts_end, ts_start, units = "secs"))
    dur_txt <- sprintf("%02d:%02d", floor(dur_sec/60), round(dur_sec %% 60))

    fmt_iso <- "%Y-%m-%d %H:%M:%S"
    start_utc <- format(ts_start, tz = "UTC", format = fmt_iso)
    end_utc   <- format(ts_end,   tz = "UTC", format = fmt_iso)
    start_loc <- format(ts_start, tz = tz_local, format = fmt_iso)
    end_loc   <- format(ts_end,   tz = tz_local, format = fmt_iso)

    n_frames <- if (is.finite(i0) && is.finite(i1)) abs(i1 - i0) + 1L else NA_integer_

    # ids/seletores do overlay
    overlay_id <- ns("clip_summary_overlay")
    parent_sel <- paste0("#parent", ns("dialogObj"), " .modal-content")

    # remove overlay anterior, se existir
    try(removeUI(selector = paste0("#", overlay_id), multiple = TRUE, immediate = TRUE), silent = TRUE)

    # insere overlay "dentro" do modal principal
    insertUI(
      selector = parent_sel,
      where    = "beforeEnd",
      ui = div(
        id = overlay_id,
        style = "position: fixed; inset: 0; background: rgba(0,0,0,.5); z-index: 1060;
                 display: flex; align-items: center; justify-content: center;",
        div(
          style = "background: #fff; border-radius: 10px; width: min(560px, 92%); 
                   padding: 16px 18px; box-shadow: 0 12px 30px rgba(0,0,0,.25);",
          tags$h4("Clip concluído", style="margin-top:0;"),
          tags$hr(),
          tags$p(tags$b("Janela do clip (UTC):")),
          tags$p(sprintf("De: %s  —  Até: %s", start_utc, end_utc)),
          tags$hr(),
          tags$p(tags$b(sprintf("Duração: %s (%.1f s)", dur_txt, dur_sec))),
          tags$p(tags$b(sprintf("Frames no intervalo: %s", ifelse(is.na(n_frames), "-", as.character(n_frames))))),
          tags$hr(),
          tags$p(tags$b("Conversão p/ fuso local")),
          tags$p(sprintf("Início: %s  —  Fim: %s  (%s)", start_loc, end_loc, tz_local)),
          div(
            style="text-align: right; margin-top: 12px;",
            actionButton(ns("clipCloseSummary"), "OK", class = "btn btn-primary btn-sm")
          )
        )
      )
    )
  }

  end_clip <- function(reason = c("limite", "manual")){
    reason <- match.arg(reason)
    if (!isTRUE(rv$clip_active)) return(invisible())

    ts_start <- rv$clip_t0
    i0       <- rv$clip_i0

    ts_end <- get_current_frame_ts(rv, tz = "UTC", fmt = NULL)
    i1     <- rv$i

    rv$clip_active <- FALSE
    rv$clip_t0     <- NULL
    rv$clip_i0     <- NA_integer_
    updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
    playing(FALSE) 

    show_clip_summary(ts_start, ts_end, i0, i1)
  }

  check_clip_elapsed <- function(){
    if (!isTRUE(rv$clip_active)) return(invisible())
    ts_now <- get_current_frame_ts(rv, tz = "UTC", fmt = NULL)
    if (is.null(ts_now) || is.null(rv$clip_t0)) return(invisible())

    max_min <- suppressWarnings(as.numeric(input$clip_max_min))
    if (!is.finite(max_min) || max_min <= 0) max_min <- 5
    max_sec <- max_min * 60

    elapsed <- as.numeric(difftime(ts_now, rv$clip_t0, units = "secs"))
    if (is.finite(elapsed) && elapsed >= max_sec) {
      end_clip("limite")
    }
  }

  # ---------- Reagir mudança de setor -> popular objetos daquele setor ----------
  obs$add(observeEvent(input$comboSetor, {
    setor <- setores |> dplyr::filter(NAME_SETOR == input$comboSetor)
    if (!nrow(setor)) return()
    objetos_setor <- objetos |> dplyr::filter(CD_ID_SETOR == setor$CD_ID_SETOR)
    updateSelectizeInput(session,"comboObjeto", choices = objetos_setor$NAME_OBJETO)
    
    output$uiCamerasFrames <- NULL
    playing(FALSE)
    loop_on <<- FALSE
    cache_clear()
    if (isTRUE(rv$clip_active)) end_clip("manual")
  }, ignoreNULL = TRUE))

  # ---------- Fetch individual (cam, ts) -> dataURL (usa cache) ----------
  fetch_dataurl_single <- function(cam, ts, update_wh_if_first = FALSE) {
    k <- key_of(cam, ts)
    hit <- cache_get(k)
    if (!is.null(hit)) return(hit)

    sql <- "SELECT DATA_FRAME FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL = ? LIMIT 1"
    res <- DBI::dbGetQuery(dbp$get_pool(), sql, params = list(as.integer(cam), as.POSIXct(ts, tz = "UTC")))
    if (!nrow(res) || is.null(res$DATA_FRAME[[1]])) return(NULL)

    raw <- res$DATA_FRAME[[1]]
    uri <- to_data_url(raw)
    cache_put(k, uri)
    uri
  }

  # ---------- Contexto compartilhado de player ----------
  if (is.null(session$userData$player_ctx)) {
    session$userData$player_ctx <- new_player_ctx(
      session = session,
      cache_get = cache_get,
      cache_put = cache_put,
      cache_trim = cache_trim,
      key_of = key_of,
      fetch_dataurl_single = fetch_dataurl_single
    )
  } else {
    session$userData$player_ctx$session <- session
  }

  # ---------- Wrappers (globais) ----------
  render_current <- function(fit_bounds = FALSE, id_map = NULL) {
    req(rv$seq, nrow(rv$seq) > 0)
    if (is.null(id_map)) id_map <- rv$id_by_cam
    ctx <- session$userData$player_ctx
    st <- ctx$render_current(
      seq_df = rv$seq, i = rv$i, w = rv$w, h = rv$h, id_map = id_map, fit_bounds = fit_bounds
    )
    if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }
  }

  prefetch_ahead_batch <- function(N = PREFETCH_AHEAD) {
    ctx <- session$userData$player_ctx
    ctx$prefetch_ahead_batch(seq_df = rv$seq, i = rv$i, N = N)
  }

  # ---------- Step (Prev/Next) ----------
  step_frame <- function(delta) {
    req(rv$seq, nrow(rv$seq) > 0)
    new_i <- rv$i + delta
    new_i <- max(1L, min(nrow(rv$seq), new_i))
    if (new_i != rv$i) {
      rv$i <- new_i
      render_current(fit_bounds = FALSE)
      prefetch_ahead_batch(PREFETCH_AHEAD)
      # >>> CLIP: verifica limite ao avançar manualmente
      check_clip_elapsed()
    }
  }
  obs$add(observeEvent(input$prevFrame, { step_frame(-1L) }, ignoreInit = TRUE))
  obs$add(observeEvent(input$nextFrame, { step_frame(+1L) }, ignoreInit = TRUE))

  # ---------- Buscar frames + montar UIs das câmeras ----------
  obs$add(observeEvent(input$btBuscar, {
    actionWebUser({
      objeto      <<- objetos |> dplyr::filter(NAME_OBJETO == isolate(input$comboObjeto))
      if (!nrow(objeto)) {
        showNotification("Selecione um objeto válido.", type = "warning")
        removeProgressLoader()
        return(invisible())
      }
      
      time_begin  <- as.POSIXct(isolate(input$datetimeBegin), tz = "UTC")
      time_end    <- as.POSIXct(isolate(input$datetimeEnd),   tz = "UTC")
      componentes <- objeto$CONFIG[[1]]$COMPONENTES[[1]]
      cameras_ids <- unique(purrr::map_int(componentes$CAMERA, "CD_ID_CAMERA"))
      
      frames_idx  <- fetch_frames(
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
      
      # Monta UI das câmeras e salva mapeamento cam -> DOM id
      res_cam <- uiCamerasComponentes(ns, input, output, componentes)
      rv$id_by_cam <- res_cam$id_by_cam
      
      output$uiCamerasFrames <- renderUI({ 
        tagList(
          br(),
          uiComponenteVideo(ns),
          res_cam$ui
        )    
      })
    
      # Reseta estado local
      cache_clear()
      if (isTRUE(rv$clip_active)){
        end_clip("manual")
        try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
      }
      rv$seq <- frames_idx
      rv$i   <- 1L
      rv$w <- 512L; rv$h <- 512L
      playing(FALSE); loop_on <<- FALSE
      
      # Descobre w/h do PRIMEIRO frame
      first <- frames_idx[1, ]
      res1  <- DBI::dbGetQuery(
        dbp$get_pool(),
        "SELECT DATA_FRAME FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL = ? LIMIT 1",
        params = list(as.integer(first$CD_ID_CAMERA), as.POSIXct(first$DT_HR_LOCAL, tz = "UTC"))
      )
      if (nrow(res1) && !is.null(res1$DATA_FRAME[[1]])) {
        wh <- img_dims(res1$DATA_FRAME[[1]])
        if (is.finite(wh[1]) && is.finite(wh[2]) && !any(is.na(wh))) {
          rv$w <- as.integer(wh[1]); rv$h <- as.integer(wh[2])
        } else {
          rv$w <- 512L; rv$h <- 256L
        }
      }
      
      # Snapshot p/ render fora do reativo
      id_map <- rv$id_by_cam
      seq_df <- rv$seq
      i0     <- rv$i
      w0     <- rv$w
      h0     <- rv$h
      
      session$onFlushed(function(){
        session$sendCustomMessage("reset_overlays", unname(id_map))
        ctx <- session$userData$player_ctx
        st <- ctx$render_current(seq_df = seq_df, i = i0, w = w0, h = h0, id_map = id_map, fit_bounds = TRUE)
        isolate({ if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h } })
        ctx$prefetch_ahead_batch(seq_df = seq_df, i = i0, N = PREFETCH_AHEAD)
      }, once = TRUE)
      
      removeProgressLoader(callback = function(){ step_frame(+1L)})
      
    }, auto.remove = FALSE)
  }, ignoreInit = TRUE))
  
  play_dir <- reactiveVal(+1L)

  # ---------- Loop de Play (later::later) ----------
  play_step <- function() {
    withReactiveDomain(session, {
      isolate({
        # Paradas de segurança
        if (!isTRUE(playing())) { loop_on <<- FALSE; return(invisible()) }
        if (is.null(rv$seq) || nrow(rv$seq) == 0L) { playing(FALSE); loop_on <<- FALSE; return(invisible()) }

        n   <- nrow(rv$seq)
        dir <- play_dir()
        if ((dir > 0L && rv$i >= n) || (dir < 0L && rv$i <= 1L)) {
          playing(FALSE); loop_on <<- FALSE; return(invisible())
        }

        # Avança/retrocede
        rv$i <- rv$i + dir

        # Contexto do player por sessão
        ctx <- session$userData$player_ctx
        if (is.null(ctx)) { playing(FALSE); loop_on <<- FALSE; return(invisible()) }

        # Render + prefetch
        st <- ctx$render_current(
          seq_df   = rv$seq,
          i        = rv$i,
          w        = rv$w,
          h        = rv$h,
          id_map   = rv$id_by_cam,
          fit_bounds = FALSE
        )
        if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }

        ctx$prefetch_ahead_batch(
          seq_df = rv$seq,
          i      = rv$i,
          N      = PREFETCH_AHEAD
        )

        # >>> CLIP: verifica limite após render
        check_clip_elapsed()

        # Intervalo
        delay <- suppressWarnings(as.numeric(input$step_ms) / 1000)
        if (!is.finite(delay) || delay <= 0) delay <- 0.001

        later::later(function(){ play_step() }, delay)
      })
    })
  }

  # ---------- Botões ----------
  obs$add(observeEvent(input$play, {
    play_dir(+1L); playing(TRUE)
    if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  
  obs$add(observeEvent(input$backPlay, {
    play_dir(-1L); playing(TRUE)
    if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  
  obs$add(observeEvent(input$pause, {
    playing(FALSE)   # loop encerra na próxima iteração
  }, ignoreInit = TRUE))

  # ---------- Clip: toggle ----------
  obs$add(observeEvent(input$clipToggle, {
    if (!isTRUE(rv$clip_active)) {
      # start
      ts0 <- get_current_frame_ts(rv, tz = "UTC", fmt = NULL)
      if (is.null(ts0)) {
        showNotification("Nenhum frame carregado para iniciar o clip.", type = "warning")
        return(invisible())
      }
      rv$clip_active <- TRUE
      rv$clip_t0     <- ts0
      rv$clip_i0     <- rv$i
      updateActionButton(session, "clipToggle", label = "Stop Clip", icon = icon("square"))
      showNotification("Clip iniciado.", type = "message")
      
      # >>> AUTOPLAY AO INICIAR O CLIP
      play_dir(+1L)         # direção para frente (ajuste se quiser manter a atual)
      playing(TRUE)
      if (!loop_on) { loop_on <<- TRUE; play_step() }
      
    } else {
      end_clip("manual")
      playing(FALSE)   # loop encerra na próxima iteração
    }
  }, ignoreInit = TRUE))


  # ---------- Botões do rodapé ----------
  obs$add(observeEvent(input$btSair,{
    obs$destroy()
    output$uiCamerasFrames <- NULL
    playing(FALSE)
    loop_on <<- FALSE
    cache_clear()
    if (isTRUE(rv$clip_active)){
       end_clip("manual")
       try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }
    removeModal(session)
    callback()
  }, ignoreInit = TRUE, ignoreNULL = TRUE))

  obs$add(observeEvent(input$btSalvar, { 
    showNotification("Atualizado!", type = "message") 
    print(as.POSIXct(get_current_frame_ts(rv), tz = Sys.timezone()))
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$clipCloseSummary, {
    try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
  }, ignoreInit = TRUE))
}

# ==================================================
# Helpers públicos/compartilhados
# ==================================================
# Retorna o timestamp do frame corrente.
# - tz: fuso desejado para a formatação (padrão "UTC")
# - fmt: se NULL, retorna POSIXct (UTC). Se string, retorna char formatado nesse tz.
get_current_frame_ts <- function(rv, tz = "UTC", fmt = NULL) {
  isolate({
    if (is.null(rv$seq) || !nrow(rv$seq)) return(NULL)
    i  <- max(1L, min(nrow(rv$seq), rv$i))
    ts <- rv$seq$DT_HR_LOCAL[i]  # este está em UTC no seu pipeline
    if (is.null(fmt)) {
      return(ts)
    } else {
      return(format(ts, tz = tz, usetz = FALSE, format = fmt))
    }
  })
}

# Barra de controles (inclui Clip)
uiComponenteVideo <- function(ns){
  splitLayout(
    # 8 filhos → 8 larguras
    cellWidths = c("auto","auto","auto","auto","auto","110px","auto","120px"),
    actionButton(ns("backPlay"),  label = "Reverse", icon = icon("backward"),
      class = "btn btn-outline-secondary", title = "Reproduzir para trás"),
    actionButton(ns("prevFrame"), label = "Prev",    icon = icon("step-backward"),
      class = "btn btn-outline-secondary", title = "Frame anterior"),
    actionButton(ns("play"),      label = "Play",  icon = icon("play"),
      class = "btn btn-outline-secondary", title = "Reproduzir"),
    actionButton(ns("pause"),     label = "Pause", icon = icon("pause"),
      class = "btn btn-outline-secondary", title = "Pausar"),
    actionButton(ns("nextFrame"), label = "Next",    icon = icon("step-forward"),
      class = "btn btn-outline-secondary", title = "Próximo frame"),
    numericInput(ns("step_ms"),"Intervalo (ms)", value = 50, min = 1, step = 1, width = "110px") |>
      tagAppendAttributes(style = ';margin-top: -25px;'),
    actionButton(ns("clipToggle"), label = "Start Clip", icon = icon("scissors"),
      class = "btn btn-outline-primary", title = "Iniciar/encerrar clip (máx. duração)"),
    numericInput(ns("clip_max_min"), "Máx (min)", value = 5, min = 1, max = 60, step = 1, width = "120px") |>
      tagAppendAttributes(style = ';margin-top: -25px;')
  )
}
