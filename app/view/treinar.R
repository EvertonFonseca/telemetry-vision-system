box::use(
  shiny[...],
  shinyjs[inlineCSS],
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
    messageAlerta
  ],
  dplyr[...],
  DT,
  shinyWidgets[airDatepickerInput,timepickerOptions],
  shinycssloaders,
  .. / logic/objeto_dao[...],
  .. / logic/camera_dao[...],
  .. / logic/setor_dao[...],
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

#' @export
uiNewTreinar <- function(ns,input,output,session,callback){

    obs       <- newObserve()
    setores   <- selectAllSetors(dbp$get_pool())
    objetos   <- selectAllObjetos(dbp$get_pool())
    objeto    <- NULL

    # ---------- Estado do player ----------
    rv <- reactiveValues(
      seq = NULL,        # data.frame: DT_HR_LOCAL, CD_ID_CAMERA
      i   = 1L,          # índice corrente
      w = 512L, h = 512L # dimensões atuais do overlay
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


    # ---------- Fetch individual (cam, ts) -> dataURL (usa cache) ----------
    fetch_dataurl_single <- function(cam, ts, update_wh_if_first = FALSE) {
      k <- key_of(cam, ts)
      hit <- cache_get(k)
      if (!is.null(hit)) return(hit)
      
      sql <- "SELECT DATA_FRAME FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL = ? LIMIT 1"
      res <- DBI::dbGetQuery(dbp$get_pool(), sql, params = list(as.integer(cam), as.POSIXct(ts, tz = "UTC")))
      if (!nrow(res) || is.null(res$DATA_FRAME[[1]])) return(NULL)
      
      raw <- res$DATA_FRAME[[1]]
      
      if (update_wh_if_first && rv$i == 1L) {
        wh <- img_dims(raw)
        if (is.finite(wh[1]) && is.finite(wh[2]) && !any(is.na(wh))) {
          rv$w <- as.integer(wh[1]); rv$h <- as.integer(wh[2])
        } else {
          rv$w <- 512L; rv$h <- 512L
        }
      }
      
      uri <- to_data_url(raw)
      cache_put(k, uri)
      uri
    }

    # ---------- Prefetch em batch por câmera ----------
    prefetch_ahead_batch <- function(N = PREFETCH_AHEAD) {

      if (is.null(rv$seq) || nrow(rv$seq) == 0) return(invisible())
      if (rv$i >= nrow(rv$seq)) return(invisible())
      
      tgt_idx <- seq.int(rv$i + 1L, min(nrow(rv$seq), rv$i + N))
      if (!length(tgt_idx)) return(invisible())
      
      seg <- rv$seq[tgt_idx, , drop = FALSE]
      
      seg$k <- mapply(key_of, seg$CD_ID_CAMERA, seg$DT_HR_LOCAL)
      seg <- seg[!vapply(seg$k, function(z) !is.null(cache_get(z)), logical(1L)), , drop = FALSE]
      if (!nrow(seg)) return(invisible())
      
      groups <- split(seg, seg$CD_ID_CAMERA)
      for (cam_str in names(groups)) {

        g <- groups[[cam_str]]
        cam <- as.integer(cam_str)
        ts_vec <- as.POSIXct(g$DT_HR_LOCAL, tz = "UTC")
        
        placeholders <- paste(rep("?", length(ts_vec)), collapse = ",")
        sql <- paste0(
          "SELECT DT_HR_LOCAL, DATA_FRAME ",
          "FROM FRAME_CAMERA ",
          "WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL IN (", placeholders, ")"
        )
        
        params <- c(list(cam), as.list(ts_vec))
        res <- tryCatch(DBI::dbGetQuery(dbp$get_pool(), sql, params = params),
        error = function(e) { NULL })
        
        if (!is.null(res) && nrow(res)) {
          res$DT_HR_LOCAL <- as.POSIXct(res$DT_HR_LOCAL, tz = "UTC")
          idx_map <- match(ts_vec, res$DT_HR_LOCAL)
          for (j in seq_along(ts_vec)) {
            if (!is.na(idx_map[j])) {
              raw <- res$DATA_FRAME[[ idx_map[j] ]]
              if (!is.null(raw)) {
                uri <- to_data_url(raw)
                cache_put(key_of(cam, ts_vec[j]), uri)
              }
            }
          }
        }
        
        missing <- vapply(seq_along(ts_vec), function(j){
          is.null(cache_get(key_of(cam, ts_vec[j])))
        }, logical(1L))
        if (any(missing)) {
          for (j in which(missing)) {
            invisible(fetch_dataurl_single(cam, ts_vec[j], update_wh_if_first = FALSE))
          }
        }
      }
      
      cache_trim()
      invisible(TRUE)
    }

    # ---------- Renderiza frame corrente ----------
    render_current <- function(fit_bounds = FALSE) {
      req(rv$seq, nrow(rv$seq) > 0)
      cur <- rv$seq[rv$i, ]
      uri <- fetch_dataurl_single(cur$CD_ID_CAMERA, cur$DT_HR_LOCAL,
        update_wh_if_first = TRUE)
        if (is.null(uri)) return(invisible())
        session$sendCustomMessage("set_frame_simple", list(
          url = uri,
          w   = as.integer(rv$w),
          h   = as.integer(rv$h),
          fit = isTRUE(fit_bounds)
        ))
      }
      
    id       <- ns('dialogObj')
    cssStyle <- list()
    cssStyle[[paste0(' #parent',id,' .modal-dialog')]]  <- paste0('width: 95% !important; height: 90% !important;')
    cssStyle[[paste0(' #parent',id,' .modal-content')]] <- paste0('width: 100% !important; height: 100% !important;')
    cssStyle[[paste0(' #parent',id,' .modal-body')]]    <- paste0('width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto; overflow-x: hidden;')
    
    showModal(
      session = session,
      div(
        id = paste0('parent', id),
        style = paste0("height: 80%; overflow: hidden;"),
        inlineCSS(cssStyle),
        dialogModal(
          title = "Novo treinamento",
          size = 'm',
          uiMain(ns,setores,objetos),  
          footer = uiOutput(ns('uiFooter')))))
    
    
    output$uiFooter <- renderUI({
      
      tagList(actionButton(ns("btSair"), label = "Voltar",icon = icon("arrow-left")),actionButton(ns('btSalvar'),class = "btn-warning",label = "Atualizar",icon = icon("save")))
      
    })
    
    obs$add(observeEvent(input$comboSetor, {
      setor <- setores |> filter(NAME_SETOR == input$comboSetor)
      objetos_setor <- objetos |> filter(CD_ID_SETOR == setor$CD_ID_SETOR)
      updateSelectizeInput(session,"comboObjeto",choices = objetos_setor$NAME_OBJETO)
    }, ignoreInit = TRUE))

    obs$add(observeEvent(input$btBuscar,{
      
      objeto      <<- objetos |> filter(NAME_OBJETO == isolate(input$comboObjeto))
      time_begin  <- as.POSIXct(isolate(input$datetimeBegin),tz = "UTC")
      time_end    <- as.POSIXct(isolate(input$datetimeEnd),tz = "UTC")
      componentes <- objeto$CONFIG[[1]]$COMPONENTES[[1]]
      camera_id   <- paste0(unique(componentes$CD_ID_CAMERA),collapse = ",")
      
      frames_idx  <- fetch_frames(time_begin = time_begin,time_end = time_end,camera_id)
      
      output$uiCamerasFrames <- renderUI({
        uiCamerasComponentes(ns,input,output,componentes)
      })
      
      if (!nrow(frames_idx)) {
        showNotification("Nenhum frame no intervalo/câmeras selecionados.", type = "warning")
        rv$seq <- NULL; rv$i <- 1L; playing(FALSE); loop_on <<- FALSE
        return(invisible())
      }
      
      cache_clear()
      rv$seq <- frames_idx
      rv$i   <- 1L
      rv$w <- 512L; rv$h <- 512L
      
      # garante pausado ao carregar
      playing(FALSE); loop_on <<- FALSE
      
      render_current(fit_bounds = TRUE)
      prefetch_ahead_batch(PREFETCH_AHEAD)
    
    },ignoreInit = TRUE))
    
    
    play_step <- function(session) {
      # Garante um domínio reativo (o da sessão) durante a execução
      withReactiveDomain(session,{
        isolate({
          # parou?
          if (!isTRUE(playing())) { loop_on <<- FALSE; return(invisible()) }
          # sem sequência?
          if (is.null(rv$seq) || nrow(rv$seq) == 0) { playing(FALSE); loop_on <<- FALSE; return(invisible()) }
          # fim da sequência?
          if (rv$i >= nrow(rv$seq)) { playing(FALSE); loop_on <<- FALSE; return(invisible()) }
          
          # avança 1 frame e prefetch
          step_frame(+1L)
          prefetch_ahead_batch(PREFETCH_AHEAD)
          
          # agenda próximo ciclo usando o step_ms atual
          delay <- as.numeric(input$step_ms) / 1000
          if (!is.finite(delay) || delay <= 0) delay <- 0.001  # guarda-chuva
          later$later(function() {
            # reentra chamando a si mesmo; não acessa reativos aqui
            play_step()
          }, delay)
        })
      })
    }
    observeEvent(input$play, {
      playing(TRUE)
      if (!loop_on) { loop_on <<- TRUE; play_step() }
    })
    
    observeEvent(input$pause, {
      playing(FALSE)  # o loop encerra sozinho na próxima iteração
    })

}
      
uiMain <- function(ns,setores,objetos,valueComboSetor = NULL,valueObjeto = NULL){
  
  div(
     # JS: cria overlay e handler para atualizar a imagem rapidamente
    tags$script(HTML("
      (function(){
        let overlay = null;
        let currentBounds = null;

        function fitWholeImage(map, w, h){
          const size  = map.getSize();
          const scaleX = size.x / w;
          const scaleY = size.y / h;
          const scale  = Math.min(scaleX, scaleY);
          let z = Math.log2(scale);
          if (!isFinite(z)) z = 0;
          const center = [h/2, w/2];
          map.setView(center, z, {animate:false});
          const bounds = [[0,0],[h,w]];
          map.setMaxBounds(bounds);
          map.options.maxBoundsViscosity = 1.0;
          map.setMinZoom(z);
          setTimeout(function(){ map.invalidateSize(); }, 0);
          currentBounds = bounds;
        }

        Shiny.addCustomMessageHandler('set_frame_simple', function(msg){
          // msg: { url, w, h, fit:boolean }
          const map = HTMLWidgets.find('#map').getMap();
          if (!map) return;

          const w = msg.w || 512;
          const h = msg.h || 512;
          const bounds = [[0,0],[h,w]];

          if (!overlay) {
            overlay = L.imageOverlay(msg.url, bounds, {opacity:1}).addTo(map);
            fitWholeImage(map, w, h);
          } else {
            const changed = !currentBounds ||
                            currentBounds[1][0] !== h ||
                            currentBounds[1][1] !== w;
            if (changed){
              overlay.setBounds(bounds);
              currentBounds = bounds;
              if (msg.fit) fitWholeImage(map, w, h);
            }
            overlay.setUrl(msg.url);
          }
        });

        window.addEventListener('resize', function(){
          const map = HTMLWidgets.find('#map').getMap();
          if (!map || !currentBounds) return;
          setTimeout(function(){ map.invalidateSize(); }, 0);
        });
      })();
    ")),
    inlineCSS(paste0("#",ns("textNameTreino")," {text-transform: uppercase;}")),
    panelTitle(title = "Configuração",
    background.color.title = 'white',
    title.color  = 'black',
    border.color = 'lightgray',
    children = fluidRow(
      style = 'padding-top: 10px; padding-left: 15px; padding-right: 15px;',
      column(3,selectizeInput(ns('comboSetor'),label = 'Setor',choices = setores$NAME_SETOR,selected = valueComboSetor)),
      column(3,selectizeInput(ns('comboObjeto'),label = 'Objeto',choices = NA)),
      column(6,splitLayout(
        cellWidths = c("45%", "45%","10%"),
        airDatepickerInput(
          inputId = ns("datetimeBegin"),
          label = "Data e Hora De:",
          language = "pt-BR",  # Set language to Portuguese-Brazil
          timepicker = TRUE,   # Enable time selection
          dateFormat = "dd/MM/yyyy",  # Brazilian date format
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
          language = "pt-BR",  # Set language to Portuguese-Brazil
          timepicker = TRUE,   # Enable time selection
          dateFormat = "dd/MM/yyyy",  # Brazilian date format
          timepickerOpts = timepickerOptions(
            hoursStep = 1,
            minutesStep = 5
          ),
          width = "98%",
          placeholder = "Escolha uma data e hora"
        ),
        actionButton(ns('btBuscar'),label = '',icon = icon("search"),   style = paste0("margin-top: 25px; margin-left: -5px;"))
      ))
    )
  ),
    br(),
    div(class="controls",
    actionButton("play",  "▶️ Play"),
    actionButton("pause", "⏸ Pause"),
    actionButton("prevFrame",  "⟵ Prev"),
    actionButton("nextFrame",  "Next ⟶")
  ),
  br(),
  uiOutput(ns('uiCamerasFrames')) |> shinycssloaders$withSpinner(color = 'lightblue'))
  
}

uiCamerasComponentes <- function(ns,input,output,componentes){
  
  divLista <- fluidRow()
  cameras  <- map_df(componentes$CAMERA,~ .x)
  cameras  <- cameras[!duplicated(cameras$CD_ID_CAMERA),]
  #loop cameras
  for(i in 1:nrow(cameras)){
    
    local({
      output[[sprintf("map_%d",i)]] <- renderLeaflet({
        leaflet(options = leafletOptions(
          crs = leafletCRS(crsClass = "L.CRS.Simple"),
          zoomSnap  = 0,        # permite zoom fracionário
          zoomDelta = 0.25      # passo de zoom ao usar scroll
        ))
      })

      camera <- cameras$NAME_CAMERA[i]
      cameraElement <- panelTitle(
        title = paste0("Câmera: ",camera),
        background.color.title = "white",
        title.color  = "black",
        border.color = "lightgray",
        children = div(
           style = "padding: 10px;",
           leafletOutput(ns(paste0("map_",i)),height = "256px", width = "100%")
        )
      )
      divLista <<- tagAppendChildren(divLista,column(6,cameraElement))
    })
  }
  divLista
}

# =========================
# Helpers: MIME / data URL / W×H do BLOB
# =========================
detect_mime <- function(raw) {
  if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
  if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return("image/png")
  "application/octet-stream"
}

to_data_url <- function(raw) paste0("data:", detect_mime(raw), ";base64,", base64encode(raw))

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

fetch_frames <- function(time_begin, time_end, camera_id) {
  # as.POSIXct(dados$DT_HR_LOCAL,tz = Sys.timezone())
  frames <- DBI::dbGetQuery(
    dbp$get_pool(),
    paste0(
      "SELECT 
          DT_HR_LOCAL,
          CD_ID_CAMERA 
          FROM frame_camera 
          WHERE DT_HR_LOCAL BETWEEN '",
      time_begin,
      "' AND '",
      time_end,
      "' AND CD_ID_CAMERA IN (",
      camera_id,
      ") ORDER BY DT_HR_LOCAL ASC"
    )
  ) |> 
    mutate(DT_HR_LOCAL = as.POSIXct(DT_HR_LOCAL,tz = Sys.timezone()))
}
