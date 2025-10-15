# options(shiny.maxRequestSize = 2048 * 1024^2)

# # app.R
# library(shiny)
# library(av)

# ui <- fluidPage(
#   tags$head(
#     tags$style(HTML("
#       .controls { display:flex; gap:.5rem; flex-wrap:wrap; align-items:center; margin-top:.5rem }
#       #player { max-width: 100%; }
#       .timebar { margin-top:.25rem; }
#     ")),
#     # JS para tocar/pausar, pular frames e sincronizar o slider
#     tags$script(HTML("
# (function(){
#   function fmt(t){ t=t||0; const m=Math.floor(t/60), s=Math.floor(t%60).toString().padStart(2,'0'); return m+':'+s; }
#   let initialized = false;

#   function init(){
#     const vid  = document.getElementById('player');
#     if(!vid) return false;

#     const seek  = document.getElementById('seek');
#     const rate  = document.getElementById('rate');
#     const clock = document.getElementById('clock');
#     const dur   = document.getElementById('dur');

#     const btnPlay   = document.getElementById('play');
#     const btnFwd    = document.getElementById('fwd');
#     const btnBack   = document.getElementById('back');
#     const btnFwd10  = document.getElementById('fwd10');
#     const btnBack10 = document.getElementById('back10');
#     const fpsInput  = document.getElementById('fps');

#     // metadata -> duração/slider
#     vid.addEventListener('loadedmetadata', function(){
#       dur.textContent = fmt(vid.duration||0);
#       seek.max = vid.duration || 100;
#       seek.value = 0;
#       Shiny.setInputValue('duration', vid.duration, {priority:'event'});
#     });

#     // sincroniza relógio e slider
#     vid.addEventListener('timeupdate', function(){
#       clock.textContent = fmt(vid.currentTime||0);
#       if (!seek.dragging) seek.value = vid.currentTime||0;
#       Shiny.setInputValue('current_time', vid.currentTime, {priority:'event'});
#     });

#     // play/pause
#     btnPlay.addEventListener('click', function(){
#       if (vid.paused) vid.play(); else vid.pause();
#     });

#     // pular N frames (pausando para precisão)
#     function step(nFrames){
#       const fps = parseFloat(fpsInput.value) || 30;
#       const dt  = nFrames / fps;
#       vid.pause();
#       const t = (vid.currentTime||0) + dt;
#       vid.currentTime = Math.max(0, Math.min(t, vid.duration || Number.MAX_VALUE));
#     }
#     btnFwd.addEventListener('click',   ()=> step(1));
#     btnBack.addEventListener('click',  ()=> step(-1));
#     btnFwd10.addEventListener('click', ()=> step(10));
#     btnBack10.addEventListener('click',()=> step(-10));

#     // seek interativo
#     seek.addEventListener('input', function(){ seek.dragging = true; vid.currentTime = parseFloat(seek.value)||0; });
#     seek.addEventListener('change', function(){ seek.dragging = false; });

#     // velocidade
#     vid.playbackRate = parseFloat(rate.value)||1;
#     rate.addEventListener('input', function(){ vid.playbackRate = parseFloat(rate.value)||1; });

#     initialized = true;
#     return true;
#   }

#   // inicializa agora e também ao re-render do UI
#   function tryInit(){
#     if(initialized) return;
#     if(!init()){
#       const mo = new MutationObserver(()=>{ if(init()) mo.disconnect(); });
#       mo.observe(document.body, {childList:true, subtree:true});
#     }
#   }
#   document.addEventListener('DOMContentLoaded', tryInit);
#   tryInit();
# })();
#     "))
#   ),

#   titlePanel("Player de vídeo com play/pausa e passo por frame"),
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("video", "Selecione um vídeo", accept = c("video/*")),
#       numericInput("vid_w", "Largura (px)", value = 900, min = 320, max = 4096, step = 10),
#       numericInput("fps", "FPS (detecção automática ou ajuste manual)", value = 30, min = 1, step = 1),
#       helpText("Dica: se a detecção de FPS falhar, ajuste manualmente para o passo por frame ficar preciso.")
#     ),
#     mainPanel(
#       uiOutput("playerUI")
#     )
#   )
# )

# server <- function(input, output, session){
#   # servir os vídeos via caminho estático (streaming pelo navegador)
#   videos_dir <- file.path(tempdir(), "videos_shiny")
#   dir.create(videos_dir, recursive = TRUE, showWarnings = FALSE)
#   addResourcePath("videos", videos_dir)

#   rv <- reactiveValues(fname=NULL)

#   observeEvent(input$video, {
#     req(input$video)
#     dest <- file.path(videos_dir, basename(input$video$name))
#     ok <- file.copy(input$video$datapath, dest, overwrite = TRUE)
#     validate(need(ok, "Falha ao copiar o arquivo de vídeo."))
#     rv$fname <- basename(input$video$name)

#     # tenta detectar FPS com {av}
#     info <- try(av::av_media_info(dest), silent = TRUE)
#     fps  <- NA_real_
#     if(!inherits(info, "try-error")){
#       get_rate <- function(x){
#         if(is.null(x)) return(NA_real_)
#         if(is.numeric(x)) return(as.numeric(x))
#         if(is.character(x)){
#           if(grepl("/", x)){
#             sp <- strsplit(x, "/", fixed = TRUE)[[1]]
#             return(suppressWarnings(as.numeric(sp[1]) / as.numeric(sp[2])))
#           } else {
#             return(suppressWarnings(as.numeric(x)))
#           }
#         }
#         NA_real_
#       }
#       fps <- get_rate(info$video$framerate)
#       if(is.na(fps)) fps <- get_rate(info$video$avg_frame_rate)
#       if(is.na(fps)) fps <- get_rate(info$video$frame_rate)
#     }
#     if(!is.na(fps) && is.finite(fps) && fps > 0){
#       updateNumericInput(session, "fps", value = round(fps))
#     }
#   })

#   output$playerUI <- renderUI({
#     req(rv$fname)
#     tagList(
#       # Player HTML5 (sem controles nativos; usamos os nossos)
#       tags$video(
#         id = "player", width = input$vid_w, preload = "auto", controls = NA,
#         tags$source(src = paste0("videos/", rv$fname))
#       ),
#       # Relógio
#       div(class="timebar",
#           "Tempo: ", tags$span(id="clock", "0:00"),
#           " / ", tags$span(id="dur", "0:00")
#       ),
#       # Controles
#       div(class="controls",
#           actionButton("play",  "▶️/⏸"),
#           actionButton("back",  "⟵ 1 frame"),
#           actionButton("fwd",   "1 frame ⟶"),
#           actionButton("back10","⟵ 10 frames"),
#           actionButton("fwd10", "10 frames ⟶"),
#           sliderInput("rate", "Velocidade", min = 0.25, max = 3, value = 1, step = 0.25, width = "280px"),
#           sliderInput("seek", "Posição (s)", min = 0, max = 100, value = 0, step = 0.01, width = "100%")
#       )
#     )
#   })
# }

# shinyApp(ui, server)

options(shiny.maxRequestSize = 2048 * 1024^2)

library(shiny)
library(leaflet)
library(leaflet.extras)
library(av)
library(jsonlite)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .controls { display:flex; gap:.5rem; flex-wrap:wrap; align-items:center; margin:.5rem 0 }
      .timebar { margin:.25rem 0; }
      #map { border:1px solid #ddd; }
    "))
  ),
  titlePanel("Leaflet + Vídeo (overlay) com desenho e controle de frames"),
  sidebarLayout(
    sidebarPanel(
      fileInput("video", "Selecione um vídeo", accept = "video/*"),
      textInput("out_url_base", "Base URL (auto)", value = "", placeholder = "auto"),
      numericInput("fps", "FPS (detectado ou manual)", value = 30, min = 1, step = 1),
      helpText("O FPS é usado para pular frames com precisão."),
      hr(),
      uiOutput("controls_ui")
    ),
    mainPanel(
      leafletOutput("map", height = 600),
      div(class="timebar",
          "Tempo: ", textOutput("t_now", inline = TRUE),
          " / ", textOutput("t_dur", inline = TRUE)
      ),
      div(class="controls",
          actionButton("play",  "▶️ Play"),
          actionButton("pause", "⏸ Pause"),
          actionButton("back",  "⟵ 1 frame"),
          actionButton("fwd",   "1 frame ⟶"),
          actionButton("back10","⟵ 10 frames"),
          actionButton("fwd10", "10 frames ⟶"),
          sliderInput("seek", "Posição (s)", min = 0, max = 100, value = 0, step = 0.01, width = "100%")
      ),
      tags$hr(),
      h4("Anotações (GeoJSON + timestamp)"),
      verbatimTextOutput("annots_out", placeholder = TRUE)
    )
  )
)

server <- function(input, output, session){

  # Pasta pública para servir o vídeo (stream pelo navegador)
  # sess_token <- session$token %||% paste0("x", as.integer(as.numeric(Sys.time())*1000))
  # sess_alias <- paste0("videos_", gsub("[^A-Za-z0-9_\\-]", "_", sess_token))
  # vids_dir   <- file.path(tempdir(), sess_alias)
  # dir.create(vids_dir, recursive = TRUE, showWarnings = FALSE)
  # addResourcePath(sess_alias,vids_dir)
  
  vids_dir <- file.path(tempdir(), "videos_shiny")
  dir.create(vids_dir, recursive = TRUE, showWarnings = FALSE)
  addResourcePath("videos",vids_dir)

  rv <- reactiveValues(
    url = NULL, path = NULL, w = 1280, h = 720, fps = 30,
    duration = NA_real_, time = 0,
    annots = list()  # lista de {time, feature}
  )

  # Controles adicionais (aparecem após escolher o vídeo)
  output$controls_ui <- renderUI({
    req(rv$url)
    tagList(
      helpText(sprintf("Vídeo: %s (%dx%d)", basename(rv$path), rv$w, rv$h)),
      helpText("Desenhe com a barra 'Draw' no canto do mapa (polígonos, retângulos...).")
    )
  })

  observeEvent(input$video, {
    req(input$video)
    dest <- file.path(vids_dir, basename(input$video$name))
    ok <- try(file.copy(input$video$datapath, dest, overwrite = TRUE), silent = TRUE)
    if (!isTRUE(ok)) {
      showNotification("Falha ao copiar o arquivo de vídeo.", type = "error")
      return(invisible())
    }

    rv$path <- dest
    rv$url  <- paste0("videos/", basename(dest))

    # tenta detectar dimensões e FPS
    info <- try(av::av_media_info(dest), silent = TRUE)
    if (!inherits(info, "try-error")) {
      if (!is.null(info$video$width) && !is.null(info$video$height)) {
        rv$w <- as.integer(info$video$width)
        rv$h <- as.integer(info$video$height)
      }
      # FPS (pode vir como '30000/1001', etc.)
      parse_rate <- function(x){
        if(is.null(x)) return(NA_real_)
        if(is.numeric(x)) return(as.numeric(x))
        if(is.character(x)){
          if (grepl("/", x)) {
            sp <- strsplit(x, "/", fixed = TRUE)[[1]]
            return(suppressWarnings(as.numeric(sp[1]) / as.numeric(sp[2])))
          } else return(suppressWarnings(as.numeric(x)))
        }
        NA_real_
      }
      fps <- parse_rate(info$video$framerate)
      if (is.na(fps)) fps <- parse_rate(info$video$avg_frame_rate)
      if (is.na(fps)) fps <- parse_rate(info$video$frame_rate)
      if (!is.na(fps) && is.finite(fps) && fps > 0) {
        rv$fps <- fps
        updateNumericInput(session, "fps", value = round(fps))
      }
    }

    # reseta duração/tempo e anotações
    rv$duration <- NA_real_
    rv$time <- 0
    rv$annots <- list()
  })

  # Mapa com CRS simples e Draw
  output$map <- renderLeaflet({
    req(rv$url, rv$w, rv$h)
    leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>%
      addMapPane("videoPane", zIndex = 200) %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polygonOptions = drawPolygonOptions(showArea = TRUE),
        rectangleOptions = drawRectangleOptions(),
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      addLayersControl(overlayGroups = c("drawn")) %>%
      addScaleBar(position = "bottomleft") %>%
      htmlwidgets::onRender(
        "
          function(el, x, data){
            var map = this;

            // bounds no CRS.Simple = [ [y_min, x_min], [y_max, x_max] ] = [ [0,0], [h,w] ]
            var bounds = [[0,0],[data.h, data.w]];
            map.fitBounds(bounds);

            // cria videoOverlay
            var overlay = L.videoOverlay(data.url, bounds, {
              autoplay: false, loop: false, muted: true, opacity: 1
            }).addTo(map);
            map._videoOverlay = overlay;
            var vid = overlay.getElement();

            // envia duração e tempo para Shiny
            var sendT = function(){
              if (window.Shiny) Shiny.setInputValue('video_time', vid.currentTime, {priority:'event'});
            };
            vid.addEventListener('timeupdate', sendT);
            vid.addEventListener('loadedmetadata', function(){
              if (window.Shiny) {
                Shiny.setInputValue('video_duration', vid.duration, {priority:'event'});
              }
            });

            // handler para comandos vindos do server
            if (window.Shiny) {
              Shiny.addCustomMessageHandler('video_cmd', function(msg){
                if(!vid) return;
                if(msg.op === 'play'){ vid.play(); }
                else if(msg.op === 'pause'){ vid.pause(); }
                else if(msg.op === 'seek'){
                  vid.currentTime = Math.max(0, Math.min(msg.t, vid.duration || Number.MAX_VALUE));
                } else if(msg.op === 'step'){
                  var fps = msg.fps || 30;
                  var dt  = msg.n / fps;
                  vid.pause();
                  var t = (vid.currentTime||0) + dt;
                  vid.currentTime = Math.max(0, Math.min(t, vid.duration || Number.MAX_VALUE));
                }
              });
            }
          }
        ",
        data = list(url = rv$url, w = rv$w, h = rv$h)
      )
  })

  # Sincroniza barra de tempo/duração
  observeEvent(input$video_duration, {
    rv$duration <- as.numeric(input$video_duration)
    updateSliderInput(session, "seek", max = rv$duration, value = 0)
  })
  observeEvent(input$video_time, {
    rv$time <- as.numeric(input$video_time)
    # não atualiza slider durante drag; simples aqui:
    updateSliderInput(session, "seek", value = rv$time)
  })

  # Controles
  observeEvent(input$play,  { session$sendCustomMessage("video_cmd", list(op = "play")) })
  observeEvent(input$pause, { session$sendCustomMessage("video_cmd", list(op = "pause")) })
  observeEvent(input$seek,  { session$sendCustomMessage("video_cmd", list(op = "seek", t = input$seek)) })

  step_cmd <- function(n){
    fps <- as.numeric(input$fps %||% rv$fps %||% 30)
    session$sendCustomMessage("video_cmd", list(op = "step", n = n, fps = fps))
  }
  observeEvent(input$back,   { step_cmd(-1) })
  observeEvent(input$fwd,    { step_cmd( 1) })
  observeEvent(input$back10, { step_cmd(-10) })
  observeEvent(input$fwd10,  { step_cmd( 10) })

  # ---- ANOTAÇÕES: capturar features com timestamp atual ----
  observeEvent(input$map_draw_new_feature, {
    req(rv$time)
    feat <- input$map_draw_new_feature  # GeoJSON-like list
    rv$annots <- append(rv$annots, list(list(time = rv$time, feature = feat)))
  })
  observeEvent(input$map_draw_edited_features, {
    # você pode atualizar rv$annots aqui (exemplo mínimo: só registra evento)
    msg <- paste("Editado:", length(input$map_draw_edited_features$features), "feature(s)")
    showNotification(msg, type = "message", duration = 2)
  })
  observeEvent(input$map_draw_deleted_features, {
    # idem: aqui você pode remover do rv$annots com base em IDs
    msg <- paste("Removido:", length(input$map_draw_deleted_features$features), "feature(s)")
    showNotification(msg, type = "warning", duration = 2)
  })

  # Mostrar anotações
  output$annots_out <- renderText({
    if (length(rv$annots) == 0) return("Nenhuma anotação ainda.")
    toJSON(rv$annots, auto_unbox = TRUE, pretty = TRUE)
  })

  # Exibir tempo e duração
  output$t_now <- renderText({ sprintf("%.3fs", rv$time) })
  output$t_dur <- renderText({ if (is.na(rv$duration)) "--:--" else sprintf("%.3fs", rv$duration) })

  # Limpeza quando a sessão terminar (remove alias e a pasta da sessão)
  # session$onSessionEnded(function(){
  #   try({
  #     shiny::removeResourcePath(sess_alias)
  #     unlink(sess_alias, recursive = TRUE, force = TRUE)
  #   }, silent = TRUE)
  # })

}

shinyApp(ui, server)
