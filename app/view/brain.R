# app.R — Leaflet + Player (DB on-demand, sem image_path; W/H padrão 512)

options(shiny.maxRequestSize = 2048 * 1024^2)

library(shiny)
library(leaflet)
library(leaflet.extras)
library(DBI)
library(RMariaDB)
library(dplyr)
library(lubridate)
library(base64enc)
library(htmlwidgets)
library(jsonlite)
library(shinyDatetimePickers)

# =========================
# CONFIG DB
# =========================
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  dbname   = "system",
  username = "root",
  password = "ssbwarcq",
  host     = "127.0.0.1",
  port     = 3306
)

# =========================
# Helpers: MIME / data URL / W×H do BLOB (fornecidos por você)
# =========================
detect_mime <- function(raw) {
  if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
  if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return("image/png")
  "application/octet-stream"
}
to_data_url <- function(raw) {
  paste0("data:", detect_mime(raw), ";base64,", base64encode(raw))
}
be32 <- function(v) {
  sum(as.integer(v) * c(256^3, 256^2, 256, 1))
}
png_dims <- function(raw) {
  if (length(raw) < 24) return(c(NA_integer_, NA_integer_))
  if (!all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return(c(NA_integer_, NA_integer_))
  w <- be32(raw[17:20]); h <- be32(raw[21:24])
  c(w, h)
}
jpeg_dims <- function(raw) {
  n <- length(raw)
  i <- 3L
  while (i + 8L <= n) {
    if (raw[i] != as.raw(0xFF)) { i <- i + 1L; next }
    marker <- as.integer(raw[i + 1L])
    if (marker == 0xD8 || marker == 0xD9) { i <- i + 2L; next }
    if (i + 3L > n) break
    len <- as.integer(raw[i + 2L]) * 256L + as.integer(raw[i + 3L])
    if (len < 2L) break
    if (marker >= 0xC0 && marker <= 0xC3) {
      if (i + 7L > n) break
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

# =========================
# UI
# =========================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .controls { display:flex; gap:.5rem; flex-wrap:wrap; align-items:center; margin:.5rem 0 }
      .timebar { margin:.25rem 0 }
      #map { border:1px solid #ddd }
      .softcard { border:1px solid #e9ecef; border-radius:12px; padding:12px; background:#fff }
    ")),
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
    "))
  ),
  
  titlePanel("Leaflet + Player (frames do DB on-demand, W/H padrão 512)"),
  sidebarLayout(
    sidebarPanel(
      div(class="softcard",
      fluidRow(
        column(6, datetimePickerInput("dt0", "Início (UTC)", value = Sys.time()-hours(1))),
        column(6, datetimePickerInput("dt1", "Fim (UTC)",    value = Sys.time()))
      ),
      selectizeInput("cam_ids", "Câmeras", choices = NULL, multiple = TRUE),
      actionButton("load_seq", "Carregar sequência", class = "btn btn-primary"),
      hr(),
      numericInput("step_ms", "Intervalo (ms) do Play", value = 120, min = 1, step = 1),
      helpText("Play avança 1 frame a cada 'Intervalo (ms)'.")
    )
  ),
  mainPanel(
    leafletOutput("map", height = 600),
    div(class="timebar",
    "Frame: ", textOutput("pos_txt", inline = TRUE),
    " | Tempo: ", textOutput("ts_txt", inline = TRUE),
    " | Câmera: ", textOutput("cam_txt", inline = TRUE)
  ),
  div(class="controls",
  actionButton("play",  "▶️ Play"),
  actionButton("pause", "⏸ Pause"),
  actionButton("prevFrame",  "⟵ Prev"),
  actionButton("nextFrame",  "Next ⟶")
)
)
)
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  # Preenche choices de câmeras (todas disponíveis)
  cams <- tryCatch(
    DBI::dbGetQuery(con, "SELECT DISTINCT CD_ID_CAMERA FROM FRAME_CAMERA ORDER BY CD_ID_CAMERA"),
    error = function(e) data.frame(CD_ID_CAMERA = integer())
  )
  updateSelectizeInput(session, "cam_ids", choices = cams$CD_ID_CAMERA, selected = head(cams$CD_ID_CAMERA, 1))
  
  # Estado do player
  rv <- reactiveValues(
    seq = NULL,        # data.frame com colunas: CD_ID_CAMERA, DT_HR_LOCAL
    i   = 1L,          # índice corrente
    play_flag = FALSE,
    w = 512L, h = 512L # dimensões atuais do overlay
  )
  
  # Cache simples (dataURL por chave 'cam|ts')
  cache <- new.env(parent = emptyenv())
  key_of <- function(cam, ts) sprintf("%s|%s", as.character(cam), format(ts, "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
  
  # Mapa base com CRS Simple
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      crs = leafletCRS(crsClass = "L.CRS.Simple"),
      zoomSnap = 0, zoomDelta = 0.25
    )) |>
    addDrawToolbar(
      targetGroup = "draw",
      polylineOptions      = FALSE,
      circleMarkerOptions  = FALSE,
      markerOptions        = FALSE,
      polygonOptions = drawPolygonOptions(
        shapeOptions = drawShapeOptions(fillOpacity = 0.2, weight = 2),
        showArea = FALSE
      ),
      rectangleOptions = drawRectangleOptions(
        shapeOptions = drawShapeOptions(fillOpacity = 0.2, weight = 2),
        showArea = FALSE
      ),
      circleOptions = FALSE,
      editOptions = editToolbarOptions(
        selectedPathOptions = selectedPathOptions()
      )
    )
  })
  
  # Textos de status
  output$pos_txt <- renderText({
    if (is.null(rv$seq) || nrow(rv$seq) == 0) return("--/--")
    sprintf("%d / %d", rv$i, nrow(rv$seq))
  })
  output$ts_txt <- renderText({
    if (is.null(rv$seq) || nrow(rv$seq) == 0) return("--")
    format(rv$seq$DT_HR_LOCAL[rv$i], "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  })
  output$cam_txt <- renderText({
    if (is.null(rv$seq) || nrow(rv$seq) == 0) return("--")
    as.character(rv$seq$CD_ID_CAMERA[rv$i])
  })
  
  # Busca BLOB para (cam, ts) e devolve dataURL (com cache)
  fetch_dataurl <- function(cam, ts) {
    k <- key_of(cam, ts)
    if (exists(k, envir = cache, inherits = FALSE)) return(get(k, envir = cache, inherits = FALSE))
    
    # Busca BLOB
    sql <- "SELECT DATA_FRAME FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL = ? LIMIT 1"
    res <- DBI::dbGetQuery(con, sql, params = list(as.integer(cam), as.POSIXct(ts, tz = "UTC")))
    if (!nrow(res) || is.null(res$DATA_FRAME[[1]])) return(NULL)
    
    raw <- res$DATA_FRAME[[1]]
    
    # Se ainda estamos com 512x512, tenta detectar no PRIMEIRO frame do play (opcional)
    if (rv$i == 1L) {
      wh <- img_dims(raw)
      if (is.finite(wh[1]) && is.finite(wh[2]) && !any(is.na(wh))) {
        rv$w <- as.integer(wh[1]); rv$h <- as.integer(wh[2])
      } else {
        rv$w <- 512L; rv$h <- 512L
      }
    }
    
    uri <- to_data_url(raw)
    assign(k, uri, envir = cache)
    uri
  }
  
  # Renderiza frame corrente (baixa on-demand e atualiza overlay)
  render_current <- function(fit_bounds = FALSE) {
    req(rv$seq, nrow(rv$seq) > 0)
    cur <- rv$seq[rv$i, ]
    uri <- fetch_dataurl(cur$CD_ID_CAMERA, cur$DT_HR_LOCAL)
    if (is.null(uri)) {
      #showNotification("Frame sem dados (BLOB não encontrado).", type = "error")
      return(invisible())
    }
    session$sendCustomMessage("set_frame_simple", list(
      url = uri,
      w   = as.integer(rv$w),
      h   = as.integer(rv$h),
      fit = isTRUE(fit_bounds)
    ))
  }
  
  # Carrega sequência (somente índice — sem blobs)
  observeEvent(input$load_seq, {
   # req(input$cam_ids, input$dt0, input$dt1)
    
    # time_begin <- as.POSIXct(input$dt0, tz = "UTC")
    # time_end   <- as.POSIXct(input$dt1, tz = "UTC")
    # cam_vec    <- as.integer(input$cam_ids)
    
    # if (!length(cam_vec)) {
    #   showNotification("Selecione ao menos uma câmera.", type = "warning")
    #   return(invisible())
    # }
    
    # qry <- paste0(
    #   "SELECT DT_HR_LOCAL, CD_ID_CAMERA ",
    #   "FROM FRAME_CAMERA ",
    #   "WHERE DT_HR_LOCAL BETWEEN ? AND ? ",
    #   "AND CD_ID_CAMERA IN (", paste(rep("?", length(cam_vec)), collapse=","), ") ",
    #   "ORDER BY DT_HR_LOCAL ASC"
    # )
    # params <- c(list(time_begin, time_end), as.list(cam_vec))
    # frames_idx <- tryCatch(DBI::dbGetQuery(con, qry, params = params),
    # error = function(e) { message(e$message); data.frame() })
    frames_idx <- DBI::dbGetQuery(con,"SELECT 
                                        DT_HR_LOCAL,
                                        CD_ID_CAMERA 
                                        FROM frame_camera 
                                        WHERE DT_HR_LOCAL BETWEEN '2025-10-18 10:25:00' AND '2025-10-19 10:25:00' AND CD_ID_CAMERA IN (1) ORDER BY DT_HR_LOCAL ASC")
    
    if (!nrow(frames_idx)) {
      showNotification("Nenhum frame no intervalo/câmeras selecionados.", type = "warning")
      rv$seq <- NULL; rv$i <- 1L; rv$play_flag <- FALSE
      return(invisible())
    }
    
    # Normaliza tipos
    frames_idx$DT_HR_LOCAL <- as.POSIXct(frames_idx$DT_HR_LOCAL, tz = "UTC")
    
    # Zera estado, cache e W/H (512 padrão, pode ser ajustado no 1º BLOB)
    rm(list = ls(envir = cache), envir = cache)
    rv$seq <- frames_idx
    rv$i   <- 1L
    rv$play_flag <- FALSE
    rv$w <- 512L; rv$h <- 512L
    
    # Renderiza primeiro frame e ajusta bounds
    render_current(fit_bounds = TRUE)
  })
  
  # Step (Prev/Next)
  step_frame <- function(delta) {
    req(rv$seq, nrow(rv$seq) > 0)
    new_i <- rv$i + delta
    new_i <- max(1L, min(nrow(rv$seq), new_i))
    changed <- (new_i != rv$i)
    rv$i <- new_i
    if (changed) render_current(fit_bounds = FALSE)
  }
  observeEvent(input$prevFrame, { step_frame(-1L) })
  observeEvent(input$nextFrame, { step_frame(+1L) })
  
  # Play/Pause
  observeEvent(input$play,  { rv$play_flag <- TRUE  })
  observeEvent(input$pause, { rv$play_flag <- FALSE })
  
  # Timer do Play
  tick <- reactiveTimer(50)  # base rápida; respeitamos step_ms manualmente
  last <- reactiveVal(Sys.time())
  observe({
    tick()
    req(rv$play_flag, !is.null(rv$seq), nrow(rv$seq) > 0)
    dt <- as.numeric(difftime(Sys.time(), last(), units = "secs")) * 1000
    if (dt >= input$step_ms) {
      if (rv$i >= nrow(rv$seq)) {
        rv$play_flag <- FALSE
      } else {
        step_frame(+1L)
      }
      last(Sys.time())
    }
  })
}

shinyApp(ui, server)
