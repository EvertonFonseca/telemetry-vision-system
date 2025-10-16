
# app.R — Leaflet + Frames do MariaDB por intervalo de DT_HR_LOCAL
options(shiny.maxRequestSize = 2048 * 1024^2)

library(shiny)
library(leaflet)
library(leaflet.extras)
library(DBI)
library(RMariaDB)
library(dplyr)
library(lubridate)
library(base64enc)
library(jsonlite)
library(pool)
library(shinyDatetimePickers)

# ========== CONFIG DB (AJUSTE AQUI) ==========
con <-DBI::dbConnect(
        RMariaDB::MariaDB(),
        dbname = 'system',
        username = 'root',
        password = 'ssbwarcq',
        host = '127.0.0.1',
        port = 3306
      )

# -------- helper: detecta mime e converte blob -> data URL --------
detect_mime <- function(raw) {
  if (length(raw) >= 3 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
  if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return("image/png")
  "application/octet-stream"
}
to_data_url <- function(raw) {
  paste0("data:", detect_mime(raw), ";base64,", base64encode(raw))
}

# -------- helper: busca frames por root_id e intervalo --------
fetch_frames <- function(root_id, t0, t1, limit = 4000L) {
  sql <- "
    SELECT DT_HR_LOCAL, DATA_FRAME
    FROM FRAME_CAMERA
    WHERE DT_HR_LOCAL BETWEEN ? AND ?
    ORDER BY DT_HR_LOCAL ASC
    LIMIT ?
  "
  rs <- dbSendQuery(con, sql)
  on.exit(try(dbClearResult(rs), silent = TRUE), add = TRUE)
  dbBind(rs,c(t0,t1,as.integer(limit)))
  df <- dbFetch(rs)
  if (!nrow(df)) return(tibble(ts = as.POSIXct(character()), data = character()))

  tibble(
    ts   = as.POSIXct(df$DT_HR_LOCAL, tz = "UTC"),
    data = vapply(df$DATA_FRAME, to_data_url, character(1))
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .controls { display:flex; gap:.5rem; flex-wrap:wrap; align-items:center; margin:.5rem 0 }
      .timebar { margin:.25rem 0; }
      #map { border:1px solid #ddd; }
      .softcard { border:1px solid #e9ecef; border-radius:12px; padding:12px; background:#fff; }
    ")),
    # --- Player JS: imageOverlay controlado por frames vindos do R ---
    tags$script(HTML("
(function(){
  let frames = [];   // data URLs
  let times  = [];   // ISO strings (UTC)
  let idx    = 0;
  let timer  = null;
  let playing= false;
  let fps    = 12;   // taxa base para step se necessário
  let mapRef = null;
  let imgLayer = null;
  let bounds = null;
  let t0 = null, t1 = null, durationSec = 0;

  function isoToDate(s){ return s ? new Date(s) : null; }
  function fmtNow(){
    if(!frames.length) return '--';
    const d = new Date(times[idx]);
    return d.toLocaleString('pt-BR', { timeZone: Intl.DateTimeFormat().resolvedOptions().timeZone });
  }

  function nearestIndexByTime(tSec){
    // tSec: segundos desde t0
    if(!times.length || !t0) return 0;
    const target = t0.getTime()/1000 + tSec;
    // busca binária simples
    let lo=0, hi=times.length-1, best=0;
    while(lo <= hi){
      let mid = (lo+hi)>>1;
      const cur = new Date(times[mid]).getTime()/1000;
      if (cur <= target){ best = mid; lo = mid+1; } else hi = mid-1;
    }
    return best;
  }

  function updateImage(){
    if(!imgLayer || !frames.length) return;
    imgLayer.setUrl(frames[idx]);
    // atualiza seek/timestamp
    const elSeek = document.getElementById('seek');
    const elNow  = document.getElementById('t_now');
    if(elSeek && durationSec){
      const curSec = (new Date(times[idx]).getTime() - t0.getTime())/1000;
      elSeek.value = String(curSec);
    }
    if(elNow) elNow.textContent = fmtNow();
  }

  function stop(){ playing=false; if(timer){ clearInterval(timer); timer=null; } }
  function play(){
    if(!frames.length || !durationSec) return;
    if(playing) return;
    playing = true;
    // usa deltas reais entre timestamps para timing mais fiel
    const tick = ()=>{
      const next = Math.min(idx+1, frames.length-1);
      if(next === idx){ stop(); return; }
      const curT = new Date(times[idx]).getTime();
      const nxtT = new Date(times[next]).getTime();
      const dtms = Math.max(1, nxtT - curT);
      idx = next;
      updateImage();
      timer = setTimeout(()=>{ if(playing) tick(); }, dtms);
    };
    tick();
  }

  // API global para o Shiny chamar
  window.FRAMES_API = {
    initLeafletHandle(mapId){
      // chamado pelo onRender para registrar o map e o imageOverlay
      mapRef = mapId ? mapId : null;
    },
    load(payload){
      frames = payload.frames || [];
      times  = payload.times  || [];
      t0     = payload.t0     ? new Date(payload.t0) : null;
      t1     = payload.t1     ? new Date(payload.t1) : null;
      durationSec = (t0 && t1) ? Math.max(0, (t1.getTime()-t0.getTime())/1000) : 0;
      idx = 0;
      stop();
      // prepara overlay se necessário
      const map = HTMLWidgets.find('#map').getMap();
      if(map){
        if(!bounds) bounds = [[0,0],[payload.h||720, payload.w||1280]];
        if(!imgLayer){
          imgLayer = L.imageOverlay(frames.length?frames[0]:'', bounds, {opacity:1}).addTo(map);
          map.fitBounds(bounds);
        } else {
          imgLayer.setUrl(frames.length?frames[0]:'');
        }
      }
      // ajusta UI
      const elSeek = document.getElementById('seek');
      const elDur  = document.getElementById('t_dur');
      if(elSeek && durationSec){
        elSeek.min = '0'; elSeek.max = String(durationSec); elSeek.step = '0.001'; elSeek.value = '0';
      }
      if(elDur){
        const df = new Date(t1);
        elDur.textContent = frames.length ? df.toLocaleString('pt-BR') : '--';
      }
      const elNow = document.getElementById('t_now');
      if(elNow) elNow.textContent = frames.length ? (new Date(times[0])).toLocaleString('pt-BR') : '--';
      updateImage();
    },
    play, pause: stop,
    step(n){
      if(!frames.length) return;
      idx = Math.min(Math.max(0, idx + (n|0)), frames.length-1);
      stop(); updateImage();
    },
    seekToSeconds(sec){
      if(!frames.length || !t0) return;
      const i = nearestIndexByTime(Number(sec)||0);
      idx = i; stop(); updateImage();
    },
    setFPS(x){ fps = Math.max(1, Number(x)||12); }
  };

  // Handlers vindos do server
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('frames_cmd', function(msg){
      if(!msg || !window.FRAMES_API) return;
      if(msg.op === 'play') window.FRAMES_API.play();
      else if(msg.op === 'pause') window.FRAMES_API.pause();
      else if(msg.op === 'step') window.FRAMES_API.step(msg.n||0);
      else if(msg.op === 'seek') window.FRAMES_API.seekToSeconds(msg.t||0);
      else if(msg.op === 'fps') window.FRAMES_API.setFPS(msg.fps||12);
    });
    Shiny.addCustomMessageHandler('frames_payload', function(payload){
      if(window.FRAMES_API && typeof window.FRAMES_API.load === 'function'){
        window.FRAMES_API.load(payload);
      }
    });
  }
})();
    "))
  ),

  titlePanel("Leaflet + Frames do DB (intervalo DT_HR_LOCAL)"),
  sidebarLayout(
    sidebarPanel(
      div(class="softcard",
        # Intervalo de tempo
        fluidRow(
          column(6, datetimePickerInput("dt0", "Início (UTC)", value = Sys.time()-hours(1))),
          column(6, datetimePickerInput("dt1", "Fim (UTC)",    value = Sys.time()))
        ),
        numericInput("limit", "Máx. frames", value = 1200, min = 10, step = 10),
        actionButton("load_db", "Carregar do DB", class = "btn btn-primary"),
        hr(),
        numericInput("fps", "FPS base p/ step", value = 12, min = 1, step = 1),
        helpText("O play usa os timestamps reais entre frames. O FPS é usado para saltos de step se necessário.")
      )
    ),
    mainPanel(
      leafletOutput("map", height = 600),
      div(class="timebar",
          "Atual: ", tags$span(id="t_now","--"),
          " / Fim: ", tags$span(id="t_dur","--")
      ),
      div(class="controls",
          actionButton("play",  "▶️ Play"),
          actionButton("pause", "⏸ Pause"),
          actionButton("back",  "⟵ 1 frame"),
          actionButton("fwd",   "1 frame ⟶"),
          actionButton("back10","⟵ 10 frames"),
          actionButton("fwd10", "10 frames ⟶"),
          sliderInput("seek", "Posição (s desde início)", min = 0, max = 1, value = 0, step = 0.001, width = "100%")
      ),
      tags$hr(),
      h4("Anotações (GeoJSON + timestamp relativo)"),
      verbatimTextOutput("annots_out", placeholder = TRUE)
    )
  )
)

server <- function(input, output, session) {

  timeDelta <- reactiveTimer(100)
  running   <- FALSE
  observeEvent(timeDelta(),{
     req(running)
     step_cmd(1)
  },ignoreInit = TRUE)

  rv <- reactiveValues(
    frames = tibble(ts = as.POSIXct(character()), data = character()),
    t0 = NULL,
    t1 = NULL,
    w = 1280,
    h = 720
  )

  # Mapa base (CRS simples; bounds definidos quando carregar frames)
  output$map <- renderLeaflet({
    leaflet(
      options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))
    ) |>
      addMapPane("imgPane", zIndex = 200)
  })

  # Carregar frames do DB
  observeEvent(input$load_db, {
    req(input$dt0, input$dt1)
    showModal(modalDialog(
      "Carregando frames...",
      footer = NULL,
      easyClose = FALSE
    ))
    on.exit(removeModal(), add = TRUE)

    t0 <- as.POSIXct(input$dt0, tz = "UTC")
    t1 <- as.POSIXct(input$dt1, tz = "UTC")
    df <- fetch_frames(input$root_id, t0, t1, limit = input$limit)
    rv$frames <- df
    rv$t0 <- if (nrow(df)) df$ts[1] else t0
    rv$t1 <- if (nrow(df)) df$ts[nrow(df)] else t1

    # Envia payload para o browser (frames + tempos + dimensões)
    session$sendCustomMessage(
      "frames_payload",
      list(
        frames = unname(df$data),
        times = unname(format(df$ts, "%Y-%m-%dT%H:%M:%OSZ")),
        t0 = format(rv$t0, "%Y-%m-%dT%H:%M:%OSZ"),
        t1 = format(rv$t1, "%Y-%m-%dT%H:%M:%OSZ"),
        w = rv$w,
        h = rv$h
      )
    )
  })

  # Controles (play/pause/seek/step)
  observeEvent(input$play, {
    running  <<- TRUE
    #session$sendCustomMessage("frames_cmd", list(op = "play"))
  })
  observeEvent(input$pause, {
    running  <<- FALSE
    #session$sendCustomMessage("frames_cmd", list(op = "pause"))
  })
  observeEvent(input$seek, {
     running  <<- FALSE
    session$sendCustomMessage("frames_cmd", list(op = "seek", t = input$seek))
  })

  step_cmd <- function(n) {
    session$sendCustomMessage("frames_cmd", list(op = "step", n = n))
  }
  observeEvent(input$back, {
    running  <<- FALSE
    step_cmd(-1)
  })
  observeEvent(input$fwd, {
    running  <<- FALSE
    step_cmd(1)
  })
  observeEvent(input$back10, {
    running  <<- FALSE
    step_cmd(-10)
  })
  observeEvent(input$fwd10, {
    running  <<- FALSE
    step_cmd(10)
  })
  observeEvent(input$fps, {
    session$sendCustomMessage("frames_cmd", list(op = "fps", fps = input$fps))
  })

  # Anotações no tempo atual (tempo relativo calculado no browser/seek)
  annots <- reactiveVal(list())
  observeEvent(input$map_draw_new_feature, {
    # aqui usamos o valor atual do slider como “tempo relativo” (s)
    rel <- isolate(input$seek) %||% 0
    feats <- annots()
    feats[[length(feats) + 1]] <- list(
      time_rel_s = rel,
      feature = input$map_draw_new_feature
    )
    annots(feats)
  })
  output$annots_out <- renderText({
    x <- annots()
    if (!length(x)) {
      return("Nenhuma anotação ainda.")
    }
    toJSON(x, auto_unbox = TRUE, pretty = TRUE)
  })
}

shinyApp(ui, server)
