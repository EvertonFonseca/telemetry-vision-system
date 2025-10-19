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
  db = .. / logic / database,
  .. / logic/objeto_dao[...],
  .. / logic/camera_dao[...],
  .. / logic/setor_dao[...],
  base64enc[...]
)

#' @export
uiNewTreinar <- function(ns,input,output,session,callback){
  
  #open database
  db$tryResetConnection(function(con){
    
    obs       <- newObserve()
    setores   <- selectAllSetors(con)
    objetos   <- selectAllObjetos(con)
    objeto    <- NULL
    
    id       <- ns('dialogObj')
    cssStyle <- list()
    cssStyle[[paste0(' #parent',id,' .modal-dialog')]]  <- paste0('width: 95% !important; height: 90% !important;')
    cssStyle[[paste0(' #parent',id,' .modal-content')]] <- paste0('width: 100% !important; height: 100% !important;')
    cssStyle[[paste0(' #parent',id,' .modal-body')]]    <- paste0('width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto;')
    
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
 
      debugLocal(function(x){ 1 + 1})
      objeto      <<- objetos |> filter(NAME_OBJETO == isolate(input$comboObjeto))
      time_begin  <- as.POSIXct(isolate(input$datetimeBegin),tz = "UTC")
      time_end    <- as.POSIXct(isolate(input$datetimeEnd),tz = "UTC")
      componentes <- objeto$CONFIG[[1]]$COMPONENTES[[1]]
      camera_id   <- paste0(unique(componentes$CD_ID_CAMERA),collapse = ",")

      con    <- db$newConnection()
      frames <- DBI::dbGetQuery(con,paste0(
        "SELECT 
          DT_HR_LOCAL,
          CD_ID_CAMERA 
          FROM frame_camera 
          WHERE DT_HR_LOCAL BETWEEN '",time_begin,"' AND '",time_end,"' AND CD_ID_CAMERA IN (",camera_id,")"))
       # as.POSIXct(dados$DT_HR_LOCAL,tz = Sys.timezone())
      df  <- fetch_frames(con,t0 = time_begin,t1 = time_end)
      objeto
      cameras <- purrr::map_df(objetosCONFIGS[[1]])

    },ignoreInit = TRUE))


  })
}
      
uiMain <- function(ns,setores,objetos,valueComboSetor = NULL,valueObjeto = NULL){
  
  div(
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
  ))
  
}

serverFrame <- function(input, output, session) {

  timeDelta <- reactiveTimer(100)
  running   <- FALSE
  observeEvent(timeDelta(), {
    req(running)
    step_cmd(1)
  }, ignoreInit = TRUE)

  rv <- reactiveValues(
    frames = tibble::tibble(ts = as.POSIXct(character()), data = character(), raw = list()),
    t0 = NULL,
    t1 = NULL,
    w = 1280,
    h = 720
  )

  # Mapa base (CRS simples)
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) |>
      addMapPane("imgPane", zIndex = 200)
  })

  # Carregar frames do DB
  observeEvent(input$load_db, {
    req(input$datetimeBegin, input$datetimeEnd)
    showModal(modalDialog("Carregando frames...", footer = NULL, easyClose = FALSE))
    on.exit(removeModal(), add = TRUE)

    t0 <- as.POSIXct(input$datetimeBegin, tz = "UTC")
    t1 <- as.POSIXct(input$datetimeEnd, tz = "UTC")
   
    df <- fetch_frames(input$root_id, t0, t1, limit = input$limit)
    rv$frames <- df
    rv$t0 <- if (nrow(df)) df$ts[1] else t0
    rv$t1 <- if (nrow(df)) df$ts[nrow(df)] else t1

    # === NOVO: mede w/h do primeiro frame de fato ===
    if (nrow(df) > 0 && length(df$raw[[1]]) > 0) {
      wh <- img_dims(df$raw[[1]])
      if (is.finite(wh[1]) && is.finite(wh[2]) && !any(is.na(wh))) {
        rv$w <- as.integer(wh[1]); rv$h <- as.integer(wh[2])
      } else {
        rv$w <- 1280L; rv$h <- 720L
      }
    } else {
      rv$w <- 1280L; rv$h <- 720L
    }

    # Envia payload para o browser (frames + tempos + dimensões reais)
    session$sendCustomMessage(
      "frames_payload",
      list(
        frames = unname(df$data),
        times  = unname(format(df$ts, "%Y-%m-%dT%H:%M:%OSZ")),
        t0     = format(rv$t0, "%Y-%m-%dT%H:%M:%OSZ"),
        t1     = format(rv$t1, "%Y-%m-%dT%H:%M:%OSZ"),
        w      = rv$w,
        h      = rv$h
      )
    )
  })

  # Controles (play/pause/seek/step)
  observeEvent(input$play,  { running <<- TRUE  })
  observeEvent(input$pause, { running <<- FALSE })
  observeEvent(input$seek,  { running <<- FALSE; session$sendCustomMessage("frames_cmd", list(op = "seek", t = input$seek)) })

  step_cmd <- function(n) {
    session$sendCustomMessage("frames_cmd", list(op = "step", n = n))
  }
  observeEvent(input$back,   { running <<- FALSE; step_cmd(-1) })
  observeEvent(input$fwd,    { running <<- FALSE; step_cmd( 1) })
  observeEvent(input$back10, { running <<- FALSE; step_cmd(-10) })
  observeEvent(input$fwd10,  { running <<- FALSE; step_cmd( 10) })
  observeEvent(input$fps,    { session$sendCustomMessage("frames_cmd", list(op = "fps", fps = input$fps)) })

}

uiSequenciaFrames <- function(ns){
  
  tagList(
    tags$head(
      tags$style(HTML("
      .controls { display:flex; gap:.5rem; flex-wrap:wrap; align-items:center; margin:.5rem 0 }
      .timebar { margin:.25rem 0; }
      #map { border:1px solid #ddd; }
      .softcard { border:1px solid #e9ecef; border-radius:12px; padding:12px; background:#fff; }
    ")),
      # --- Player JS (usa payload.w/payload.h p/ bounds) ---
      tags$script(HTML("
      (function(){
        let frames = [];   // data URLs
        let times  = [];   // ISO strings (UTC)
        let idx    = 0;
        let timer  = null;
        let playing= false;
        let fps    = 12;
        let imgLayer = null;
        let bounds = null;
        let t0 = null, t1 = null, durationSec = 0;

        function fmtNow(){
          if(!frames.length) return '--';
          const d = new Date(times[idx]);
          return d.toLocaleString('pt-BR', { timeZone: Intl.DateTimeFormat().resolvedOptions().timeZone });
        }
        function nearestIndexByTime(tSec){
          if(!times.length || !t0) return 0;
          const target = t0.getTime()/1000 + tSec;
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
          const elSeek = document.getElementById('seek');
          const elNow  = document.getElementById('t_now');
          if(elSeek && durationSec){
            const curSec = (new Date(times[idx]).getTime() - t0.getTime())/1000;
            elSeek.value = String(curSec);
          }
          if(elNow) elNow.textContent = fmtNow();
        }
        function stop(){ playing=false; if(timer){ clearTimeout(timer); timer=null; } }
        function play(){
          if(!frames.length || !durationSec) return;
          if(playing) return;
          playing = true;
          const tick = ()=>{
            const next = Math.min(idx+1, frames.length-1);
            if(next === idx){ stop(); return; }
            const curT = new Date(times[idx]).getTime();
            const nxtT = new Date(times[next]).getTime();
            let dtms = nxtT - curT;
            if(!Number.isFinite(dtms) || dtms < 1) dtms = Math.max(1, Math.round(1000/Math.max(1,fps)));
            idx = next;
            updateImage();
            timer = setTimeout(()=>{ if(playing) tick(); }, dtms);
          };
          tick();
        }

        window.FRAMES_API = {
          load(payload){
            frames = payload.frames || [];
            times  = payload.times  || [];
            t0     = payload.t0 ? new Date(payload.t0) : (times.length? new Date(times[0]) : null);
            t1     = payload.t1 ? new Date(payload.t1) : (times.length? new Date(times[times.length-1]) : null);
            durationSec = (t0 && t1) ? Math.max(0, (t1.getTime()-t0.getTime())/1000) : 0;
            idx = 0;
            stop();

            const map = HTMLWidgets.find('#map').getMap();
            if(map){
              const h = payload.h || 720, w = payload.w || 1280;
              bounds = [[0,0],[h, w]];   // respeita dimensões do frame
              if(!imgLayer){
                imgLayer = L.imageOverlay(frames.length?frames[0]:'', bounds, {opacity:1}).addTo(map);
                map.fitBounds(bounds);
              } else {
                imgLayer.setUrl(frames.length?frames[0]:'');
                imgLayer.setBounds(bounds);
                map.fitBounds(bounds);
              }
            }
            const elSeek = document.getElementById('seek');
            const elDur  = document.getElementById('t_dur');
            if(elSeek && durationSec){ elSeek.min='0'; elSeek.max=String(durationSec); elSeek.step='0.001'; elSeek.value='0'; }
            if(elDur){ elDur.textContent = frames.length ? (new Date(times.at(-1))).toLocaleString('pt-BR') : '--'; }
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
    mainPanel(
      leafletOutput(ns("map"), height = 512),
      div(class="timebar",
      "Atual: ", tags$span(id=ns("t_now"),"--"),
      " / Fim: ", tags$span(id=ns("t_dur"),"--")
    ),
    div(class="controls",
    actionButton(ns("play"),  "▶️ Play"),
    actionButton(ns("pause"), "⏸ Pause"),
    actionButton(ns("back"),  "⟵ 1 frame"),
    actionButton(ns("fwd"),   "1 frame ⟶"),
    actionButton(ns("back10"),"⟵ 10 frames"),
    actionButton(ns("fwd10"), "10 frames ⟶"),
    sliderInput(ns("seek"), "Posição (s desde início)", min = 0, max = 1, value = 0, step = 0.001, width = "100%")
  )
))
  
}


######## HELPERS ######

# -------- helpers: mime / data URL --------
detect_mime <- function(raw) {
  if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
  if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return("image/png")
  "application/octet-stream"
}
to_data_url <- function(raw) {
  paste0("data:", detect_mime(raw), ";base64,", base64encode(raw))
}

# -------- helpers: extração de dimensões W x H direto do BLOB --------
# PNG: IHDR (bytes 17:24) width/height (big-endian)
be32 <- function(v) {
  sum(as.integer(v) * c(256^3, 256^2, 256, 1))
}
png_dims <- function(raw) {
  if (length(raw) < 24) return(c(NA_integer_, NA_integer_))
  if (!all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return(c(NA_integer_, NA_integer_))
  w <- be32(raw[17:20]); h <- be32(raw[21:24])
  c(w, h)
}
# JPEG: procura SOF0..SOF3
jpeg_dims <- function(raw) {
  n <- length(raw)
  i <- 3L
  while (i + 8L <= n) {
    if (raw[i] != as.raw(0xFF)) { i <- i + 1L; next }
    marker <- as.integer(raw[i + 1L])
    # tamanhos de bloco
    if (marker == 0xD8 || marker == 0xD9) { i <- i + 2L; next }  # SOI/EOI
    if (i + 3L > n) break
    len <- as.integer(raw[i + 2L]) * 256L + as.integer(raw[i + 3L])
    if (len < 2L) break
    # SOF0..SOF3 (baseline, extended, progressive)
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

# -------- helper: busca frames por intervalo --------
# OBS: agora retornamos também a coluna 'raw' (bytes) para medir w/h do 1º frame
fetch_frames <- function(con,t0, t1) {
  sql <- "
    SELECT DT_HR_LOCAL, DATA_FRAME
    FROM FRAME_CAMERA
    WHERE DT_HR_LOCAL BETWEEN ? AND ?
    ORDER BY DT_HR_LOCAL ASC
  "
  rs <- DBI::dbSendQuery(con, sql)
  on.exit(try(DBI::dbClearResult(rs), silent = TRUE), add = TRUE)
  DBI::dbBind(rs, c(t0, t1))
  df <- DBI::dbFetch(rs)
  if (!nrow(df)) {
    return(tibble::tibble(ts = as.POSIXct(character()), data = character(), raw = list()))
  }
  raws <- df$DATA_FRAME
  tibble::tibble(
    ts   = as.POSIXct(df$DT_HR_LOCAL, tz = "UTC"),
    data = vapply(raws, to_data_url, character(1)),
    raw  = as.list(raws)
  )
}
