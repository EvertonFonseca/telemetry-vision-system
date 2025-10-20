# app.R — Leaflet + Player (DB on-demand por (cam,ts); cache batch + LRU; W/H padrão 512)

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
library(later)

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

  titlePanel("Leaflet + Player (frames do DB on-demand, cache batch + LRU)"),
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

  # ---------- Parâmetros de cache/prefetch ----------
  PREFETCH_AHEAD   <- 16L   # quantos frames antecipar
  MAX_CACHE_ITEMS  <- 400L  # limite LRU de itens (dataURLs) na sessão

  # ---------- Lista de câmeras ----------
  cams <- tryCatch(
    DBI::dbGetQuery(con, "SELECT DISTINCT CD_ID_CAMERA FROM FRAME_CAMERA ORDER BY CD_ID_CAMERA"),
    error = function(e) data.frame(CD_ID_CAMERA = integer())
  )
  updateSelectizeInput(session, "cam_ids", choices = cams$CD_ID_CAMERA, selected = head(cams$CD_ID_CAMERA, 1))

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

  # ---------- Mapa base ----------
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

  # ---------- Status ----------
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

  # ---------- Fetch individual (cam, ts) -> dataURL (usa cache) ----------
  fetch_dataurl_single <- function(cam, ts, update_wh_if_first = FALSE) {
    k <- key_of(cam, ts)
    hit <- cache_get(k)
    if (!is.null(hit)) return(hit)

    sql <- "SELECT DATA_FRAME FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL = ? LIMIT 1"
    res <- DBI::dbGetQuery(con, sql, params = list(as.integer(cam), as.POSIXct(ts, tz = "UTC")))
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
      res <- tryCatch(DBI::dbGetQuery(con, sql, params = params),
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

  # ---------- Carrega sequência (somente índice) ----------
  observeEvent(input$load_seq, {
    # (troque pelo seu filtro real se quiser)
    frames_idx <- DBI::dbGetQuery(con,"
      SELECT DT_HR_LOCAL, CD_ID_CAMERA
      FROM frame_camera
      WHERE DT_HR_LOCAL BETWEEN '2025-10-18 10:25:00' AND '2025-10-19 10:25:00'
        AND CD_ID_CAMERA IN (1)
      ORDER BY DT_HR_LOCAL ASC
    ")

    if (!nrow(frames_idx)) {
      showNotification("Nenhum frame no intervalo/câmeras selecionados.", type = "warning")
      rv$seq <- NULL; rv$i <- 1L; playing(FALSE); loop_on <<- FALSE
      return(invisible())
    }

    frames_idx$DT_HR_LOCAL <- as.POSIXct(frames_idx$DT_HR_LOCAL, tz = "UTC")

    cache_clear()
    rv$seq <- frames_idx
    rv$i   <- 1L
    rv$w <- 512L; rv$h <- 512L

    # garante pausado ao carregar
    playing(FALSE); loop_on <<- FALSE

    render_current(fit_bounds = TRUE)
    prefetch_ahead_batch(PREFETCH_AHEAD)
  })

  # ---------- Step (Prev/Next) ----------
  step_frame <- function(delta) {
    req(rv$seq, nrow(rv$seq) > 0)
    new_i <- rv$i + delta
    new_i <- max(1L, min(nrow(rv$seq), new_i))
    if (new_i != rv$i) {
      rv$i <- new_i
      render_current(fit_bounds = FALSE)
      prefetch_ahead_batch(PREFETCH_AHEAD)
    }
  }
  observeEvent(input$prevFrame, { step_frame(-1L) })
  observeEvent(input$nextFrame, { step_frame(+1L) })

  # ---------- Loop de Play com later (sem reentrância) ----------
  play_step <- function() {
    # Garante um domínio reativo (o da sessão) durante a execução
    withReactiveDomain(session, {
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
        later::later(function() {
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

shinyApp(ui, server)
