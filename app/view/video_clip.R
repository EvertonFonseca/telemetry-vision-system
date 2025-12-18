# video_clip.R  ({mirai} async: frames+ffmpeg em daemons)
# ======================================================

box::use(
  shiny[...],
  DBI,
  RMariaDB[MariaDB],
  base64enc,
  later,
  promises,
  mirai,
  parallel,
  stringi,
  shinyjs,
  .. / logic/treinar_dao[fetch_frames, db_fetch_many_frames],  # reusa seu DAO
  dbp = ../infra/db_pool
)

# ---------------------------
# utils locais
# ---------------------------
.vclip_env <- function(session, key = "video_clip_env") {
  if (is.null(session$userData[[key]])) {
    session$userData[[key]] <- new.env(parent = emptyenv())
  }
  session$userData[[key]]
}

.rand_id <- function(n = 10L) {
  paste0(sample(c(letters, 0:9), n, replace = TRUE), collapse = "")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.has_ffmpeg <- function() nzchar(Sys.which("ffmpeg"))

.detect_mime <- function(raw) {
  if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
  if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return("image/png")
  "application/octet-stream"
}

# tenta salvar tudo como JPG pra facilitar o ffmpeg
.write_frame_jpg <- function(raw, file_jpg) {
  mime <- .detect_mime(raw)
  if (identical(mime, "image/jpeg")) {
    writeBin(raw, file_jpg)
    return(TRUE)
  }
  if (identical(mime, "image/png")) {
    if (requireNamespace("magick", quietly = TRUE)) {
      img <- magick::image_read(raw)
      img <- magick::image_convert(img, format = "jpeg")
      magick::image_write(img, path = file_jpg, format = "jpeg")
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  FALSE
}

.chunk <- function(x, size = 500L) {
  if (!length(x)) return(list())
  split(x, ceiling(seq_along(x) / size))
}

.thin_frames <- function(df, max_frames = 3000L) {
  if (!nrow(df)) return(df)
  if (nrow(df) <= max_frames) return(df)
  idx <- unique(round(seq.int(1, nrow(df), length.out = max_frames)))
  df[idx, , drop = FALSE]
}

# ---------------------------
# JS: controle de <video> + reverse
# ---------------------------
.vclip_js <- function() {
  tags$script(HTML("
    (function(){
      const revTimers = {};
      function clearRev(id){
        if(revTimers[id]) { clearInterval(revTimers[id]); delete revTimers[id]; }
      }
      window.vclipPlay = function(id){ clearRev(id); const v=document.getElementById(id); if(v) v.play(); }
      window.vclipPause= function(id){ clearRev(id); const v=document.getElementById(id); if(v) v.pause(); }
      window.vclipPrev = function(id, step){
        clearRev(id);
        const v=document.getElementById(id); if(!v) return;
        v.pause();
        v.currentTime = Math.max(0, v.currentTime - (step||0.1));
      }
      window.vclipNext = function(id, step){
        clearRev(id);
        const v=document.getElementById(id); if(!v) return;
        v.pause();
        v.currentTime = Math.min(v.duration||1e9, v.currentTime + (step||0.1));
      }
      window.vclipReverse = function(id, step){
        clearRev(id);
        const v=document.getElementById(id); if(!v) return;
        v.pause();
        const s = step || 0.1;
        revTimers[id] = setInterval(function(){
          try{
            v.currentTime = Math.max(0, v.currentTime - s);
            if(v.currentTime <= 0){ clearRev(id); }
          }catch(e){ clearRev(id); }
        }, Math.max(10, s*1000));
      }
      Shiny.addCustomMessageHandler('vclip_stop_all', function(ids){
        (ids||[]).forEach(function(id){ clearRev(id); const v=document.getElementById(id); if(v) v.pause(); });
      });
    })();
  "))
}

# ---------------------------
# Semáforo global p/ jobs de clip (por processo)
# ---------------------------
.vclip_sem <- new.env(parent = emptyenv())
.vclip_sem$running     <- 0L
.vclip_sem$max_running <- suppressWarnings(as.integer(Sys.getenv("TVS_MAX_VCLIP", "1")))
if (is.na(.vclip_sem$max_running) || .vclip_sem$max_running < 1L) .vclip_sem$max_running <- 1L

.vclip_try_acquire <- function() {
  r <- suppressWarnings(as.integer(.vclip_sem$running)); if (is.na(r) || r < 0L) r <- 0L
  m <- suppressWarnings(as.integer(.vclip_sem$max_running)); if (is.na(m) || m < 1L) m <- 1L
  if (r >= m) return(FALSE)
  .vclip_sem$running <- r + 1L
  TRUE
}
.vclip_release <- function() {
  r <- suppressWarnings(as.integer(.vclip_sem$running)); if (is.na(r) || r < 0L) r <- 0L
  .vclip_sem$running <- max(0L, r - 1L)
  invisible(TRUE)
}

# ---------------------------
# garante mirai ativo (não “obriga” boot aqui; só tenta se precisar)
# ---------------------------
.mirai_state <- new.env(parent = emptyenv())
.mirai_state$started <- FALSE

.mirai_start <- function() {
  if (isTRUE(getOption("TVS_MIRAI_BOOTED", FALSE))) return(invisible(TRUE))
  if (isTRUE(.mirai_state$started)) return(invisible(TRUE))

  n <- suppressWarnings(as.integer(Sys.getenv("TVS_MIRAI_DAEMONS", "")))
  if (is.na(n) || n < 1L) {
    cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
    if (is.na(cores) || cores < 1L) cores <- 2L
    n <- max(cores - 2L, 1L)
  }

  res <- try(mirai::daemons(n), silent = TRUE)
  if (inherits(res, "try-error")) {
    .mirai_state$started <- FALSE
    return(invisible(FALSE))
  }
  .mirai_state$started <- TRUE
  invisible(TRUE)
}

# ------------------------------------------------------------
#' @export
# video_clip_open (async com mirai)
# - abre modal IMEDIATO (loading)
# - gera MP4 em daemon (DB+frames+ffmpeg)
# - preenche modal quando pronto
# ------------------------------------------------------------
video_clip_open <- function(ns, input, output, session,
                            pool,             # mantém assinatura (não usado no daemon)
                            title,
                            time_begin, time_end,
                            camera_ids,
                            camera_names = NULL,
                            fps = 5L,
                            max_frames = 300L,
                            callback = NULL) {

  stopifnot(length(camera_ids) >= 1)

  if (!.has_ffmpeg()) {
    showNotification("ffmpeg não encontrado no servidor. Instale/adicione ao PATH para gerar MP4.", type = "error")
    return(invisible(NULL))
  }

  e <- .vclip_env(session)

  # ---- estado da sessão (p/ ignorar callback se fechar)
  e$job_done  <- FALSE
  e$alive     <- TRUE
  e$cancelled <- FALSE
  session$onSessionEnded(function() e$alive <- FALSE)

  # cleanup anterior (se já tinha aberto)
  if (!is.null(e$res_prefix)) {
    try(shiny::removeResourcePath(e$res_prefix), silent = TRUE)
  }
  if (!is.null(e$tmp_dir) && dir.exists(e$tmp_dir)) {
    try(unlink(e$tmp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
  }

  e$tmp_dir    <- file.path(tempdir(), paste0("vclip_", .rand_id(12)))
  dir.create(e$tmp_dir, recursive = TRUE, showWarnings = FALSE)

  e$res_prefix <- paste0("vclip_", .rand_id(10))
  shiny::addResourcePath(e$res_prefix, e$tmp_dir)

  # garante cleanup ao fechar sessão
  if (is.null(e$on_end_registered) || !isTRUE(e$on_end_registered)) {
    e$on_end_registered <- TRUE
    session$onSessionEnded(function(){
      try({
        if (!is.null(e$res_prefix)) shiny::removeResourcePath(e$res_prefix)
        if (!is.null(e$tmp_dir) && dir.exists(e$tmp_dir)) unlink(e$tmp_dir, recursive = TRUE, force = TRUE)
      }, silent = TRUE)
    })
  }

  # normaliza tempo
  tb <- as.POSIXct(time_begin, tz = "UTC")
  te <- as.POSIXct(time_end,   tz = "UTC")

  camera_ids <- as.integer(unique(camera_ids))
  if (is.null(camera_names)) {
    camera_names <- vapply(camera_ids, function(cid) paste0("Câmera ", cid), character(1))
  }

  # ids UI
  modal_close_id <- ns("video_clip_close")
  body_id        <- "vclip_body"     # output[[body_id]]
  body_ns        <- ns(body_id)

  # reactive “resultado”
  if (is.null(e$rv)) e$rv <- shiny::reactiveVal(NULL)
  e$rv(list(status = "loading", msg = "Gerando MP4 …"))

  # output que monta o corpo do modal
  output[[body_id]] <- renderUI({
    st <- e$rv()
    if (is.null(st)) return(div())   # <- limpa total

    if (identical(st$status, "loading")) {
      return(
        div(
          .vclip_js(),
          div(style="padding:12px;",
              tags$h4("Gerando clip…", style="margin:0 0 8px 0;"),
              tags$p(st$msg %||% "Aguarde.", style="color:#666; font-size:12px;"),
              div(class="progress", style="height:10px;",
                  div(class="progress-bar progress-bar-striped active", style="width:100%")
              ),
              tags$p(paste0("Jobs de clip neste processo: ",
                            .vclip_sem$running, "/", .vclip_sem$max_running),
                     style="margin-top:10px; color:#888; font-size:11px;")
          )
        )
      )
    }

    if (identical(st$status, "error")) {
      return(
        div(
          .vclip_js(),
          div(style="padding:12px; color:tomato;",
              tags$h4("Erro ao gerar clip"),
              tags$pre(st$error %||% "Falha desconhecida.")
          )
        )
      )
    }

    # ready
    videos <- st$videos
    if (is.null(videos) || !length(videos)) {
      return(
        div(
          .vclip_js(),
          div(style="padding:12px;",
              tags$h4("Sem vídeos para exibir"),
              tags$p("Não foi possível gerar nenhum MP4 (ver ffmpeg/magick/DB).", style="color:tomato;")
          )
        )
      )
    }

    cards <- tagList()
    video_ids <- character(0)

    for (v in videos) {
      cam  <- v$cam
      name <- v$name
      rel  <- v$rel
      vid_id <- ns(paste0("vclip_video_", cam))
      video_ids <- c(video_ids, vid_id)

      dl_id <- paste0("vclip_download_", cam)

      # downloadHandler
      local({
        cam_local <- cam
        abs_local <- v$abs
        output[[dl_id]] <- downloadHandler(
          filename = function() sprintf("clip_cam_%d_%s_%s.mp4",
                                        cam_local,
                                        format(tb, "%Y%m%d_%H%M%S", tz="UTC"),
                                        format(te, "%Y%m%d_%H%M%S", tz="UTC")),
          content = function(file) file.copy(abs_local, file, overwrite = TRUE),
          contentType = "video/mp4"
        )
      })

      step_sec <- 1 / max(1, as.numeric(st$fps %||% 10L))

      cards <- tagAppendChildren(cards,
        column(
          width = 12,
          div(
            style = "border:1px solid #e5e5e5; border-radius:10px; padding:12px; margin-bottom:12px;",
            tags$h4(paste0("🎥 ", name, " (", cam, ")"), style="margin:0 0 10px 0;"),
            # tags$video(
            #   id = vid_id,
            #   src = paste0("/", rel),
            #   controls = NA,
            #   style = "width:100%; max-height:420px; background:#000; border-radius:10px;"
            # ),
            tags$video(
              id = vid_id,
              src = paste0("/", rel),
              controls = NA,
              preload = "metadata",   # <<< não baixa tudo
              playsinline = NA,
              style = "width:100%; max-height:420px; background:#000; border-radius:10px;"
            ),
            br(),
            splitLayout(
              cellWidths = c("auto","auto","auto","auto","auto","auto"),
              tags$button(class="btn btn-outline-secondary btn-sm",
                          onclick = sprintf("vclipReverse('%s', %f)", vid_id, step_sec),
                          icon("backward"), " Reverse"),
              tags$button(class="btn btn-outline-secondary btn-sm",
                          onclick = sprintf("vclipPrev('%s', %f)", vid_id, step_sec),
                          icon("step-backward"), " Prev"),
              tags$button(class="btn btn-outline-secondary btn-sm",
                          onclick = sprintf("vclipPlay('%s')", vid_id),
                          icon("play"), " Play"),
              tags$button(class="btn btn-outline-secondary btn-sm",
                          onclick = sprintf("vclipPause('%s')", vid_id),
                          icon("pause"), " Pause"),
              tags$button(class="btn btn-outline-secondary btn-sm",
                          onclick = sprintf("vclipNext('%s', %f)", vid_id, step_sec),
                          icon("step-forward"), " Next"),
              downloadButton(ns(dl_id), "Download MP4", class = "btn btn-primary btn-sm")
            )
          )
        )
      )
    }

    div(
      .vclip_js(),
      div(
        style = "max-height: 75vh; overflow-x: hidden;",
        tags$p(sprintf("Intervalo: %s → %s",
                       format(tb, "%d/%m/%Y %H:%M:%S", tz= Sys.timezone()),
                       format(te, "%d/%m/%Y %H:%M:%S", tz= Sys.timezone()))),
        fluidRow(cards)
      )
    )
  })

  # abre modal IMEDIATO
  showModal(
    modalDialog(
      title = title %||% "Clip do gráfico",
      size  = "l",
      easyClose = FALSE,
      footer = tagList(
        actionButton(modal_close_id, "Fechar", class = "btn btn-outline-secondary")
      ),
      uiOutput(body_ns)
    )
  )

  # close modal + marca cancelado (NÃO apaga pasta se job ainda rodando)
  observeEvent(input[[ "video_clip_close" ]], {
    e$cancelled <- TRUE
    
    # pede cancelamento do job (se já existe)
    if (!is.null(e$job)) {
      try(mirai::stop_mirai(e$job), silent = TRUE)
    }
    # 1) limpa o estado do UI (evita reaproveitar o HTML antigo)
    if (!is.null(e$rv)) e$rv(NULL)
    session$sendCustomMessage("vclip_stop_all", list())
    if(!is.null(callback)) callback()
    removeModal()
    
    # ✅ cleanup IMEDIATO se job já terminou (ready/error) ou nem chegou a iniciar
    if (isTRUE(e$job_done) || is.null(e$job)) {
      try(shiny::removeResourcePath(e$res_prefix), silent = TRUE)
      try(unlink(e$tmp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
      
      e$res_prefix <- NULL
      e$tmp_dir    <- NULL
    }

  }, ignoreInit = TRUE, once = TRUE)

  # ------------------------------------------------------------
  # dispara job em daemon (mirai)
  # ------------------------------------------------------------
  ok_mirai <- .mirai_start()
  if (!isTRUE(ok_mirai)) {
    e$rv(list(status="error", error="mirai não iniciou. Verifique daemons() no boot / permissões."))
    return(invisible(NULL))
  }

  # se semáforo cheio, re-agenda (não bloqueia)
  .try_start_job <- function() {
    if (!isTRUE(e$alive) || isTRUE(e$cancelled)) return(invisible(NULL))

    if (!.vclip_try_acquire()) {
      e$rv(list(status="loading", msg="Fila cheia neste processo. Vou tentar novamente em instantes…"))
      later::later(.try_start_job, 0.6)
      return(invisible(NULL))
    }

    e$rv(list(status="loading", msg="Gerando MP4 em background …"))

    tmp_dir <- e$tmp_dir
    res_prefix <- e$res_prefix

    # --- JOB em daemon ---
    job <- mirai::mirai({
      suppressMessages(library(DBI))
      suppressMessages(library(RMariaDB))

      # helpers locais (evita depender do ambiente do processo principal)
      detect_mime <- function(raw) {
        if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
        if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return("image/png")
        "application/octet-stream"
      }
      write_frame_jpg <- function(raw, file_jpg) {
        mime <- detect_mime(raw)
        if (identical(mime, "image/jpeg")) { writeBin(raw, file_jpg); return(TRUE) }
        if (identical(mime, "image/png")) {
          if (requireNamespace("magick", quietly = TRUE)) {
            img <- magick::image_read(raw)
            img <- magick::image_convert(img, format = "jpeg")
            magick::image_write(img, path = file_jpg, format = "jpeg")
            return(TRUE)
          } else return(FALSE)
        }
        FALSE
      }
      chunk <- function(x, size = 500L) if (!length(x)) list() else split(x, ceiling(seq_along(x) / size))
      thin_frames <- function(df, max_frames = 3000L) {
        if (!nrow(df)) return(df)
        if (nrow(df) <= max_frames) return(df)
        idx <- unique(round(seq.int(1, nrow(df), length.out = max_frames)))
        df[idx, , drop = FALSE]
      }

      con <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        dbname   = Sys.getenv("DBNAME"),
        host     = Sys.getenv("HOST"),
        port     = Sys.getenv("PORT"),
        username = Sys.getenv("USERNAME"),
        password = Sys.getenv("PASSWORD")
      )
      on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

      # idx frames
      frames_idx <- fetch_frames(
        conn          = con,
        time_begin    = tb,
        time_end      = te,
        camera_id_vec = as.integer(camera_ids)
      )
      if (is.null(frames_idx) || !nrow(frames_idx)) {
        return(list(ok=FALSE, error="Nenhum frame encontrado nesse intervalo."))
      }

      frames_idx <- thin_frames(frames_idx, max_frames = as.integer(max_frames))

      videos <- list()

      for (k in seq_along(camera_ids)) {
        cam <- as.integer(camera_ids[k])
        cam_dir <- file.path(tmp_dir, paste0("cam_", cam))
        dir.create(cam_dir, recursive = TRUE, showWarnings = FALSE)

        df_cam <- frames_idx[frames_idx$CD_ID_CAMERA == cam, , drop = FALSE]
        if (!nrow(df_cam)) next

        ts_vec <- as.POSIXct(df_cam$DT_HR_LOCAL, tz = "UTC")

        raw_list <- list()
        chs <- chunk(ts_vec, size = 300L)
        for (cc in chs) {
          res <- tryCatch(db_fetch_many_frames(con, cam, cc), error = function(e) NULL)
          if (!is.null(res) && nrow(res)) {
            res$DT_HR_LOCAL <- as.POSIXct(res$DT_HR_LOCAL, tz = "UTC")
            raw_list[[length(raw_list) + 1L]] <- res
          }
        }
        if (!length(raw_list)) next
        res_all <- do.call(rbind, raw_list)
        res_all <- res_all[order(res_all$DT_HR_LOCAL), , drop = FALSE]

        ok_any <- FALSE
        for (i in seq_len(nrow(res_all))) {
          raw <- res_all$DATA_FRAME[[i]]
          if (is.null(raw)) next
          fjpg <- file.path(cam_dir, sprintf("frame_%06d.jpg", i))
          ok   <- write_frame_jpg(raw, fjpg)
          if (isTRUE(ok)) ok_any <- TRUE
        }
        if (!ok_any) next

        out_mp4_abs <- file.path(cam_dir, "clip.mp4")

        args <- c(
          "-y",
          "-hide_banner",
          "-loglevel", "error",
          "-framerate", as.character(as.integer(fps)),
          "-i", file.path(cam_dir, "frame_%06d.jpg"),
          
          "-c:v", "libx264",
          "-preset", "ultrafast",
          "-crf", "30", 
          "-pix_fmt", "yuv420p",
          "-movflags", "faststart",
          out_mp4_abs
        )

        suppressWarnings(system2("ffmpeg", args = args, stdout = TRUE, stderr = TRUE))

        if (!file.exists(out_mp4_abs)) next

        out_mp4_rel <- file.path(res_prefix, paste0("cam_", cam), "clip.mp4")

        videos[[length(videos) + 1L]] <- list(
          cam = cam,
          rel = out_mp4_rel,
          abs = out_mp4_abs
        )
      }

      if (!length(videos)) {
        return(list(ok=FALSE, error="Não foi possível gerar nenhum MP4 (ffmpeg/magick/frames)."))
      }

      list(ok=TRUE, videos=videos, fps=as.integer(fps))
    },
    # args serializados
    fetch_frames = fetch_frames,
    db_fetch_many_frames = db_fetch_many_frames,
    tb = tb,
    te = te,
    camera_ids = camera_ids,
    fps = as.integer(fps),
    max_frames = as.integer(max_frames),
    tmp_dir = tmp_dir,
    res_prefix = res_prefix)

    e$job <- job
    # --- callback no Shiny ---
    p <- promises::then(job, function(res) {

       e$job_done <- TRUE
      .vclip_release()
      
      # Se foi cancelado/interrompido: faz cleanup e sai silencioso
      if (mirai::is_mirai_interrupt(res) || mirai::is_error_value(res)) {
        try(shiny::removeResourcePath(e$res_prefix), silent = TRUE)
        try(unlink(e$tmp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
        e$res_prefix <- NULL
        e$tmp_dir <- NULL
        return(invisible(NULL))
      }

      if (!isTRUE(e$alive)) {
        # sessão morreu: limpa e sai
        try(shiny::removeResourcePath(e$res_prefix), silent = TRUE)
        try(unlink(e$tmp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
        e$res_prefix <- NULL
        e$tmp_dir <- NULL
        return(invisible(NULL))
      }

      if (isTRUE(e$cancelled)) {
        # usuário fechou antes de terminar: cleanup agora
        try(shiny::removeResourcePath(e$res_prefix), silent = TRUE)
        try(unlink(e$tmp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
        e$res_prefix <- NULL
        e$tmp_dir <- NULL
        return(invisible(NULL))
      }

      if (is.null(res) || !isTRUE(res$ok)) {
        e$rv(list(status="error", error = res$error %||% "Falha ao gerar clip."))
        return(invisible(NULL))
      }

      # injeta nomes
      vids <- res$videos
      # associa name por cam
      for (i in seq_along(vids)) {
        cam <- vids[[i]]$cam
        idx <- match(cam, camera_ids)
        vids[[i]]$name <- camera_names[[idx]] %||% paste0("Câmera ", cam)
      }

      e$rv(list(status="ready", videos=vids, fps=res$fps))

      invisible(TRUE)
    })

    promises::catch(p, function(err) {
      .vclip_release()

      if (isTRUE(e$alive) && !isTRUE(e$cancelled)) {
        e$rv(list(status="error", error = conditionMessage(err)))
      } else {
        # cleanup se já não precisa
        try(shiny::removeResourcePath(e$res_prefix), silent = TRUE)
        try(unlink(e$tmp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
        e$res_prefix <- NULL
        e$tmp_dir <- NULL
      }
      NULL
    })

    invisible(TRUE)
  }

  .try_start_job()
  invisible(TRUE)
}
