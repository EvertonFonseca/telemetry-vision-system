# video_clip.R
box::use(
  shiny[...],
  DBI,
  base64enc,
  later,
  stringi,
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

detect_mime <- function(raw) {
  if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
  if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return("image/png")
  "application/octet-stream"
}

# tenta salvar tudo como JPG pra facilitar o ffmpeg
.write_frame_jpg <- function(raw, file_jpg) {
  mime <- detect_mime(raw)
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
      # fallback: grava PNG com extensão errada? NÃO recomendo.
      # melhor falhar com msg clara:
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

.has_ffmpeg <- function() {
  nzchar(Sys.which("ffmpeg"))
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
#' @export
# video_clip_open
# - abre modal com MP4 por câmera e download
# ---------------------------
video_clip_open <- function(ns, input, output, session,
                            pool,
                            title,
                            time_begin, time_end,
                            camera_ids,
                            camera_names = NULL,
                            fps = 10L,
                            max_frames = 3000L) {

  stopifnot(length(camera_ids) >= 1)

  if (!.has_ffmpeg()) {
    showNotification("ffmpeg não encontrado no servidor. Instale/adicione ao PATH para gerar MP4.", type = "error")
    return(invisible(NULL))
  }

  e <- .vclip_env(session)

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

  # garante cleanup ao fechar a sessão
  if (is.null(e$on_end_registered) || !isTRUE(e$on_end_registered)) {
    e$on_end_registered <- TRUE
    session$onSessionEnded(function(){
      try({
        if (!is.null(e$res_prefix)) shiny::removeResourcePath(e$res_prefix)
        if (!is.null(e$tmp_dir) && dir.exists(e$tmp_dir)) unlink(e$tmp_dir, recursive = TRUE, force = TRUE)
      }, silent = TRUE)
    })
  }

  tb <- as.POSIXct(time_begin, tz = "UTC")
  te <- as.POSIXct(time_end,   tz = "UTC")

  # idx frames (timestamp + cam)
  frames_idx <- fetch_frames(
    conn          = pool,
    time_begin    = tb,
    time_end      = te,
    camera_id_vec = as.integer(camera_ids)
  )

  if (!nrow(frames_idx)) {
    showNotification("Nenhum frame encontrado nesse intervalo.", type = "warning")
    return(invisible(NULL))
  }

  # limita quantidade total de frames (pra não matar servidor num clip enorme)
  frames_idx <- .thin_frames(frames_idx, max_frames = max_frames)

  # nomes default
  camera_ids <- as.integer(unique(camera_ids))
  if (is.null(camera_names)) {
    camera_names <- vapply(camera_ids, function(cid) paste0("Câmera ", cid), character(1))
  }

  # gera MP4 por câmera
  mp4_rel <- character(0)
  mp4_abs <- character(0)

  for (k in seq_along(camera_ids)) {
    cam <- camera_ids[k]
    cam_dir <- file.path(e$tmp_dir, paste0("cam_", cam))
    dir.create(cam_dir, recursive = TRUE, showWarnings = FALSE)

    df_cam <- frames_idx[frames_idx$CD_ID_CAMERA == cam, , drop = FALSE]
    if (!nrow(df_cam)) next

    ts_vec <- as.POSIXct(df_cam$DT_HR_LOCAL, tz = "UTC")

    # busca blobs em chunks (IN com muitos params explode)
    raw_list <- list()
    chs <- .chunk(ts_vec, size = 300L)
    for (cc in chs) {
      res <- tryCatch(db_fetch_many_frames(pool, cam, cc), error = function(e) NULL)
      if (!is.null(res) && nrow(res)) {
        res$DT_HR_LOCAL <- as.POSIXct(res$DT_HR_LOCAL, tz = "UTC")
        raw_list[[length(raw_list) + 1L]] <- res
      }
    }
    if (!length(raw_list)) next
    res_all <- do.call(rbind, raw_list)

    # ordena pelo ts
    res_all <- res_all[order(res_all$DT_HR_LOCAL), , drop = FALSE]

    # grava frames como frame_000001.jpg ...
    ok_any <- FALSE
    for (i in seq_len(nrow(res_all))) {
      raw <- res_all$DATA_FRAME[[i]]
      if (is.null(raw)) next
      fjpg <- file.path(cam_dir, sprintf("frame_%06d.jpg", i))
      ok   <- .write_frame_jpg(raw, fjpg)
      if (isTRUE(ok)) ok_any <- TRUE
    }
    if (!ok_any) {
      showNotification(paste0("Sem frames válidos para câmera ", cam, " (PNG sem magick?)"), type = "warning")
      next
    }

    out_mp4_abs <- file.path(cam_dir, "clip.mp4")

    # ffmpeg: gera mp4
    args <- c(
      "-y",
      "-hide_banner",
      "-loglevel", "error",
      "-framerate", as.character(as.integer(fps)),
      "-i", file.path(cam_dir, "frame_%06d.jpg"),
      "-c:v", "libx264",
      "-pix_fmt", "yuv420p",
      out_mp4_abs
    )
    st <- suppressWarnings(system2("ffmpeg", args = args, stdout = TRUE, stderr = TRUE))
    if (!file.exists(out_mp4_abs)) {
      showNotification(paste0("Falha ao gerar MP4 (cam ", cam, ")."), type = "error")
      next
    }

    out_mp4_rel <- file.path(e$res_prefix, paste0("cam_", cam), "clip.mp4")
    mp4_rel <- c(mp4_rel, out_mp4_rel)
    mp4_abs <- c(mp4_abs, out_mp4_abs)
  }

  if (!length(mp4_rel)) {
    showNotification("Não foi possível gerar nenhum MP4 (ver logs/ffmpeg/magick).", type = "error")
    return(invisible(NULL))
  }

  # ids UI
  modal_id <- ns("video_clip_modal")
  close_id <- ns("video_clip_close")

  # constrói cards por câmera
  cards <- tagList()
  video_ids <- character(0)

  for (k in seq_along(camera_ids)) {
    cam <- camera_ids[k]
    name <- camera_names[k]

    rel <- file.path(e$res_prefix, paste0("cam_", cam), "clip.mp4")
    if (!(rel %in% mp4_rel)) next

    vid_id <- ns(paste0("vclip_video_", cam))
    video_ids <- c(video_ids, vid_id)

    dl_id <- paste0("vclip_download_", cam)
    dl_ns <- ns(dl_id)

    # downloadHandler
    local({
      cam_local <- cam
      rel_local <- rel
      abs_local <- file.path(e$tmp_dir, paste0("cam_", cam_local), "clip.mp4")
      output[[dl_id]] <- downloadHandler(
        filename = function() sprintf("clip_cam_%d_%s_%s.mp4",
                                      cam_local,
                                      format(tb, "%Y%m%d_%H%M%S", tz="UTC"),
                                      format(te, "%Y%m%d_%H%M%S", tz="UTC")),
        content = function(file) file.copy(abs_local, file, overwrite = TRUE),
        contentType = "video/mp4"
      )
    })

    step_sec <- 1 / max(1, as.numeric(fps))

    cards <- tagAppendChildren(cards,
      column(
        width = 12,
        div(
          style = "border:1px solid #e5e5e5; border-radius:10px; padding:12px; margin-bottom:12px;",
          tags$h4(paste0("🎥 ", name, " (", cam, ")"), style="margin:0 0 10px 0;"),
          tags$video(
            id = vid_id,
            src = paste0("/", rel),
            controls = NA,
            style = "width:100%; max-height:420px; background:#000; border-radius:10px;"
          ),
          br(),
          splitLayout(
            cellWidths = c("auto","auto","auto","auto","auto","auto"),
            tags$button(class="btn btn-outline-secondary btn-sm",
                        onclick = sprintf("vclipReverse('%s', %f)", vid_id, step_sec),
                        shiny::icon("backward"), " Reverse"),
            tags$button(class="btn btn-outline-secondary btn-sm",
                        onclick = sprintf("vclipPrev('%s', %f)", vid_id, step_sec),
                        shiny::icon("step-backward"), " Prev"),
            tags$button(class="btn btn-outline-secondary btn-sm",
                        onclick = sprintf("vclipPlay('%s')", vid_id),
                        shiny::icon("play"), " Play"),
            tags$button(class="btn btn-outline-secondary btn-sm",
                        onclick = sprintf("vclipPause('%s')", vid_id),
                        shiny::icon("pause"), " Pause"),
            tags$button(class="btn btn-outline-secondary btn-sm",
                        onclick = sprintf("vclipNext('%s', %f)", vid_id, step_sec),
                        shiny::icon("step-forward"), " Next"),
            downloadButton(dl_ns, "Download MP4", class = "btn btn-primary btn-sm")
          )
        )
      )
    )
  }

  # abre modal
  showModal(
    modalDialog(
      title = title %||% "Clip do gráfico",
      size  = "l",
      easyClose = TRUE,
      footer = tagList(
        actionButton(ns(close_id), "Fechar", class = "btn btn-outline-secondary")
      ),
      div(
        style = "overflow-x: hidden;",
        .vclip_js(),
        div(
          style = "max-height: 75vh; overflow-x: hidden;",
          tags$p(sprintf("Intervalo: %s → %s",
                         format(tb, "%d/%m/%Y %H:%M:%S", tz= Sys.timezone()),
                         format(te, "%d/%m/%Y %H:%M:%S", tz= Sys.timezone()))),
          fluidRow(cards)
        )
      )
    )
  )

  # fecha modal + cleanup
  observeEvent(input[[close_id]], {
    # pausa todos os vídeos
    session$sendCustomMessage("vclip_stop_all", video_ids)
    removeModal()

    # limpa arquivos/paths
    try(shiny::removeResourcePath(e$res_prefix), silent = TRUE)
    try(unlink(e$tmp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
    e$res_prefix <- NULL
    e$tmp_dir    <- NULL
  }, ignoreInit = TRUE, once = TRUE)

  invisible(TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
