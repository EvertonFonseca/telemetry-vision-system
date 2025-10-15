# install.packages(c("processx","magick"))
box::use(app/logic/utils[...])
box::use(app/logic/database[...])
library(processx)
library(magick)
library(DBI)
library(RMariaDB)
library(gtools)

read_raw <- function(path) {
  f <- file(path, "rb"); on.exit(close(f), add = TRUE)
  readBin(f, what="raw", n=file.info(path)$size)
}

insertFrame <- function(con,file,id_camera = 1L){
  raw_png <- read_raw(file)
  sql     <- 'INSERT INTO FRAME_CAMERA (DATA_FRAME, CD_ID_CAMERA) VALUES (?, ?)'
  dbExecute(con, sql, params = list(blob::blob(raw_png), id_camera)) 
}

# Monta RTSP no padrão Intelbras/Dahua
make_rtsp_url <- function(ip, port = 554, user = NULL, pwd = NULL,
                          channel = 1, subtype = 1) {
  stopifnot(nzchar(ip), length(port) == 1, channel >= 1, subtype %in% c(0,1))
  # encode credenciais (importante por causa de #, @, :, etc.)
  if (!is.null(user) && !is.null(pwd)) {
    user_enc <- utils::URLencode(user, reserved = TRUE)
    pwd_enc  <- utils::URLencode(pwd,  reserved = TRUE)
    creds <- sprintf("%s:%s@", user_enc, pwd_enc)
  } else {
    creds <- ""  # sem auth embutida
  }
  sprintf("rtsp://%s%s:%d/cam/realmonitor?channel=%d&subtype=%d",
          creds, ip, port, channel, subtype)
}

ip   <- "172.30.0.211"
port <- 554
user <- "everton"
pwd  <- "kNFR#8wnT4p"  # <<< senha com # codificada

# comando
# ffmpeg -y -rtsp_transport tcp `
#   -i "rtsp://everton:kNFR%238wnT4p@172.30.0.211:554/cam/realmonitor?channel=5&subtype=1" `
#   -frames:v 1 -q:v 2 -update 1 out.jpg

# 1) Caminho do ffmpeg (ajuste se preciso)
ffmpeg_bin <- Sys.which("ffmpeg")
if (ffmpeg_bin == "") ffmpeg_bin <- "C:/ffmpeg/bin/ffmpeg.exe"
stopifnot(file.exists(ffmpeg_bin))

# 2) URL RTSP que funcionou (senha com # já codificada: # -> %23)
rtsp_url <- make_rtsp_url(ip, port, user, pwd, channel = 5, subtype = 1)

# captura 1 frame via ffmpeg -> arquivo temporário -> magick
grab_one_file <- function(url, timeout_sec = 10) {
  outfile <- tempfile(fileext = ".jpg")
  px <- processx::run(
    ffmpeg_bin,
    c("-y","-rtsp_transport","tcp",
      "-i", url,
      "-frames:v","1","-q:v","2","-update","1",
      outfile),
    windows_verbatim_args = FALSE,   # deixa o processx fazer o quoting
    error_on_status = FALSE
  )
  if (px$status != 0L || !file.exists(outfile) || file.info(outfile)$size <= 0) {
    # descomente para debugar:
    # message("ffmpeg stderr:\n", rawToChar(px$stderr))
    return(NULL)
  }
  image_read(outfile)
}

# loop infinito de captura
capture_rtsp_loop <- function(
  url,
  out_root         = "frames_corte_estampo",
  fps              = 1L,         # frames por segundo (amostragem)
  size             = c(512,512), # size image out
  annotate_time    = FALSE,      # desenhar timestamp no canto
  make_thumb       = FALSE,      # gerar miniatura adicional?
  thumb_width      = 320,        # largura da miniatura
  retention_hours  = 24,         # manter arquivos apenas pelas últimas N horas (NA = não apagar)
  stop_flag        = "STOP.txt"  # se esse arquivo existir, o loop encerra limpo
){
  message("Iniciando captura… pressione Ctrl+C para interromper, ",
          "ou crie o arquivo '", stop_flag, "' para parar limpo.")

  repeat {
    # encerra se o arquivo STOP.txt existir (graceful stop)
    if (file.exists(stop_flag)) {
      message("Flag de parada detectada (", stop_flag, "). Encerrando loop.")
      break
    }

    # pasta por dia
    out_dir <- file.path(out_root, format(Sys.time(), "%Y-%m-%d"))
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    t0  <- Sys.time()
    img <- grab_one_file(url, timeout_sec = 10)

    if (!is.null(img)) {
      # anotação de timestamp (opcional)
      if (annotate_time) {
        ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        img <- image_annotate(
          img, ts, size = 18, color = "white",
          boxcolor = "rgba(0,0,0,0.4)",
          gravity = "southwest", location = "+10+8"
        )
      }

      # nome do arquivo com timestamp
      f <- file.path(
        out_dir,
        sprintf("corte_estampo_%s.jpg", format(Sys.time(), "%Y%m%dT%H%M%OS3"))
      )
      con <- newConnection()

      image_write(img |> image_resize(paste0(size[1],"x",size[2],"!")), f, format = "jpg")
      insertFrame(con,f,1L)
      
      cat("✔ salvo:", f, "\n")

      closerDatabase(con)

      # gera miniatura (opcional)
      if (isTRUE(make_thumb)) {
        thumb <- image_scale(img, paste0(thumb_width))
        tf <- sub("\\.jpg$", "_thumb.jpg", f, ignore.case = TRUE)
        image_write(thumb, tf, format = "jpg")
      }
    } else {
      warning("Falha ao capturar frame; tentando novamente…")
      Sys.sleep(0.5)
    }

    # retenção (apaga arquivos mais antigos que N horas)
    if (!is.na(retention_hours) && retention_hours > 0) {
      cutoff <- Sys.time() - retention_hours*3600
      old <- list.files(out_root, pattern = "\\.(jpg|png)$", recursive = TRUE, full.names = TRUE)
      if (length(old)) {
        info <- file.info(old)
        rmv  <- rownames(info)[!is.na(info$mtime) & info$mtime < cutoff]
        if (length(rmv)) unlink(rmv, force = TRUE)
      }
    }

    # controla “fps” (amostragem)
    dt <- 1 / max(0.1, fps)
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (dt > elapsed) Sys.sleep(dt - elapsed)
  }

  message("Captura finalizada.")
}

# === Execute o loop infinito (ajuste os parâmetros que quiser) ===
# Ex.: 2 fps, manter por 24h, com timestamp, sem thumb
capture_rtsp_loop(rtsp_url, fps = 1L, retention_hours = 24)

# Para encerrar sem Ctrl+C: crie um arquivo vazio "STOP.txt" no diretório de trabalho:
# file.create("STOP.txt")
