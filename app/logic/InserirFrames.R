box::use(app/logic/utils[...])
box::use(app/logic/database[...])
library(DBI)
library(RMariaDB)
library(gtools)
library(magick)

# 2) lê como raw (BLOB)
read_raw <- function(path) {
  f <- file(path, "rb"); on.exit(close(f), add = TRUE)
  readBin(f, what="raw", n=file.info(path)$size)
}

insertFrame <- function(con,file,id_camera = 1L){

  raw_png <- read_raw(file)
  sql     <- 'INSERT INTO FRAME_CAMERA (DATA_FRAME, CD_ID_CAMERA) VALUES (?, ?)'
  dbExecute(con, sql, params = list(blob::blob(raw_png), id_camera)) 
}

con <- newConnection()
# png_path <- "C:/Sistema/Everton/VisionTelemetrycSystem/frames/0/image_000060.png"
# raw_png <- read_raw(png_path)

# # 3) insere (DT_HR_LOCAL usa DEFAULT do banco)
# sql <- 'INSERT INTO FRAME_CAMERA (DATA_FRAME, CD_ID_CAMERA) VALUES (?, ?)'
# n <- dbExecute(con, sql, params = list(blob::blob(raw_png), 1L))  # 42 = exemplo de câmera
# print(n)  # linhas afetadas

# # verificação (não puxe o LONGBLOB completo)
# dados <- DBI::dbGetQuery(con, "SELECT  CD_ID_CAMERA,DATA_FRAME,DT_HR_LOCAL FROM FRAME_CAMERA")

# img <- image_read(dados$DATA_FRAME[[1]])


file_0 <- mixedsort(list.files("C:/Sistema/Everton/TrainModelStatic/data/0",pattern = "image",full.names = T))
file_1 <- mixedsort(list.files("C:/Sistema/Everton/TrainModelStatic/data/1",pattern = "image",full.names = T))
file_2 <- mixedsort(list.files("C:/Sistema/Everton/TrainModelStatic/data/2",pattern = "image",full.names = T))
file_3 <- mixedsort(list.files("C:/Sistema/Everton/TrainModelStatic/data/3",pattern = "image",full.names = T))

files <- c(file_0,file_1,file_2,file_3)

for(i in seq_along(files)){
  insertFrame(con,files[i])
  Sys.sleep(0.5)
}

dbDisconnect(con)

