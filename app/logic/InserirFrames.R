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

#################################################
library(dplyr)
library(tidyr)
library(purrr)

fetch_frames <- function(conn,tb, te, camera_id_vec) {

  #tb <- as.POSIXct(time_begin, tz = "UTC")
  #te <- as.POSIXct(time_end,   tz = "UTC")

  placeholders <- paste(rep("?", length(camera_id_vec)), collapse = ",")
  sql <- paste0(
    "SELECT DATA_FRAME,DT_HR_LOCAL, CD_ID_CAMERA
     FROM FRAME_CAMERA
     WHERE DT_HR_LOCAL BETWEEN ? AND ?
       AND CD_ID_CAMERA IN (", placeholders, ")
     ORDER BY DT_HR_LOCAL ASC"
  )

  params <- c(list(tb, te), as.list(as.integer(camera_id_vec)))
  df <- DBI::dbGetQuery(conn, sql, params = params)
  if (!nrow(df)) return(df)
  df$DT_HR_LOCAL <- as.POSIXct(df$DT_HR_LOCAL, tz = "UTC")
  df
}

con <- newConnection()
query <- paste0("WITH oc_latest AS (
    SELECT CD_ID_OBJ_CONF, CD_ID_OBJETO
    FROM (
      SELECT
        oc.*,
        ROW_NUMBER() OVER (
          PARTITION BY oc.CD_ID_OBJETO
          ORDER BY oc.DT_HR_LOCAL DESC, oc.CD_ID_OBJ_CONF DESC
        ) AS rn
      FROM OBJETO_CONFIG oc
    ) x
    WHERE rn = 1
  ),
  cams AS (
    SELECT
      ol.CD_ID_OBJETO,
      GROUP_CONCAT(DISTINCT c.CD_ID_CAMERA ORDER BY c.CD_ID_CAMERA SEPARATOR ',') AS CD_ID_CAMERAS
    FROM oc_latest ol
    LEFT JOIN COMPONENTE c
      ON c.CD_ID_OBJ_CONF = ol.CD_ID_OBJ_CONF
    GROUP BY ol.CD_ID_OBJETO
  )
  SELECT
    p.*,
    COALESCE(cams.CD_ID_CAMERAS, '') AS CD_ID_CAMERAS
  FROM PACOTE_IA p
  LEFT JOIN cams
    ON cams.CD_ID_OBJETO = p.CD_ID_OBJETO
  -- opcional: filtrar apenas pacotes ativos
  -- WHERE p.FG_ATIVO = 1
  ORDER BY p.CD_ID_IA")

frames <- DBI::dbGetQuery(con,query) 

dataset <- frames |> 
  group_by(CD_ID_OBJETO) |> 
  nest() |> 
  ungroup() |> 
  mutate(data = map(data,function(x){

    x |> 
      group_by(CD_ID_IA) |> 
      nest() |> 
      ungroup() |> 
      mutate(frame = map(data,function(y){
        fetch_frames(con,tb = y$DT_HR_LOCAL_BEGIN,te = y$DT_HR_LOCAL_END,camera_id_vec = y$CD_ID_CAMERAS)
      })) |> 
      unnest(data)
  
  })) |> 
  unnest(data)

saveRDS(dataset,"dataset_train.rds")
