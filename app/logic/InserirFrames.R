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

fetch_frames <- function(conn, tb, te, camera_id_vec, limit = 50L) {
  stopifnot(length(camera_id_vec) >= 1)

  # normaliza datas para UTC
  tb <- as.POSIXct(tb, tz = "UTC")
  te <- as.POSIXct(te, tz = "UTC")

  # monta lista de placeholders "?, ?, ?, ..."
  cam_placeholders <- paste(rep("?", length(camera_id_vec)), collapse = ",")

  sql <- paste0(
    "
    SELECT
      fc.DT_HR_LOCAL,
      fc.CD_ID_CAMERA,
      b.DATA_FRAME
    FROM FRAME_CAMERA fc
    INNER JOIN FRAME_CAMERA_BLOB b
      ON b.CD_ID_FRAME = fc.CD_ID_FRAME
    WHERE fc.DT_HR_LOCAL BETWEEN ? AND ?
      AND fc.CD_ID_CAMERA IN (", cam_placeholders, ")
    ORDER BY fc.DT_HR_LOCAL ASC, fc.CD_ID_CAMERA ASC
    LIMIT ?
    "
  )

  params <- c(
    list(tb, te),
    as.list(as.integer(camera_id_vec)),
    list(as.integer(limit))
  )

  df <- DBI::dbGetQuery(conn, sql, params = params)

  if (!nrow(df)) {
    return(df)
  }

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

        -- todas as câmeras envolvidas nesse objeto (última config)
        GROUP_CONCAT(
          DISTINCT c.CD_ID_CAMERA
          ORDER BY c.CD_ID_CAMERA
          SEPARATOR ','
        ) AS CD_ID_CAMERAS,

        -- array JSON: um item por componente, incluindo qual câmera ele pertence
        JSON_ARRAYAGG(
          JSON_OBJECT(
            'CD_ID_CAMERA', c.CD_ID_CAMERA,
            'CD_ID_COMPONENTE', c.CD_ID_COMPONENTE,
            'POLIGNO_COMPONENTE', c.POLIGNO_COMPONENTE
          )
        ) AS POLIGNOS_COMPONENTES

      FROM oc_latest ol
      LEFT JOIN COMPONENTE c
        ON c.CD_ID_OBJ_CONF = ol.CD_ID_OBJ_CONF
      GROUP BY ol.CD_ID_OBJETO
    )

    SELECT
      p.*,
      COALESCE(cams.CD_ID_CAMERAS, '') AS CD_ID_CAMERAS,
      COALESCE(cams.POLIGNOS_COMPONENTES, JSON_ARRAY()) AS POLIGNOS_COMPONENTES
    FROM PACOTE_IA p
    LEFT JOIN cams
      ON cams.CD_ID_OBJETO = p.CD_ID_OBJETO
    -- exemplo se quiser só pacotes ativos:
    -- WHERE p.FG_ATIVO = 1
    ORDER BY p.CD_ID_IA
    ")

frames <- DBI::dbGetQuery(con,query) 
limit  <- 50

dataset <- frames |> 
  group_by(CD_ID_OBJETO) |> 
  nest() |> 
  ungroup() |> 
  mutate(data = map(data,function(x){

    x |> 
      group_by(CD_ID_IA) |> 
      nest() |> 
      ungroup() |> 
      mutate(DATAS = map(data,function(y){
        polignos    <- jsonlite::fromJSON(y$POLIGNOS_COMPONENTES)
        cameras     <- stringr::str_split(y$CD_ID_CAMERAS,",")[[1]]
        payload     <- purrr::map(cameras,function(camera){
          componentes <- polignos |> filter(CD_ID_CAMERA == camera)
          frames_db   <- fetch_frames(con,tb = y$DT_HR_LOCAL_BEGIN,te = y$DT_HR_LOCAL_END,camera_id_vec = camera,limit = limit)
          tibble(COMPONENTES = list(componentes),FRAMES = list(frames_db))
        })
        status <- map_vec(payload,~ nrow(.x$FRAMES[[1]]) > 0)
        if(status)
         y |> mutate(PAYLOAD = list(payload),LIMIT = limit)
        else
         NULL
      })) |> 
      unnest(data)
  
  })) |> 
  unnest(data) |> 
  filter(map_vec(DATAS,~ !is.null(.x)))

objeto <- dataset$
saveRDS(dataset,paste0("dataset_train.rds"))

dataset <- rbind(df,dataset)


library(torch)
library(torchvision)
library(tidyverse)
library(magick)
library(gtools)
library(tokenizers)
library(purrr)
library(dplyr)
library(tidyr)

df <- readRDS("dataset_train.rds")
df <- map_df(seq_along(df$DATAS),function(i){
  datas <- df$DATAS[[i]]
  datas |> mutate(CD_ID_OBJETO = df$CD_ID_OBJETO[i],CD_ID_IA = df$CD_ID_IA[i])
})

rows <- list()
idx  <- 1L

df_tmp <- NULL
#pacotes
for(i in seq_along(df$PAYLOAD)){

  cd_id_objeto    <- df$CD_ID_OBJETO[i]
  pacote_ia       <- df$CD_ID_IA[i]
  input           <- df$INPUT_IA[i]
  output          <- df$OUTPUT_IA[i]
  pacotes_camera  <- df$PAYLOAD[[i]]
  limit           <- df$LIMIT[i]
  
  for(j in 1:limit){
    camera_frame_list <- list()
    for(camera in length(pacotes_camera)){ # aqui vai pegar os frames por camera do objeto 
      frames          <- pacotes_camera[[camera]]$FRAMES[[1L]]
      componentes     <- pacotes_camera[[camera]]$COMPONENTES[[1L]]
      
      camera_frame_list[[camera]] <- tibble(
        componentes = list(componentes),
        frame       = list(frames$DATA_FRAME[j]),
        date_time   = frames$DT_HR_LOCAL[j]
      )
    }
    df_camera <- tibble(
      pacote_ia     = pacote_ia,
      cd_id_objeto  = cd_id_objeto,
      input         = input,
      output        = output,
      info          = list(camera_frame_list),
      time          = j
    )
    df_tmp <- rbind(df_tmp,df_camera)
  }

}

df_tmp <- df_tmp |> 
  group_by(cd_id_objeto,pacote_ia) |>
  nest() |> 
  ungroup() |> 
  mutate(data = map(data,~ .x |> mutate(time = row_number()))) |> 
  unnest(data)
 

# df_tmp  <- df_tmp |> 
# group_by(cd_id_objeto,pacote_ia) |> 
# nest() |> 
# ungroup() |> 
# mutate(data = map(data,function(x){
#   min_amostra = min(table(x$output)) 
#   x |> 
#   group_by(output) |> 
#   nest() |> 
#   ungroup() |> 
#   mutate(data = map(data,function(y){
#      counter <- nrow(y)
#      begin   <- sample(1:max(1,counter - min_amostra),1)
#      end     <- begin + min_amostra - 1L
#      y       <- y |> arrange(time)
#      y[begin:end,]
#   })) |> 
#     unnest(data)
# })) |> 
#  unnest(data)