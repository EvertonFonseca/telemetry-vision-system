box::use(app/logic/utils[...])
box::use(app/logic/database[...])
library(DBI)
library(RMariaDB)
library(gtools)
library(magick)

# # 2) lê como raw (BLOB)
# read_raw <- function(path) {
#   f <- file(path, "rb"); on.exit(close(f), add = TRUE)
#   readBin(f, what="raw", n=file.info(path)$size)
# }

# insertFrame <- function(con,file,id_camera = 1L){

#   raw_png <- read_raw(file)
#   sql     <- 'INSERT INTO FRAME_CAMERA (DATA_FRAME, CD_ID_CAMERA) VALUES (?, ?)'
#   dbExecute(con, sql, params = list(blob::blob(raw_png), id_camera)) 
# }

# con <- newConnection()
# # png_path <- "C:/Sistema/Everton/VisionTelemetrycSystem/frames/0/image_000060.png"
# # raw_png <- read_raw(png_path)

# # # 3) insere (DT_HR_LOCAL usa DEFAULT do banco)
# # sql <- 'INSERT INTO FRAME_CAMERA (DATA_FRAME, CD_ID_CAMERA) VALUES (?, ?)'
# # n <- dbExecute(con, sql, params = list(blob::blob(raw_png), 1L))  # 42 = exemplo de câmera
# # print(n)  # linhas afetadas

# # # verificação (não puxe o LONGBLOB completo)
# # dados <- DBI::dbGetQuery(con, "SELECT  CD_ID_CAMERA,DATA_FRAME,DT_HR_LOCAL FROM FRAME_CAMERA")

# # img <- image_read(dados$DATA_FRAME[[1]])
# file_0 <- mixedsort(list.files("C:/Sistema/Everton/TrainModelStatic/data/0",pattern = "image",full.names = T))
# file_1 <- mixedsort(list.files("C:/Sistema/Everton/TrainModelStatic/data/1",pattern = "image",full.names = T))
# file_2 <- mixedsort(list.files("C:/Sistema/Everton/TrainModelStatic/data/2",pattern = "image",full.names = T))
# file_3 <- mixedsort(list.files("C:/Sistema/Everton/TrainModelStatic/data/3",pattern = "image",full.names = T))

# files <- c(file_0,file_1,file_2,file_3)

# for(i in seq_along(files)){
#   insertFrame(con,files[i])
#   Sys.sleep(0.5)
# }

# dbDisconnect(con)

# #################################################
# library(dplyr)
# library(tidyr)
# library(purrr)

# con <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   host = Sys.getenv("DB_HOST", "127.0.0.1"),
#   port = as.integer(Sys.getenv("DB_PORT", "5434")),
#   dbname = Sys.getenv("DB_NAME", "analytia_db"),
#   user = Sys.getenv("DB_USER", "analytia"),
#   password = Sys.getenv("DB_PASS", "lytIA#2026!Elite@")
# )

# fetch_frames <- function(conn, tb, te, camera_id_vec, limit = 1000L) {
#   stopifnot(length(camera_id_vec) >= 1)

#   # normaliza datas para UTC
#   tb <- as.POSIXct(tb, tz = "UTC")
#   te <- as.POSIXct(te, tz = "UTC")

#   # monta lista de placeholders "?, ?, ?, ..."
#   cam_placeholders <- paste(rep("?", length(camera_id_vec)), collapse = ",")

#   sql <- paste0(
#     "
#     SELECT
#       b.CD_ID_FRAME,
#       fc.DT_HR_LOCAL,
#       fc.CD_ID_CAMERA,
#       b.DATA_FRAME
#     FROM FRAME_CAMERA fc
#     INNER JOIN FRAME_CAMERA_BLOB b
#       ON b.CD_ID_FRAME = fc.CD_ID_FRAME
#     WHERE fc.DT_HR_LOCAL BETWEEN ? AND ?
#       AND fc.CD_ID_CAMERA IN (", cam_placeholders, ")
#     ORDER BY fc.DT_HR_LOCAL ASC, fc.CD_ID_CAMERA ASC
#     LIMIT ?
#     "
#   )

#   params <- c(
#     list(tb, te),
#     as.list(as.integer(camera_id_vec)),
#     list(as.integer(limit))
#   )

#   df <- DBI::dbGetQuery(conn, sql, params = params)

#   if (!nrow(df)) {
#     return(df)
#   }

#   df$DT_HR_LOCAL <- as.POSIXct(df$DT_HR_LOCAL, tz = "UTC")
#   df
# }


# con <- newConnection()
# query <- paste0("WITH oc_latest AS (
#       SELECT CD_ID_OBJ_CONF, CD_ID_OBJETO
#       FROM (
#         SELECT
#           oc.*,
#           ROW_NUMBER() OVER (
#             PARTITION BY oc.CD_ID_OBJETO
#             ORDER BY oc.DT_HR_LOCAL DESC, oc.CD_ID_OBJ_CONF DESC
#           ) AS rn
#         FROM OBJETO_CONFIG oc
#       ) x
#       WHERE rn = 1
#     ),
#     cams AS (
#       SELECT
#         ol.CD_ID_OBJETO,

#         -- todas as câmeras envolvidas nesse objeto (última config)
#         GROUP_CONCAT(
#           DISTINCT c.CD_ID_CAMERA
#           ORDER BY c.CD_ID_CAMERA
#           SEPARATOR ','
#         ) AS CD_ID_CAMERAS,

#         -- array JSON: um item por componente, incluindo qual câmera ele pertence
#         JSON_ARRAYAGG(
#           JSON_OBJECT(
#             'CD_ID_CAMERA', c.CD_ID_CAMERA,
#             'CD_ID_COMPONENTE', c.CD_ID_COMPONENTE,
#             'NAME_COMPONENTE',c.NAME_COMPONENTE,
#             'POLIGNO_COMPONENTE', c.POLIGNO_COMPONENTE,
#             'CD_ID_ESTRUTURA', e.CD_ID_ESTRUTURA
#           )
#         ) AS POLIGNOS_COMPONENTES

#       FROM oc_latest ol
#       LEFT JOIN COMPONENTE c
#         ON c.CD_ID_OBJ_CONF = ol.CD_ID_OBJ_CONF
#       LEFT JOIN ESTRUTURA e ON e.CD_ID_ESTRUTURA = c.CD_ID_ESTRUTURA
#       GROUP BY ol.CD_ID_OBJETO
#     )

#     SELECT
#       p.*,
#       COALESCE(cams.CD_ID_CAMERAS, '') AS CD_ID_CAMERAS,
#       COALESCE(cams.POLIGNOS_COMPONENTES, JSON_ARRAY()) AS POLIGNOS_COMPONENTES
#     FROM PACOTE_IA p
#     LEFT JOIN cams
#       ON cams.CD_ID_OBJETO = p.CD_ID_OBJETO
#     -- exemplo se quiser só pacotes ativos:
#     -- WHERE p.FG_ATIVO = 1
#     ORDER BY p.CD_ID_IA
#     ")

# frames <- DBI::dbGetQuery(con,query) 
# limit  <- 150L

# dataset <- frames |> 
#   group_by(CD_ID_OBJETO) |> 
#   nest() |> 
#   ungroup() |> 
#   mutate(data = map(data,function(x){

#     x |> 
#       group_by(CD_ID_IA) |> 
#       nest() |> 
#       ungroup() |> 
#       mutate(DATAS = map(data,function(y){
        
#         output      <- jsonlite::fromJSON(y$OUTPUT_IA,simplifyVector = F)
#         comps       <- jsonlite::fromJSON(y$POLIGNOS_COMPONENTES)
       
#         for(z in seq_along(output)){
#            nome_comp   <- names(output[[z]])
#            index_      <- which(comps$NAME_COMPONENTE %in% nome_comp)
#            if(length(index_) == 0) next
#            output[[z]]$ID <- comps$CD_ID_COMPONENTE[index_]
#         }

#         output      <- as.character(jsonlite::toJSON(output,auto_unbox = TRUE))
#         cameras     <- stringr::str_split(y$CD_ID_CAMERAS,",")[[1]]
#         payload     <- purrr::map(cameras,function(camera){
#           componentes <- comps |> filter(CD_ID_CAMERA == camera)
#           frames_db   <- fetch_frames(con,tb = y$DT_HR_LOCAL_BEGIN,te = y$DT_HR_LOCAL_END,camera_id_vec = camera,limit = limit)
#           tibble(COMPONENTES = list(componentes),FRAMES = list(frames_db))
#         })
#         status <- map_vec(payload,~ nrow(.x$FRAMES[[1]]) > 0)
#         if(all(status)){
#           y |> mutate(OUTPUT_IA = output,PAYLOAD = list(payload),LIMIT = limit)
#         }else{
#           NULL
#         }
#       })) |> 
#       unnest(data)
  
#   })) |> 
#   unnest(data) |> 
#   filter(map_vec(DATAS,~ !is.null(.x)))

# #saveRDS(dataset,paste0("train/dataset_embalagem.rds"))

# dataset_fining <- dataset[which(grepl("Finetuning",dataset$TITULO_IA,ignore.case = F)),]

# saveRDS(dataset |> filter(!grepl("Teste",TITULO_IA,ignore.case = F)),paste0("train/dataset_train.rds"))
# saveRDS(dataset |> filter(grepl("Teste",TITULO_IA,ignore.case = F)),paste0("train/dataset_test.rds"))
# saveRDS(dataset,paste0("train/dataset_esquadros.rds"))
# saveRDS(last(dataset),paste0("train/dataset_train_dynamic.rds"))

# df <- last(dataset)
# df$OUTPUT_IA

###############################
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)
library(tibble)


# ============================================================
# Helpers: lower names
# ============================================================

upper_names <- function(df) {
  names(df) <- toupper(names(df))
  df
}

lower_names <- function(df) {
  nms <- names(df)
  names(df) <- tolower(nms)
  df
}

normalize_comps_upper <- function(comps) {
  if (is.null(comps)) return(tibble())

  # jsonb pode vir como string ou como lista
  if (is.character(comps)) {
    comps <- jsonlite::fromJSON(comps, simplifyVector = TRUE)
  }

  # lista/data.frame -> tibble
  comps <- tryCatch(
    tibble::as_tibble(comps),
    error = function(e) as_tibble(as.data.frame(comps, stringsAsFactors = FALSE))
  )

  # força UPPERCASE nas colunas
  comps <- upper_names(comps)

  # mapeia possíveis nomes lower/variantes -> nomes do OLD
  # (se já existir, não mexe)
  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENT" %in% names(comps)) {
    names(comps)[names(comps) == "NAME_COMPONENT"] <- "NAME_COMPONENTE"
  }
  if (!"NAME_COMPONENTE" %in% names(comps) && "NOME_COMPONENTE" %in% names(comps)) {
    names(comps)[names(comps) == "NOME_COMPONENTE"] <- "NAME_COMPONENTE"
  }
  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # já ok
  }
  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # já ok
  }

  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # noop
  }

  # comuns do seu "novo": name_componente / cd_id_componente / cd_id_camera
  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # noop
  }
  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # noop
  }

  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # noop
  }

  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # noop
  }

  # se vier do DB em lower e você upper_names, vira NAME_COMPONENTE etc.
  # então aqui é mais para variantes diferentes.

  # garante esses 3 como existirem (se vier em lower, upper_names resolve)
  # tenta mapear nomes muito comuns:
  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # noop
  }
  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # noop
  }

  # mapeia exatamente os do novo (após upper_names)
  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # noop
  }
  if (!"NAME_COMPONENTE" %in% names(comps) && "NAME_COMPONENTE" %in% names(comps)) {
    # noop
  }

  # se vier NAME_COMPONENTE já ok
  # se vier NAME_COMPONENTE como NAME_COMPONENTE, ok

  # mapeia "NAME_COMPONENTE" a partir de "NAME_COMPONENTE" não precisa
  # mapeia "NAME_COMPONENTE" a partir de "NAME_COMPONENTE" não precisa

  # mapeia "NAME_COMPONENTE" se por acaso vier "NAME_COMPONENTE" não precisa

  # mapeia do padrão novo (após upper_names):
  # NAME_COMPONENTE, CD_ID_COMPONENTE, CD_ID_CAMERA já ficam assim.
  # Só garantimos que existam, senão cria vazio:
  if (!"NAME_COMPONENTE" %in% names(comps)) comps$NAME_COMPONENTE <- NA_character_
  if (!"CD_ID_COMPONENTE" %in% names(comps)) comps$CD_ID_COMPONENTE <- NA_integer_
  if (!"CD_ID_CAMERA" %in% names(comps)) comps$CD_ID_CAMERA <- NA_integer_

  comps
}


# ============================================================
# ✅ Postgres: fetch_frames (corrigido)
# - usa ANY($3::int[])
# - aceita camera_id_vec escalar ("6") ou vetor c(6,7)
# ============================================================
fetch_frames <- function(conn, tb, te, camera_id_vec, limit = 1000L) {
  stopifnot(length(camera_id_vec) >= 1)

  cams <- as.integer(camera_id_vec)
  cams <- cams[!is.na(cams)]
  stopifnot(length(cams) >= 1)

  # Postgres usa $1, $2, ...
  # tb = $1, te = $2, cams = $3..$(2+N), limit = $(3+N)
  n <- length(cams)
  cam_placeholders <- paste0("$", 3:(2 + n), collapse = ",")
  limit_ph <- paste0("$", 3 + n)

  sql <- paste0(
    "
    SELECT
      b.cd_id_frame,
      fc.dt_hr_local,
      fc.cd_id_camera,
      b.data_frame
    FROM frame_camera fc
    INNER JOIN frame_camera_blob b
      ON b.cd_id_frame = fc.cd_id_frame
    WHERE fc.dt_hr_local BETWEEN $1 AND $2
      AND fc.cd_id_camera IN (", cam_placeholders, ")
    ORDER BY fc.dt_hr_local ASC, fc.cd_id_camera ASC
    LIMIT ", limit_ph, "
    "
  )

  params <- c(list(tb, te), as.list(cams), list(as.integer(limit)))

  df <- DBI::dbGetQuery(conn, sql, params = params)

  if (!nrow(df)) return(df)

  names(df) <- toupper(names(df))
  df
}


# ============================================================
# Conexão
# ============================================================

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST", "127.0.0.1"),
  port = as.integer(Sys.getenv("DB_PORT", "5434")),
  dbname = Sys.getenv("DB_NAME", "analytia_db"),
  user = Sys.getenv("DB_USER", "analytia"),
  password = Sys.getenv("DB_PASS", "lytIA#2026!Elite@")
)

# ============================================================
# ✅ Postgres: query convertida (tudo lower no resultado final)
# ============================================================
query <- "
WITH oc_latest AS (
  SELECT cd_id_obj_conf, cd_id_objeto
  FROM (
    SELECT
      oc.*,
      ROW_NUMBER() OVER (
        PARTITION BY oc.cd_id_objeto
        ORDER BY oc.dt_hr_local DESC, oc.cd_id_obj_conf DESC
      ) AS rn
    FROM objeto_config oc
  ) x
  WHERE rn = 1
),
comps_base AS (
  SELECT
    ol.cd_id_objeto,
    c.cd_id_camera,
    c.cd_id_componente,
    c.name_componente,
    c.poligno_componente,
    e.cd_id_estrutura
  FROM oc_latest ol
  LEFT JOIN componente c
    ON c.cd_id_obj_conf = ol.cd_id_obj_conf
  LEFT JOIN estrutura e
    ON e.cd_id_estrutura = c.cd_id_estrutura
),
cams AS (
  SELECT
    cb.cd_id_objeto,

    -- ✅ csv de cameras: DISTINCT + ORDER BY (via subselect)
    COALESCE((
      SELECT string_agg(d.cd_id_camera::text, ',' ORDER BY d.cd_id_camera)
      FROM (
        SELECT DISTINCT cd_id_camera
        FROM comps_base
        WHERE cd_id_objeto = cb.cd_id_objeto
          AND cd_id_camera IS NOT NULL
      ) d
    ), '') AS cd_id_cameras,

    -- ✅ json array de componentes
    COALESCE(
      jsonb_agg(
        jsonb_build_object(
          'cd_id_camera', cb.cd_id_camera,
          'cd_id_componente', cb.cd_id_componente,
          'name_componente', cb.name_componente,
          'poligno_componente', cb.poligno_componente,
          'cd_id_estrutura', cb.cd_id_estrutura
        )
      ) FILTER (WHERE cb.cd_id_componente IS NOT NULL),
      '[]'::jsonb
    ) AS polignos_componentes

  FROM comps_base cb
  GROUP BY cb.cd_id_objeto
)

SELECT
  p.*,
  COALESCE(cams.cd_id_cameras, '') AS cd_id_cameras,
  COALESCE(cams.polignos_componentes, '[]'::jsonb) AS polignos_componentes
FROM pacote_ia p
LEFT JOIN cams
  ON cams.cd_id_objeto = p.cd_id_objeto
ORDER BY p.cd_id_ia
"

frames <- DBI::dbGetQuery(con, query)
frames <- lower_names(frames)

limit <- 150L
frames_u <- frames %>% upper_names()

dataset <- frames_u |>
  dplyr::group_by(CD_ID_OBJETO) |>
  tidyr::nest() |>
  dplyr::ungroup() |>
  dplyr::mutate(data = purrr::map(data, function(x) {

    x <- x %>% upper_names()

    x |>
      dplyr::group_by(CD_ID_IA) |>
      tidyr::nest() |>
      dplyr::ungroup() |>
      dplyr::mutate(DATAS = purrr::map(data, function(y) {

        y <- y %>% upper_names()

        # se tiver mais de 1 linha por CD_ID_IA, pega a 1ª (igual seu comentário)
        y1 <- y[1, , drop = FALSE]

        output <- jsonlite::fromJSON(y1$OUTPUT_IA, simplifyVector = FALSE)

        # POLIGNOS_COMPONENTES (jsonb) pode vir como string ou lista
        comps_raw <- as.character(toupper(y1$POLIGNOS_COMPONENTES[[1]]))
        comps     <- normalize_comps_upper(comps_raw)

        # injeta ID do componente no output pelo NAME_COMPONENTE
        for (z in seq_along(output)) {
          # mantém compatibilidade com o OLD (pega o nome do componente)
          nome_comp <- names(output[[z]])[1]
          idx <- which(comps$NAME_COMPONENTE %in% nome_comp)
          if (length(idx) == 0) next
          output[[z]]$ID <- comps$CD_ID_COMPONENTE[idx[1]]
        }

        output <- as.character(jsonlite::toJSON(output, auto_unbox = TRUE))

        cameras <- y1$CD_ID_CAMERAS
        cameras <- if (is.na(cameras) || !nzchar(cameras)) character(0) else stringr::str_split(cameras, ",")[[1]]
        cameras <- stringr::str_trim(cameras)
        cameras <- cameras[nzchar(cameras)]
        if (length(cameras) == 0) return(NULL)

        payload <- purrr::map(cameras, function(camera) {
          camera_i <- as.integer(camera)

          componentes <- comps |>
                         dplyr::filter(.data$CD_ID_CAMERA == camera_i)

          frames_db <- fetch_frames(
            con,
            tb = y1$DT_HR_LOCAL_BEGIN,
            te = y1$DT_HR_LOCAL_END,
            camera_id_vec = camera_i,   # ✅ int
            limit = limit
          )
          
          tibble::tibble(
            COMPONENTES = list(componentes),
            FRAMES      = list(frames_db)
          )
        })

        status <- purrr::map_lgl(payload, ~ nrow(.x$FRAMES[[1]]) > 0)

        if (all(status)) {
          y1 |>
            dplyr::mutate(
              OUTPUT_IA = output,
              PAYLOAD   = list(payload),
              LIMIT     = limit
            ) |> 
             dplyr::mutate(POLIGNOS_COMPONENTES = as.character(toupper(POLIGNOS_COMPONENTES)))
        } else {
          NULL
        }
      })) |>
      tidyr::unnest(data)

  })) |>
  tidyr::unnest(data) |>
  dplyr::filter(purrr::map_lgl(DATAS, ~ !is.null(.x))) |> 
  mutate(POLIGNOS_COMPONENTES = as.character(toupper(POLIGNOS_COMPONENTES)))


saveRDS(dataset,paste0("train/dataset_pintura_2.rds"))
