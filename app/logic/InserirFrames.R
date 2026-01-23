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

fetch_frames <- function(conn, tb, te, camera_id_vec, limit = 1000L) {
  stopifnot(length(camera_id_vec) >= 1)

  # normaliza datas para UTC
  tb <- as.POSIXct(tb, tz = "UTC")
  te <- as.POSIXct(te, tz = "UTC")

  # monta lista de placeholders "?, ?, ?, ..."
  cam_placeholders <- paste(rep("?", length(camera_id_vec)), collapse = ",")

  sql <- paste0(
    "
    SELECT
      b.CD_ID_FRAME,
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
            'NAME_COMPONENTE',c.NAME_COMPONENTE,
            'POLIGNO_COMPONENTE', c.POLIGNO_COMPONENTE,
            'CD_ID_ESTRUTURA', e.CD_ID_ESTRUTURA
          )
        ) AS POLIGNOS_COMPONENTES

      FROM oc_latest ol
      LEFT JOIN COMPONENTE c
        ON c.CD_ID_OBJ_CONF = ol.CD_ID_OBJ_CONF
      LEFT JOIN ESTRUTURA e ON e.CD_ID_ESTRUTURA = c.CD_ID_ESTRUTURA
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
limit  <- 150L

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
        
        output      <- jsonlite::fromJSON(y$OUTPUT_IA,simplifyVector = F)
        comps       <- jsonlite::fromJSON(y$POLIGNOS_COMPONENTES)
       
        for(z in seq_along(output)){
           nome_comp   <- names(output[[z]])
           index_      <- which(comps$NAME_COMPONENTE %in% nome_comp)
           if(length(index_) == 0) next
           output[[z]]$ID <- comps$CD_ID_COMPONENTE[index_]
        }

        output      <- as.character(jsonlite::toJSON(output,auto_unbox = TRUE))
        cameras     <- stringr::str_split(y$CD_ID_CAMERAS,",")[[1]]
        payload     <- purrr::map(cameras,function(camera){
          componentes <- comps |> filter(CD_ID_CAMERA == camera)
          frames_db   <- fetch_frames(con,tb = y$DT_HR_LOCAL_BEGIN,te = y$DT_HR_LOCAL_END,camera_id_vec = camera,limit = limit)
          tibble(COMPONENTES = list(componentes),FRAMES = list(frames_db))
        })
        status <- map_vec(payload,~ nrow(.x$FRAMES[[1]]) > 0)
        if(all(status)){
          y |> mutate(OUTPUT_IA = output,PAYLOAD = list(payload),LIMIT = limit)
        }else{
          NULL
        }
      })) |> 
      unnest(data)
  
  })) |> 
  unnest(data) |> 
  filter(map_vec(DATAS,~ !is.null(.x)))

#saveRDS(dataset,paste0("train/dataset_embalagem.rds"))

dataset_fining <- dataset[which(grepl("Finetuning",dataset$TITULO_IA,ignore.case = F)),]

saveRDS(dataset |> filter(!grepl("Teste",TITULO_IA,ignore.case = F)),paste0("train/dataset_train.rds"))
saveRDS(dataset |> filter(grepl("Teste",TITULO_IA,ignore.case = F)),paste0("train/dataset_test.rds"))
saveRDS(dataset,paste0("train/dataset_esquadros.rds"))
saveRDS(last(dataset),paste0("train/dataset_train_dynamic.rds"))

df <- last(dataset)
df$OUTPUT_IA

###############################

library(jsonlite)
library(tibble)
library(dplyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------
# parse robusto de timestamp
# ----------------------------
.parse_time_any <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXct")) return(as.POSIXct(x, tz = tz))
  if (is.null(x) || is.na(x) || !nzchar(as.character(x))) return(as.POSIXct(NA, tz = tz))

  s <- as.character(x)

  out <- suppressWarnings(as.POSIXct(s, tz = tz, format = "%Y-%m-%d %H:%M:%OS"))
  if (!is.na(out)) return(out)

  out <- suppressWarnings(as.POSIXct(s, tz = tz, format = "%Y-%m-%d %H:%M:%S"))
  if (!is.na(out)) return(out)

  suppressWarnings(as.POSIXct(s, tz = tz))
}

.as_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.list(x)) return(x)
  list(x)
}

# sempre retorna character (nunca classe "json")
.attrs_to_json_chr <- function(attrs) {
  as.character(jsonlite::toJSON(attrs %||% list(), auto_unbox = TRUE, null = "null"))
}

# ============================================================
# FUNÇÃO PRINCIPAL
# ============================================================
build_keyframe_interval_tables <- function(output_ia_json,
                                          clip_begin,
                                          clip_end,
                                          tz_clip = "UTC",
                                          tz_keyframes = "UTC",
                                          clamp_to_clip = TRUE,
                                          include_attrs_json = TRUE) {

  clip_begin <- .parse_time_any(clip_begin, tz = tz_clip)
  clip_end   <- .parse_time_any(clip_end,   tz = tz_clip)

  if (is.na(clip_begin) || is.na(clip_end)) stop("clip_begin e clip_end precisam ser datas válidas.")
  if (clip_end <= clip_begin) stop("clip_end precisa ser > clip_begin.")

  root <- tryCatch(fromJSON(output_ia_json, simplifyVector = FALSE),
                   error = function(e) NULL)
  if (is.null(root)) stop("output_ia_json inválido (não parseou).")

  root_list <- if (is.list(root) && !is.null(names(root))) list(root) else .as_list(root)

  all_dyn_rects <- list()
  for (item in root_list) {
    if (is.list(item) && !is.null(item$DYN_RECTS)) {
      all_dyn_rects <- c(all_dyn_rects, .as_list(item$DYN_RECTS))
    }
  }

  if (!length(all_dyn_rects)) {
    return(list(intervals_tbl = tibble(), poly_tbl = tibble(), rects = list()))
  }

  rects_out <- list()
  intervals_rows <- list()
  poly_rows <- list()

  seg_id <- 0L

  for (ridx in seq_along(all_dyn_rects)) {
    rect <- all_dyn_rects[[ridx]]

    rect_id      <- rect$RECT_ID %||% NA
    camera_id    <- rect$CAMERA_ID %||% NA
    cd_estrutura <- rect$CD_ID_ESTRUTURA %||% NA
    name_estr    <- rect$NAME_ESTRUTURA %||% NA

    kfs <- .as_list(rect$KEYFRAMES)

    # sem keyframes -> 1 segmento pro clip todo
    if (!length(kfs)) {
      seg_id <- seg_id + 1L
      attrs <- rect$ATRIBUTOS %||% list()

      seg <- list(
        SEGMENT_ID = seg_id,
        RECT_ID = rect_id,
        CAMERA_ID = camera_id,
        CD_ID_ESTRUTURA = cd_estrutura,
        NAME_ESTRUTURA = name_estr,
        BEGIN = clip_begin,
        END   = clip_end,
        ATRIBUTOS = attrs,
        POLY = NULL,
        TS_KEYFRAME = NA
      )

      rects_out[[length(rects_out) + 1L]] <- list(rect = rect, intervals = list(seg))

      intervals_rows[[length(intervals_rows) + 1L]] <- tibble(
        SEGMENT_ID = seg_id,
        RECT_ID = rect_id,
        CAMERA_ID = camera_id,
        CD_ID_ESTRUTURA = cd_estrutura,
        NAME_ESTRUTURA = name_estr,
        SEG_IDX = 1L,
        BEGIN = as.POSIXct(clip_begin, tz = tz_clip),
        END   = as.POSIXct(clip_end,   tz = tz_clip),
        TS_KEYFRAME = as.POSIXct(NA, tz = tz_clip),
        ATRIBUTOS_JSON = if (isTRUE(include_attrs_json)) .attrs_to_json_chr(attrs) else NA_character_
      )
      next
    }

    # parse timestamps
    ts <- vapply(kfs, function(k) {
      .parse_time_any(k$TS_UTC %||% k$TS %||% k$DATE_TIME, tz = tz_keyframes)
    }, FUN.VALUE = as.POSIXct(NA, tz = tz_keyframes))

    ord <- order(ts)
    kfs <- kfs[ord]
    ts  <- ts[ord]

    keep <- !is.na(ts)
    kfs <- kfs[keep]
    ts  <- ts[keep]

    if (!length(kfs)) {
      seg_id <- seg_id + 1L
      attrs <- rect$ATRIBUTOS %||% list()

      seg <- list(
        SEGMENT_ID = seg_id,
        RECT_ID = rect_id,
        CAMERA_ID = camera_id,
        CD_ID_ESTRUTURA = cd_estrutura,
        NAME_ESTRUTURA = name_estr,
        BEGIN = clip_begin,
        END   = clip_end,
        ATRIBUTOS = attrs,
        POLY = NULL,
        TS_KEYFRAME = NA
      )
      rects_out[[length(rects_out) + 1L]] <- list(rect = rect, intervals = list(seg))

      intervals_rows[[length(intervals_rows) + 1L]] <- tibble(
        SEGMENT_ID = seg_id,
        RECT_ID = rect_id,
        CAMERA_ID = camera_id,
        CD_ID_ESTRUTURA = cd_estrutura,
        NAME_ESTRUTURA = name_estr,
        SEG_IDX = 1L,
        BEGIN = as.POSIXct(clip_begin, tz = tz_clip),
        END   = as.POSIXct(clip_end,   tz = tz_clip),
        TS_KEYFRAME = as.POSIXct(NA, tz = tz_clip),
        ATRIBUTOS_JSON = if (isTRUE(include_attrs_json)) .attrs_to_json_chr(attrs) else NA_character_
      )
      next
    }

    if (isTRUE(clamp_to_clip)) {
      ts <- pmax(ts, clip_begin)
      ts <- pmin(ts, clip_end)
    }

    intervals <- list()
    prev_begin <- clip_begin

    # segmentos até cada keyframe i
    for (i in seq_along(kfs)) {
      end_i <- ts[i]
      if (end_i <= prev_begin) next

      seg_id <- seg_id + 1L

      attrs_i <- kfs[[i]]$ATRIBUTOS %||% rect$ATRIBUTOS %||% list()
      poly_i  <- kfs[[i]]$POLY %||% NULL

      seg <- list(
        SEGMENT_ID = seg_id,
        RECT_ID = rect_id,
        CAMERA_ID = camera_id,
        CD_ID_ESTRUTURA = cd_estrutura,
        NAME_ESTRUTURA = name_estr,
        BEGIN = prev_begin,
        END   = end_i,
        ATRIBUTOS = attrs_i,
        POLY = poly_i,
        TS_KEYFRAME = end_i
      )
      intervals[[length(intervals) + 1L]] <- seg

      intervals_rows[[length(intervals_rows) + 1L]] <- tibble(
        SEGMENT_ID = seg_id,
        RECT_ID = rect_id,
        CAMERA_ID = camera_id,
        CD_ID_ESTRUTURA = cd_estrutura,
        NAME_ESTRUTURA = name_estr,
        SEG_IDX = length(intervals),
        BEGIN = as.POSIXct(prev_begin, tz = tz_clip),
        END   = as.POSIXct(end_i,      tz = tz_clip),
        TS_KEYFRAME = as.POSIXct(end_i, tz = tz_clip),
        ATRIBUTOS_JSON = if (isTRUE(include_attrs_json)) .attrs_to_json_chr(attrs_i) else NA_character_
      )

      # explode POLY
      if (!is.null(poly_i) && length(poly_i) > 0) {
        if (is.data.frame(poly_i)) {
          xs <- poly_i$x; ys <- poly_i$y
        } else {
          xs <- vapply(poly_i, function(p) p$x %||% NA_real_, numeric(1))
          ys <- vapply(poly_i, function(p) p$y %||% NA_real_, numeric(1))
        }
        n <- length(xs)
        if (n > 0) {
          poly_rows[[length(poly_rows) + 1L]] <- tibble(
            SEGMENT_ID = seg_id,
            RECT_ID = rect_id,
            CAMERA_ID = camera_id,
            CD_ID_ESTRUTURA = cd_estrutura,
            NAME_ESTRUTURA = name_estr,
            SEG_IDX = length(intervals),
            BEGIN = as.POSIXct(prev_begin, tz = tz_clip),
            END   = as.POSIXct(end_i,      tz = tz_clip),
            POINT_IDX = seq_len(n),
            x = as.numeric(xs),
            y = as.numeric(ys)
          )
        }
      }

      prev_begin <- end_i
    }

    # último segmento [ts_last, clip_end]
    if (prev_begin < clip_end) {
      last_kf <- kfs[[length(kfs)]]
      attrs_last <- last_kf$ATRIBUTOS %||% rect$ATRIBUTOS %||% list()
      poly_last  <- last_kf$POLY %||% NULL
      ts_last    <- ts[length(ts)]

      seg_id <- seg_id + 1L
      seg <- list(
        SEGMENT_ID = seg_id,
        RECT_ID = rect_id,
        CAMERA_ID = camera_id,
        CD_ID_ESTRUTURA = cd_estrutura,
        NAME_ESTRUTURA = name_estr,
        BEGIN = prev_begin,
        END   = clip_end,
        ATRIBUTOS = attrs_last,
        POLY = poly_last,
        TS_KEYFRAME = ts_last
      )
      intervals[[length(intervals) + 1L]] <- seg

      intervals_rows[[length(intervals_rows) + 1L]] <- tibble(
        SEGMENT_ID = seg_id,
        RECT_ID = rect_id,
        CAMERA_ID = camera_id,
        CD_ID_ESTRUTURA = cd_estrutura,
        NAME_ESTRUTURA = name_estr,
        SEG_IDX = length(intervals),
        BEGIN = as.POSIXct(prev_begin, tz = tz_clip),
        END   = as.POSIXct(clip_end,   tz = tz_clip),
        TS_KEYFRAME = as.POSIXct(ts_last, tz = tz_clip),
        ATRIBUTOS_JSON = if (isTRUE(include_attrs_json)) .attrs_to_json_chr(attrs_last) else NA_character_
      )

      if (!is.null(poly_last) && length(poly_last) > 0) {
        if (is.data.frame(poly_last)) {
          xs <- poly_last$x; ys <- poly_last$y
        } else {
          xs <- vapply(poly_last, function(p) p$x %||% NA_real_, numeric(1))
          ys <- vapply(poly_last, function(p) p$y %||% NA_real_, numeric(1))
        }
        n <- length(xs)
        if (n > 0) {
          poly_rows[[length(poly_rows) + 1L]] <- tibble(
            SEGMENT_ID = seg_id,
            RECT_ID = rect_id,
            CAMERA_ID = camera_id,
            CD_ID_ESTRUTURA = cd_estrutura,
            NAME_ESTRUTURA = name_estr,
            SEG_IDX = length(intervals),
            BEGIN = as.POSIXct(prev_begin, tz = tz_clip),
            END   = as.POSIXct(clip_end,   tz = tz_clip),
            POINT_IDX = seq_len(n),
            x = as.numeric(xs),
            y = as.numeric(ys)
          )
        }
      }
    }

    rects_out[[length(rects_out) + 1L]] <- list(rect = rect, intervals = intervals)
  }

  intervals_tbl <- if (length(intervals_rows)) bind_rows(intervals_rows) else tibble()
  poly_tbl      <- if (length(poly_rows))      bind_rows(poly_rows)      else tibble()

  intervals_tbl <- intervals_tbl %>%
    arrange(CAMERA_ID, RECT_ID, BEGIN, END, SEGMENT_ID)

  poly_tbl <- poly_tbl %>%
    arrange(SEGMENT_ID, POINT_IDX)

  list(
    intervals_tbl = intervals_tbl,
    poly_tbl      = poly_tbl,
    rects         = rects_out
  )
}


OUTPUT_IA_string  = last(frames$OUTPUT_IA)
dt_hr_local_begin = last(frames$DT_HR_LOCAL_BEGIN)
dt_hr_local_end  = last(frames$DT_HR_LOCAL_END)

# ============================
# EXEMPLO DE USO
# ============================
res <- build_keyframe_interval_tables(
  output_ia_json = OUTPUT_IA_string,
  clip_begin = dt_hr_local_begin,
  clip_end   = dt_hr_local_end,
  tz_clip = "UTC",
  tz_keyframes = "UTC",
  clamp_to_clip = TRUE,
  include_attrs_json = TRUE
)

res$intervals_tbl  # 1 linha por segmento (BEGIN/END) + SEGMENT_ID
res$poly_tbl       # 1 linha por ponto do POLY, ligado pelo SEGMENT_ID


View(res$poly_tbl )
View(res$intervals_tbl)

x <- res$poly_tbl  |> filter(SEGMENT_ID == 1)
