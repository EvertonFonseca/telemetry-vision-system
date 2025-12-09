# frame_dao.R  (versão otimizada por CD_ID_FRAME)
box::use(
  DBI,
  dplyr[pull],
  ./utils[...],
  lubridate
)

# ==================================================
# fetch_frames
# --------------------------------------------------
# Retorna o índice de frames (ID + timestamp + câmera),
# leve e rápido, para montar a sequência do player.
#
# ✅ Melhoria:
# - inclui CD_ID_FRAME (chave primária/lookup ideal pro blob)
# - evita depender de igualdade de DATETIME pro blob
# ==================================================
#' @export
fetch_frames <- function(conn, time_begin, time_end, camera_id_vec, limit = NULL) {
  stopifnot(length(camera_id_vec) >= 1)

  tb <- as.POSIXct(time_begin, tz = "UTC")
  te <- as.POSIXct(time_end,   tz = "UTC")

  camera_id_vec <- as.integer(camera_id_vec)
  camera_id_vec <- camera_id_vec[!is.na(camera_id_vec)]
  stopifnot(length(camera_id_vec) >= 1)

  placeholders <- paste(rep("?", length(camera_id_vec)), collapse = ",")

  # OBS:
  # - ORDER BY "CD_ID_CAMERA, DT_HR_LOCAL" costuma ser mais amigo do índice,
  #   mas mantive "DT_HR_LOCAL, CD_ID_CAMERA" (se você prefere time-first).
  # - Se quiser performance máxima com índice, troque para:
  #   ORDER BY fc.CD_ID_CAMERA ASC, fc.DT_HR_LOCAL ASC
  sql <- paste0(
    "
    SELECT
      fc.CD_ID_FRAME,
      fc.DT_HR_LOCAL,
      fc.CD_ID_CAMERA
    FROM FRAME_CAMERA fc
    WHERE fc.DT_HR_LOCAL BETWEEN ? AND ?
      AND fc.CD_ID_CAMERA IN (", placeholders, ")
    ORDER BY fc.DT_HR_LOCAL ASC, fc.CD_ID_CAMERA ASC
    "
  )

  if (!is.null(limit) && is.finite(limit) && as.integer(limit) > 0) {
    sql <- paste0(sql, " LIMIT ", as.integer(limit))
  }

  params <- c(
    list(tb, te),
    as.list(camera_id_vec)
  )

  df <- DBI::dbGetQuery(conn, sql, params = params)

  if (!nrow(df)) {
    return(df)
  }

  # normaliza tipos
  df$CD_ID_FRAME  <- as.integer(df$CD_ID_FRAME)
  df$CD_ID_CAMERA <- as.integer(df$CD_ID_CAMERA)
  df$DT_HR_LOCAL  <- as.POSIXct(df$DT_HR_LOCAL, tz = "UTC")

  df
}

# ==================================================
# db_fetch_frame_by_id
# --------------------------------------------------
# Busca UM blob por CD_ID_FRAME (lookup direto por PK/índice).
# ✅ Melhoria: evita buscar por (camera + timestamp)
# ==================================================
#' @export
db_fetch_frame_by_id <- function(pool, frame_id) {
  sql <- "
    SELECT
      b.CD_ID_FRAME,
      b.DATA_FRAME
    FROM FRAME_CAMERA_BLOB b
    WHERE b.CD_ID_FRAME = ?
    LIMIT 1
  "
  out <- DBI::dbGetQuery(pool, sql, params = list(as.integer(frame_id)))
  if (!nrow(out)) return(out)

  out$CD_ID_FRAME <- as.integer(out$CD_ID_FRAME)
  out
}

# ==================================================
# db_fetch_many_frames_by_id
# --------------------------------------------------
# Prefetch em lote por CD_ID_FRAME.
# ✅ Melhoria: IN em IDs é MUITO melhor que IN em timestamps.
#
# Retorna {CD_ID_FRAME, DATA_FRAME}.
# Você pode reordenar no R para bater com o vetor de entrada.
# ==================================================
#' @export
db_fetch_many_frames_by_id <- function(pool, frame_id_vec) {

  frame_id_vec <- as.integer(frame_id_vec)
  frame_id_vec <- frame_id_vec[!is.na(frame_id_vec)]

  if (!length(frame_id_vec)) {
    return(data.frame(
      CD_ID_FRAME = integer(0),
      DATA_FRAME  = I(list())
    ))
  }

  placeholders <- paste(rep("?", length(frame_id_vec)), collapse = ",")

  sql <- paste0(
    "
    SELECT
      b.CD_ID_FRAME,
      b.DATA_FRAME
    FROM FRAME_CAMERA_BLOB b
    WHERE b.CD_ID_FRAME IN (", placeholders, ")
    "
  )

  out <- DBI::dbGetQuery(pool, sql, params = as.list(frame_id_vec))
  if (!nrow(out)) {
    return(data.frame(
      CD_ID_FRAME = integer(0),
      DATA_FRAME  = I(list())
    ))
  }

  out$CD_ID_FRAME <- as.integer(out$CD_ID_FRAME)

  # ✅ opcional: reordenar para mesma ordem do frame_id_vec
  ord <- match(frame_id_vec, out$CD_ID_FRAME)
  ord <- ord[!is.na(ord)]
  out <- out[ord, , drop = FALSE]

  out
}

# ==================================================
# Wrappers COMPATÍVEIS (para não quebrar o resto do app)
# --------------------------------------------------
# Se você já chama db_fetch_frame_raw(pool, camera_id, ts_utc),
# ele continua existindo, mas agora converte -> ID.
# ==================================================

#' @export
db_fetch_frame_raw <- function(pool, camera_id, ts_utc) {

  # 1) acha o CD_ID_FRAME via (camera + timestamp)
  sql_id <- "
    SELECT fc.CD_ID_FRAME
    FROM FRAME_CAMERA fc
    WHERE fc.CD_ID_CAMERA = ?
      AND fc.DT_HR_LOCAL = ?
    LIMIT 1
  "

  id <- DBI::dbGetQuery(
    pool,
    sql_id,
    params = list(
      as.integer(camera_id),
      as.POSIXct(ts_utc, tz = "UTC")
    )
  )

  if (!nrow(id)) return(data.frame(DATA_FRAME = I(list())))

  # 2) busca o blob por ID (lookup rápido)
  blob <- db_fetch_frame_by_id(pool, id$CD_ID_FRAME[1])
  if (!nrow(blob)) return(data.frame(DATA_FRAME = I(list())))

  # mantém o formato antigo: só DATA_FRAME
  data.frame(DATA_FRAME = blob$DATA_FRAME)
}

#' @export
db_fetch_many_frames <- function(pool, camera_id, ts_vec_utc) {

  ts_vec_utc <- as.POSIXct(ts_vec_utc, tz = "UTC")
  if (!length(ts_vec_utc)) {
    return(data.frame(
      DT_HR_LOCAL = as.POSIXct(character(0), tz = "UTC"),
      DATA_FRAME  = I(list())
    ))
  }

  # 1) resolve IDs para esses timestamps
  placeholders <- paste(rep("?", length(ts_vec_utc)), collapse = ",")

  sql_ids <- paste0(
    "
    SELECT
      fc.CD_ID_FRAME,
      fc.DT_HR_LOCAL
    FROM FRAME_CAMERA fc
    WHERE fc.CD_ID_CAMERA = ?
      AND fc.DT_HR_LOCAL IN (", placeholders, ")
    "
  )

  ids <- DBI::dbGetQuery(
    pool,
    sql_ids,
    params = c(list(as.integer(camera_id)), as.list(ts_vec_utc))
  )

  if (!nrow(ids)) {
    return(data.frame(
      DT_HR_LOCAL = as.POSIXct(character(0), tz = "UTC"),
      DATA_FRAME  = I(list())
    ))
  }

  ids$CD_ID_FRAME <- as.integer(ids$CD_ID_FRAME)
  ids$DT_HR_LOCAL <- as.POSIXct(ids$DT_HR_LOCAL, tz = "UTC")

  # 2) busca blobs por ID
  blobs <- db_fetch_many_frames_by_id(pool, ids$CD_ID_FRAME)
  if (!nrow(blobs)) {
    return(data.frame(
      DT_HR_LOCAL = ids$DT_HR_LOCAL,
      DATA_FRAME  = I(vector("list", nrow(ids)))
    ))
  }

  # 3) junta DT_HR_LOCAL + DATA_FRAME (na ordem dos timestamps)
  m <- match(ids$CD_ID_FRAME, blobs$CD_ID_FRAME)
  data.frame(
    DT_HR_LOCAL = ids$DT_HR_LOCAL,
    DATA_FRAME  = I(blobs$DATA_FRAME[m])
  )
}

# ==================================================
# Outros DAO (mantidos)
# ==================================================
#' @export
selectAllTypesPacote <- function(pool){
  DBI::dbGetQuery(pool,"SELECT * FROM TIPO_PACOTE")
}

#' @export
selectAllPacoteToTraino <- function(pool) {

  query <- "
    WITH oc_latest AS (
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
        GROUP_CONCAT(
          DISTINCT c.CD_ID_CAMERA
          ORDER BY c.CD_ID_CAMERA
          SEPARATOR ','
        ) AS CD_ID_CAMERAS
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
    ORDER BY p.CD_ID_IA
  "
  DBI::dbGetQuery(pool, query)
}
