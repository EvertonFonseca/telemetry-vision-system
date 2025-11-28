box::use(
  DBI,
  dplyr[pull],
  ./utils[...],
  lubridate
)

# ==================================================
# fetch_frames
# --------------------------------------------------
# Retorna o índice de frames (timestamp + câmera)
# usado pra montar rv$seq no player principal.
# NÃO traz o blob (leve e rápido).
# ==================================================
#' @export
fetch_frames <- function(conn, time_begin, time_end, camera_id_vec) {
  stopifnot(length(camera_id_vec) >= 1)

  tb <- as.POSIXct(time_begin, tz = "UTC")
  te <- as.POSIXct(time_end,   tz = "UTC")

  placeholders <- paste(rep("?", length(camera_id_vec)), collapse = ",")
  sql <- paste0(
    "
    SELECT
      fc.DT_HR_LOCAL,
      fc.CD_ID_CAMERA
    FROM FRAME_CAMERA fc
    WHERE fc.DT_HR_LOCAL BETWEEN ? AND ?
      AND fc.CD_ID_CAMERA IN (", placeholders, ")
    ORDER BY fc.DT_HR_LOCAL ASC, fc.CD_ID_CAMERA ASC
    "
  )

  params <- c(
    list(tb, te),
    as.list(as.integer(camera_id_vec))
  )

  df <- DBI::dbGetQuery(conn, sql, params = params)

  if (!nrow(df)) {
    return(df)
  }

  # normaliza coluna de tempo pra POSIXct UTC
  df$DT_HR_LOCAL <- as.POSIXct(df$DT_HR_LOCAL, tz = "UTC")

  df
}

# ==================================================
# db_fetch_frame_raw
# --------------------------------------------------
# Busca UM frame específico (câmera + timestamp exato)
# já trazendo o blob DATA_FRAME.
# Usado pelo player pra renderizar o frame atual.
# ==================================================
#' @export
db_fetch_frame_raw <- function(pool, camera_id, ts_utc) {

  sql <- "
    SELECT
      b.DATA_FRAME
    FROM FRAME_CAMERA fc
    INNER JOIN FRAME_CAMERA_BLOB b
      ON b.CD_ID_FRAME = fc.CD_ID_FRAME
    WHERE fc.CD_ID_CAMERA = ?
      AND fc.DT_HR_LOCAL = ?
    LIMIT 1
  "

  DBI::dbGetQuery(
    pool,
    sql,
    params = list(
      as.integer(camera_id),
      as.POSIXct(ts_utc, tz = "UTC")
    )
  )
}

# ==================================================
# db_fetch_many_frames
# --------------------------------------------------
# Prefetch em lote:
# recebe vários timestamps (mesma câmera),
# devolve {DT_HR_LOCAL, DATA_FRAME} pra cada ts.
#
# Isso alimenta o LRU cache e acelera a navegação.
# ==================================================
#' @export
db_fetch_many_frames <- function(pool, camera_id, ts_vec_utc) {

  # se não tiver nada pra buscar, retorna df vazio coerente
  if (length(ts_vec_utc) == 0) {
    return(data.frame(
      DT_HR_LOCAL = as.POSIXct(character(0), tz = "UTC"),
      DATA_FRAME  = I(list())
    ))
  }

  placeholders <- paste(rep("?", length(ts_vec_utc)), collapse = ",")

  sql <- paste0(
    "
    SELECT
      fc.DT_HR_LOCAL,
      b.DATA_FRAME
    FROM FRAME_CAMERA fc
    INNER JOIN FRAME_CAMERA_BLOB b
      ON b.CD_ID_FRAME = fc.CD_ID_FRAME
    WHERE fc.CD_ID_CAMERA = ?
      AND fc.DT_HR_LOCAL IN (", placeholders, ")
    "
  )

  params <- c(
    list(as.integer(camera_id)),
    as.list(as.POSIXct(ts_vec_utc, tz = "UTC"))
  )

  out <- DBI::dbGetQuery(pool, sql, params = params)

  if (nrow(out)) {
    out$DT_HR_LOCAL <- as.POSIXct(out$DT_HR_LOCAL, tz = "UTC")
  }

  out
}

#' @export
selectAllTypesPacote <- function(pool){
   DBI::dbGetQuery(pool,"SELECT * FROM TIPO_PACOTE")
}

# ==================================================
# selectAllPacoteToTraino
# --------------------------------------------------
# Retorna todos os pacotes de treino (PACOTE_IA),
# já agregando as câmeras associadas ao objeto
# (última config de cada objeto).
#
# IMPORTANTE:
# - essa parte não depende da mudança FRAME_CAMERA /
#   FRAME_CAMERA_BLOB, então a lógica continua válida.
# ==================================================
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
