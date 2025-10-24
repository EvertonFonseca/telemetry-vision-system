box::use(DBI,dplyr[pull],./utils[...],lubridate)

#' @export
fetch_frames <- function(conn,time_begin, time_end, camera_id_vec) {
  stopifnot(length(camera_id_vec) >= 1)
  tb <- as.POSIXct(time_begin, tz = "UTC")
  te <- as.POSIXct(time_end,   tz = "UTC")

  placeholders <- paste(rep("?", length(camera_id_vec)), collapse = ",")
  sql <- paste0(
    "SELECT DT_HR_LOCAL, CD_ID_CAMERA
     FROM FRAME_CAMERA
     WHERE DT_HR_LOCAL BETWEEN ? AND ?
       AND CD_ID_CAMERA IN (", placeholders, ")
     ORDER BY DT_HR_LOCAL ASC"
  )

  params <- c(list(tb, te), as.list(as.integer(camera_id_vec)))
  df <- DBI$dbGetQuery(conn, sql, params = params)
  if (!nrow(df)) return(df)
  df$DT_HR_LOCAL <- as.POSIXct(df$DT_HR_LOCAL, tz = "UTC")
  df
}

# ==================================================
# DB fetchers (frame único e lote para prefetch)
# ==================================================
#' @export
db_fetch_frame_raw <- function(pool, camera_id, ts_utc) {
  DBI$dbGetQuery(
    pool,
    "SELECT DATA_FRAME FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL = ? LIMIT 1",
    params = list(as.integer(camera_id), as.POSIXct(ts_utc, tz = "UTC"))
  )
}

#' @export
db_fetch_many_frames <- function(pool, camera_id, ts_vec_utc) {
  placeholders <- paste(rep("?", length(ts_vec_utc)), collapse = ",")
  sql <- paste0(
    "SELECT DT_HR_LOCAL, DATA_FRAME
       FROM FRAME_CAMERA
      WHERE CD_ID_CAMERA = ? AND DT_HR_LOCAL IN (", placeholders, ")"
  )
  DBI$dbGetQuery(pool, sql, params = c(list(as.integer(camera_id)), as.list(as.POSIXct(ts_vec_utc, tz = "UTC"))))
}

#' @export
selectAllPacoteToTraino <- function(pool){

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

  DBI$dbGetQuery(pool,query) 
}