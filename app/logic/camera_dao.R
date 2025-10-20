box::use(DBI,dplyr[pull],./utils[...],lubridate)

#' @export
checkifExistNameCamera <- function(con,name){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM CAMERA_VIEW WHERE NAME_CAMERA = ?',params = list(name)) |>  pull() > 0
  
} 
#' @export
checkifExistNameCameraEdit <- function(con,id,name){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM CAMERA_VIEW WHERE NAME_CAMERA = ? AND CD_ID_CAMERA != ?',params = list(name,id)) |>  pull() > 0
  
}
#' @export
checkifExistUrlCamera <- function(con,url){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM CAMERA_VIEW WHERE URL_CAMERA = ?',params = list(url)) |>  pull() > 0
  
}

#' @export
checkifExistUrlCameraEdit <- function(con,id,url){
  
  DBI$dbGetQuery(con,'SELECT COUNT(*) AS STATUS FROM CAMERA_VIEW WHERE URL_CAMERA = ? AND CD_ID_CAMERA != ?',params = list(url,id)) |>  pull() > 0
  
}
#' @export
selectAllFrame <- function(con){
  
  DBI$dbGetQuery(con, "SELECT CD_ID_FRAME, CD_ID_CAMERA,DATA_FRAME , DT_HR_LOCAL
                        FROM FRAME_CAMERA ORDER BY CD_ID_FRAME")
}

#' @export
selectAllFrameById <- function(con,id){
  
  DBI$dbGetQuery(con,paste0("SELECT CD_ID_CAMERA,DATA_FRAME, DT_HR_LOCAL
                              FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ",id," ORDER BY DATA_FRAME"))
}

#' @export
selectLastFrameById <- function(con,id){
  
  DBI$dbGetQuery(con,paste0("SELECT CD_ID_CAMERA,DATA_FRAME, DT_HR_LOCAL
                              FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ",id," ORDER BY DT_HR_LOCAL DESC LIMIT 1"))
}

#' @export
insertNewCamera <- function(con, id, camera) {
  
  stopifnot(!is.null(id), !is.null(camera$NAME_CAMERA), !is.null(camera$URL_CAMERA), !is.null(camera$FPS_CAMERA))
  
  n1 <- DBI$dbExecute(
    con,
    'INSERT INTO CAMERA_VIEW (CD_ID_CAMERA, NAME_CAMERA, URL_CAMERA) VALUES (?, ?, ?)',
    params = list(
      as.integer(id),
      camera$NAME_CAMERA,
      camera$URL_CAMERA
    )
  )
  
  n2 <- DBI$dbExecute(
    con,
    'INSERT INTO CAMERA_CONFIG (CD_ID_CAMERA, FPS_CAMERA) VALUES (?, ?)',
    params = list(
      as.integer(id),
      camera$FPS_CAMERA
    )
  )
  
  # Falha lógica se nenhum statement afetou linhas (casos raros, mas checamos)
  if (!.affected_ok(n1) || !.affected_ok(n2)) stop("INSERT não afetou linhas.")
  invisible(TRUE)
  
}

#' @export
updateCamera <- function(con, camera) {
  
  stopifnot(!is.null(camera$CD_ID_CAMERA))
  
  n1 <- DBI$dbExecute(
    con,
    'UPDATE CAMERA_VIEW
         SET NAME_CAMERA = ?,
             URL_CAMERA  = ?
       WHERE CD_ID_CAMERA = ?',
    params = list(
      camera$NAME_CAMERA,
      camera$URL_CAMERA,
      as.integer(camera$CD_ID_CAMERA)
    )
  )
  
  n2 <- DBI$dbExecute(
    con,
    'UPDATE CAMERA_CONFIG
          SET FPS_CAMERA = ?
        WHERE CD_ID_CAMERA = ?',
    params = list(
      camera$FPS_CAMERA,
      as.integer(camera$CD_ID_CAMERA)
    )
  )
  
  # Se nenhuma tabela foi afetada, consideramos falha lógica
  if (!.affected_ok(n1) && !.affected_ok(n2)) warning("UPDATE não afetou linhas.")
  invisible(TRUE)
}

# Continua retornando um data.frame (consulta)
#' @export
selectAllCameras <- function(con) {
  sql <- '
    SELECT
      cv.CD_ID_CAMERA,
      cv.NAME_CAMERA,
      cv.URL_CAMERA,
      cc.FPS_CAMERA,
      cc.DT_HR_LOCAL
    FROM CAMERA_VIEW cv
    LEFT JOIN (
      SELECT c1.CD_ID_CAMERA, c1.FPS_CAMERA, c1.DT_HR_LOCAL
      FROM CAMERA_CONFIG c1
      JOIN (
        SELECT CD_ID_CAMERA, MAX(DT_HR_LOCAL) AS max_dt
        FROM CAMERA_CONFIG
        GROUP BY CD_ID_CAMERA
      ) latest
        ON latest.CD_ID_CAMERA = c1.CD_ID_CAMERA
       AND latest.max_dt      = c1.DT_HR_LOCAL
    ) cc ON cc.CD_ID_CAMERA = cv.CD_ID_CAMERA
    ORDER BY cv.CD_ID_CAMERA
  '
  DBI$dbGetQuery(con, sql)
}

#' @export
selectCameraByComponente <- function(con,componente){
  
  sql <- paste0("SELECT * FROM CAMERA_VIEW WHERE CD_ID_CAMERA = ",componente$CD_ID_CAMERA)
  # Execute query
  DBI$dbGetQuery(con, sql)
}

#' @export
selectLastFramesByCamera <- function(con, camera, janela = 3L, chronological = TRUE) {
  # aceita data.frame/list com $CD_ID_CAMERA ou um id numérico diretamente
  camera_id <- if (is.list(camera) && !is.null(camera$CD_ID_CAMERA)) camera$CD_ID_CAMERA else camera
  camera_id <- as.integer(camera_id)
  janela    <- as.integer(janela)
  
  stopifnot(length(camera_id) == 1L, !is.na(camera_id), janela > 0L)
  
  # Mais recente primeiro (ORDER BY ... DESC) + LIMIT
  sql <- "
    SELECT *
    FROM FRAME_CAMERA
    WHERE CD_ID_CAMERA = ?
      AND DT_HR_LOCAL IS NOT NULL
    ORDER BY DT_HR_LOCAL DESC
    LIMIT ?;
   "
  
  # Use bind para evitar SQL injection e manter tipos corretos
  rs <- DBI$dbSendQuery(con, sql)
  on.exit(DBI$dbClearResult(rs), add = TRUE)
  DBI$dbBind(rs, list(camera_id, janela))
  out <- DBI$dbFetch(rs)
  
  # Se quiser devolver em ordem cronológica (do mais antigo para o mais novo dentro do recorte)
  if (chronological && nrow(out)) {
    out <- out[order(out$DT_HR_LOCAL), , drop = FALSE]
  }
  out
}

#' @export
selectFramesByCamera <- function(
  con,
  camera,
  janela = 3L,
  date_time = Sys.time(),
  time_limet_space = 1L, # 1 minuto precisa esta dentro do espaco de frames
  chronological  = FALSE,
  include_anchor = TRUE
) {
  camera_id <- if (is.list(camera) && !is.null(camera$CD_ID_CAMERA)) camera$CD_ID_CAMERA else camera
  camera_id <- as.integer(camera_id); janela <- as.integer(janela)
  stopifnot(length(camera_id) == 1L, !is.na(camera_id), janela > 0L)
  if (inherits(date_time, "character")) date_time <- as.POSIXct(date_time, tz = Sys.timezone())
  stopifnot(inherits(date_time, "POSIXct"))
  
  # operador temporal para incluir/excluir o frame âncora
  op <- if (isTRUE(include_anchor)) "<=" else "<"
  
  # ÚNICA query:
  # 1) subselect 'a' encontra o DT_HR_LOCAL do frame mais próximo (âncora)
  # 2) a query externa usa a.t0 para pegar 'janela' frames para trás
  sql <- sprintf("
    SELECT f.*
    FROM FRAME_CAMERA f
    JOIN (
      SELECT DT_HR_LOCAL AS t0
      FROM FRAME_CAMERA
      WHERE CD_ID_CAMERA = ?
        AND DT_HR_LOCAL IS NOT NULL
      ORDER BY
        ABS(TIMESTAMPDIFF(SECOND, DT_HR_LOCAL, ?)) ASC,
        DT_HR_LOCAL DESC
      LIMIT 1
    ) a
    WHERE f.CD_ID_CAMERA = ?
      AND f.DT_HR_LOCAL %s a.t0
    ORDER BY f.DT_HR_LOCAL DESC
    LIMIT ?;
  ", op)
  
  rs <- DBI$dbSendQuery(con, sql); on.exit(try(DBI$dbClearResult(rs), silent = TRUE), add = TRUE)
  # binds: id na âncora, date_time, id na janela, tamanho da janela
  DBI$dbBind(rs, list(camera_id, date_time, camera_id, janela))
  out <- DBI$dbFetch(rs)
  
  #try limit
  if(nrow(out) > 0){
    date_max    <- max(out$DT_HR_LOCAL)
    distancia_t <- difftime(date_time,date_max)   
    if(as.numeric(distancia_t, units="mins") > time_limet_space){
      return(NULL)
    }
  }
  
  if (chronological && nrow(out)) out <- out[order(out$DT_HR_LOCAL), , drop = FALSE]
  out
}

clearFramesByCamera <- function(con,id){
  
  # Apaga configs primeiro (se houver FK)
  n1 <- DBI$dbExecute(con, 'DELETE FROM FRAME_CAMERA WHERE CD_ID_CAMERA = ?', params = list(as.integer(id)))
  # Se nada foi apagado na VIEW principal, tratamos como falha lógica
  if (!.affected_ok(n1)) stop("DELETE não encontrou a câmera informada.")
  invisible(TRUE)
  
}

#' @export
deleteCamera <- function(con, id) {
  stopifnot(!is.null(id))
  
  # Apaga configs primeiro (se houver FK)
  n1 <- DBI$dbExecute(con, 'DELETE FROM CAMERA_VIEW   WHERE CD_ID_CAMERA = ?', params = list(as.integer(id)))
  # Se nada foi apagado na VIEW principal, tratamos como falha lógica
  if (!.affected_ok(n1)) stop("DELETE não encontrou a câmera informada.")
  invisible(TRUE)
  
}
