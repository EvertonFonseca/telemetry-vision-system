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
