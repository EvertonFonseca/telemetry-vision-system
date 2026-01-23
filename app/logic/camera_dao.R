# app/infra/<seu_arquivo>.R  (Postgres / RPostgres)
box::use(
  DBI,
  dplyr[...],
  ./utils[...],
  lubridate
)

# ---- helpers ----
.affected_ok <- function(n) isTRUE(!is.na(n) && n > 0)
.count1 <- function(df) as.integer(df[[1]][[1]])

# (opcional) normaliza nomes de listas para lower
# use: camera <- .names_to_lower(camera)
.names_to_lower <- function(x) {
  if (!is.list(x) || is.null(names(x))) return(x)
  names(x) <- tolower(names(x))
  x
}

#' @export
checkifExistNameCamera <- function(con, name_camera) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*) from camera_view where name_camera = $1",
    params = list(name_camera)
  )
  .count1(df) > 0
}

#' @export
checkifExistNameCameraEdit <- function(con, cd_id_camera, name_camera) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*)
       from camera_view
      where name_camera = $1
        and cd_id_camera <> $2",
    params = list(name_camera, as.integer(cd_id_camera))
  )
  .count1(df) > 0
}

#' @export
checkifExistUrlCamera <- function(con, url_camera) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*) from camera_view where url_camera = $1",
    params = list(url_camera)
  )
  .count1(df) > 0
}

#' @export
checkifExistUrlCameraEdit <- function(con, cd_id_camera, url_camera) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*)
       from camera_view
      where url_camera = $1
        and cd_id_camera <> $2",
    params = list(url_camera, as.integer(cd_id_camera))
  )
  .count1(df) > 0
}

#' @export
selectAllFrame <- function(con) {
  sql <- "
    select
      fc.cd_id_frame,
      fc.cd_id_camera,
      fc.dt_hr_local,
      b.data_frame
    from frame_camera fc
    left join frame_camera_blob b
      on b.cd_id_frame = fc.cd_id_frame
    order by fc.cd_id_frame asc
  "
  DBI$dbGetQuery(con, sql)
}

#' @export
selectAllFrameById <- function(con, cd_id_camera) {
  cd_id_camera <- as.integer(cd_id_camera)

  sql <- "
    select
      fc.cd_id_frame,
      fc.cd_id_camera,
      fc.dt_hr_local,
      b.data_frame
    from frame_camera fc
    left join frame_camera_blob b
      on b.cd_id_frame = fc.cd_id_frame
    where fc.cd_id_camera = $1
    order by fc.dt_hr_local asc
  "
  DBI$dbGetQuery(con, sql, params = list(cd_id_camera))
}

#' @export
selectLastFrameById <- function(con, cd_id_camera) {
  cd_id_camera <- as.integer(cd_id_camera)

  sql <- "
    select
      fc.cd_id_frame,
      fc.cd_id_camera,
      fc.dt_hr_local,
      b.data_frame
    from frame_camera fc
    left join frame_camera_blob b
      on b.cd_id_frame = fc.cd_id_frame
    where fc.cd_id_camera = $1
    order by fc.dt_hr_local desc
    limit 1
  "
  DBI$dbGetQuery(con, sql, params = list(cd_id_camera))
}

#' @export
insertNewCamera <- function(con, cd_id_camera, camera) {
  camera <- .names_to_lower(camera)

  stopifnot(
    !is.null(cd_id_camera),
    !is.null(camera$name_camera),
    !is.null(camera$url_camera),
    !is.null(camera$fps_camera)
  )

  n1 <- DBI$dbExecute(
    con,
    "insert into camera_view (cd_id_camera, name_camera, url_camera)
     values ($1, $2, $3)",
    params = list(
      as.integer(cd_id_camera),
      camera$name_camera,
      camera$url_camera
    )
  )

  n2 <- DBI$dbExecute(
    con,
    "insert into camera_config (cd_id_camera, fps_camera)
     values ($1, $2)",
    params = list(
      as.integer(cd_id_camera),
      as.integer(camera$fps_camera)
    )
  )

  if (!.affected_ok(n1) || !.affected_ok(n2)) stop("insert nao afetou linhas.")
  invisible(TRUE)
}

#' @export
updateCamera <- function(con, camera) {
  camera <- .names_to_lower(camera)

  stopifnot(!is.null(camera$cd_id_camera))

  n1 <- DBI$dbExecute(
    con,
    "update camera_view
        set name_camera = $1,
            url_camera  = $2
      where cd_id_camera = $3",
    params = list(
      camera$name_camera,
      camera$url_camera,
      as.integer(camera$cd_id_camera)
    )
  )

  n2 <- DBI$dbExecute(
    con,
    "update camera_config
        set fps_camera = $1
      where cd_id_camera = $2",
    params = list(
      as.integer(camera$fps_camera),
      as.integer(camera$cd_id_camera)
    )
  )

  if (!.affected_ok(n1) && !.affected_ok(n2)) warning("update nao afetou linhas.")
  invisible(TRUE)
}

#' @export
selectAllCameras <- function(con) {
  sql <- "
    select
      cv.cd_id_camera,
      cv.name_camera,
      cv.url_camera,
      cc.fps_camera,
      cc.dt_hr_local
    from camera_view cv
    left join (
      select c1.cd_id_camera, c1.fps_camera, c1.dt_hr_local
      from camera_config c1
      join (
        select cd_id_camera, max(dt_hr_local) as max_dt
        from camera_config
        group by cd_id_camera
      ) latest
        on latest.cd_id_camera = c1.cd_id_camera
       and latest.max_dt      = c1.dt_hr_local
    ) cc on cc.cd_id_camera = cv.cd_id_camera
    order by cv.cd_id_camera
  "
  DBI$dbGetQuery(con, sql)
}

#' @export
selectCameraByComponente <- function(con, componente) {
  componente <- .names_to_lower(componente)
  stopifnot(!is.null(componente$cd_id_camera))

  sql <- "select * from camera_view where cd_id_camera = $1"
  DBI$dbGetQuery(con, sql, params = list(as.integer(componente$cd_id_camera)))
}

#' @export
selectLastFramesByCamera <- function(con, camera, janela = 3L, chronological = TRUE) {
  camera <- .names_to_lower(camera)

  camera_id <- if (is.list(camera) && !is.null(camera$cd_id_camera)) camera$cd_id_camera else camera
  camera_id <- as.integer(camera_id)
  janela    <- as.integer(janela)

  stopifnot(length(camera_id) == 1L, !is.na(camera_id), janela > 0L)

  sql <- "
     select
      fc.cd_id_frame,
      fc.cd_id_camera,
      fc.dt_hr_local,
      fcb.id_frame_blob,
      fcb.data_frame
    from frame_camera fc
    left join frame_camera_blob fcb
           on fcb.cd_id_frame = fc.cd_id_frame
    where fc.cd_id_camera = $1
      and fc.dt_hr_local is not null
    order by fc.dt_hr_local desc
    limit $2
   "

  rs <- DBI$dbSendQuery(con, sql)
  on.exit(DBI$dbClearResult(rs), add = TRUE)
  DBI$dbBind(rs, list(camera_id, janela))
  out <- DBI$dbFetch(rs)

  if (chronological && nrow(out)) out <- out[order(out$dt_hr_local), , drop = FALSE]
  out
}

#' @export
selectFramesByCamera <- function(
  con,
  camera,
  janela = 3L,
  date_time = Sys.time(),
  time_limet_space = 1L,
  chronological  = FALSE,
  include_anchor = TRUE
) {
  camera <- .names_to_lower(camera)

  camera_id <- if (is.list(camera) && !is.null(camera$cd_id_camera)) camera$cd_id_camera else camera
  camera_id <- as.integer(camera_id)
  janela    <- as.integer(janela)

  stopifnot(length(camera_id) == 1L, !is.na(camera_id), janela > 0L)
  if (inherits(date_time, "character")) date_time <- as.POSIXct(date_time, tz = Sys.timezone())
  stopifnot(inherits(date_time, "POSIXct"))

  op <- if (isTRUE(include_anchor)) "<=" else "<"

  sql <- sprintf("
     select
      fc.cd_id_frame,
      fc.cd_id_camera,
      fc.dt_hr_local,
      fcb.id_frame_blob,
      fcb.data_frame
    from frame_camera fc
    join (
      select dt_hr_local as t0
      from frame_camera
      where cd_id_camera = $1
        and dt_hr_local is not null
      order by
        abs(extract(epoch from (dt_hr_local - $2))) asc,
        dt_hr_local desc
      limit 1
    ) a on 1=1
    left join frame_camera_blob fcb
           on fcb.cd_id_frame = fc.cd_id_frame
    where fc.cd_id_camera = $3
      and fc.dt_hr_local %s a.t0
    order by fc.dt_hr_local desc
    limit $4
  ", op)

  rs <- DBI$dbSendQuery(con, sql)
  on.exit(try(DBI$dbClearResult(rs), silent = TRUE), add = TRUE)
  DBI$dbBind(rs, list(camera_id, date_time, camera_id, janela))
  out <- DBI$dbFetch(rs)

  if (nrow(out) > 0) {
    date_max    <- max(out$dt_hr_local)
    distancia_t <- difftime(date_time, date_max)
    if (as.numeric(distancia_t, units = "mins") > time_limet_space) return(NULL)
  }

  if (chronological && nrow(out)) out <- out[order(out$dt_hr_local), , drop = FALSE]
  out
}

#' @export
clearFramesByCamera <- function(con, cd_id_camera) {
  n1 <- DBI$dbExecute(
    con,
    "delete from frame_camera where cd_id_camera = $1",
    params = list(as.integer(cd_id_camera))
  )
  if (!.affected_ok(n1)) stop("delete nao encontrou a camera informada.")
  invisible(TRUE)
}

#' @export
deleteCamera <- function(con, cd_id_camera) {
  stopifnot(!is.null(cd_id_camera))

  n1 <- DBI$dbExecute(
    con,
    "delete from camera_view where cd_id_camera = $1",
    params = list(as.integer(cd_id_camera))
  )
  if (!.affected_ok(n1)) stop("delete nao encontrou a camera informada.")
  invisible(TRUE)
}
