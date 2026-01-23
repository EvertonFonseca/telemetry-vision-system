# frame_dao.R  (Postgres / RPostgres) - lowercase + $1..$n + listas em lower
box::use(
  DBI,
  dplyr[pull],
  ./utils[...],
  lubridate
)

# ---- helpers ----
.names_to_lower <- function(x) {
  if (!is.list(x) || is.null(names(x))) return(x)
  names(x) <- tolower(names(x))
  x
}

.df_names_lower <- function(df) {
  if (is.data.frame(df) && ncol(df)) names(df) <- tolower(names(df))
  df
}

# placeholders $n
.ph_seq <- function(n, start = 1L) paste0("$", start:(start + n - 1L), collapse = ",")

# ==================================================
# fetch_frames
# ==================================================
#' @export
fetch_frames <- function(conn, time_begin, time_end, camera_id_vec, limit = NULL) {
  stopifnot(length(camera_id_vec) >= 1)

  tb <- as.POSIXct(time_begin, tz = "UTC")
  te <- as.POSIXct(time_end,   tz = "UTC")

  camera_id_vec <- as.integer(camera_id_vec)
  camera_id_vec <- camera_id_vec[!is.na(camera_id_vec)]
  stopifnot(length(camera_id_vec) >= 1)

  # params:
  # $1 = tb, $2 = te, $3.. = camera_ids
  in_ph <- .ph_seq(length(camera_id_vec), start = 3L)

  sql <- sprintf("
    select
      fc.cd_id_frame,
      fc.dt_hr_local,
      fc.cd_id_camera
    from frame_camera fc
    where fc.dt_hr_local between $1 and $2
      and fc.cd_id_camera in (%s)
    order by fc.dt_hr_local asc, fc.cd_id_camera asc
  ", in_ph)

  if (!is.null(limit) && is.finite(limit) && as.integer(limit) > 0) {
    sql <- paste0(sql, " limit ", as.integer(limit))
  }

  params <- c(list(tb, te), as.list(camera_id_vec))

  df <- DBI::dbGetQuery(conn, sql, params = params)
  df <- .df_names_lower(df)

  if (!nrow(df)) return(df)

  df$cd_id_frame  <- as.integer(df$cd_id_frame)
  df$cd_id_camera <- as.integer(df$cd_id_camera)
  df$dt_hr_local  <- as.POSIXct(df$dt_hr_local, tz = "UTC")

  df
}

# ==================================================
# db_fetch_frame_by_id
# ==================================================
#' @export
db_fetch_frame_by_id <- function(pool, frame_id) {
  sql <- "
    select
      b.cd_id_frame,
      b.data_frame
    from frame_camera_blob b
    where b.cd_id_frame = $1
    limit 1
  "
  out <- DBI::dbGetQuery(pool, sql, params = list(as.integer(frame_id)))
  out <- .df_names_lower(out)
  if (!nrow(out)) return(out)

  out$cd_id_frame <- as.integer(out$cd_id_frame)
  out
}

# ==================================================
# db_fetch_many_frames_by_id
# ==================================================
#' @export
db_fetch_many_frames_by_id <- function(pool, frame_id_vec) {
  frame_id_vec <- as.integer(frame_id_vec)
  frame_id_vec <- frame_id_vec[!is.na(frame_id_vec)]

  if (!length(frame_id_vec)) {
    return(data.frame(
      cd_id_frame = integer(0),
      data_frame  = I(list())
    ))
  }

  in_ph <- .ph_seq(length(frame_id_vec), start = 1L)

  sql <- sprintf("
    select
      b.cd_id_frame,
      b.data_frame
    from frame_camera_blob b
    where b.cd_id_frame in (%s)
  ", in_ph)

  out <- DBI::dbGetQuery(pool, sql, params = as.list(frame_id_vec))
  out <- .df_names_lower(out)

  if (!nrow(out)) {
    return(data.frame(
      cd_id_frame = integer(0),
      data_frame  = I(list())
    ))
  }

  out$cd_id_frame <- as.integer(out$cd_id_frame)

  # reordena para mesma ordem do vetor de entrada (quando possível)
  ord <- match(frame_id_vec, out$cd_id_frame)
  ord <- ord[!is.na(ord)]
  out <- out[ord, , drop = FALSE]

  out
}

# ==================================================
# Wrappers COMPATÍVEIS
# ==================================================

#' @export
db_fetch_frame_raw <- function(pool, cd_id_camera, ts_utc) {

  sql_id <- "
    select fc.cd_id_frame
    from frame_camera fc
    where fc.cd_id_camera = $1
      and fc.dt_hr_local  = $2
    limit 1
  "

  id <- DBI::dbGetQuery(
    pool,
    sql_id,
    params = list(
      as.integer(cd_id_camera),
      as.POSIXct(ts_utc, tz = "UTC")
    )
  )
  id <- .df_names_lower(id)

  if (!nrow(id)) return(data.frame(data_frame = I(list())))

  blob <- db_fetch_frame_by_id(pool, id$cd_id_frame[1])
  if (!nrow(blob)) return(data.frame(data_frame = I(list())))

  data.frame(data_frame = blob$data_frame)
}

#' @export
db_fetch_many_frames <- function(pool, cd_id_camera, ts_vec_utc) {

  ts_vec_utc <- as.POSIXct(ts_vec_utc, tz = "UTC")
  if (!length(ts_vec_utc)) {
    return(data.frame(
      dt_hr_local = as.POSIXct(character(0), tz = "UTC"),
      data_frame  = I(list())
    ))
  }

  # params:
  # $1 = cd_id_camera, $2.. = timestamps
  in_ph <- .ph_seq(length(ts_vec_utc), start = 2L)

  sql_ids <- sprintf("
    select
      fc.cd_id_frame,
      fc.dt_hr_local
    from frame_camera fc
    where fc.cd_id_camera = $1
      and fc.dt_hr_local in (%s)
  ", in_ph)

  ids <- DBI::dbGetQuery(
    pool,
    sql_ids,
    params = c(list(as.integer(cd_id_camera)), as.list(ts_vec_utc))
  )
  ids <- .df_names_lower(ids)

  if (!nrow(ids)) {
    return(data.frame(
      dt_hr_local = as.POSIXct(character(0), tz = "UTC"),
      data_frame  = I(list())
    ))
  }

  ids$cd_id_frame <- as.integer(ids$cd_id_frame)
  ids$dt_hr_local <- as.POSIXct(ids$dt_hr_local, tz = "UTC")

  blobs <- db_fetch_many_frames_by_id(pool, ids$cd_id_frame)
  blobs <- .df_names_lower(blobs)

  if (!nrow(blobs)) {
    return(data.frame(
      dt_hr_local = ids$dt_hr_local,
      data_frame  = I(vector("list", nrow(ids)))
    ))
  }

  m <- match(ids$cd_id_frame, blobs$cd_id_frame)

  data.frame(
    dt_hr_local = ids$dt_hr_local,
    data_frame  = I(blobs$data_frame[m])
  )
}

# ==================================================
# Outros DAO (convertidos para Postgres + lowercase)
# ==================================================

#' @export
selectAllTypesPacote <- function(pool) {
  df <- DBI::dbGetQuery(pool, "select * from tipo_pacote order by cd_id_tipo_pacote")
  .df_names_lower(df)
}

#' @export
selectAllPacoteToTraino <- function(pool) {

  # Postgres:
  # - group_concat -> string_agg
  # - separator -> ',' dentro do string_agg
  # - window ok
  query <- "
    with oc_latest as (
      select cd_id_obj_conf, cd_id_objeto
      from (
        select
          oc.*,
          row_number() over (
            partition by oc.cd_id_objeto
            order by oc.dt_hr_local desc, oc.cd_id_obj_conf desc
          ) as rn
        from objeto_config oc
      ) x
      where rn = 1
    ),

    cams as (
      select
        ol.cd_id_objeto,
        coalesce(
          string_agg(distinct c.cd_id_camera::text, ',' order by c.cd_id_camera),
          ''
        ) as cd_id_cameras
      from oc_latest ol
      left join componente c
        on c.cd_id_obj_conf = ol.cd_id_obj_conf
      group by ol.cd_id_objeto
    )

    select
      p.*,
      coalesce(cams.cd_id_cameras, '') as cd_id_cameras
    from pacote_ia p
    left join cams
      on cams.cd_id_objeto = p.cd_id_objeto
    order by p.cd_id_ia
  "

  df <- DBI::dbGetQuery(pool, query)
  .df_names_lower(df)
}
