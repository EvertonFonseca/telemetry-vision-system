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

.upper_names <- function(df) {
  if (is.data.frame(df) && ncol(df)) names(df) <- toupper(names(df))
  df
}

.sanitize_file_piece <- function(x, fallback = "dataset") {
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(trimws(x))) return(fallback)

  x <- tolower(trimws(x))
  x <- gsub("\\s+", "_", x)
  x <- gsub("[^a-z0-9_\\-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  if (!nzchar(x)) fallback else x
}

.parse_camera_ids <- function(x) {
  if (is.null(x) || !length(x)) return(integer(0))

  value <- as.character(x[[1]])
  if (is.na(value) || !nzchar(trimws(value))) return(integer(0))

  out <- unlist(strsplit(value, ",", fixed = TRUE), use.names = FALSE)
  out <- trimws(out)
  out <- out[nzchar(out)]
  out <- suppressWarnings(as.integer(out))
  out <- out[is.finite(out)]
  unique(out)
}

.normalize_components_upper <- function(comps) {
  empty <- tibble::tibble(
    NAME_COMPONENTE = character(0),
    CD_ID_COMPONENTE = integer(0),
    CD_ID_CAMERA = integer(0)
  )

  if (is.null(comps) || !length(comps)) return(empty)

  if (is.list(comps) && length(comps) == 1L && is.character(comps[[1]])) {
    comps <- comps[[1]]
  }

  if (is.character(comps) && length(comps) == 1L) {
    txt <- trimws(comps[[1]])
    if (!nzchar(txt)) return(empty)

    comps <- tryCatch(
      jsonlite::fromJSON(txt, simplifyVector = TRUE),
      error = function(e) NULL
    )
  }

  if (is.null(comps)) return(empty)

  comps <- tryCatch(
    tibble::as_tibble(comps),
    error = function(e) {
      tryCatch(
        tibble::as_tibble(as.data.frame(comps, stringsAsFactors = FALSE)),
        error = function(e2) empty
      )
    }
  )

  comps <- .upper_names(comps)

  rename_if_exists <- function(df, from, to) {
    if (to %in% names(df)) return(df)
    if (from %in% names(df)) names(df)[names(df) == from] <- to
    df
  }

  comps <- rename_if_exists(comps, "NAME_COMPONENT", "NAME_COMPONENTE")
  comps <- rename_if_exists(comps, "NOME_COMPONENTE", "NAME_COMPONENTE")
  comps <- rename_if_exists(comps, "ID_COMPONENTE", "CD_ID_COMPONENTE")
  comps <- rename_if_exists(comps, "ID_CAMERA", "CD_ID_CAMERA")

  if (!"NAME_COMPONENTE" %in% names(comps)) comps$NAME_COMPONENTE <- NA_character_
  if (!"CD_ID_COMPONENTE" %in% names(comps)) comps$CD_ID_COMPONENTE <- NA_integer_
  if (!"CD_ID_CAMERA" %in% names(comps)) comps$CD_ID_CAMERA <- NA_integer_

  comps$NAME_COMPONENTE <- toupper(as.character(comps$NAME_COMPONENTE))
  comps$CD_ID_COMPONENTE <- suppressWarnings(as.integer(comps$CD_ID_COMPONENTE))
  comps$CD_ID_CAMERA <- suppressWarnings(as.integer(comps$CD_ID_CAMERA))

  comps
}

#' @export
fetch_frames_blob <- function(pool, time_begin, time_end, camera_id_vec, limit = 1000L) {
  stopifnot(length(camera_id_vec) >= 1)

  tb <- as.POSIXct(time_begin, tz = "UTC")
  te <- as.POSIXct(time_end, tz = "UTC")

  camera_id_vec <- suppressWarnings(as.integer(camera_id_vec))
  camera_id_vec <- camera_id_vec[is.finite(camera_id_vec)]
  stopifnot(length(camera_id_vec) >= 1)

  in_ph <- .ph_seq(length(camera_id_vec), start = 3L)
  limit_idx <- 3L + length(camera_id_vec)

  sql <- sprintf(
    "
    select
      b.cd_id_frame,
      fc.dt_hr_local,
      fc.cd_id_camera,
      b.data_frame
    from frame_camera fc
    inner join frame_camera_blob b
      on b.cd_id_frame = fc.cd_id_frame
    where fc.dt_hr_local between $1 and $2
      and fc.cd_id_camera in (%s)
    order by fc.dt_hr_local asc, fc.cd_id_camera asc
    limit $%d
    ",
    in_ph,
    limit_idx
  )

  params <- c(list(tb, te), as.list(camera_id_vec), list(as.integer(limit)))

  df <- DBI::dbGetQuery(pool, sql, params = params)
  df <- .df_names_lower(df)

  if (!nrow(df)) return(df)

  df$cd_id_frame <- suppressWarnings(as.integer(df$cd_id_frame))
  df$cd_id_camera <- suppressWarnings(as.integer(df$cd_id_camera))
  df$dt_hr_local <- as.POSIXct(df$dt_hr_local, tz = "UTC")

  df
}

.build_pacote_query <- function(filter_by_objeto = FALSE) {
  where_clause <- if (isTRUE(filter_by_objeto)) "where p.cd_id_objeto = $1" else ""

  sprintf(
    "
    with oc_latest as (
      select
        x.cd_id_obj_conf,
        x.cd_id_objeto,
        o.grupo,
        o.name_objeto,
        o.cd_id_setor,
        s.name_setor
      from (
        select
          oc.*,
          row_number() over (
            partition by oc.cd_id_objeto
            order by oc.dt_hr_local desc, oc.cd_id_obj_conf desc
          ) as rn
        from objeto_config oc
      ) x
      left join objeto o
        on o.cd_id_objeto = x.cd_id_objeto
      left join setor s
        on s.cd_id_setor = o.cd_id_setor
      where x.rn = 1
    ),
    comps_base as (
      select
        ol.cd_id_objeto,
        ol.name_objeto,
        ol.grupo,
        ol.cd_id_setor,
        ol.name_setor,
        c.cd_id_camera,
        c.cd_id_componente,
        c.name_componente,
        c.poligno_componente,
        e.cd_id_estrutura,
        e.name_estrutura
      from oc_latest ol
      left join componente c
        on c.cd_id_obj_conf = ol.cd_id_obj_conf
      left join estrutura e
        on e.cd_id_estrutura = c.cd_id_estrutura
    ),
    cams as (
      select
        cb.cd_id_objeto,
        max(cb.name_objeto) as name_objeto,
        bool_or(cb.grupo) as grupo,
        max(cb.cd_id_setor) as cd_id_setor,
        max(cb.name_setor) as name_setor,
        coalesce((
          select string_agg(d.cd_id_camera::text, ',' order by d.cd_id_camera)
          from (
            select distinct cd_id_camera
            from comps_base
            where cd_id_objeto = cb.cd_id_objeto
              and cd_id_camera is not null
          ) d
        ), '') as cd_id_cameras,
        coalesce(
          jsonb_agg(
            jsonb_build_object(
              'cd_id_camera', cb.cd_id_camera,
              'cd_id_componente', cb.cd_id_componente,
              'name_componente', cb.name_componente,
              'poligno_componente', cb.poligno_componente,
              'cd_id_estrutura', cb.cd_id_estrutura,
              'name_estrutura', cb.name_estrutura
            )
          ) filter (where cb.cd_id_componente is not null),
          '[]'::jsonb
        ) as polignos_componentes
      from comps_base cb
      group by cb.cd_id_objeto
    )
    select
      p.*,
      tp.name_tipo_pacote,
      cams.name_objeto,
      cams.grupo,
      cams.cd_id_setor,
      cams.name_setor,
      coalesce(cams.cd_id_cameras, '') as cd_id_cameras,
      coalesce(cams.polignos_componentes, '[]'::jsonb) as polignos_componentes
    from pacote_ia p
    left join cams
      on cams.cd_id_objeto = p.cd_id_objeto
    left join tipo_pacote tp
      on tp.cd_id_tipo_pacote = p.cd_id_tipo_pacote
    %s
    order by p.cd_id_objeto, p.cd_id_ia
    ",
    where_clause
  )
}

#' @export
selectBuildTreinoPacotes <- function(pool, cd_id_objeto = NULL) {
  has_filter <- !is.null(cd_id_objeto) && length(cd_id_objeto) >= 1 && is.finite(as.integer(cd_id_objeto[[1]]))
  sql <- .build_pacote_query(filter_by_objeto = has_filter)
  df <- if (isTRUE(has_filter)) {
    DBI::dbGetQuery(pool, sql, params = list(as.integer(cd_id_objeto[[1]])))
  } else {
    DBI::dbGetQuery(pool, sql)
  }
  .df_names_lower(df)
}

#' @export
buildTreinoFileName <- function(name_setor, name_objeto) {
  setor_piece <- .sanitize_file_piece(name_setor, fallback = "setor")
  obj_piece <- .sanitize_file_piece(name_objeto, fallback = "objeto")
  paste0("dataset_", setor_piece, "_", obj_piece, ".rds")
}

#' @export
selectBuildTreinoSummary <- function(pool) {
  pacotes <- selectBuildTreinoPacotes(pool)

  out <- data.frame(
    cd_id_objeto = integer(0),
    name_setor = character(0),
    name_objeto = character(0),
    tipos_pacote = character(0),
    qtd_pacotes = integer(0),
    dt_hr_local_begin = as.POSIXct(character(0), tz = "UTC"),
    dt_hr_local_end = as.POSIXct(character(0), tz = "UTC"),
    arquivo_rds = character(0),
    stringsAsFactors = FALSE
  )

  if (!nrow(pacotes)) return(out)

  if ("dt_hr_local_begin" %in% names(pacotes)) {
    pacotes$dt_hr_local_begin <- as.POSIXct(pacotes$dt_hr_local_begin, tz = "UTC")
  }
  if ("dt_hr_local_end" %in% names(pacotes)) {
    pacotes$dt_hr_local_end <- as.POSIXct(pacotes$dt_hr_local_end, tz = "UTC")
  }

  safe_extreme_time <- function(x, fn = min) {
    x <- as.POSIXct(x, tz = "UTC")
    x <- x[!is.na(x)]
    if (!length(x)) return(as.POSIXct(NA, tz = "UTC"))
    fn(x)
  }

  split_by_obj <- split(pacotes, as.character(pacotes$cd_id_objeto))
  rows <- lapply(split_by_obj, function(df_obj) {
    tipos <- unique(trimws(as.character(df_obj$name_tipo_pacote)))
    tipos <- tipos[!is.na(tipos) & nzchar(tipos)]
    tipos <- sort(tipos)

    data.frame(
      cd_id_objeto = suppressWarnings(as.integer(df_obj$cd_id_objeto[[1]])),
      name_setor = as.character(df_obj$name_setor[[1]]),
      name_objeto = as.character(df_obj$name_objeto[[1]]),
      tipos_pacote = if (length(tipos)) paste(tipos, collapse = ", ") else "-",
      qtd_pacotes = nrow(df_obj),
      dt_hr_local_begin = safe_extreme_time(df_obj$dt_hr_local_begin, fn = min),
      dt_hr_local_end = safe_extreme_time(df_obj$dt_hr_local_end, fn = max),
      arquivo_rds = buildTreinoFileName(df_obj$name_setor[[1]], df_obj$name_objeto[[1]]),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @export
buildTreinoDatasetRow <- function(pool, pacote_row, limit = 150L, logger = NULL) {
  logger <- if (is.function(logger)) logger else function(...) invisible(NULL)

  if (is.null(pacote_row) || !nrow(pacote_row)) return(NULL)

  y1 <- .upper_names(pacote_row[1, , drop = FALSE])

  pacote_id <- if ("CD_ID_IA" %in% names(y1)) y1$CD_ID_IA[[1]] else NA_integer_
  pacote_titulo <- if ("TITULO_IA" %in% names(y1)) y1$TITULO_IA[[1]] else ""

  logger(sprintf("Pacote %s: %s", pacote_id, pacote_titulo))

  output <- tryCatch(
    jsonlite::fromJSON(y1$OUTPUT_IA[[1]], simplifyVector = FALSE),
    error = function(e) {
      logger(sprintf("  - Falha ao ler OUTPUT_IA: %s", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(output)) return(NULL)

  comps <- .normalize_components_upper(y1$POLIGNOS_COMPONENTES[[1]])

  if (length(output)) {
    for (z in seq_along(output)) {
      nome_comp <- toupper(names(output[[z]])[1])
      idx <- which(toupper(as.character(comps$NAME_COMPONENTE)) %in% nome_comp)
      if (!length(idx)) next
      output[[z]]$ID <- comps$CD_ID_COMPONENTE[[idx[1]]]
    }
  }

  output_json <- as.character(jsonlite::toJSON(output, auto_unbox = TRUE, null = "null"))
  camera_ids <- .parse_camera_ids(y1$CD_ID_CAMERAS)

  if (!length(camera_ids)) {
    logger("  - Nenhuma camera vinculada. Pacote ignorado.")
    return(NULL)
  }

  payload <- vector("list", length(camera_ids))
  status <- logical(length(camera_ids))

  for (j in seq_along(camera_ids)) {
    camera_id <- suppressWarnings(as.integer(camera_ids[[j]]))
    componentes <- comps[comps$CD_ID_CAMERA == camera_id, , drop = FALSE]
    frames_db <- fetch_frames_blob(
      pool,
      time_begin = y1$DT_HR_LOCAL_BEGIN[[1]],
      time_end = y1$DT_HR_LOCAL_END[[1]],
      camera_id_vec = camera_id,
      limit = limit
    )

    logger(sprintf("  - Camera %s: %d frame(s)", camera_id, nrow(frames_db)))

    payload[[j]] <- tibble::tibble(
      COMPONENTES = list(componentes),
      FRAMES = list(.upper_names(frames_db))
    )
    status[[j]] <- nrow(frames_db) > 0
  }

  if (!all(status)) {
    logger("  - Pacote ignorado por falta de frames em uma ou mais cameras.")
    return(NULL)
  }

  data_row <- y1 |>
    dplyr::mutate(
      OUTPUT_IA = output_json,
      PAYLOAD = list(payload),
      LIMIT = as.integer(limit),
      POLIGNOS_COMPONENTES = as.character(toupper(POLIGNOS_COMPONENTES))
    )

  out <- y1
  out$POLIGNOS_COMPONENTES <- as.character(toupper(out$POLIGNOS_COMPONENTES))
  out$DATAS <- list(data_row)
  out
}
