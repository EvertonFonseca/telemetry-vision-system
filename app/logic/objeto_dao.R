# app/infra/objeto_dao.R  (Postgres / RPostgres) - tudo em lowercase
box::use(
  DBI,
  dplyr[pull, mutate],
  purrr[...],
  jsonlite,
  lubridate[with_tz]
)

# ---- helpers ----
.count1 <- function(df) as.integer(df[[1]][[1]])

.names_to_lower <- function(x) {
  if (!is.list(x) || is.null(names(x))) return(x)
  names(x) <- tolower(names(x))
  x
}

.df_names_lower <- function(df) {
  if (is.data.frame(df) && ncol(df)) names(df) <- tolower(names(df))
  df
}

.has_value <- function(x) {
  !is.null(x) && length(x) && !all(is.na(x))
}

.safe_tz_local <- function(tz = NULL) {
  tz <- as.character(tz)[1]
  if (is.na(tz) || !nzchar(tz)) {
    tz <- tryCatch(Sys.timezone(), error = function(e) "")
  }
  if (!length(tz) || is.na(tz) || !nzchar(tz)) {
    tz <- "America/Sao_Paulo"
  }
  tz
}

# fg_ativo IN (...) com placeholders
.in_placeholders <- function(n, start = 1L) {
  if (n <= 0L) return("null")
  paste0("$", start:(start + n - 1L), collapse = ",")
}

.ids_unique <- function(x) {
  x <- suppressWarnings(as.integer(x))
  x <- x[is.finite(x)]
  unique(x)
}

.safe_from_json <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  x <- as.character(x[[1]])
  if (is.na(x) || !nzchar(x)) return(NULL)
  tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
}

.load_cameras_lookup <- function(con, camera_ids) {
  camera_ids <- .ids_unique(camera_ids)
  if (!length(camera_ids)) {
    return(list(by_id = list(), empty = data.frame()))
  }

  ph <- .in_placeholders(length(camera_ids))
  cameras <- DBI::dbGetQuery(
    con,
    sprintf(
      "select * from camera_view where cd_id_camera in (%s) order by cd_id_camera",
      ph
    ),
    params = as.list(camera_ids)
  )
  cameras <- .df_names_lower(cameras)

  list(
    by_id = if (nrow(cameras)) split(cameras, as.character(cameras$cd_id_camera)) else list(),
    empty = cameras[0, , drop = FALSE]
  )
}

.load_estruturas_lookup <- function(con, estrutura_ids) {
  estrutura_ids <- .ids_unique(estrutura_ids)
  if (!length(estrutura_ids)) {
    empty <- data.frame()
    empty$configs <- vector("list", 0L)
    return(list(by_id = list(), empty = empty))
  }

  ph <- .in_placeholders(length(estrutura_ids))
  estruturas <- DBI::dbGetQuery(
    con,
    sprintf(
      "select *
         from estrutura
        where cd_id_estrutura in (%s)
        order by cd_id_estrutura",
      ph
    ),
    params = as.list(estrutura_ids)
  )
  estruturas <- .df_names_lower(estruturas)

  empty_estrutura <- estruturas[0, , drop = FALSE]
  empty_estrutura$configs <- vector("list", nrow(empty_estrutura))

  if (!nrow(estruturas)) {
    return(list(by_id = list(), empty = empty_estrutura))
  }

  estrutura_configs <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      select y.*
        from (
          select
            ec.*,
            row_number() over (
              partition by ec.cd_id_estrutura
              order by ec.dt_hr_local desc, ec.cd_id_estrutura_config desc
            ) as rn
          from estrutura_config ec
          where ec.cd_id_estrutura in (%s)
        ) y
       where y.rn = 1
       order by y.cd_id_estrutura, y.dt_hr_local desc, y.cd_id_estrutura_config desc
      ",
      ph
    ),
    params = as.list(estrutura_ids)
  )
  estrutura_configs <- .df_names_lower(estrutura_configs)
  if ("rn" %in% names(estrutura_configs)) estrutura_configs$rn <- NULL

  if (nrow(estrutura_configs)) {
    cfg_ids <- .ids_unique(estrutura_configs$cd_id_estrutura_config)
    ph_cfg <- .in_placeholders(length(cfg_ids))

    atributos <- DBI::dbGetQuery(
      con,
      sprintf(
        "
        select
          a.cd_id_atributo,
          a.cd_id_estrutura_config,
          a.name_atributo,
          a.value_atributo,
          a.fg_ativo,
          a.cd_id_data,
          td.name_data,
          td.r_data
        from atributo a
        left join tipo_data td
          on td.cd_id_data = a.cd_id_data
        where a.cd_id_estrutura_config in (%s)
        order by a.cd_id_estrutura_config, a.cd_id_atributo
        ",
        ph_cfg
      ),
      params = as.list(cfg_ids)
    )
    atributos <- .df_names_lower(atributos)

    attrs_by_cfg <- if (nrow(atributos)) {
      split(atributos, as.character(atributos$cd_id_estrutura_config))
    } else {
      list()
    }
    empty_attrs <- atributos[0, , drop = FALSE]

    estrutura_configs$atributos <- purrr::map(
      estrutura_configs$cd_id_estrutura_config,
      function(id) {
        out <- attrs_by_cfg[[as.character(id)]]
        if (is.null(out)) empty_attrs else out
      }
    )
  } else {
    estrutura_configs$atributos <- vector("list", 0L)
  }

  cfg_by_estrutura <- if (nrow(estrutura_configs)) {
    split(estrutura_configs, as.character(estrutura_configs$cd_id_estrutura))
  } else {
    list()
  }
  empty_cfg <- estrutura_configs[0, , drop = FALSE]

  estrutura_by_id <- list()
  for (i in seq_len(nrow(estruturas))) {
    est <- estruturas[i, , drop = FALSE]
    key <- as.character(est$cd_id_estrutura[[1]])

    cfg <- cfg_by_estrutura[[key]]
    if (is.null(cfg)) cfg <- empty_cfg

    est$configs <- list(cfg)
    estrutura_by_id[[key]] <- est
  }

  list(by_id = estrutura_by_id, empty = empty_estrutura)
}

.load_componentes_by_config_ids <- function(con, config_ids) {
  config_ids <- .ids_unique(config_ids)
  if (!length(config_ids)) return(data.frame())

  ph <- .in_placeholders(length(config_ids))
  componentes <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      select *
        from componente
       where cd_id_obj_conf in (%s)
       order by cd_id_obj_conf, cd_id_componente
      ",
      ph
    ),
    params = as.list(config_ids)
  )
  componentes <- .df_names_lower(componentes)

  if (!nrow(componentes)) {
    componentes$poligno_componente <- list()
    componentes$estrutura <- list()
    componentes$cameras <- list()
    componentes$camera <- list()
    return(componentes)
  }

  componentes <- dplyr::mutate(
    componentes,
    poligno_componente = purrr::map(poligno_componente, .safe_from_json)
  )

  cam_lookup <- if ("cd_id_camera" %in% names(componentes)) {
    .load_cameras_lookup(con, componentes$cd_id_camera)
  } else {
    list(by_id = list(), empty = data.frame())
  }

  est_lookup <- if ("cd_id_estrutura" %in% names(componentes)) {
    .load_estruturas_lookup(con, componentes$cd_id_estrutura)
  } else {
    empty <- data.frame()
    empty$configs <- vector("list", 0L)
    list(by_id = list(), empty = empty)
  }

  componentes$estrutura <- purrr::map(
    if ("cd_id_estrutura" %in% names(componentes)) componentes$cd_id_estrutura else rep(NA_integer_, nrow(componentes)),
    function(id) {
      out <- est_lookup$by_id[[as.character(id)]]
      if (is.null(out)) est_lookup$empty else out
    }
  )

  componentes$cameras <- purrr::map(
    if ("cd_id_camera" %in% names(componentes)) componentes$cd_id_camera else rep(NA_integer_, nrow(componentes)),
    function(id) {
      out <- cam_lookup$by_id[[as.character(id)]]
      if (is.null(out)) cam_lookup$empty else out
    }
  )

  # compatibilidade: alguns pontos usam $camera, outros $cameras
  componentes$camera <- componentes$cameras
  componentes
}

.load_latest_objeto_configs <- function(con, objeto_ids) {
  objeto_ids <- .ids_unique(objeto_ids)
  if (!length(objeto_ids)) return(data.frame())

  ph <- .in_placeholders(length(objeto_ids))
  configs <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      select y.*
        from (
          select
            oc.*,
            row_number() over (
              partition by oc.cd_id_objeto
              order by oc.dt_hr_local desc nulls last, oc.cd_id_obj_conf desc
            ) as rn
          from objeto_config oc
          where oc.cd_id_objeto in (%s)
        ) y
       where y.rn = 1
       order by y.cd_id_objeto, y.dt_hr_local desc nulls last, y.cd_id_obj_conf desc
      ",
      ph
    ),
    params = as.list(objeto_ids)
  )
  configs <- .df_names_lower(configs)
  if ("rn" %in% names(configs)) configs$rn <- NULL

  if (!nrow(configs)) {
    configs$componentes <- vector("list", 0L)
    return(configs)
  }

  componentes <- .load_componentes_by_config_ids(con, configs$cd_id_obj_conf)
  comp_by_cfg <- if (nrow(componentes)) {
    split(componentes, as.character(componentes$cd_id_obj_conf))
  } else {
    list()
  }
  empty_comp <- componentes[0, , drop = FALSE]

  configs$componentes <- purrr::map(
    configs$cd_id_obj_conf,
    function(id) {
      out <- comp_by_cfg[[as.character(id)]]
      if (is.null(out)) empty_comp else out
    }
  )

  configs
}

#' @export
checkifExistNameObjeto <- function(con, name_objeto) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*) from objeto where name_objeto = $1",
    params = list(name_objeto)
  )
  .count1(df) > 0
}

#' @export
checkifExistNameComponente <- function(con, name_componente) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*) from componente where name_componente = $1",
    params = list(name_componente)
  )
  .count1(df) > 0
}

#' @export
checkifExistNameObjetoEdit <- function(con, cd_id_objeto, name_objeto) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*)
       from objeto
      where name_objeto = $1
        and cd_id_objeto <> $2",
    params = list(name_objeto, as.integer(cd_id_objeto))
  )
  .count1(df) > 0
}

#' @export
insertNewObjeto <- function(con, cd_id_objeto, objeto) {
  objeto <- .names_to_lower(objeto)

  stopifnot(
    !is.null(cd_id_objeto),
    !is.null(objeto$name_objeto),
    !is.null(objeto$fg_ativo),
    !is.null(objeto$is_dev),
    !is.null(objeto$grupo),
    !is.null(objeto$id_grupo),
    !is.null(objeto$cd_id_setor),
    !is.null(objeto$cd_id_objeto_tipo),
    !is.null(objeto$timeline_context_sec)
  )

  sql <- "
    insert into objeto
      (cd_id_objeto, name_objeto, fg_ativo, is_dev, grupo, id_grupo, cd_id_setor, cd_id_objeto_tipo, timeline_context_sec)
    values
      ($1, $2, $3, $4, $5, $6, $7, $8, $9)
  "

  DBI$dbExecute(
    con, sql,
    params = list(
      as.integer(cd_id_objeto),
      objeto$name_objeto,
      as.logical(objeto$fg_ativo),
      as.logical(objeto$is_dev),
      as.logical(objeto$grupo),
      as.integer(objeto$id_grupo),
      as.integer(objeto$cd_id_setor),
      as.integer(objeto$cd_id_objeto_tipo),
      as.integer(objeto$timeline_context_sec)
    )
  )

  as.integer(cd_id_objeto)
}

#' @export
insertNewObjetoConfig <- function(con, cd_id_obj_conf, objeto) {
  objeto <- .names_to_lower(objeto)
  stopifnot(!is.null(cd_id_obj_conf), !is.null(objeto$cd_id_objeto))

  sql <- "insert into objeto_config (cd_id_obj_conf, cd_id_objeto) values ($1, $2)"

  DBI$dbExecute(
    con, sql,
    params = list(as.integer(cd_id_obj_conf), as.integer(objeto$cd_id_objeto))
  )

  as.integer(cd_id_obj_conf)
}

#' @export
insertNewComponente <- function(con, cd_id_componente, objeto) {
  objeto <- .names_to_lower(objeto)

  stopifnot(
    !is.null(cd_id_componente),
    !is.null(objeto$name_componente),
    !is.null(objeto$poligno_componente),
    !is.null(objeto$cd_id_obj_conf),
    !is.null(objeto$cd_id_camera)
  )

  sql <- "
    insert into componente
      (cd_id_componente, name_componente, poligno_componente, cd_id_obj_conf, cd_id_camera)
    values
      ($1, $2, $3, $4, $5)
  "

  DBI$dbExecute(
    con, sql,
    params = list(
      as.integer(cd_id_componente),
      objeto$name_componente,
      objeto$poligno_componente,          # JSON/text como você já grava
      as.integer(objeto$cd_id_obj_conf),
      as.integer(objeto$cd_id_camera)
    )
  )

  invisible(TRUE)
}

#' @export
insertNewAtributo <- function(con, cd_id_atributo, objeto) {
  objeto <- .names_to_lower(objeto)

  stopifnot(
    !is.null(cd_id_atributo),
    !is.null(objeto$name_atributo),
    !is.null(objeto$value_atributo),
    !is.null(objeto$fg_ativo),
    !is.null(objeto$cd_id_componente),
    !is.null(objeto$cd_id_data)
  )

  sql <- "
    insert into atributo
      (cd_id_atributo, name_atributo, value_atributo, fg_ativo, cd_id_componente, cd_id_data)
    values
      ($1, $2, $3, $4, $5, $6)
  "

  DBI$dbExecute(
    con, sql,
    params = list(
      as.integer(cd_id_atributo),
      objeto$name_atributo,
      objeto$value_atributo,
      as.logical(objeto$fg_ativo),
      as.integer(objeto$cd_id_componente),
      as.integer(objeto$cd_id_data)
    )
  )

  invisible(TRUE)
}

#' @export
updateObjeto <- function(con, obj) {
  obj <- .names_to_lower(obj)
  stopifnot(!is.null(obj$cd_id_objeto), !is.null(obj$id_grupo))

  sql <- "
    update objeto
       set name_objeto = $1,
           cd_id_setor = $2,
           fg_ativo    = $3,
           is_dev      = $4,
           grupo       = $5,
           id_grupo    = $6
     where cd_id_objeto = $7
  "

  DBI$dbExecute(
    con, sql,
    params = list(
      obj$name_objeto,
      as.integer(obj$cd_id_setor),
      as.logical(obj$fg_ativo),
      as.logical(obj$is_dev),
      as.logical(obj$grupo),
      as.integer(obj$id_grupo),
      as.integer(obj$cd_id_objeto)
    )
  )

  invisible(TRUE)
}

#' @export
selectObjetosLookup <- function(con, cd_id_setor = NULL) {
  sql <- "
    select
      o.cd_id_objeto,
      o.name_objeto,
      o.cd_id_setor,
      o.fg_ativo,
      s.name_setor
    from objeto o
    left join setor s
      on s.cd_id_setor = o.cd_id_setor
    where 1 = 1
  "

  params <- list()
  if (.has_value(cd_id_setor)) {
    sql <- paste0(sql, " and o.cd_id_setor = $1")
    params <- list(as.integer(cd_id_setor[[1]]))
  }

  sql <- paste0(
    sql,
    "
    order by
      s.name_setor nulls last,
      o.name_objeto nulls last,
      o.cd_id_objeto
    "
  )

  out <- DBI::dbGetQuery(con, sql, params = params)
  .df_names_lower(out)
}

#' @export
selectObjetoContexto <- function(con,
                                 cd_id_objeto,
                                 dt_de_utc = NULL,
                                 dt_ate_utc = NULL,
                                 tz_local = NULL) {
  stopifnot(!is.null(cd_id_objeto))

  sql <- "
    select
      oc.cd_id_oc,
      oc.data_oc    as contexto,
      oc.dt_hr_local as momento
    from objeto_contexto oc
    where oc.cd_id_objeto = $1
  "

  params <- list(as.integer(cd_id_objeto[[1]]))
  p <- 2L

  if (.has_value(dt_de_utc)) {
    sql <- paste0(sql, " and oc.dt_hr_local >= $", p)
    params <- c(params, list(as.POSIXct(dt_de_utc[[1]], tz = "UTC")))
    p <- p + 1L
  }

  if (.has_value(dt_ate_utc)) {
    sql <- paste0(sql, " and oc.dt_hr_local <= $", p)
    params <- c(params, list(as.POSIXct(dt_ate_utc[[1]], tz = "UTC")))
  }

  sql <- paste0(
    sql,
    "
    order by
      oc.dt_hr_local desc nulls last,
      oc.cd_id_oc desc
    "
  )

  out <- DBI::dbGetQuery(con, sql, params = params)
  out <- .df_names_lower(out)

  if ("contexto" %in% names(out)) {
    out$contexto <- as.character(out$contexto)
  }

  if ("momento" %in% names(out)) {
    if (!inherits(out$momento, "POSIXct")) {
      out$momento <- as.POSIXct(out$momento, tz = "UTC")
    } else {
      attr(out$momento, "tzone") <- "UTC"
    }
    out$momento <- lubridate::with_tz(out$momento, tzone = .safe_tz_local(tz_local))
  }

  out
}

#' @export
deleteObjetoContextoByPeriodo <- function(con,
                                          cd_id_objeto,
                                          dt_de_utc = NULL,
                                          dt_ate_utc = NULL) {
  stopifnot(!is.null(cd_id_objeto))

  sql <- "
    delete from objeto_contexto
    where cd_id_objeto = $1
  "
  params <- list(as.integer(cd_id_objeto[[1]]))
  p <- 2L

  if (.has_value(dt_de_utc)) {
    sql <- paste0(sql, " and dt_hr_local >= $", p)
    params <- c(params, list(as.POSIXct(dt_de_utc[[1]], tz = "UTC")))
    p <- p + 1L
  }

  if (.has_value(dt_ate_utc)) {
    sql <- paste0(sql, " and dt_hr_local <= $", p)
    params <- c(params, list(as.POSIXct(dt_ate_utc[[1]], tz = "UTC")))
  }

  if (length(params) <= 1L) {
    stop("Informe ao menos um filtro de data para excluir registros de contexto.")
  }

  DBI::dbExecute(con, sql, params = params)
  invisible(TRUE)
}

#' @export
selectAllObjetos <- function(con, fg_ativo = c(TRUE, FALSE), ...) {
  dots <- list(...)
  if (!is.null(dots$fg.ativo)) fg_ativo <- dots$fg.ativo

  fg_ativo <- unique(as.logical(fg_ativo))
  fg_ativo <- fg_ativo[!is.na(fg_ativo)]

  if (!length(fg_ativo)) {
    objetos <- DBI::dbGetQuery(
      con,
      "
      select
        o.*,
        s.name_setor,
        op.name_objeto_tipo
      from objeto o
      left join setor s
        on s.cd_id_setor = o.cd_id_setor
      left join objeto_tipo op
        on op.cd_id_objeto_tipo = o.cd_id_objeto_tipo
      where 1 = 0
      order by o.cd_id_objeto
      "
    )
    objetos <- .df_names_lower(objetos)
    objetos$config <- vector("list", 0L)
    return(objetos)
  }

  ph <- .in_placeholders(length(fg_ativo), start = 1L)
  sql <- sprintf(
    "
    select
      o.*,
      s.name_setor,
      op.name_objeto_tipo
    from objeto o
    left join setor s
      on s.cd_id_setor = o.cd_id_setor
    left join objeto_tipo op
      on op.cd_id_objeto_tipo = o.cd_id_objeto_tipo
    where o.fg_ativo in (%s)
    order by o.cd_id_objeto
    ",
    ph
  )

  objetos <- DBI::dbGetQuery(con, sql, params = as.list(as.integer(fg_ativo)))
  objetos <- .df_names_lower(objetos)

  if (!nrow(objetos)) {
    objetos$config <- vector("list", 0L)
    return(objetos)
  }

  configs <- .load_latest_objeto_configs(con, objetos$cd_id_objeto)
  cfg_by_obj <- if (nrow(configs)) {
    split(configs, as.character(configs$cd_id_objeto))
  } else {
    list()
  }
  empty_cfg <- configs[0, , drop = FALSE]

  objetos$config <- purrr::map(
    objetos$cd_id_objeto,
    function(id) {
      out <- cfg_by_obj[[as.character(id)]]
      if (is.null(out)) empty_cfg else out
    }
  )

  objetos
}

# (interno)
selectObjetoConfig <- function(con, obj) {
  obj <- .df_names_lower(obj)
  stopifnot(!is.null(obj$cd_id_objeto))
  configs <- .load_latest_objeto_configs(con, obj$cd_id_objeto)
  if (!nrow(configs)) return(configs)

  out <- split(configs, as.character(configs$cd_id_objeto))[[as.character(as.integer(obj$cd_id_objeto[[1]]))]]
  if (is.null(out)) configs[0, , drop = FALSE] else out
}

# (interno)
selectAllComponentesByObjeto <- function(con, config) {
  config <- .df_names_lower(config)
  stopifnot(!is.null(config$cd_id_obj_conf))
  componentes <- .load_componentes_by_config_ids(con, config$cd_id_obj_conf)
  if (!nrow(componentes)) return(componentes)

  out <- split(componentes, as.character(componentes$cd_id_obj_conf))[[as.character(as.integer(config$cd_id_obj_conf[[1]]))]]
  if (is.null(out)) componentes[0, , drop = FALSE] else out
}

#' @export
deleteObjeto <- function(con, cd_id_objeto) {
  DBI::dbExecute(
    con,
    "delete from objeto where cd_id_objeto = $1",
    params = list(as.integer(cd_id_objeto))
  )
  invisible(TRUE)
}

#' @export
selectTipoObjeto <- function(con) {
  df <- DBI$dbGetQuery(con, "select * from objeto_tipo order by cd_id_objeto_tipo")
  .df_names_lower(df)
}
