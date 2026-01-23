# app/infra/objeto_dao.R  (Postgres / RPostgres) - tudo em lowercase
box::use(
  DBI,
  dplyr[pull, mutate],
  purrr[...],
  ./camera_dao[selectCameraByComponente],
  ./estrutura_dao[selectAllEstruturaByIdComponente],
  jsonlite
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

# fg_ativo IN (...) com placeholders
.in_placeholders <- function(n, start = 1L) paste0("$", start:(start + n - 1L), collapse = ",")

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
    !is.null(objeto$cd_id_setor),
    !is.null(objeto$cd_id_objeto_tipo),
    !is.null(objeto$timeline_context_sec)
  )

  sql <- "
    insert into objeto
      (cd_id_objeto, name_objeto, fg_ativo, cd_id_setor, cd_id_objeto_tipo, timeline_context_sec)
    values
      ($1, $2, $3, $4, $5, $6)
  "

  DBI$dbExecute(
    con, sql,
    params = list(
      as.integer(cd_id_objeto),
      objeto$name_objeto,
      as.logical(objeto$fg_ativo),
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
  stopifnot(!is.null(obj$cd_id_objeto))

  sql <- "
    update objeto
       set name_objeto = $1,
           cd_id_setor = $2,
           fg_ativo    = $3
     where cd_id_objeto = $4
  "

  DBI$dbExecute(
    con, sql,
    params = list(
      obj$name_objeto,
      as.integer(obj$cd_id_setor),
      as.logical(obj$fg_ativo),
      as.integer(obj$cd_id_objeto)
    )
  )

  invisible(TRUE)
}

#' @export
selectAllObjetos <- function(con, fg_ativo = c(TRUE, FALSE)) {
  fg_ativo <- as.logical(fg_ativo)
  ph <- .in_placeholders(length(fg_ativo), start = 1L)

  sql <- sprintf("
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
  ", ph)

  objetos <- DBI$dbGetQuery(con, sql, params = as.list(as.integer(fg_ativo)))
  objetos <- .df_names_lower(objetos)

  objetos$config <- purrr::map(seq_len(nrow(objetos)), function(i) {
    selectObjetoConfig(con, objetos[i, , drop = FALSE])
  })

  objetos
}

# (interno)
selectObjetoConfig <- function(con, obj) {
  obj <- .df_names_lower(obj)
  stopifnot(!is.null(obj$cd_id_objeto))

  configs <- DBI$dbGetQuery(
    con,
    "select *
       from objeto_config
      where cd_id_objeto = $1
      order by dt_hr_local desc
      limit 1",
    params = list(as.integer(obj$cd_id_objeto))
  )
  configs <- .df_names_lower(configs)

  configs$componentes <- purrr::map(seq_len(nrow(configs)), function(i) {
    selectAllComponentesByObjeto(con, configs[i, , drop = FALSE])
  })

  configs
}

# (interno)
selectAllComponentesByObjeto <- function(con, config) {
  config <- .df_names_lower(config)
  stopifnot(!is.null(config$cd_id_obj_conf))

  componentes <- DBI$dbGetQuery(
    con,
    "select *
       from componente
      where cd_id_obj_conf = $1
      order by cd_id_componente",
    params = list(as.integer(config$cd_id_obj_conf))
  )
  componentes <- .df_names_lower(componentes)

  # poligno_componente: vem texto/json -> vira lista
  if (nrow(componentes)) {
    componentes <- dplyr::mutate(
      componentes,
      poligno_componente = purrr::map(poligno_componente, ~ jsonlite::fromJSON(.x))
    )
  } else {
    componentes$poligno_componente <- list()
  }

  # estrutura e cameras
  componentes$estrutura <- purrr::map(seq_len(nrow(componentes)), function(i) {
    selectAllEstruturaByIdComponente(con, componentes$cd_id_estrutura[i])
  })

  componentes$cameras <- purrr::map(seq_len(nrow(componentes)), function(i) {
    selectCameraByComponente(con, componentes[i, , drop = FALSE])
  })

  componentes
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
