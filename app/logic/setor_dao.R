# app/infra/setor.R  (Postgres / RPostgres) - tudo em lowercase
box::use(
  DBI,
  dplyr[...],
  ./utils[...]
)

# ---- helpers ----
.affected_ok <- function(n) isTRUE(!is.na(n) && n > 0)
.count1 <- function(df) as.integer(df[[1]][[1]])

# normaliza nomes de listas para lower (se vier NAME_SETOR etc.)
.names_to_lower <- function(x) {
  if (!is.list(x) || is.null(names(x))) return(x)
  names(x) <- tolower(names(x))
  x
}

#' @export
checkifExistNameSetor <- function(con, name_setor) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*) from setor where name_setor = $1",
    params = list(name_setor)
  )
  .count1(df) > 0
}

#' @export
checkifExistNameSetorEdit <- function(con, cd_id_setor, name_setor) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*)
       from setor
      where name_setor = $1
        and cd_id_setor <> $2",
    params = list(name_setor, as.integer(cd_id_setor))
  )
  .count1(df) > 0
}

#' @export
checkifExistUrlSetor <- function(con, url_setor) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*) from setor where url_setor = $1",
    params = list(url_setor)
  )
  .count1(df) > 0
}

#' @export
checkifExistUrlSetorEdit <- function(con, cd_id_setor, url_setor) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*)
       from setor
      where url_setor = $1
        and cd_id_setor <> $2",
    params = list(url_setor, as.integer(cd_id_setor))
  )
  .count1(df) > 0
}

#' @export
insertNewSetor <- function(con, cd_id_setor, setor) {
  setor <- .names_to_lower(setor)

  stopifnot(
    !is.null(cd_id_setor),
    !is.null(setor$name_setor),
    !is.null(setor$tempo_reativar_setor),
    !is.null(setor$tempo_reativar_unidade_setor),
    !is.null(setor$tempo_passado_setor),
    !is.null(setor$tempo_passado_unidade_setor)
  )

  sql <- "
    insert into setor (
      cd_id_setor,
      name_setor,
      tempo_reativar_setor,
      tempo_reativar_unidade_setor,
      tempo_passado_setor,
      tempo_passado_unidade_setor
    ) values ($1, $2, $3, $4, $5, $6)
  "

  n <- DBI$dbExecute(
    con, sql,
    params = list(
      as.integer(cd_id_setor),
      setor$name_setor,
      setor$tempo_reativar_setor,
      setor$tempo_reativar_unidade_setor,
      setor$tempo_passado_setor,
      setor$tempo_passado_unidade_setor
    )
  )

  if (!.affected_ok(n)) stop("insert nao afetou linhas.")
  as.integer(cd_id_setor)
}

#' @export
updateSetor <- function(con, setor) {
  setor <- .names_to_lower(setor)
  stopifnot(!is.null(setor$cd_id_setor))

  sql <- "
    update setor
       set name_setor                   = $1,
           tempo_reativar_setor         = $2,
           tempo_reativar_unidade_setor = $3,
           tempo_passado_setor          = $4,
           tempo_passado_unidade_setor  = $5
     where cd_id_setor = $6
  "

  n <- DBI$dbExecute(
    con, sql,
    params = list(
      setor$name_setor,
      setor$tempo_reativar_setor,
      setor$tempo_reativar_unidade_setor,
      setor$tempo_passado_setor,
      setor$tempo_passado_unidade_setor,
      as.integer(setor$cd_id_setor)
    )
  )

  if (!.affected_ok(n)) warning("update nao afetou linhas.")
  invisible(TRUE)
}

#' @export
selectAllSetors <- function(con) {
  DBI$dbGetQuery(con, "select * from setor order by cd_id_setor")
}

#' @export
deleteSetor <- function(con, cd_id_setor) {
  n <- DBI$dbExecute(
    con,
    "delete from setor where cd_id_setor = $1",
    params = list(as.integer(cd_id_setor))
  )
  if (!.affected_ok(n)) warning("delete nao encontrou o setor informado.")
  invisible(TRUE)
}
