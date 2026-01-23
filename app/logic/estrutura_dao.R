# app/infra/estrutura.R  (Postgres / RPostgres) - tudo em lowercase
box::use(
  DBI,
  dplyr[...],
  ./utils[...],
  purrr[...]
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

#' @export
checkifExistNameEstrutura <- function(con, name_estrutura) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*) from estrutura where name_estrutura = $1",
    params = list(name_estrutura)
  )
  .count1(df) > 0
}

#' @export
checkifExistNameEstruturaEdit <- function(con, cd_id_estrutura, name_estrutura) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*)
       from estrutura
      where name_estrutura = $1
        and cd_id_estrutura <> $2",
    params = list(name_estrutura, as.integer(cd_id_estrutura))
  )
  .count1(df) > 0
}

#' @export
checkifExistUrlEstrutura <- function(con, url_estrutura) {
  df <- DBI$dbGetQuery(
    con,
    "select count(*) from estrutura where url_estrutura = $1",
    params = list(url_estrutura)
  )
  .count1(df) > 0
}

#' @export
selectAllEstrutura <- function(con) {
  estruturas <- DBI$dbGetQuery(con, "select * from estrutura order by cd_id_estrutura")
  estruturas <- .df_names_lower(estruturas)

  estruturas$configs <- purrr::map(seq_len(nrow(estruturas)), function(i) {
    selectConfigByEstrutura(con, estruturas[i, , drop = FALSE])
  })

  estruturas
}

#' @export
selectAllEstruturaByIdComponente <- function(con, cd_id_estrutura) {
  estruturas <- DBI$dbGetQuery(
    con,
    "select * from estrutura where cd_id_estrutura = $1",
    params = list(as.integer(cd_id_estrutura))
  )
  estruturas <- .df_names_lower(estruturas)

  estruturas$configs <- purrr::map(seq_len(nrow(estruturas)), function(i) {
    selectConfigByEstrutura(con, estruturas[i, , drop = FALSE])
  })

  estruturas
}

# (interno)
selectConfigByEstrutura <- function(con, estrutura) {
  # garante que coluna existe em lower
  estrutura <- .df_names_lower(estrutura)
  stopifnot(!is.null(estrutura$cd_id_estrutura))

  configs <- DBI$dbGetQuery(
    con,
    "select *
       from estrutura_config
      where cd_id_estrutura = $1
      order by dt_hr_local desc
      limit 1",
    params = list(as.integer(estrutura$cd_id_estrutura))
  )
  configs <- .df_names_lower(configs)

  configs$atributos <- purrr::map(seq_len(nrow(configs)), function(i) {
    selectAllAtributosByEstruturas(con, configs[i, , drop = FALSE])
  })

  configs
}

# (interno)
selectAllAtributosByEstruturas <- function(con, estrutura_config) {
  estrutura_config <- .df_names_lower(estrutura_config)
  stopifnot(!is.null(estrutura_config$cd_id_estrutura_config))

  df <- DBI$dbGetQuery(
    con,
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
    where a.cd_id_estrutura_config = $1
    ",
    params = list(as.integer(estrutura_config$cd_id_estrutura_config))
  )

  .df_names_lower(df)
}

#' @export
selectAllTipoDados <- function(con) {
  df <- DBI$dbGetQuery(con, "select * from tipo_data order by cd_id_data")
  .df_names_lower(df)
}
