box::use(
  dplyr[...],
  DBI,
  htmlwidgets
)

# ===============================
# Query consulta contexto
# ===============================
#' @export
build_sql <- function(dt_de_utc, dt_ate_utc, setor = NULL, objeto = NULL) {

  base <- paste(
    "SELECT 
        oc.DATA_OC     AS ATRIBUTOS,
        oc.DT_HR_LOCAL AS DATE_TIME,
        o.NAME_OBJETO  AS OBJETO,
        s.NAME_SETOR   AS SETOR
     FROM objeto_contexto oc
     LEFT JOIN objeto o       ON o.CD_ID_OBJETO = oc.CD_ID_OBJETO
     LEFT JOIN setor s        ON s.CD_ID_SETOR  = o.CD_ID_SETOR
     WHERE o.FG_ATIVO = 1 AND VISIBLE >= 90"
  )

  where <- c()

  # intervalo de tempo (já em UTC formatado)
  where <- c(where, sprintf(
    "oc.DT_HR_LOCAL BETWEEN '%s' AND '%s'",
    fmt_sql_dt_utc(dt_de_utc), fmt_sql_dt_utc(dt_ate_utc)
  ))

  # remove registros com "erro" (AREA_DE_TRABALHO / _A / _B contendo ESTADO)
  # e NÃO elimina linhas com DATA_OC NULL/vazio
  where <- c(where, "
  (
    oc.DATA_OC IS NULL OR oc.DATA_OC = ''
    OR (
      JSON_VALID(oc.DATA_OC) = 1
      AND JSON_CONTAINS_PATH(oc.DATA_OC, 'one', '$.AREA_DE_TRABALHO.ESTADO') = 0
      AND JSON_CONTAINS_PATH(oc.DATA_OC, 'one', '$.AREA_DE_TRABALHO_A.ESTADO') = 0
      AND JSON_CONTAINS_PATH(oc.DATA_OC, 'one', '$.AREA_DE_TRABALHO_B.ESTADO') = 0
    )
  )
  ")

  if (!is.null(setor) && length(setor)) {
    in_setor <- .sql_in(setor)
    if (!is.null(in_setor)) where <- c(where, paste0("s.NAME_SETOR IN ", in_setor))
  }

  if (!is.null(objeto) && length(objeto)) {
    in_obj <- .sql_in(objeto)
    if (!is.null(in_obj)) where <- c(where, paste0("o.NAME_OBJETO IN ", in_obj))
  }

  paste0(base, "\n AND ", paste(where, collapse = " AND "), "\n")
}

#' @export
run_query <- function(pool, dt_de_utc, dt_ate_utc, setor = NULL, objeto = NULL) {
  sql <- build_sql(dt_de_utc, dt_ate_utc, setor = setor, objeto = objeto)
  DBI::dbGetQuery(pool, sql)
}

#' @export
fetch_choices <- function(pool) {
  sql <- "
    SELECT DISTINCT s.NAME_SETOR AS SETOR, o.NAME_OBJETO AS OBJETO
    FROM objeto o
    LEFT JOIN setor s ON s.CD_ID_SETOR = o.CD_ID_SETOR
    WHERE o.NAME_OBJETO IS NOT NULL AND o.NAME_OBJETO <> ''
  "
  x <- DBI::dbGetQuery(pool, sql)
  list(
    setores  = sort(unique(x$SETOR[!is.na(x$SETOR) & nzchar(x$SETOR)])),
    maquinas = sort(unique(x$OBJETO[!is.na(x$OBJETO) & nzchar(x$OBJETO)]))
  )
}
#' @export
tz_local <- function() {
  # se quiser fixo: "America/Sao_Paulo"
  Sys.timezone()
}
#' @export
to_utc <- function(x, tz = tz_local()) {
  # x: POSIXct "no tz certo" (local). Converte para UTC mantendo o instante.
  if (is.null(x) || all(is.na(x))) return(x)
  as.POSIXct(format(as.POSIXct(x, tz = tz), tz = "UTC", usetz = TRUE), tz = "UTC")
}
#' @export
from_utc <- function(x, tz = tz_local()) {
  # x: POSIXct em UTC. Converte para tz local mantendo o instante.
  if (is.null(x) || all(is.na(x))) return(x)
  as.POSIXct(format(as.POSIXct(x, tz = "UTC"), tz = tz, usetz = TRUE), tz = tz)
}

#' @export
fmt_sql_dt_utc <- function(x_utc) {
  # x_utc deve estar em tz="UTC"
  format(as.POSIXct(x_utc, tz = "UTC"), "%Y-%m-%d %H:%M:%S")
}
