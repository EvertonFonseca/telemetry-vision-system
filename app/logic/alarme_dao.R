# app/logic/alarme_dao.R
box::use(
  DBI,
  dplyr[...],
  jsonlite
)

# ----------------------------
# Campos disponíveis para build:
# retorna: field ("DOBRA.ESTADO"), dtype ("text"/"numeric"), etc.
# ----------------------------
#' @export
listAlarmFields <- function(conn, only_active = FALSE) {

  sql <- paste0(
    "SELECT ",
    "  e.CD_ID_ESTRUTURA, e.NAME_ESTRUTURA, ",
    "  ec.CD_ID_ESTRUTURA_CONFIG, ",
    "  a.CD_ID_ATRIBUTO, a.NAME_ATRIBUTO, a.FG_ATIVO, ",
    "  td.NAME_DATA, td.R_DATA ",
    "FROM ESTRUTURA_CONFIG ec ",
    "JOIN ESTRUTURA e ON e.CD_ID_ESTRUTURA = ec.CD_ID_ESTRUTURA ",
    "JOIN ATRIBUTO a ON a.CD_ID_ESTRUTURA_CONFIG = ec.CD_ID_ESTRUTURA_CONFIG ",
    "JOIN TIPO_DATA td ON td.CD_ID_DATA = a.CD_ID_DATA ",
    if (isTRUE(only_active)) "WHERE a.FG_ATIVO = 1 " else "",
    "ORDER BY e.NAME_ESTRUTURA, a.NAME_ATRIBUTO"
  )

  df <- DBI::dbGetQuery(conn, sql)

  if (nrow(df) == 0) return(df)

  df$field <- paste0(toupper(df$NAME_ESTRUTURA), ".", toupper(df$NAME_ATRIBUTO))
  # dtype simples pro builder
  df$dtype <- ifelse(tolower(df$R_DATA) %in% c("numeric","number","double","float","integer"),
                     "numeric", "text")
  df
}

# ----------------------------
# Alarmes
# ----------------------------
#' @export
selectAllAlarmes <- function(conn, cd_id_setor = NULL, cd_id_objeto = NULL) {
  sql <- "SELECT * FROM ALARME WHERE 1=1"
  args <- list()

  if (!is.null(cd_id_setor)) {
    sql <- paste0(sql, " AND CD_ID_SETOR = ?")
    args <- c(args, cd_id_setor)
  }
  if (!is.null(cd_id_objeto)) {
    sql <- paste0(sql, " AND CD_ID_OBJETO = ?")
    args <- c(args, cd_id_objeto)
  }

  sql <- paste0(sql, " ORDER BY DT_HR_LOCAL DESC")

  if (length(args) == 0) DBI::dbGetQuery(conn, sql)
  else DBI::dbGetQuery(conn, sql, params = args)
}
#' @export
insertNewAlarme <- function(conn, obj) {
  stopifnot(!is.null(obj$NAME_ALARME), !is.null(obj$JSON_EXPR))

  sql <- paste0(
    "INSERT INTO ALARME (NAME_ALARME, DS_ALARME, CD_ID_SETOR, CD_ID_OBJETO, SEVERITY, FG_ATIVO, JSON_EXPR) ",
    "VALUES (?,?,?,?,?,?,CAST(? AS JSON))"
  )

  DBI::dbExecute(conn, sql, params = list(
    obj$NAME_ALARME,
    obj$DS_ALARME %||% NULL,
    obj$CD_ID_SETOR %||% NULL,
    obj$CD_ID_OBJETO %||% NULL,
    obj$SEVERITY %||% "MEDIUM",
    as.integer(isTRUE(obj$FG_ATIVO %||% TRUE)),
    obj$JSON_EXPR
  ))
}
#' @export
updateAlarme <- function(conn, cd_id_alarme, obj) {
  sql <- paste0(
    "UPDATE ALARME SET ",
    " NAME_ALARME=?, DS_ALARME=?, CD_ID_SETOR=?, CD_ID_OBJETO=?, ",
    " SEVERITY=?, FG_ATIVO=?, JSON_EXPR=CAST(? AS JSON) ",
    "WHERE CD_ID_ALARME=?"
  )

  DBI::dbExecute(conn, sql, params = list(
    obj$NAME_ALARME,
    obj$DS_ALARME %||% NULL,
    obj$CD_ID_SETOR %||% NULL,
    obj$CD_ID_OBJETO %||% NULL,
    obj$SEVERITY %||% "MEDIUM",
    as.integer(isTRUE(obj$FG_ATIVO %||% TRUE)),
    obj$JSON_EXPR,
    cd_id_alarme
  ))
}
#' @export
deleteAlarme <- function(conn, cd_id_alarme) {
  DBI::dbExecute(conn, "DELETE FROM ALARME WHERE CD_ID_ALARME=?", params = list(cd_id_alarme))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
