# app/infra/database.R
box::use(
  DBI, stringi,
  shiny[showNotification],
  ./db_pool[get_pool, with_conn]
)

# --------- UTIL ---------
nextSequenciaID <- function(conn = get_pool(),table, schema = 'system') {
  DBI$dbGetQuery(
    conn,
    "SELECT AUTO_INCREMENT AS ID
       FROM INFORMATION_SCHEMA.TABLES
      WHERE TABLE_NAME = ? AND TABLE_SCHEMA = ?",
    params = c(table, schema)
  )$ID |> as.integer()
}

# --------- INSERT (parametrizado) ---------
insertTable <- function(conn = get_pool(),table, obj, verbose = FALSE) {
  stopifnot(is.list(obj), length(obj) > 0)
  cols <- names(obj)
  qs   <- paste(rep("?", length(cols)), collapse = ",")
  sql  <- sprintf("INSERT INTO %s (%s) VALUES (%s)", table, paste(cols, collapse=","), qs)
  if (verbose) message(sql)

  # Use as.list() para não perder tipos (BLOB deve ser raw)
  DBI$dbExecute(conn, sql, params = unname(obj))
}

# --------- UPDATE (parametrizado) ---------
# where_cols: vetor de colunas do WHERE
# where_vals: lista/vetor com os valores correspondentes
updateTable <- function(conn = get_pool(),table, obj, where_cols = character(), where_vals = list(), verbose = FALSE) {
  stopifnot(is.list(obj), length(obj) > 0)
  set_part <- paste(sprintf("%s = ?", names(obj)), collapse = ", ")

  where_sql <- ""
  params <- c(unname(as.list(obj)))
  if (length(where_cols)) {
    where_sql <- paste(" WHERE ", paste(sprintf("%s = ?", where_cols), collapse = " AND "))
    params <- c(params, unname(as.list(where_vals)))
  }

  sql <- sprintf("UPDATE %s SET %s%s", table, set_part, where_sql)
  if (verbose) message(sql)
  DBI$dbExecute(conn, sql, params = params)
}

# --------- DELETE (parametrizado) ---------
deleteTable <- function(conn = get_pool(),table, where_cols = character(), where_vals = list(), verbose = FALSE) {
  where_sql <- ""
  params <- list()
  if (length(where_cols)) {
    where_sql <- paste(" WHERE ", paste(sprintf("%s = ?", where_cols), collapse = " AND "))
    params <- unname(as.list(where_vals))
  }
  sql <- sprintf("DELETE FROM %s%s", table, where_sql)
  if (verbose) message(sql)
  DBI$dbExecute(conn, sql, params = params)
}

# --------- TRANSAÇÃO SEGURA ---------
tryTransaction <- function(expr,debug = FALSE) {
  # garante mesma conexão durante a transação
  with_conn(function(conn) {
    DBI$dbBegin(conn)
    ok <- FALSE
    if(debug){
      debug(expr)
    }
    tryCatch({
      expr(conn)  # passe 'conn' e use DBI$* com essa conn
      DBI$dbCommit(conn)
      ok <- TRUE
    }, error = function(e) {
      DBI$dbRollback(conn)
      warning(e)
    },finally = {
      if(debug){
        undebug(expr)
      }
    })
    ok
  })
}

# --------- FORMATOS ---------
formatDataType <- function(data, type) {
  tp <- toupper(type)
  switch (
    tp,
    'CHARACTER' = as.character(data),
    'CHAR'      = as.character(data),
    'VARCHAR'   = as.character(data),
    'VARCHAR2'  = as.character(data),
    'TEXT'      = as.character(data),

    'BOOLEAN'   = as.logical(data),
    'BOOL'      = as.logical(data),

    'SMALLINT'  = as.integer(data),
    'INTEGER'   = as.integer(data),
    'INT'       = as.integer(data),
    'BIGINT'    = as.integer(data),

    'DECIMAL'   = as.numeric(stringi$stri_replace_all_fixed(as.character(data), ",", ".")),
    'DEC'       = as.numeric(stringi$stri_replace_all_fixed(as.character(data), ",", ".")),
    'NUMERIC'   = as.numeric(stringi$stri_replace_all_fixed(as.character(data), ",", ".")),
    'NUMBER'    = as.numeric(stringi$stri_replace_all_fixed(as.character(data), ",", ".")),
    'REAL'      = as.numeric(data),
    'FLOAT'     = as.numeric(data),
    'DOUBLE PRECISION' = as.numeric(data),

    'DATE'      = as.POSIXct(data, tz = "UTC"),
    'TIME'      = as.POSIXct(data, tz = "UTC"),
    'TIMESTAMP' = as.POSIXct(data, tz = "UTC"),

    'CLOB'      = as.character(data),
    'CHARACTER LARGE OBJECT' = as.character(data),

    'BLOB'      = as.raw(data),     # <— mantenha raw
    'BINARY LARGE OBJECT' = as.raw(data),

    data
  )
}

# --------- NOTIFICAÇÃO DE REDE (opcional) ---------
notifyReconnect <- function() {
  try(showNotification("Reconectando ao banco...", type = "warning"), silent = TRUE)
}

# --- SELECT genérico (retorna data.frame) -----------------
db_query <- function(sql, params = list()) {
  res <- DBI$dbGetQuery(get_pool(), sql, params = unlist(params))
  res
}

# --- SELECT que espera 1 linha (ou NULL) ------------------
db_query_one <- function(sql, params = list()) {
  df <- DBI$dbGetQuery(get_pool(), sql, params = unlist(params))
  if (nrow(df) == 0) return(NULL)
  df[1, , drop = FALSE]
}

# --- EXECUTE genérico (UPDATE/DELETE/DDL) -----------------
db_execute <- function(sql, params = list()) {
  DBI$dbExecute(get_pool(), sql, params = unlist(params))
}

# --- Helper: IN dinâmico seguro ---------------------------
# uso: x <- c(1,2,3); q <- sprintf("SELECT * FROM T WHERE id IN (%s)", sql_in(x)$placeholders)
#      db_query(q, sql_in(x)$params)
sql_in <- function(vec) {
  if (length(vec) == 0) return(list(placeholders = "NULL", params = list()))
  list(
    placeholders = paste(rep("?", length(vec)), collapse = ","),
    params = as.list(vec)
  )
}

# --- Helper: BETWEEN com parâmetros -----------------------
sql_between <- function(col) sprintf("%s BETWEEN ? AND ?", col)

# --- LAST_INSERT_ID (mesma conexão; use com with_conn) ----
last_insert_id <- function(conn) {
  DBI$dbGetQuery(conn, "SELECT LAST_INSERT_ID() AS id")$id[[1]]
}
