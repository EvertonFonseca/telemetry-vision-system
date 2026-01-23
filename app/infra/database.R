# app/infra/database.R
box::use(
  DBI, stringi,
  shiny[showNotification],
  ./db_pool[get_pool, with_conn]
)

# --------- UTIL: quote helpers ----------
qi <- function(conn, x) as.character(DBI::dbQuoteIdentifier(conn, x))

# Constrói um DBI::Id quando você quer schema + table
qid_table <- function(schema, table) DBI::Id(schema = schema, table = table)

# --------- SEQUENCE (Postgres) ----------
# Retorna o nome da sequence ligada a um serial/identity (ex: public.minha_tabela / id)
pg_get_sequence <- function(conn, schema, table, id_col) {
  # monta "schema.table" com quoting (suporta nomes com maiúsculas/espacos se existirem)
  schema_q <- as.character(DBI::dbQuoteIdentifier(conn, schema))
  table_q  <- as.character(DBI::dbQuoteIdentifier(conn, table))
  rel      <- paste0(schema_q, ".", table_q)

  seq <- DBI::dbGetQuery(
    conn,
    "select pg_get_serial_sequence($1, $2) as seq",
    params = list(rel, id_col)
  )$seq[[1]]

  if (is.na(seq) || !nzchar(seq)) {
    stop(sprintf(
      "Nao encontrei sequence para %s.%s(%s). A coluna eh serial/identity?",
      schema, table, id_col
    ))
  }
  seq
}

# "Próximo" ID (recomendado: usar RETURNING no insert; mas se você precisar disso)
# Atenção: nextval() AVANÇA a sequence.
# Postgres: pega a sequence ligada à coluna serial/identity e faz nextval corretamente
nextSequenciaID <- function(conn, table, id_col = "id", schema = "public") {
  rel <- paste0(schema, ".", table)

  seq <- DBI::dbGetQuery(
    conn,
    "select pg_get_serial_sequence($1, $2) as seq",
    params = list(rel, id_col)
  )$seq[[1]]

  if (is.na(seq) || !nzchar(seq)) {
    stop(sprintf(
      "Nao encontrei sequence para %s.%s(%s). A coluna eh serial/identity?",
      schema, table, id_col
    ))
  }

  # ✅ seq precisa ser tratado como regclass (ou string literal)
  DBI::dbGetQuery(
    conn,
    "select nextval($1::regclass) as id",
    params = list(seq)
  )$id[[1]] |> as.integer()
}

# --------- UTIL: placeholders Postgres ----------
pg_placeholders <- function(n) {
  if (n <= 0) stop("n deve ser > 0")
  paste0("$", seq_len(n), collapse = ",")
}

# --- Helper: IN dinâmico seguro (Postgres) -----------------
# uso:
#   x <- c(1,2,3)
#   in <- sql_in_pg(x)
#   q  <- sprintf("select * from t where id in (%s)", in$placeholders)
#   DBI::dbGetQuery(conn, q, params = in$params)
sql_in_pg <- function(vec, start_index = 1L) {
  vec <- as.list(vec)
  n <- length(vec)
  if (n == 0) return(list(placeholders = "null", params = list()))
  idx <- start_index:(start_index + n - 1L)
  list(
    placeholders = paste0("$", idx, collapse = ","),
    params = vec
  )
}

# --- Helper: BETWEEN com parâmetros (Postgres) -------------
sql_between_pg <- function(col, start_index = 1L) {
  sprintf("%s between $%d and $%d", col, start_index, start_index + 1L)
}

# --------- INSERT (parametrizado) ----------
insertTable <- function(conn = get_pool(), table, obj, verbose = FALSE) {
  stopifnot(is.list(obj), length(obj) > 0)
  cols <- names(obj)
  ph   <- pg_placeholders(length(cols))

  cols_q <- paste(vapply(cols, \(x) qi(conn, x), character(1)), collapse = ",")
  sql <- sprintf("insert into %s (%s) values (%s)", table, cols_q, ph)

  if (verbose) message(sql)
  DBI::dbExecute(conn, sql, params = unname(obj))
}

# ✅ Recomendado no Postgres: INSERT ... RETURNING id
insertTableReturning <- function(conn, table, obj, returning_col = "id", verbose = FALSE) {
  stopifnot(is.list(obj), length(obj) > 0)
  cols <- names(obj)
  ph   <- pg_placeholders(length(cols))

  cols_q <- paste(vapply(cols, \(x) qi(conn, x), character(1)), collapse = ",")
  ret_q  <- qi(conn, returning_col)

  sql <- sprintf(
    "insert into %s (%s) values (%s) returning %s",
    table, cols_q, ph, ret_q
  )
  if (verbose) message(sql)

  DBI::dbGetQuery(conn, sql, params = unname(obj))[[returning_col]][[1]]
}

# --------- UPDATE (parametrizado) ----------
updateTable <- function(conn = get_pool(), table, obj, where_cols = character(), where_vals = list(), verbose = FALSE) {
  stopifnot(is.list(obj), length(obj) > 0)

  set_cols <- names(obj)
  set_part <- paste(
    sprintf("%s = $%d", vapply(set_cols, \(x) qi(conn, x), character(1)), seq_along(set_cols)),
    collapse = ", "
  )

  params <- unname(as.list(obj))

  where_sql <- ""
  if (length(where_cols)) {
    start <- length(set_cols) + 1L
    where_part <- paste(
      sprintf("%s = $%d",
              vapply(where_cols, \(x) qi(conn, x), character(1)),
              start:(start + length(where_cols) - 1L)),
      collapse = " and "
    )
    where_sql <- paste0(" where ", where_part)
    params <- c(params, unname(as.list(where_vals)))
  }

  sql <- sprintf("update %s set %s%s", table, set_part, where_sql)
  if (verbose) message(sql)

  DBI::dbExecute(conn, sql, params = params)
}

# --------- DELETE (parametrizado) ---------
deleteTable <- function(conn = get_pool(), table, where_cols = character(), where_vals = list(), verbose = FALSE) {
  params <- list()
  where_sql <- ""

  if (length(where_cols)) {
    where_part <- paste(
      sprintf("%s = $%d",
              vapply(where_cols, \(x) qi(conn, x), character(1)),
              seq_along(where_cols)),
      collapse = " and "
    )
    where_sql <- paste0(" where ", where_part)
    params <- unname(as.list(where_vals))
  }

  sql <- sprintf("delete from %s%s", table, where_sql)
  if (verbose) message(sql)

  DBI::dbExecute(conn, sql, params = params)
}

# --------- TRANSAÇÃO SEGURA ---------
tryTransaction <- function(expr, debug = FALSE) {
  with_conn(function(conn) {
    DBI::dbBegin(conn)
    ok <- FALSE

    if (debug) debug(expr)
    tryCatch({
      expr(conn)
      DBI::dbCommit(conn)
      ok <- TRUE
    }, error = function(e) {
      DBI::dbRollback(conn)
      warning(e)
    }, finally = {
      if (debug) undebug(expr)
    })

    ok
  })
}

# --------- FORMATOS ---------
formatDataType <- function(data, type) {
  tp <- toupper(type)
  switch(
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

    'DECIMAL'   = as.numeric(stringi::stri_replace_all_fixed(as.character(data), ",", ".")),
    'DEC'       = as.numeric(stringi::stri_replace_all_fixed(as.character(data), ",", ".")),
    'NUMERIC'   = as.numeric(stringi::stri_replace_all_fixed(as.character(data), ",", ".")),
    'NUMBER'    = as.numeric(stringi::stri_replace_all_fixed(as.character(data), ",", ".")),
    'REAL'      = as.numeric(data),
    'FLOAT'     = as.numeric(data),
    'DOUBLE PRECISION' = as.numeric(data),

    # Postgres: timestamp/timestamptz - aqui mantemos UTC como você já fazia
    'DATE'      = as.POSIXct(data, tz = "UTC"),
    'TIME'      = as.POSIXct(data, tz = "UTC"),
    'TIMESTAMP' = as.POSIXct(data, tz = "UTC"),
    'TIMESTAMPTZ' = as.POSIXct(data, tz = "UTC"),

    'CLOB'      = as.character(data),

    # Postgres: bytea (continua raw em R)
    'BLOB'      = as.raw(data),
    'BYTEA'     = as.raw(data),

    data
  )
}

# --------- NOTIFICAÇÃO DE REDE (opcional) ---------
notifyReconnect <- function() {
  try(shiny::showNotification("Reconectando ao banco...", type = "warning"), silent = TRUE)
}

# --- SELECT genérico (retorna data.frame) -----------------
db_query <- function(sql, params = list()) {
  DBI::dbGetQuery(get_pool(), sql, params = unlist(params))
}

# --- SELECT que espera 1 linha (ou NULL) ------------------
db_query_one <- function(sql, params = list()) {
  df <- DBI::dbGetQuery(get_pool(), sql, params = unlist(params))
  if (nrow(df) == 0) return(NULL)
  df[1, , drop = FALSE]
}

# --- EXECUTE genérico (UPDATE/DELETE/DDL) -----------------
db_execute <- function(sql, params = list()) {
  DBI::dbExecute(get_pool(), sql, params = unlist(params))
}

# --- Helper: IN dinâmico seguro ---------------------------
sql_in <- function(vec) {
  if (length(vec) == 0) return(list(placeholders = "NULL", params = list()))
  list(
    placeholders = paste(rep("?", length(vec)), collapse = ","),
    params = as.list(vec)
  )
}

# --- Helper: BETWEEN com parâmetros -----------------------
sql_between <- function(col) sprintf("%s BETWEEN ? AND ?", col)

# --- last_insert_id (Postgres) ----------------------------
# Recomendação: use insertTableReturning().
# Se ainda precisar: currval() só funciona na MESMA conexão depois de um nextval/insert.
last_insert_id <- function(conn, schema = "public", table, id_col = "id") {
  seq <- pg_get_sequence(conn, schema, table, id_col)
  seq_q <- qi(conn, DBI::SQL(seq))
  sql <- paste0("SELECT currval(", seq_q, ") AS id")
  DBI::dbGetQuery(conn, sql)$id[[1]]
}
