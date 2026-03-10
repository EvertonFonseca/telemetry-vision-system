# app/infra/db_pool.R
box::use(
  pool[dbPool, poolClose, poolCheckout, poolReturn],
  RPostgres[...],
  DBI
)

.state <- new.env(parent = emptyenv())

`%||%` <- function(x, y) if (is.null(x)) y else x

.normalize_cfg <- function(cfg) {
  list(
    dbname = as.character(cfg$dbname %||% ""),
    host = as.character(cfg$host %||% ""),
    port = suppressWarnings(as.integer(cfg$port %||% NA_integer_)),
    user = as.character(cfg$user %||% ""),
    password = as.character(cfg$password %||% "")
  )
}

.current_cfg <- function() {
  .normalize_cfg(list(
    dbname = Sys.getenv("DB_NAME", "system"),
    host = Sys.getenv("DB_HOST", "127.0.0.1"),
    port = Sys.getenv("DB_PORT", "3306"),
    user = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASS", "ssbwarcq")
  ))
}

.same_cfg <- function(x, y) identical(.normalize_cfg(x), .normalize_cfg(y))

.pool_is_ready <- function(pool) {
  if (is.null(pool)) return(FALSE)

  conn <- try(poolCheckout(pool), silent = TRUE)
  if (inherits(conn, "try-error")) return(FALSE)

  on.exit(try(poolReturn(conn), silent = TRUE), add = TRUE)
  isTRUE(tryCatch(DBI::dbIsValid(conn), error = function(e) FALSE))
}

apply_db_env <- function(dbname, host, port, user, password) {
  Sys.setenv(
    DB_NAME = as.character(dbname),
    DB_HOST = as.character(host),
    DB_PORT = as.character(port),
    DB_USER = as.character(user),
    DB_PASS = as.character(password)
  )
  invisible(NULL)
}

close_pool <- function() {
  if (!is.null(.state$pool)) {
    try(poolClose(.state$pool), silent = TRUE)
    .state$pool <- NULL
  }
  invisible(NULL)
}

init <- function(force = FALSE) {
  cfg <- .current_cfg()

  if (!is.null(.state$pool) && !isTRUE(force)) {
    if (.same_cfg(.state$cfg, cfg) && .pool_is_ready(.state$pool)) {
      return(invisible(.state$pool))
    }
  }

  if (!is.null(.state$pool)) {
    close_pool()
  }

  .state$pool <- dbPool(
    drv      = RPostgres::Postgres(),
    dbname   = cfg$dbname,
    host     = cfg$host,
    port     = cfg$port,
    user     = cfg$user,
    password = cfg$password,
    idleTimeout = as.integer(Sys.getenv("DB_IDLE_TIMEOUT", "120"))
  )
  .state$cfg <- cfg

  invisible(.state$pool)
}

get_pool <- function() {
  if (is.null(.state$pool)) init()
  .state$pool
}

active_sessions <- function() {
  as.integer(.state$active_sessions %||% 0L)
}

can_activate_config <- function(dbname, host, port, user, password) {
  if (is.null(.state$pool) || is.null(.state$cfg)) return(TRUE)
  if (.same_cfg(.state$cfg, list(
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password
  ))) {
    return(TRUE)
  }

  active_sessions() == 0L
}

# Executa varias operacoes na MESMA conexao (transacao opcional)
with_conn <- function(fun) {
  conn <- poolCheckout(get_pool())
  on.exit(poolReturn(conn), add = TRUE)
  fun(conn)
}

session_register <- function(session) {
  .state$active_sessions <- (.state$active_sessions %||% 0L) + 1L

  session$onSessionEnded(function() {
    .state$active_sessions <- max((.state$active_sessions %||% 1L) - 1L, 0L)
    if (.state$active_sessions == 0L) {
      close_pool()
    }
  })
}
