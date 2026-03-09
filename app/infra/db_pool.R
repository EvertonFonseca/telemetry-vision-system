# app/infra/db_pool.R
box::use(
  pool[dbPool, poolClose, poolCheckout, poolReturn],
  RPostgres[...],
  DBI,
  shiny[onStop]
)

.state <- new.env(parent = emptyenv())

`%||%` <- function(x, y) if (is.null(x)) y else x

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
  if (!is.null(.state$pool)) {
    if (!isTRUE(force)) return(invisible(.state$pool))
    close_pool()
  }

  .state$pool <- dbPool(
    drv      = RPostgres::Postgres(),
    dbname   = Sys.getenv("DB_NAME", "system"),
    host     = Sys.getenv("DB_HOST", "127.0.0.1"),
    port     = as.integer(Sys.getenv("DB_PORT", "3306")),
    user     = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASS", "ssbwarcq"),
    idleTimeout = as.integer(Sys.getenv("DB_IDLE_TIMEOUT", "120"))
  )

  if (!isTRUE(.state$on_stop_registered)) {
    onStop(function() {
      close_pool()
    })
    .state$on_stop_registered <- TRUE
  }

  invisible(.state$pool)
}

get_pool <- function() {
  if (is.null(.state$pool)) init()
  .state$pool
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
