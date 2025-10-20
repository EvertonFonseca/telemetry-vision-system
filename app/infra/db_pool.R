# app/infra/db_pool.R
box::use(
  pool[dbPool, poolClose, poolCheckout, poolReturn],
  RMariaDB[MariaDB],
  DBI,
  shiny[onStop]
)

.state <- new.env(parent = emptyenv())

init <- function() {
  if (!is.null(.state$pool)) return(invisible(.state$pool))

  .state$pool <- dbPool(
    drv      = MariaDB(),
    dbname   = Sys.getenv("DB_NAME", "system"),
    host     = Sys.getenv("DB_HOST", "127.0.0.1"),
    port     = as.integer(Sys.getenv("DB_PORT", "3306")),
    username = Sys.getenv("DB_USER", "root"),
    password = Sys.getenv("DB_PASS", "ssbwarcq"),
    idleTimeout = as.integer(Sys.getenv("DB_IDLE_TIMEOUT", "120"))
  )

  onStop(function() {
    if (!is.null(.state$pool)) poolClose(.state$pool)
    .state$pool <- NULL
  })

  invisible(.state$pool)
}

get_pool <- function() {
  if (is.null(.state$pool)) init()
  .state$pool
}

# Executa várias operações na MESMA conexão (transação opcional)
with_conn <- function(fun) {
  conn <- poolCheckout(get_pool())
  on.exit(poolReturn(conn), add = TRUE)
  fun(conn)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

session_register <- function(session) {
  if (is.null(.state$pool)) init()

  .state$active_sessions <- (.state$active_sessions %||% 0L) + 1L

  session$onSessionEnded(function(){
    .state$active_sessions <- max((.state$active_sessions %||% 1L) - 1L, 0L)
    if (.state$active_sessions == 0L) {
      # ninguém mais usando: pode fechar
      if (!is.null(.state$pool)) try(poolClose(.state$pool), silent = TRUE)
      .state$pool <- NULL
    }
  })
}
