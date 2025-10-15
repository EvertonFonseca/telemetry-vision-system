box::use(DBI)

# -------------------------------
# Utilitários transacionais
# -------------------------------
#' @export
.affected_ok <- function(n) {
  # Alguns drivers retornam NA para INSERT.
  isTRUE(n > 0L) || is.na(n)
}

#' @export
.run_tx_bool <- function(con, expr) {
  DBI$dbBegin(con)
  ok <- FALSE
  on.exit({
    if (!ok) DBI$dbRollback(con)
  }, add = TRUE)

  out <- try(expr, silent = TRUE)
  if (inherits(out, "try-error")) {
    return(FALSE)
  }

  # Se chegou aqui, não houve erro na expr
  DBI$dbCommit(con)
  ok <- TRUE
  TRUE
}
