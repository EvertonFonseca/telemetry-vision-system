
# ===============================================================
# Opções globais do processo (ex: desabilitar SETUP)
# ===============================================================
options(TVS_DISABLE_SETUP = TRUE)

.tvs_auto_max_async <- function() {
  # Se o usuário setou explicitamente, respeita
  x <- Sys.getenv("TVS_MAX_ASYNC", unset = "")
  if (nzchar(x)) {
    v <- suppressWarnings(as.integer(x))
    if (!is.na(v) && v >= 1L) return(v)
  }

  cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
  if (is.na(cores) || cores < 1L) cores <- 2L

  # Reserva p/ Shiny + SO + DB + ffmpeg etc.
  reserve <- 2L

  # Default: no máx metade dos cores, mantendo reserva
  v <- max(1L, min(cores - reserve, ceiling(cores / 2)))

  v
}

.tvs_auto_max_vclip <- function() {
  x <- Sys.getenv("TVS_MAX_VCLIP", unset = "")
  if (nzchar(x)) {
    v <- suppressWarnings(as.integer(x))
    if (!is.na(v) && v >= 1L) return(v)
  }

  cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
  if (is.na(cores) || cores < 1L) cores <- 2L

  # 1 em máquinas pequenas; 2 só se tiver folga de CPU/IO
  if (cores <= 4L) return(1L)
  2L
}

Sys.setenv(TVS_MAX_VCLIP = as.character(.tvs_auto_max_vclip()))
Sys.setenv(TVS_MAX_ASYNC = as.character(.tvs_auto_max_async()))

# Database connection
Sys.setenv(DBNAME   = "system")
Sys.setenv(HOST     = "127.0.0.1")
Sys.setenv(PORT     = 3306)
Sys.setenv(USERNAME = "root")
Sys.setenv(PASSWORD = "ssbwarcq")

.tvs_mirai_boot <- local({
  started <- FALSE
  function() {
    if (isTRUE(started)) return(invisible(TRUE))

    n <- suppressWarnings(as.integer(Sys.getenv("TVS_MIRAI_DAEMONS", "")))
    if (is.na(n) || n < 1L) {
      cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
      if (is.na(cores) || cores < 1L) cores <- 2L
      n <- max(cores - 2L, 1L)
    }

    ok <- TRUE
    tryCatch({
      mirai::daemons(n, dispatcher = TRUE)  # ou dispatcher=FALSE pra boot mais leve
    }, error = function(e) {
      ok <<- FALSE
      message("[TVS] mirai boot falhou: ", conditionMessage(e))
    })

    started <<- isTRUE(ok)
    options(TVS_MIRAI_BOOTED = started)
    invisible(NULL)
  }
})

# ✅ boot start
.tvs_mirai_boot()

shiny::onStop(function() {
  try(mirai::daemons(0L), silent = TRUE)
  options(TVS_MIRAI_BOOTED = FALSE)
})

shiny::runApp(rhino::app(), host = "127.0.0.1", port = 5000)
