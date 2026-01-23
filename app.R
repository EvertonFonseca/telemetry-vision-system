# # ===============================================================
# # run_app.R (runner do Rhino)
# # ===============================================================
# options(TVS_DISABLE_SETUP = TRUE, USE_CACHE = FALSE)

# .tvs_auto_max_async <- function() {
#   x <- Sys.getenv("TVS_MAX_ASYNC", unset = "")
#   if (nzchar(x)) {
#     v <- suppressWarnings(as.integer(x))
#     if (!is.na(v) && v >= 1L) return(v)
#   }
#   cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
#   if (is.na(cores) || cores < 1L) cores <- 2L
#   reserve <- 4L
#   max(1L, min(cores - reserve, ceiling(cores / 2)))
# }

# .tvs_auto_max_vclip <- function() {
#   x <- Sys.getenv("TVS_MAX_VCLIP", unset = "")
#   if (nzchar(x)) {
#     v <- suppressWarnings(as.integer(x))
#     if (!is.na(v) && v >= 1L) return(v)
#   }
#   cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
#   if (is.na(cores) || cores < 1L) cores <- 2L
#   if (cores <= 4L) 1L else 2L
# }

Sys.setenv(
  #TVS_MAX_ASYNC = as.character(.tvs_auto_max_async()),
  #TVS_MAX_VCLIP = as.character(.tvs_auto_max_vclip()),
  DB_NAME = "analytia_db",
  DB_HOST = "127.0.0.1",
  DB_PORT = 5434,
  DB_USER = "analytia",
  DB_PASS = "analytia"
)

# # IMPORTANT: crie o app ANTES do with()
app <- rhino::app()  # (segue a recomendação da doc do mirai) :contentReference[oaicite:3]{index=3}

# n_async <- as.integer(Sys.getenv("TVS_MAX_ASYNC", "2"))
# n_vclip <- as.integer(Sys.getenv("TVS_MAX_VCLIP", "1"))

# # ---------------------------------------------------------------
# # ✅ Boot mirai 1x por processo (antes das sessões)
# # ---------------------------------------------------------------
# if (!isTRUE(getOption("TVS_MIRAI_BOOTED", FALSE))) {

#   n_async <- as.integer(Sys.getenv("TVS_MAX_ASYNC", "2"))
#   n_vclip <- as.integer(Sys.getenv("TVS_MAX_VCLIP", "1"))

#   # Sobe dois dispatchers (nomes diferentes)
#   mirai::daemons(n_async, dispatcher = TRUE, .compute = "dashui")
#   mirai::daemons(n_vclip, dispatcher = TRUE, .compute = "vclip")

#   # Flag global pra módulos não tentarem bootar de novo
#   options(TVS_MIRAI_BOOTED = TRUE)

#   # Opcional: preload de pacotes em TODOS os workers
#   # (isso evita o "primeiro job" ser lento por carregar libs)
#   try(mirai::everywhere({
#     suppressMessages(library(DBI))
#     suppressMessages(library(RMariaDB))
#     suppressMessages(library(jsonlite))
#     suppressMessages(library(dplyr))
#     suppressMessages(library(tidyr))
#     suppressMessages(library(lubridate))
#     suppressMessages(library(tibble))
#     suppressMessages(library(purrr))
#   }, .compute = "dashui"), silent = TRUE)

#   # (se vclip também precisa de libs específicas, faz outro everywhere)
#   try(mirai::everywhere({suppressMessages(library(av)) }, .compute="vclip"), silent=TRUE)

#   # Garante shutdown quando o processo cair (CTRL+C / stop)
#   on.exit({
#     try(mirai::daemons(0L, .compute = "dashui"), silent = TRUE)
#     try(mirai::daemons(0L, .compute = "vclip"), silent = TRUE)
#     options(TVS_MIRAI_BOOTED = FALSE)
#   }, add = TRUE)
# }

# #Run
# with(mirai::daemons(n_async, dispatcher = TRUE, .compute = "dashui"), {
#   with(mirai::daemons(n_vclip, dispatcher = TRUE, .compute = "vclip"), {
#     shiny::runApp(app, host = "127.0.0.1", port = 5000)
#   })
# })

shiny::runApp(app, host = "127.0.0.1", port = 5000)

