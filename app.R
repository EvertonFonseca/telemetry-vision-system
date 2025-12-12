options(TVS_DISABLE_SETUP = TRUE)

# Configuração global do future (uma vez por processo)
future::plan(
  future::multisession,
  workers = max(parallel::detectCores() - 2L, 1L)
)

# Entrypoint
shiny::runApp(rhino::app(), host = "127.0.0.1", port = 5000)

