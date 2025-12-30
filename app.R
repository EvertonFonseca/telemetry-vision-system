# ===============================================================
# run_app.R (runner do Rhino)
# ===============================================================
options(TVS_DISABLE_SETUP = TRUE, USE_CACHE = FALSE)

.tvs_auto_max_async <- function() {
  x <- Sys.getenv("TVS_MAX_ASYNC", unset = "")
  if (nzchar(x)) {
    v <- suppressWarnings(as.integer(x))
    if (!is.na(v) && v >= 1L) return(v)
  }
  cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
  if (is.na(cores) || cores < 1L) cores <- 2L
  reserve <- 4L
  max(1L, min(cores - reserve, ceiling(cores / 2)))
}

.tvs_auto_max_vclip <- function() {
  x <- Sys.getenv("TVS_MAX_VCLIP", unset = "")
  if (nzchar(x)) {
    v <- suppressWarnings(as.integer(x))
    if (!is.na(v) && v >= 1L) return(v)
  }
  cores <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
  if (is.na(cores) || cores < 1L) cores <- 2L
  if (cores <= 4L) 1L else 2L
}

Sys.setenv(
  TVS_MAX_ASYNC = as.character(.tvs_auto_max_async()),
  TVS_MAX_VCLIP = as.character(.tvs_auto_max_vclip()),
  DBNAME   = "system",
  HOST     = "127.0.0.1",
  PORT     = "3306",
  USERNAME = "root",
  PASSWORD = "ssbwarcq"
)

# IMPORTANT: crie o app ANTES do with()
app <- rhino::app()  # (segue a recomendação da doc do mirai) :contentReference[oaicite:3]{index=3}

n_async <- as.integer(Sys.getenv("TVS_MAX_ASYNC", "2"))
n_vclip <- as.integer(Sys.getenv("TVS_MAX_VCLIP", "1"))

# ---------------------------------------------------------------
# ✅ Boot mirai 1x por processo (antes das sessões)
# ---------------------------------------------------------------
if (!isTRUE(getOption("TVS_MIRAI_BOOTED", FALSE))) {

  n_async <- as.integer(Sys.getenv("TVS_MAX_ASYNC", "2"))
  n_vclip <- as.integer(Sys.getenv("TVS_MAX_VCLIP", "1"))

  # Sobe dois dispatchers (nomes diferentes)
  mirai::daemons(n_async, dispatcher = TRUE, .compute = "dashui")
  mirai::daemons(n_vclip, dispatcher = TRUE, .compute = "vclip")

  # Flag global pra módulos não tentarem bootar de novo
  options(TVS_MIRAI_BOOTED = TRUE)

  # Opcional: preload de pacotes em TODOS os workers
  # (isso evita o "primeiro job" ser lento por carregar libs)
  try(mirai::everywhere({
    suppressMessages(library(DBI))
    suppressMessages(library(RMariaDB))
    suppressMessages(library(jsonlite))
    suppressMessages(library(dplyr))
    suppressMessages(library(tidyr))
    suppressMessages(library(lubridate))
    suppressMessages(library(tibble))
    suppressMessages(library(purrr))
  }, .compute = "dashui"), silent = TRUE)

  # (se vclip também precisa de libs específicas, faz outro everywhere)
  try(mirai::everywhere({ suppressMessages(library(av)) }, .compute="vclip"), silent=TRUE)

  # Garante shutdown quando o processo cair (CTRL+C / stop)
  on.exit({
    try(mirai::daemons(0L, .compute = "dashui"), silent = TRUE)
    try(mirai::daemons(0L, .compute = "vclip"), silent = TRUE)
    options(TVS_MIRAI_BOOTED = FALSE)
  }, add = TRUE)
}

#Run
with(mirai::daemons(n_async, dispatcher = TRUE, .compute = "dashui"), {
  with(mirai::daemons(n_vclip, dispatcher = TRUE, .compute = "vclip"), {
    shiny::runApp(app, host = "127.0.0.1", port = 5000)
  })
})


# ###############################

# # app.R  (EXEMPLO MÍNIMO VISNETWORK + MÓDULO NAMESPACE)
# # =====================================================
# library(shiny)
# library(visNetwork)

# # ---------------------------
# # Module UI
# # ---------------------------
# flow_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     tags$style(HTML("
#       .panel{ padding:10px; border:1px solid #eee; border-radius:8px; }
#       .hint { font-size:12px; opacity:.75; margin-top:6px; }
#     ")),
#     div(class="panel",
#       h4("Fluxo de Setores (vertical)"),
#       visNetworkOutput(ns("net"), height = "520px"),
#       div(class="hint",
#           "Clique em um nó. O nó selecionado aparece abaixo:"),
#       verbatimTextOutput(ns("dbg"))
#     )
#   )
# }

# # ---------------------------
# # Module Server
# # ---------------------------
# flow_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns

#     # ✅ dados corretos (IDs batendo)
#     sectors_df <- data.frame(
#       id        = c("FAB", "A", "B", "B1"),
#       label     = c("Fábrica", "Estamparia", "Centra Dobra", "Pintura"),
#       parent_id = c(NA, "FAB", "FAB", "B"),
#       stringsAsFactors = FALSE
#     )

#     machines_df <- data.frame(
#       id        = c("M1","M2","M3","M4"),
#       label     = c("SETREMA","ESQUADROS","DOBRA","PINTURA"),
#       sector_id = c("A","A","B","B1"),
#       stringsAsFactors = FALSE
#     )

#     nodes_sector <- data.frame(
#       id    = paste0("S:", sectors_df$id),
#       label = paste0("🏭 ", sectors_df$label),
#       group = "sector",
#       shape = "box",
#       stringsAsFactors = FALSE
#     )

#     nodes_machine <- data.frame(
#       id    = paste0("M:", machines_df$id),
#       label = paste0("⚙️ ", machines_df$label),
#       group = "machine",
#       shape = "box",
#       stringsAsFactors = FALSE
#     )

#     nodes <- rbind(nodes_sector, nodes_machine)

#     edges_sector <- subset(sectors_df, !is.na(parent_id))
#     edges_sector <- data.frame(
#       from = paste0("S:", edges_sector$parent_id),
#       to   = paste0("S:", edges_sector$id),
#       arrows = "to",
#       stringsAsFactors = FALSE
#     )

#     edges_machine <- data.frame(
#       from = paste0("S:", machines_df$sector_id),
#       to   = paste0("M:", machines_df$id),
#       arrows = "to",
#       stringsAsFactors = FALSE
#     )

#     edges <- rbind(edges_sector, edges_machine)

#     output$net <- renderVisNetwork({
#       visNetwork(nodes, edges, height = "520px") |>
#         visNodes(
#           font = list(size = 16),
#           margin = 10,
#           color = list(
#             border = "rgba(0,0,0,0.25)",
#             background = "rgba(255,255,255,0.98)",
#             highlight = list(background = "rgba(220,240,255,0.95)")
#           )
#         ) |>
#         visGroups(groupname = "sector", color = list(background = "#f7fbff")) |>
#         visGroups(groupname = "machine", color = list(background = "#fffdf6")) |>
#         visEdges(
#           smooth = list(type = "cubicBezier", forceDirection = "vertical", roundness = 0.4),
#           color  = list(color = "rgba(120,120,120,0.35)")
#         ) |>
#         visHierarchicalLayout(
#           enabled = TRUE,
#           direction = "UD",
#           sortMethod = "directed",
#           nodeSpacing = 180,
#           levelSeparation = 130
#         ) |>
#         visInteraction(hover = TRUE, navigationButtons = TRUE) |>
#         visOptions(highlightNearest = TRUE)
#     })

#     output$dbg <- renderPrint({
#       list(
#         selected_nodes = input$net_selected,
#         selected_edges = input$net_selectedEdges
#       )
#     })
#   })
# }

# # ---------------------------
# # App
# # ---------------------------
# ui <- fluidPage(
#   flow_ui("flow")
# )

# server <- function(input, output, session) {
#   flow_server("flow")
# }

# shinyApp(ui, server)
