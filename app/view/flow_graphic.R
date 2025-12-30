# app/view/flow_graphic.R
# ===============================================================
# Flow vertical de Setores -> Máquinas (mini Gantt por máquina)
# Rhino module: ui(ns), server(ns, ...)
# ===============================================================

box::use(
  shiny[
    NS, tagList, div, tags,
    reactive, reactiveVal, observeEvent, req, isolate,
    uiOutput,renderUI,
  ],
  shinydashboardPlus[box],
  shinydashboard,
  visNetwork[
    visNetworkOutput, renderVisNetwork,
    visNetwork, visNodes, visEdges,
    visOptions, visInteraction, visLayout, visHierarchicalLayout
  ],
  plotly,
  dplyr[...],
  tidyr,
  lubridate,
  htmltools[HTML],
  jsonlite,

  dbp = ../infra/db_pool,
  ../logic/objeto_dao[selectAllObjetos],

  ../logic/dashboard_dao[
    tz_local, to_utc, from_utc
  ]
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------
# ✅ Config visual
# ---------------------------------------------------------------
.STATE_COLORS <- c(
  "OPERANDO" = "#00a65a",
  "PARADO"   = "tomato",
  "SETUP"    = "#f39c12"
)

.norm_state <- function(x) toupper(trimws(as.character(x %||% "")))

# ---------------------------------------------------------------
# ✅ Mini Gantt em SVG (embed no label do nó)
# episodes: data.frame(start_time, end_time, ESTADO)
# ---------------------------------------------------------------
.svg_timeline <- function(episodes, t0, t1, width = 190, height = 18) {
  t0 <- as.POSIXct(t0); t1 <- as.POSIXct(t1)
  if (is.na(t0) || is.na(t1) || t1 <= t0) return("")

  total <- as.numeric(difftime(t1, t0, units = "secs"))
  if (!is.finite(total) || total <= 0) return("")

  svg <- sprintf(
    "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d' viewBox='0 0 %d %d' style='display:block;'>",
    width, height, width, height
  )
  svg <- paste0(svg, sprintf(
    "<rect x='0' y='0' width='%d' height='%d' rx='3' ry='3' fill='rgba(0,0,0,0.06)'/>",
    width, height
  ))

  if (is.null(episodes) || !nrow(episodes)) {
    svg <- paste0(svg, "</svg>")
    return(svg)
  }

  ep <- episodes
  ep$start_time <- as.POSIXct(ep$start_time)
  ep$end_time   <- as.POSIXct(ep$end_time)
  ep$ESTADO     <- .norm_state(ep$ESTADO)

  ep$start_time <- pmax(ep$start_time, t0)
  ep$end_time   <- pmin(ep$end_time,   t1)
  ep <- ep[!is.na(ep$start_time) & !is.na(ep$end_time) & ep$end_time > ep$start_time, , drop = FALSE]
  if (!nrow(ep)) {
    svg <- paste0(svg, "</svg>")
    return(svg)
  }

  ep <- ep[order(ep$start_time), , drop = FALSE]

  for (i in seq_len(nrow(ep))) {
    s <- as.numeric(difftime(ep$start_time[i], t0, units = "secs"))
    e <- as.numeric(difftime(ep$end_time[i],   t0, units = "secs"))
    x <- (s / total) * width
    w <- max(1, (e - s) / total * width)

    st  <- ep$ESTADO[i]
    col <- .STATE_COLORS[[st]] %||% "#888888"

    tip <- paste0(
      st, " • ",
      format(ep$start_time[i], "%H:%M"), " → ", format(ep$end_time[i], "%H:%M")
    )

    svg <- paste0(svg, sprintf(
      "<g><title>%s</title><rect x='%.2f' y='2' width='%.2f' height='%d' rx='3' ry='3' fill='%s'/></g>",
      htmltools::htmlEscape(tip),
      x, w, max(1, height - 4), col
    ))
  }

  paste0(svg, "</svg>")
}

# ---------------------------------------------------------------
# ✅ Resumo por setor (% utilização)
# episodes_df esperado: SETOR, OBJETO, start_time, end_time, ESTADO
# ---------------------------------------------------------------
.sector_summary <- function(episodes_df) {
  if (is.null(episodes_df) || !nrow(episodes_df)) return(NULL)

  ep <- episodes_df
  req(all(c("SETOR","start_time","end_time","ESTADO") %in% names(ep)))

  ep$ESTADO <- .norm_state(ep$ESTADO)
  ep$dur    <- as.numeric(difftime(as.POSIXct(ep$end_time), as.POSIXct(ep$start_time), units = "secs"))
  ep <- ep[is.finite(ep$dur) & ep$dur > 0, , drop = FALSE]
  if (!nrow(ep)) return(NULL)

  # agrega por setor e estado
  agg <- ep |>
    dplyr::group_by(SETOR, ESTADO) |>
    dplyr::summarise(secs = sum(dur, na.rm = TRUE), .groups = "drop")

  wide <- tidyr::pivot_wider(
    agg,
    names_from = ESTADO,
    values_from = secs,
    values_fill = list(secs = 0)
  )

  # garante colunas
  if (!"OPERANDO" %in% names(wide)) wide$OPERANDO <- 0
  if (!"PARADO"   %in% names(wide)) wide$PARADO   <- 0
  if (!"SETUP"    %in% names(wide)) wide$SETUP    <- 0

  wide |>
    dplyr::mutate(
      total = OPERANDO + PARADO + SETUP,
      util  = dplyr::if_else(total > 0, OPERANDO / total * 100, as.numeric(NA)),
      op_h  = OPERANDO / 3600,
      pa_h  = PARADO   / 3600,
      su_h  = SETUP    / 3600
    ) |>
    dplyr::select(SETOR, util, op_h, pa_h, su_h, total)
}

# ---------------------------------------------------------------
# ✅ Builder do grafo
# sectors_df: id, label, parent_id
# machines_df: id, label, sector_id
# episodes_df: SETOR, OBJETO, start_time, end_time, ESTADO
# ---------------------------------------------------------------
.build_graph <- function(sectors_df, machines_df, episodes_df, t0, t1) {

  sec_sum <- .sector_summary(episodes_df)

  # nodes setor (com %)
  n_sector <- sectors_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      util = {
        if (is.null(sec_sum)) NA_real_
        else {
          s <- sec_sum |> dplyr::filter(as.character(SETOR) == as.character(label))
          if (nrow(s)) s$util[1] else NA_real_
        }
      },
      title_html = {
        if (is.null(sec_sum)) paste0("<b>Setor:</b> ", htmltools::htmlEscape(label))
        else {
          s <- sec_sum |> dplyr::filter(as.character(SETOR) == as.character(label))
          if (!nrow(s)) {
            paste0("<b>Setor:</b> ", htmltools::htmlEscape(label))
          } else {
            paste0(
              "<b>Setor:</b> ", htmltools::htmlEscape(label),
              "<br><small>Utilização: <b>", ifelse(is.na(s$util[1]), "—", sprintf("%.1f%%", s$util[1])), "</b></small>",
              "<br><small>Operando: ", sprintf("%.1f h", s$op_h[1]),
              " • Parado: ", sprintf("%.1f h", s$pa_h[1]),
              " • Setup: ", sprintf("%.1f h", s$su_h[1]), "</small>"
            )
          }
        }
      },
      label_html = {
        if (is.na(util)) {
          paste0("<div style='font-weight:800;font-size:12px;'>", htmltools::htmlEscape(label), "</div>")
        } else {
          paste0(
            "<div style='line-height:1.05'>",
              "<div style='font-weight:900;font-size:12px;'>", htmltools::htmlEscape(label), "</div>",
              "<div style='font-size:11px;opacity:.9;margin-top:4px'>",
                "Utilização: <b>", sprintf("%.1f%%", util), "</b>",
              "</div>",
            "</div>"
          )
        }
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      id    = paste0("S:", id),
      rawid = id,
      type  = "sector",
      label = label_html,
      title = title_html,
      shape = "box"
    )

  # episodes por máquina (para mini gantt)
  get_eps_for_machine <- function(mlabel) {
    if (is.null(episodes_df) || !nrow(episodes_df)) return(NULL)
    ep <- episodes_df[as.character(episodes_df$OBJETO) == as.character(mlabel), , drop = FALSE]
    if (!nrow(ep)) return(NULL)
    ep
  }

  # nodes máquina
  n_machine <- machines_df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      id_node = paste0("M:", id),
      eps = list(get_eps_for_machine(label)),
      svg = .svg_timeline(eps[[1]], t0 = t0, t1 = t1, width = 190, height = 18),
      label_html = paste0(
        "<div style='line-height:1.1'>",
          "<div style='font-weight:800;font-size:12px;'>", htmltools::htmlEscape(label), "</div>",
          "<div style='margin-top:4px;'>", svg, "</div>",
        "</div>"
      ),
      title_html = paste0(
        "<b>Máquina:</b> ", htmltools::htmlEscape(label),
        "<br><small>Janela: ", format(as.POSIXct(t0), "%d/%m %H:%M"),
        " → ", format(as.POSIXct(t1), "%d/%m %H:%M"), "</small>"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      id    = id_node,
      rawid = id,
      type  = "machine",
      label = label_html,
      title = title_html,
      shape = "box"
    )

  nodes <- dplyr::bind_rows(n_sector, n_machine)

  # edges
  e_sector <- sectors_df |>
    dplyr::filter(!is.na(parent_id)) |>
    dplyr::transmute(
      from = paste0("S:", parent_id),
      to   = paste0("S:", id)
    )

  e_machine <- machines_df |>
    dplyr::transmute(
      from = paste0("S:", sector_id),
      to   = paste0("M:", id)
    )

  edges <- dplyr::bind_rows(e_sector, e_machine)

  list(nodes = nodes, edges = edges)
}

# ---------------------------------------------------------------
# ✅ Fallback: monta tree simples via selectAllObjetos(pool)
# ---------------------------------------------------------------
.default_tree_from_db <- function(pool) {
  objs <- selectAllObjetos(pool)
  req(all(c("NAME_SETOR", "NAME_OBJETO") %in% names(objs)))

  root <- dplyr::tibble(id = "ROOT", label = "ROOT", parent_id = NA_character_)

  sectors <- objs |>
    dplyr::distinct(NAME_SETOR) |>
    dplyr::arrange(NAME_SETOR) |>
    dplyr::mutate(
      id = as.character(NAME_SETOR),
      label = as.character(NAME_SETOR),
      parent_id = "ROOT"
    ) |>
    dplyr::select(id, label, parent_id)

  sectors_df <- dplyr::bind_rows(root, sectors)

  machines_df <- objs |>
    dplyr::distinct(NAME_SETOR, NAME_OBJETO) |>
    dplyr::mutate(
      id = paste0(NAME_SETOR, "::", NAME_OBJETO),
      label = as.character(NAME_OBJETO),
      sector_id = as.character(NAME_SETOR)
    ) |>
    dplyr::select(id, label, sector_id)

  list(sectors = sectors_df, machines = machines_df)
}

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
#' @export
ui <- function(ns) {
  ns <- NS(ns)

  tagList(
    tags$style(HTML("
      .tvs-flow-wrap { margin-top: 10px; }
      .tvs-flow-legend { font-size: 12px; opacity: .85; margin: 8px 0 0 0; }
      .tvs-flow-legend span { display:inline-flex; align-items:center; margin-right:12px; gap:6px; }
      .tvs-dot { width:10px; height:10px; border-radius:2px; display:inline-block; }
      .tvs-flow-side { padding: 10px; }
    ")),

    div(class = "tvs-flow-wrap",
      shinydashboardPlus::box(
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        title = tags$span("Fluxo de Setores (Vertical)", style = "font-size:16px;"),
        div(
          style = "display:flex; gap:10px;",

          # esquerda: grafo
          div(style="flex: 1 1 65%; min-height: 560px;",
            visNetworkOutput(ns("flow_net"), height = "560px"),
            div(class="tvs-flow-legend",
              tags$span("Legenda:"),
              tags$span(tags$span(class="tvs-dot", style=paste0("background:", .STATE_COLORS[["OPERANDO"]])), "Operando"),
              tags$span(tags$span(class="tvs-dot", style=paste0("background:", .STATE_COLORS[["PARADO"]])),   "Parado"),
              tags$span(tags$span(class="tvs-dot", style=paste0("background:", .STATE_COLORS[["SETUP"]])),    "Setup"),
              tags$span(style="margin-left:6px; opacity:.7;", "(clique na máquina → clique no segmento → abre o clip)")
            )
          ),

          # direita: detalhe
          div(style="flex: 1 1 35%; min-height: 560px;",
            shinydashboardPlus::box(
              width = 12, solidHeader = TRUE, collapsible = TRUE,
              title = tags$span("Detalhe da Máquina", style="font-size:14px;"),
              div(class="tvs-flow-side",
                uiOutput(ns("machine_title")),
                plotly::plotlyOutput(ns("machine_gantt"), height = "480px")
              )
            )
          )
        )
      )
    )
  )
}

# ---------------------------------------------------------------
# Server
# ---------------------------------------------------------------
#' @export
server <- function(ns,
                   input, output, session,
                   pool = NULL,
                   get_tree = NULL,
                   get_episodes = NULL,
                   # ✅ novo: janela (t0/t1) vem do dashboard (filtros)
                   get_time_window = NULL,
                   # ✅ novo: callback para abrir clip no dashboard
                   on_clip = NULL,
                   window_hours = 24) {

  if (is.null(pool)) pool <- dbp$get_pool()
  tzL <- tz_local()

  # janela de tempo
  .time_window <- reactive({
    if (is.function(get_time_window)) {
      tw <- get_time_window()
      if (is.list(tw) && !is.null(tw$t0) && !is.null(tw$t1)) {
        return(list(t0 = as.POSIXct(tw$t0, tz = tzL), t1 = as.POSIXct(tw$t1, tz = tzL)))
      }
    }
    t1 <- as.POSIXct(Sys.time(), tz = tzL)
    t0 <- t1 - as.numeric(window_hours) * 3600
    list(t0 = t0, t1 = t1)
  })

  # tree source
  tree_data <- reactive({
    if (is.function(get_tree)) {
      x <- get_tree()
      req(is.list(x), !is.null(x$sectors), !is.null(x$machines))
      return(x)
    }
    .default_tree_from_db(pool)
  })

  # episodes source
  episodes_data <- reactive({
    if (is.function(get_episodes)) return(get_episodes())
    NULL
  })

  # estado do “máquina selecionada”
  rv_sel <- reactiveVal(NULL)  # list(mid=..., mlabel=..., setor=...)

  # render graph
  output$flow_net <- renderVisNetwork({
    td <- tree_data()
    tw <- .time_window()
    ep <- episodes_data()

    g <- .build_graph(
      sectors_df  = td$sectors,
      machines_df = td$machines,
      episodes_df = ep,
      t0 = tw$t0,
      t1 = tw$t1
    )

    visNetwork::visNetwork(g$nodes, g$edges, height = "560px", width = "100%") |>
      visNetwork::visNodes(
        font = list(face = "Arial", size = 12, multi = "html"),
        shapeProperties = list(borderRadius = 4),
        margin = 10
      ) |>
      visNetwork::visEdges(
        arrows = "to",
        smooth = list(type = "cubicBezier", forceDirection = "vertical", roundness = 0.4),
        color  = list(color = "rgba(120,120,120,0.35)", highlight = "rgba(120,120,120,0.8)")
      ) |>
      visNetwork::visHierarchicalLayout(
        enabled = TRUE,
        direction = "UD",
        sortMethod = "directed",
        nodeSpacing = 160,
        levelSeparation = 130,
        treeSpacing = 200
      ) |>
      visNetwork::visInteraction(
        hover = TRUE,
        navigationButtons = TRUE,
        zoomView = TRUE,
        dragView = TRUE
      ) |>
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) |>
      visNetwork::visLayout(randomSeed = 42)
  })

  # clique no nó -> detalhe
  observeEvent(input$flow_net_selected, {
    sel <- input$flow_net_selected
    if (is.null(sel) || !length(sel)) return()

    node_id <- as.character(sel[[1]])
    if (!startsWith(node_id, "M:")) return()

    mid <- sub("^M:", "", node_id)

    td <- tree_data()
    mrow <- td$machines |> dplyr::filter(as.character(id) == as.character(mid))
    if (!nrow(mrow)) return()

    mlabel <- as.character(mrow$label[1])

    # setor (via sector_id)
    setor <- NA_character_
    try({
      setor_id <- as.character(mrow$sector_id[1])
      srow <- td$sectors |> dplyr::filter(as.character(id) == setor_id)
      if (nrow(srow)) setor <- as.character(srow$label[1]) else setor <- setor_id
    }, silent = TRUE)

    rv_sel(list(mid = mid, mlabel = mlabel, setor = setor))

    output$machine_title <- renderUI({
      HTML(paste0(
        "<div style='font-weight:900;font-size:13px;'>Máquina: ",
        htmltools::htmlEscape(mlabel),
        "</div>",
        "<div style='font-size:11px; opacity:.75; margin-top:2px;'>",
        "Setor: ", htmltools::htmlEscape(setor %||% "—"),
        "</div>"
      ))
    })

    tw <- .time_window()
    ep <- episodes_data()

    if (is.null(ep) || !nrow(ep)) {
      output$machine_gantt <- plotly::renderPlotly({ NULL })
      return()
    }

    eps <- ep |> dplyr::filter(as.character(OBJETO) == as.character(mlabel))
    if (!nrow(eps)) {
      output$machine_gantt <- plotly::renderPlotly({ NULL })
      return()
    }

    eps$ESTADO <- .norm_state(eps$ESTADO)
    eps$start_time <- as.POSIXct(eps$start_time, tz = tzL)
    eps$end_time   <- as.POSIXct(eps$end_time,   tz = tzL)

    eps <- eps |>
      dplyr::mutate(
        start_time = pmax(start_time, tw$t0),
        end_time   = pmin(end_time,   tw$t1)
      ) |>
      dplyr::filter(!is.na(start_time), !is.na(end_time), end_time > start_time) |>
      dplyr::arrange(start_time)

    output$machine_gantt <- plotly::renderPlotly({
      if (!nrow(eps)) return(NULL)

      eps$row_id <- seq_len(nrow(eps))

      p <- plotly::plot_ly(source = "flow_gantt")

      for (i in seq_len(nrow(eps))) {
        st <- eps$start_time[i]
        en <- eps$end_time[i]
        estado <- as.character(eps$ESTADO[i])
        col <- .STATE_COLORS[[estado]] %||% "#888888"

        p <- plotly::add_segments(
          p,
          x = st, xend = en,
          y = 1,  yend = 1,
          line = list(width = 18, color = col),
          hoverinfo = "text",
          text = paste0(
            "<b>", htmltools::htmlEscape(estado), "</b>",
            "<br>", format(st, "%d/%m %H:%M"), " → ", format(en, "%H:%M"),
            "<br>Duração: ", round(as.numeric(difftime(en, st, units="mins")), 1), " min",
            "<br><span style='opacity:.75'>(clique para abrir clip)</span>"
          ),
          key = eps$row_id[i],
          showlegend = FALSE
        )
      }

      plotly::layout(
        p,
        xaxis = list(title = "", type = "date"),
        yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
        margin = list(l=10, r=10, t=10, b=30),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) |>
        plotly::config(displaylogo = FALSE, displayModeBar = TRUE)
    })

  }, ignoreInit = TRUE)

  # clique no segmento do gantt -> abre clip (callback no dashboard)
  observeEvent(plotly::event_data("plotly_click", source = "flow_gantt"), {
    ed <- plotly::event_data("plotly_click", source = "flow_gantt")
    if (is.null(ed) || is.null(ed$key)) return()

    sel <- rv_sel()
    if (is.null(sel)) return()

    ep <- episodes_data()
    if (is.null(ep) || !nrow(ep)) return()

    mlabel <- sel$mlabel
    eps <- ep |> dplyr::filter(as.character(OBJETO) == as.character(mlabel))
    if (!nrow(eps)) return()

    eps$start_time <- as.POSIXct(eps$start_time, tz = tzL)
    eps$end_time   <- as.POSIXct(eps$end_time,   tz = tzL)
    eps$ESTADO     <- .norm_state(eps$ESTADO)
    eps$row_id     <- seq_len(nrow(eps))

    rid <- as.integer(ed$key[1])
    linha <- eps[eps$row_id == rid, , drop = FALSE]
    if (!nrow(linha)) return()

    if (is.function(on_clip)) {
      on_clip(
        objeto = mlabel,
        setor  = sel$setor %||% NA_character_,
        start_time = linha$start_time[1],
        end_time   = linha$end_time[1],
        estado     = linha$ESTADO[1]
      )
    }
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
