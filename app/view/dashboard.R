box::use(
  shiny[...],
  shinyjs,
  ggplot2,
  plotly,
  jsonlite,
  dplyr[filter, arrange, mutate, select, across, everything, distinct, relocate, rename, bind_rows,tibble],
  tidyr[unnest_wider],
  lubridate,
  shinydashboardPlus[box],
  DBI,
  dbp  = ../infra/db_pool,
  ./global[ actionWebUser,tagAppendAttributesFind,debugLocal]
)


# ===============================
# Utilidades
# ===============================
# Flatten recursivo para ATRIBUTOS (JSON). Produz nomes do tipo "PRENSA.ESTADO".
.flatten_once <- function(x, prefix = NULL) {
  out <- list()
  if (is.list(x)) {
    nm <- names(x)
    if (is.null(nm)) nm <- seq_along(x)
    for (i in seq_along(x)) {
      key <- as.character(nm[i])
      pfx <- if (length(prefix)) paste0(prefix, ".", key) else key
      val <- x[[i]]
      if (is.list(val)) {
        out <- c(out, .flatten_once(val, pfx))
      } else {
        out[[pfx]] <- val
      }
    }
  } else {
    out[[prefix %||% "value"]] <- x
  }
  out
}

safe_parse_atributos <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(tibble::tibble())
  obj <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(obj)) return(tibble::tibble())
  flat <- .flatten_once(obj)
  tibble::as_tibble(flat)
}

# Constrói SQL base; aplica filtros opcionais.
build_sql <- function(minutos, setor = NULL, objeto = NULL) {
  # Usa COALESCE para suportar esquemas com oc.ATRIBUTOS OU oc.DATA_OC
  base <- paste("SELECT 
        oc.DATA_OC AS ATRIBUTOS,
        f.DT_HR_LOCAL AS DATE_TIME,
        o.NAME_OBJETO AS OBJETO,
        s.NAME_SETOR AS SETOR
        FROM objeto_contexto oc
        LEFT JOIN objeto o ON o.CD_ID_OBJETO = oc.CD_ID_OBJETO
        LEFT JOIN setor s ON s.CD_ID_SETOR = o.CD_ID_SETOR
        LEFT JOIN frame_camera f ON f.CD_ID_FRAME = oc.CD_ID_FRAME")
  base
}

# Executa consulta de forma segura
run_query <- function(pool, minutos) {
  sql <- build_sql(minutos, setor, objeto)

  DBI::dbGetQuery(pool, sql)
}

# Transforma tabela "longa" expandindo ATRIBUTOS -> colunas dinâmicas
expand_atributos <- function(df_raw) {
  if (!nrow(df_raw)) return(df_raw)
  # Parse linha a linha
  rows <- lapply(seq_len(nrow(df_raw)), function(i) {
    base   <- df_raw[i, c("DATE_TIME","OBJETO","SETOR"), drop = FALSE]
    extra  <- safe_parse_atributos(df_raw$ATRIBUTOS[i])
    if (!ncol(extra)) extra <- tibble::tibble()
    dplyr::bind_cols(base, extra)
  })
  out <- dplyr::bind_rows(rows)
  # Ordena por tempo asc p/ gráficos
  out <- out |>
    dplyr::mutate(DATE_TIME = as.POSIXct(DATE_TIME,Sys.timezone())) |>
    dplyr::arrange(DATE_TIME)
  out
}

# Extrai lista de métricas numéricas e categóricas com base nas colunas após expandir
split_metrics <- function(df) {
  base_cols <- c("DATE_TIME","OBJETO","SETOR")
  attr_cols <- setdiff(names(df), base_cols)
  # Detectar numéricas vs texto
  num_cols <- attr_cols[vapply(df[attr_cols], function(v) is.numeric(v) || is.integer(v), logical(1))]
  chr_cols <- attr_cols[vapply(df[attr_cols], function(v) is.character(v) || is.factor(v), logical(1))]
  list(numeric = num_cols, categorical = chr_cols)
}

# ===============================
# UI
# ===============================
#' @export
ui <- function(ns) {
 tagList(
  
  fluidRow(
    style = "margin-top: 50px;",
    column(3,
      shinydashboard::valueBox(
        width = '100%',
        value = span(paste0(
          10,
          ' ',
         10,'(s)'
        ),style = 'font-size: 25px;'),
        subtitle = 'Real Time',
        icon = icon('clock'),
        color = "light-blue"
      )  |> tagAppendAttributesFind(target = 1,style = 'min-height: 102px')
    ),
    column(3,
      shinydashboard::valueBox(
        width = '100%',
        value = span(paste0(
          10,
          ' ',
          10,'(s)'
        ),style = 'font-size: 25px;'),
        subtitle = 'Olhar para atrás',
        icon = icon('eye'),
        color = "orange"
      )  |> tagAppendAttributesFind(target = 1,style = 'min-height: 102px')
    ),
    column(3,
      shinydashboard::valueBox(
        width = '100%',
        value = uiOutput(paste0('notificacaoValueBox',1)),
        subtitle = paste0(
          'Nos ultimos ',20,
          ' ',
          20,'(s)'
        ),
        icon = icon('bell'),
        color = "red"
      ) |> tagAppendAttributesFind(target = 1,style = 'min-height: 102px')
    ),
    column(3,
      shinydashboard::valueBox(
        width = '100%',
        value = uiOutput(paste0('communicationValueBox',1)),
        subtitle = paste0(
          'Falha de conexão nos ultimos ',20,
          ' ',
          20,'(s)'
        ),
        icon = icon('satellite-dish'),
        color = "yellow"
      ) |> tagAppendAttributesFind(target = 1,style = 'min-height: 102px')
    ),
    column(12,uiOutput(ns("dashbody")))
  ),
  br()
 )
}

# ===============================
# Server
# ===============================
#' @export
server <- function(ns, input, output, session) {
  
  pool <- dbp$get_pool()
  
  df <- run_query(pool,minutos = 10000)
  df <- expand_atributos(df)
  
  output$dashbody <- renderUI({
    
    tagList(
      insertNewPlotComponent(ns,"Dobra e Solda",1),
      insertNewPlotComponent(ns,"Setrema",2)
    )
    
  })
  
  output[[paste0("plotout_",1)]] <- plotly$renderPlotly({
    
    df_plot <- bind_rows(
      tibble(MAQUINA = "SOLDA", ESTADO = df$`SOLDA.ESTADO`),
      tibble(MAQUINA = "DOBRA", ESTADO = df$`DOBRA.ESTADO`)
    ) |>
    filter(!is.na(ESTADO))
    
    ggplot2$ggplot(df_plot,ggplot2$aes(x = ESTADO, fill = MAQUINA)) +
    ggplot2$geom_bar(position = ggplot2$position_dodge(width = 0.8), width = 0.7) +
    ggplot2$labs(x = "ESTADO", y = "FREQ", fill = "Máquina")
    
  })
  
  output[[paste0("plotout_",2)]] <- plotly$renderPlotly({
    
    df_plot <- bind_rows(
      tibble(COMPONENTE = "BOBINA", ESTADO = df$`BOBINA.ESTADO`),
      tibble(COMPONENTE = "PRENSA", ESTADO = df$`PRENSA.ESTADO`)
    ) |>
    filter(!is.na(ESTADO))
    
    ggplot2$ggplot(df_plot,ggplot2$aes(x = ESTADO, fill = COMPONENTE)) +
    ggplot2$geom_bar(position = ggplot2$position_dodge(width = 0.8), width = 0.7) +
    ggplot2$labs(x = "ESTADO", y = "FREQ", fill = "Componente")
    
  })

}

insertNewPlotComponent <- function(ns,title,id){
  
  child         <- ns(paste0('plot_',id))
  boxid         <- ns(paste0('plotbox_',id))

  div(
    id = ns(paste0('child-',child)),
    box(
      id = boxid,
      solidHeader = T,
      collapsible = T,
      title = tags$span(title,style = 'font-size: 16px;'),
      width = 6,
      absolutePanel(
        height = 45,
        width = 'auto',
        top   = 5,
        right = 35,
        div(
          # actionButton(inputId = ns(paste0('btEraser',id)),
          #              label = '',
          #              icon = icon('eraser')
          # ),
          actionButton(inputId = ns(paste0('bteye',id)),
                       label = '',
                       icon = icon('window-maximize')
          )#,
          # shinyWidgets::dropdownButton(
          #   inputId = paste0('btgears', plot$CD_ID_PLOT),
          #   tags$h2("List of Input"),
          #   selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
          #   selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
          #   sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
          #   circle = FALSE,
          #   icon = icon("gear"),
          #   width = "300px",
          #   tooltip = tooltipOptions(title = "Configuração extras")) |>
          #   tagAppendAttributes( style = 'float: right; margin-left: 5px;') |>
          #   tagAppendAttributesFind(2,style = 'margin-left: -250px; border-color: gray;')
        )
      ), 
      div(style = 'padding: 15px; height: auto; width: 100%;',plotly$plotlyOutput(ns(paste0("plotout_",id))))
    ))
}

themasPlotyGGplot <- function(args = NULL,text.x.angle = 0){
  
  return(
    ggplot2::theme(args,
                   axis.text.x = ggplot2::element_text(angle = text.x.angle),
                   #legend.background = element_rect(fill = 'transparent'),
                   legend.title = ggplot2::element_blank(),
                   #legend.text  = element_text(colour = 'white'),
                   #axis.text = element_text(colour = 'white'),
                   #axis.title = element_text(colour = 'white',size = 10),
                   axis.line = ggplot2::element_line(colour = 'gray'),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype  = 'solid', colour = "lightgray")
    )
    
  )
}

layoutPloty <- function(x,legend.text = ''){
  
  return(plotly::layout(
    x,
    plot_bgcolor = 'transparent',
    paper_bgcolor = 'transparent',
    modebar = list(
      bgcolor = 'transparent',
      color = 'transparent',
      activecolor = 'transparent'
    ),
    showlegend = TRUE,
    legend  = list(title= list(text=legend.text,font = list(color = 'black',size = 12)),font = list(color = 'black',size = 10)),
    xaxis=list(fixedrange=TRUE),
    yaxis=list(fixedrange=TRUE)
  ) |>  htmlwidgets::onRender("function(el,x){
                               el.on('plotly_legendclick', function(){ return false; })
                               }"))
}

layoutPlotyDefault <- function(x,legend.text = ''){
  
  plotly::layout(
    x,
    plot_bgcolor = 'transparent',
    paper_bgcolor = 'transparent',
    showlegend = TRUE,
    modebar = list(
      bgcolor = 'transparent',
      color = 'lightgray',
      activecolor = 'darkgray'
    ),
    legend  = list(title= list(text=legend.text,font = list(color = 'black',size = 12)),font = list(color = 'black',size = 10))
  )
}

