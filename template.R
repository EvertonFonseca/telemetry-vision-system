# app.R
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(DT)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      tags$span("adminty", style="font-weight:600; letter-spacing:.5px;"),
      tags$small(" dashboard", style="opacity:.6;")
    ),
    tags$li(class="dropdown",
      tags$div(
        style="padding:8px 12px; width:320px;",
        textInput("top_search", NULL, placeholder = "Search or enter website name")
      )
    ),
    tags$li(class="dropdown",
      dropdownMenu(type="messages", badgeStatus = NULL, headerText = "Messages",
                   messageItem("John", "Hi, check report", time = "2m"))
    ),
    tags$li(class="dropdown",
      dropdownMenu(type="notifications", badgeStatus = NULL,
                   notificationItem("1 new task", icon = icon("tasks")))
    )
  ),

  dashboardSidebar(
    useShinyjs(),
    sidebarMenuOutput("sbMenu")
  ),

  dashboardBody(
    tags$head(
          # Fonte
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=""),
    tags$link(
      href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap",
      rel="stylesheet"),
      tags$style(HTML("
      :root{
  --bg:#f5f7fb;
  --card:#ffffff;
  --radius:14px;
  --shadow:0 8px 24px rgba(16,24,40,.08);
  --sidebar:#1b2434;           /* azul-escuro */
  --sidebarHover:#121a2a;
  --muted:#94a3b8;
  --text:#0f172a;
  --purple1:#7c3aed; --purple2:#6d28d9;
  --green1:#10b981;  --green2:#059669;
  --amber1:#f59e0b;  --amber2:#d97706;
  --cyan1:#06b6d4;   --cyan2:#0891b2;
}
html,body,.content-wrapper,.right-side{background:var(--bg) !important; font-family:'Inter', system-ui, -apple-system, Segoe UI, Roboto, 'Helvetica Neue', Arial, 'Noto Sans', 'Liberation Sans', sans-serif;}
/* Header branco com sombra */
.main-header .navbar{background:#fff !important; color:var(--text) !important; box-shadow:0 2px 12px rgba(16,24,40,.06);}
.main-header .logo{background:#fff !important; color:var(--text) !important; border-right:1px solid #e5e7eb;}
.navbar .form-control{border-radius:10px; border:1px solid #e5e7eb; background:#f8fafc; height:34px; box-shadow:none;}
.navbar-custom-menu>.navbar-nav>li>a{color:#64748b !important;}
/* Sidebar escuro */
.main-sidebar{background:var(--sidebar) !important;}
.skin-blue .sidebar a{color:#cbd5e1 !important;}
.skin-blue .sidebar-menu>li>a{border-left:3px solid transparent;}
.skin-blue .sidebar-menu>li.active>a,
.skin-blue .sidebar-menu>li:hover>a{background:var(--sidebarHover) !important; color:#fff !important; border-left-color:#60a5fa;}
.sidebar-menu .treeview-menu{background:rgba(255,255,255,.03) !important; padding:6px 0;}
.sidebar-menu .treeview-menu>li>a{color:#cbd5e1 !important; padding-left:30px;}
.sidebar-menu .treeview-menu>li>a:hover{color:#fff !important;}
.sidebar-menu .treeview-menu>li>a:before{content:'»'; margin-right:8px; color:#94a3b8;}
/* Ocultar tab neutra */
.sidebar-menu a[data-value='noop']{display:none !important;}
/* Badges mini (NEW/HOT) */
.badge-mini{font-size:10px; padding:2px 6px; border-radius:8px; margin-left:6px;}
.badge-new{background:#10b981; color:#fff;}
.badge-hot{background:#ef4444; color:#fff;}
/* Cards (box) */
.box{border:0 !important; border-radius:var(--radius); box-shadow:var(--shadow); background:var(--card);}
.box>.box-header{border-bottom:1px solid #eef2f7 !important; padding:14px 16px;}
.box .box-title{font-weight:600; color:#111827;}
.box .box-body{padding:16px 18px;}
/* ValueBoxes (small-box) com gradiente e ícone grande esmaecido */
.small-box{border-radius:16px; box-shadow:var(--shadow); overflow:hidden; position:relative;}
.small-box .icon{top:10px; right:12px; font-size:56px; color:rgba(255,255,255,.35);}
.small-box h3{font-weight:700;}
.small-box p{font-size:13px;}
.small-box p small{display:block; opacity:.9; font-weight:400;}
.small-box.bg-purple{background:linear-gradient(135deg,var(--purple1),var(--purple2)) !important;}
.small-box.bg-green{ background:linear-gradient(135deg,var(--green1), var(--green2)) !important;}
.small-box.bg-yellow{background:linear-gradient(135deg,var(--amber1), var(--amber2)) !important;}
.small-box.bg-aqua{  background:linear-gradient(135deg,var(--cyan1),  var(--cyan2)) !important;}
/* Botão laranja do relatório */
.btn-warning.btn-block{border:none; border-radius:10px; font-weight:600; box-shadow:0 6px 16px rgba(245,158,11,.35);}
.btn-warning.btn-block:hover{filter:brightness(.98);}
.btn:focus{outline:0 !important;}
/* DataTables mais clean */
table.dataTable{border-collapse:separate; border-spacing:0 6px;}
table.dataTable tbody tr{background:#fff; box-shadow:0 1px 0 rgba(0,0,0,.02);}
.dataTables_wrapper .dataTables_paginate .paginate_button{border:0 !important; background:transparent !important;}
/* Caixas auxiliares da coluna de risco */
.box .box{box-shadow:none; border:1px solid #eef2f7; border-radius:12px;}
      /* Sidebar look */
      .main-sidebar{background:#1f2937;}
      .skin-blue .sidebar a{color:#cdd6e0;}
      .skin-blue .sidebar-menu>li.active>a,
      .skin-blue .sidebar-menu>li:hover>a{background:#111827;color:#fff;}
      .sidebar-menu .treeview-menu>li>a{padding-left:30px;}
      .badge-mini{font-size:10px; padding:2px 6px; border-radius:8px; margin-left:6px;}
      .badge-new{background:#10b981; color:#fff;}
      .badge-hot{background:#ef4444; color:#fff;}

      /* Hide noop subItem */
      .sidebar-menu a[data-value='noop']{display:none !important;}

      /* ValueBox subtitle small text */
      .value-box small{display:block; opacity:.8; font-weight:400;}
    "))),

    # ---- TOP CARDS (valueBoxes)
    fluidRow(
      valueBoxOutput("vb_earn", 3),
      valueBoxOutput("vb_views", 3),
      valueBoxOutput("vb_tasks", 3),
      valueBoxOutput("vb_down", 3)
    ),

    # ---- ROW: Chart + Project Risk gauge
    fluidRow(
      box(
        title = tagList(icon("chart-line"), "Sales Analytics",
                        tags$small(style="margin-left:8px; opacity:.7;", "For details about usage, please refer amCharts license.")),
        width = 8, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        sliderInput("year_range", "Years", min = 1970, max = 2020, value = c(1990, 2015), step = 1, width = "100%"),
        plotlyOutput("sales_line", height = "300px")
      ),
      box(
        title = tagList(icon("shield-alt"), "Project Risk"),
        width = 4, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        div(style="display:flex; justify-content:center;",
            plotlyOutput("risk_gauge", height="220px", width="220px")),
        div(style="text-align:center; margin-top:-12px;",
            tags$strong("Balanced")),
        br(),
        fluidRow(
          column(6, box(title=NULL, width=12, solidHeader=TRUE,
                        div(style="text-align:center;",
                            tags$div("NR", style="opacity:.6;"), tags$h4("AWS 2455")))),
          column(6, box(title=NULL, width=12, solidHeader=TRUE,
                        div(style="text-align:center;",
                            tags$div("Created", style="opacity:.6;"), tags$h4("30th Sep"))))
        ),
        actionButton("download_report", "Download Overall Report", icon = icon("download"),
                     class = "btn-block btn-warning")
      )
    ),

    # ---- ROW: Application Sales + User Activity
    fluidRow(
      box(
        title = tagList(icon("table"), "Application Sales"),
        width = 8, status = "info", solidHeader = TRUE, collapsible = TRUE,
        DTOutput("sales_table")
      ),
      box(
        title = tagList(icon("users"), "User Activity"),
        width = 4, status = "success", solidHeader = TRUE, collapsible = TRUE,
        uiOutput("user_activity")
      )
    ),

    # ---- Tabs exigidos pelo menu (inclui noop)
    tabItems(
      tabItem(tabName = "noop",        div()),
      tabItem(tabName = "cameraNew",   div()),
      tabItem(tabName = "cameraTable", div())
    )
  ),
  skin = "blue"
)

server <- function(input, output, session){

  # ----- MENU DINÂMICO (com badges e item oculto 'noop')
  output$sbMenu <- renderMenu({
    sidebarMenu(id = "camera",
      menuItem("Dashboard", tabName = "noop", icon = icon("tachometer-alt")),
      menuItem("Camera", icon = icon("camera"),
        menuSubItem(HTML("Novo <span class='badge-mini badge-new'>NEW</span>"),  tabName = "cameraNew"),
        menuSubItem(HTML("Lista <span class='badge-mini badge-hot'>HOT</span>"), tabName = "cameraTable"),
        menuSubItem(text = HTML("&nbsp;"), tabName = "noop") # reset
      ),
      menuItem("Setor", icon = icon("folder-open")),
      menuItem("Objeto e Componente", icon = icon("cubes")),
      menuItem("Sistema", icon = icon("desktop")),
      menuItem("Usuário", icon = icon("user")),
      menuItem("Contatos", icon = icon("address-book"))
    )
  })

  # ----- VALUE BOXES
  output$vb_earn  <- renderValueBox(
    valueBox("$30200", HTML("All Earnings<small>update: 2:15 am</small>"),
             icon = icon("wallet"), color = "purple")
  )
  output$vb_views <- renderValueBox(
    valueBox("290+", HTML("Page Views<small>update: 2:15 am</small>"),
             icon = icon("eye"), color = "green")
  )
  output$vb_tasks <- renderValueBox(
    valueBox("145", HTML("Task Completed<small>update: 2:15 am</small>"),
             icon = icon("check-circle"), color = "yellow")
  )
  output$vb_down  <- renderValueBox(
    valueBox("500", HTML("Downloads<small>update: 2:15 am</small>"),
             icon = icon("download"), color = "aqua")
  )

  # ----- SALES LINE (plotly)
  sales_df <- reactive({
    set.seed(123)
    years <- 1970:2020
    y <- scales::rescale(sin(seq(0, 12, length.out = length(years))) + rnorm(length(years),0,.2),
                         to = c(0.2, 1))
    data.frame(year = years, sales = y)
  })

  output$sales_line <- renderPlotly({
    df <- subset(sales_df(), year >= input$year_range[1] & year <= input$year_range[2])
    plot_ly(df, x = ~year, y = ~sales, type = 'scatter', mode = 'lines+markers') %>%
      layout(yaxis = list(title = "Sales (idx)"), xaxis = list(title = "Year"),
             margin = list(l=40,r=20,b=40,t=20))
  })

  # ----- RISK GAUGE (plotly indicator)
  output$risk_gauge <- renderPlotly({
    risk <- 5  # 0..10
    plot_ly(
      type = "indicator", mode = "gauge+number",
      value = risk, number = list(suffix=""),
      gauge = list(axis = list(range = list(NULL, 10)))
    ) %>% layout(margin=list(l=15,r=15,t=0,b=0))
  })

  # ----- APPLICATION SALES TABLE (DT)
  output$sales_table <- renderDT({
    dat <- data.frame(
      Application = c("Able Pro", "Photoshop", "GuruAble", "Flatable"),
      Sales       = c(16300, 26421, 8265, 10652),
      Change      = c("+23%", "-3%", "+18%", "+2%"),
      AvgPrice    = c(53, 35, 20, 20),
      Total       = c("$16,652", "$18,785", "$9,652", "$7,856"),
      check.names = FALSE
    )
    datatable(dat, rownames = FALSE, options = list(dom="t", pageLength = 5))
  })

  # ----- USER ACTIVITY LIST
  output$user_activity <- renderUI({
    items <- list(
      list(name="John Doe", time="2 min ago", text="Lorem ipsum is simply dummy text..."),
      list(name="John Doe", time="5 min ago", text="Database backup completed."),
      list(name="John Doe", time="8 min ago", text="New product added! Congrats.")
    )
    tagList(lapply(items, function(it){
      div(style="display:flex; gap:10px; margin-bottom:12px;",
          div(style="width:36px; height:36px; background:#e5e7eb; border-radius:50%;
                     display:flex; align-items:center; justify-content:center;",
              icon("user", class="")),
          div(
            tags$b(it$name), tags$span(style="opacity:.6; margin-left:6px;", it$time), br(),
            tags$span(it$text)
          )
      )
    }))
  })

  # ----- MODAIS disparados pelos menuSubItem
  modalNovo <- function(){
    modalDialog(
      title = "Novo - Câmera",
      textInput("nome_cam", "Nome", placeholder = "Digite o nome"),
      textInput("url_cam",  "URL",  placeholder = "rtsp://192.168.1.100:554/Streaming"),
      selectInput("fps_cam","FPS",  choices = c(1,5,10), selected = 1),
      easyClose = TRUE, size = "l",
      footer = tagList(modalButton("Fechar"), actionButton("salvar_novo","Salvar"))
    )
  }
  modalLista <- function(){
    modalDialog(
      title = "Lista de Câmeras",
      DTOutput("tbl_cam_list"),
      easyClose = TRUE, size = "l",
      footer = modalButton("Fechar")
    )
  }
  output$tbl_cam_list <- renderDT({
    datatable(data.frame(
      Nome=c("Cam 01","Cam 02","Cam 03"),
      URL=c("rtsp://a","rtsp://b","rtsp://c"), FPS=c(5,10,1)
    ), rownames=FALSE, options=list(pageLength=5))
  })

  observeEvent(input$camera, ignoreInit = TRUE, {
    sel <- input$camera
    if (identical(sel, "cameraNew"))  showModal(modalNovo())
    if (identical(sel, "cameraTable")) showModal(modalLista())
    if (!is.null(sel) && sel %in% c("cameraNew","cameraTable"))
      updateTabItems(session, inputId = "camera", selected = "noop") # reset
  })

  # botão de download (dummy)
  observeEvent(input$download_report, {
    showModal(modalDialog("Relatório gerado (exemplo).", easyClose = TRUE))
  })
}

shinyApp(ui, server)
