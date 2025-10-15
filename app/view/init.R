box::use(
  shinydashboardPlus[...],
  shinydashboard[...],
  shiny[...],
  shinyjs[...],
  vov[...],
  #waiter[use_waiter],
  ../logic/time[format_br],
  ./setor,
  ./model[...],
  ./themes[...],
  ./global[...],
  ./objeto,
  ./plot,
  ./estrutura
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  sidebar <-  shinydashboardPlus::dashboardSidebar(
    width = 230,
    uiOutput(ns('menuSideBarMain'))
    #sidebarMenuOutput(ns("menuSideBarMain"))
  )
  sidebar$attribs$`data-collapsed` <- "true"

    shinydashboardPlus::dashboardPage(
      title = 'Telemetry Vision System',
      header = shinydashboardPlus::dashboardHeader(
        fixed = T,
        controlbarIcon  = "gears",
        title = tagList(
          span(class = "logo-lg",'Telemetry Vision'),
          img(src = "static/favicon.png",height = '20px',width = '20px')),
          uiOutput(ns('containerPlay'),class = 'dropdown-toggle',style = 'position: absolute; left: 50px; margin-top: 10px;'),
          uiOutput(ns('containerHeader')),
          dropdownMenuOutput(ns('dropsMensagem')),
          dropdownMenuOutput(ns('dropsAlertas'))
      ),
      sidebar = sidebar,
      body = dashboardBody(
        id = 'root',
        useShinyjs(),
        use_vov(),
        includeScript('app/js/swiper.min.js'),
        includeScript('app/js/main.js'),
        includeCSS('app/styles/config.css'),
        includeCSS('app/styles/swiper.min.css'),
        shinyDashboardThemes(theme = ""),
        tags$head(
          tags$meta(name="viewport", content="initial-scale=1, maximum-scale=1"),
          tags$link(rel = "shortcut icon", href = "favicon.png"),
          tags$script(HTML('
          
          function reportWindowSize() {
            Shiny.setInputValue("onResized",[window.innerWidth,window.innerHeight], {priority: "event"});
          }
  
          window.addEventListener("resize", reportWindowSize);
  
         /* $(function() {
           $(this).bind("contextmenu", function(e) {
            e.preventDefault();
            Shiny.setInputValue("onMouseRightClicked","", {priority: "event"});
            return false;
          });
       });*/ \n

      ')),
        tags$style(
            HTML(" 
            html {overflow: hidden;}
            .selectize-dropdown { z-index: 2147483647 !important; }
            body{
                 font-family: 'Segoe UI', serif;
            }
            .shiny-split-layout > div {overflow: visible;}
           
            .form-control[disabled], .form-control[readonly], fieldset[disabled] .form-control {
              background-color: white;
              color: #696767;
              opacity: 1;
            }
            
            .selectize-control.single .selectize-input:after {
               right: 5px!important;
            }
           
           .main-header .navbar-custom-menu, .main-header .navbar-right {

               margin-right: 10px!important;;
           }
        
            .tabbable > .nav > li > a {color: gray;}
            
            .tabbable > .nav > li > a:hover{color: gray;}
            
            .skin-blue .main-header .navbar .sidebar-toggle{
        
               color: white;
            }
            
            //.tab-content{margin-top: -30px !important;}
            
            .skin-blue .main-header .navbar .sidebar-toggle:hover{
        
               color: gray;
             }
              .box-header {
                  margin-left: 25px;
              }
              
              .recalculating {
               opacity: 1!important;
               }
              .js-plotly-plot .plotly .modebar {
                left: 2px;
               }
             /*.shiny-notification {
               position:fixed;
               top: 10px;
               right: 5px;
             }*/
             .main-footer{
              color: gray;
              border-top-style: solid;
              background-color: white;
             }
             .skin-blue .sidebar-menu > li > .treeview-menu {
              background: rgb(52,62,72)!important;
             }" )
          )
        ),
        # Set up shinyjs
        tags$head(tags$title("Telemetry Vision System")),
        tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
        uiOutput(ns('painelTabSetores')),
        # Áudio (em módulo, use ns(...))
        tagList(
          tags$audio(id = "soundNotificationOn",
                    src = "static/sounds/bell_warning.mp3",
                    type = "audio/mp3", preload = "auto", style = "display:none;"),
          tags$audio(id =  "soundLoginOn",
                    src = "static/sounds/login_on.wav",
                    type = "audio/wav", preload = "auto", style = "display:none;"),
          tags$audio(id =  "soundSessionDialog",
                    src = "static/sounds/dialog_open.mp3",
                    type = "audio/mp3", preload = "auto", style = "display:none;"),
          tags$audio(id =  "soundBoxMessage",
                    src = "static/sounds/letterbox.mp3",
                    type = "audio/mp3", preload = "auto", style = "display:none;")
        ),
        br(),
         tags$head(tags$style(
          ".sidebar-menu a[data-value='noop'] { display:none !important; }"
        ))
      ), 
      footer = dashboardFooter(
        left  = "Version 1.0.0",
        right = tags$i(style = 'color: gray; font-size: 12px;',paste0("Copyright © 2020-",format(Sys.Date(),'%Y')," Big Brain, All Rights Reserved."))
      )
    )  #|> tagAppendAttributes(class = 'sidebar-collapse')
}

#' @export
server <- function(id) {

  moduleServer(id, function(input, output, session) {

    reactiveNotification         <- reactiveVal(NULL)
    reactiveMessageUsers         <- reactiveVal(NULL)
    inputDropsNotification       <- reactiveVal(list())
    inputDropsMessage            <- reactiveVal(list())
    inputListNotificationNotRead <- list()
    inputListMessageNotRead      <- list()
    timeReactive                 <- reactiveTimer(60000)
    queeNotification             <- NULL
    queeMessageUsers             <- NULL
    ns <- NS(id)
    output$menuSideBarMain <- renderMenuSideBarMain(ns)

    # ---- Obsevent Menu Camera
      observeEvent(input$camera, {

        box::use(./camera,)
        sel <- input$camera

        if (identical(sel, "cameraNew")) {
          camera$uiNewCamera(ns,input,output,session,function(){
            box::unload(camera)
            gc()
          })
        } else if (identical(sel, "cameraTable")) {
          camera$uiEditCamera(ns,input,output,session,function(){
            box::unload(camera)
            gc()
          })
        }
        # reset da seleção para permitir clicar no mesmo item novamente
        if (!is.null(sel) && sel %in% c("cameraNew", "cameraTable"))
            updateTabItems(session, inputId = "camera", selected = "noop")
      },ignoreInit = TRUE,ignoreNULL = TRUE)

      # ---- Obsevent Menu Setor
      observeEvent(input$camera, {
        sel <- input$setor

        if (identical(sel, "setorNew")) {
          setor$uiNewSetor(ns,input,output,session)
        } else if (identical(sel, "setorTable")) {
          setor$uiEditSetor(ns,input,output,session)
        }
        # reset da seleção para permitir clicar no mesmo item novamente
        if (!is.null(sel) && sel %in% c("setorNew", "setorTable"))
            updateTabItems(session, inputId = "setor", selected = "noop")
      },ignoreInit = TRUE,ignoreNULL = TRUE)
      
      # ---- Obsevent Menu Objeto
      observeEvent(input$objeto, {

        sel <- input$objeto

        if (identical(sel, "objetoNew")) {
          objeto$uiNewObjeto(ns,input,output,session)
        } else if (identical(sel, "objetoTable")) {
          objeto$uiEditObjeto(ns,input,output,session)
        }
        # reset da seleção para permitir clicar no mesmo item novamente
        if (!is.null(sel) && sel %in% c("objetoNew", "objetoTable"))
            updateTabItems(session, inputId = "objeto", selected = "noop")
        },ignoreInit = TRUE,ignoreNULL = TRUE)

              # ---- Obsevent Menu Objeto
      observeEvent(input$estrutura, {

        sel <- input$estrutura
    
        if (identical(sel, "estruturaNew")) {
          estrutura$uiNewEstrutura(ns,input,output,session)
        } else if (identical(sel, "estruturaTable")) {
          estrutura$uiEditEstrutura(ns,input,output,session)
        }
        # reset da seleção para permitir clicar no mesmo item novamente
        if (!is.null(sel) && sel %in% c("estruturaNew", "estruturaTable"))
            updateTabItems(session, inputId = "objeto", selected = "noop")
        },ignoreInit = TRUE,ignoreNULL = TRUE)

      # ---- Obsevent Menu Plot
      observeEvent(input$plot, {

        sel <- input$plot

        if (identical(sel, "plotNew")) {
          plot$uiNewPlot(ns,input,output,session)
        } else if (identical(sel, "plotTable")) {
          plot$uiEditPlot(ns,input,output,session)
        }
        # reset da seleção para permitir clicar no mesmo item novamente
        if (!is.null(sel) && sel %in% c("plotNew", "plotTable"))
           updateTabItems(session, inputId = "plot", selected = "noop")
        },ignoreInit = TRUE,ignoreNULL = TRUE)
    
  componentHeader <- function(input,output,textoInformacao,size.right = 50 * 2) {
  
  uiHeader <-   renderUI({
    
    div(style = paste0('position: absolute;
                        width: auto;
                        height: 25px;
                        right: ',size.right,'px;
                        top: 25%;'),
        tags$span(style='float: left; color: white; font-size: 15px;',textoInformacao())
    )
  })
  
  return(uiHeader)
}

output$containerHeader <- componentHeader(input,output,function(){
  
  timeReactive()
  span(format(Sys.time(),"%d/%m/%Y %H:%M"))
  
})

output$dropsMensagem <- renderMenu({
  
  dropsNotification <- inputDropsMessage()
  n <- length(dropsNotification)
  
  if(n > 0){
    #play sound message
    runjs("document.getElementById('soundBoxMessage').play();")
  }
  
   dropdowncomponent <- dropdownMenu(
                          type = "messages",
                          badgeStatus = "danger",
                          headerText =  paste0('Você tem ',n,' ',{
                            if(n == 0 || n == 1){
                              'mensagem.'
                            }else{
                              'mensagens'
                            }
                          }),
                          .list = dropsNotification
                        )
    if(dropdowncomponent$children[[1]]$children[[2]]$children[[1]] > 99){
      dropdowncomponent$children[[1]]$children[[2]]$children[[1]] <- paste0('+99')
    }
    
    dropdowncomponent
  })

    output$dropsAlertas <- renderMenu({
      
      dropsNotification <- inputDropsNotification()
      n <- length(dropsNotification)
      
      if(n > 0){
        #play sound notification
        runjs("document.getElementById('soundNotificationOn').play();")
      }

      dropdowncomponent <- dropdownMenu(
                            type = "notifications",
                            icon = icon('bell'),
                            badgeStatus = "danger",
                            headerText = paste0('Você tem ',n,' ',{
                              if(n == 0 || n == 1){
                                'notificação.'
                              }else{
                                'notificações'
                              }
                            }),
                            .list = dropsNotification
                          )
        
        
      if(dropdowncomponent$children[[1]]$children[[2]]$children[[1]] > 99){
        dropdowncomponent$children[[1]]$children[[2]]$children[[1]] <- paste0('+99')
      }

      dropdowncomponent
      
    })


  })
  
}

renderMenuSideBarMain <- function(ns){

    # ---- MENUS ------
    renderUI({

      tagList(
        sidebarMenu(id = ns("camera"),
          menuItem("Camera", icon = icon("camera"),
            menuSubItem("Nova",  tabName = "cameraNew"),
            menuSubItem("Lista", tabName = "cameraTable"),
            # subItem oculto para resetar seleção
            menuSubItem(text = htmltools::HTML("&nbsp;"), tabName = "noop",selected = TRUE)
          )
        ),
        sidebarMenu(id = ns("setor"),
          menuItem("Setor", icon = icon("industry"),
            menuSubItem("Novo",  tabName = "setorNew"),
            menuSubItem("Lista", tabName = "setorTable"),
            menuSubItem("Agenda", tabName = "setorAgenda"),
            # subItem oculto para resetar seleção
            menuSubItem(text = htmltools::HTML("&nbsp;"), tabName = "noop",selected = TRUE)
          )
        ),
        sidebarMenu(id = ns("objeto"),
          menuItem("Objeto", icon = icon("cube"),
            menuSubItem("Novo",  tabName = "objetoNew"),
            menuSubItem("Lista", tabName = "objetoTable"),
            # subItem oculto para resetar seleção
            menuSubItem(text = htmltools::HTML("&nbsp;"), tabName = "noop",selected = TRUE)
          )
        ),
        sidebarMenu(id = ns("estrutura"),
          menuItem("Estrutura", icon = icon("puzzle-piece"),
            menuSubItem("Novo",  tabName = "estruturaNew"),
            menuSubItem("Lista", tabName = "estruturaTable"),
            # subItem oculto para resetar seleção
            menuSubItem(text = htmltools::HTML("&nbsp;"), tabName = "noop",selected = TRUE)
          )
        ),
         sidebarMenu(id = ns("plot"),
          menuItem("Grafico", icon = icon("chart-line"),
            menuSubItem("Novo",  tabName = "plotNew"),
            menuSubItem("Lista", tabName = "plotTable"),
            # subItem oculto para resetar seleção
            menuSubItem(text = htmltools::HTML("&nbsp;"), tabName = "noop",selected = TRUE)
          )
        )
      )

    })
}