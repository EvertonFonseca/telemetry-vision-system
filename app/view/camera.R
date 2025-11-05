box::use(
  shinyjs[inlineCSS],
  shiny[
    NS,
    moduleServer,
    actionButton,
    tagList,
    showModal,
    div,
    textInput,
    selectizeInput,
    br,
    fluidRow,
    column,
    observeEvent,
    removeModal,
    uiOutput,
    renderUI,
    reactiveVal,
    showNotification,
    isolate,
    updateTextInput,
    updateSelectInput,
    getDefaultReactiveDomain,
    icon,
    textOutput,
    renderText,
    req
  ],
  stringi,
  . / model[...],
  . /
    global[
      dialogTitleClose,
      panelTitle,
      removeModalClear,
      newObserve,
      shinySetInputValue,
      play_sound,
      debugLocal,
      console,
      messageAlerta
    ],
  .. / model / swiper[...],
  DT,
  shinycssloaders,
  .. / logic / camera_dao[...],
  stringr,
  dplyr[...],
  lubridate[...],
  dbp  = ../infra/db_pool,
  db   = ../infra/database
)

#' @export
 uiNewCamera <- function(ns,input,output,session,callback){

  obs <- newObserve()
 
  showModal(
      session = session,
      dialogModal(
      title = 'Nova Câmera',
      size  = 'm',
      uiMain(ns),
      footer = tagList(actionButton(inputId = ns("btSair"),"Sair",icon = icon("arrow-left")),
                       actionButton(ns('btClear'), "Limpar", icon = icon("eraser")),
                       actionButton(ns('btSalvar'),'Salvar',class = "btn-primary",icon = icon("save"))
                      )))
   # Sair 
   obs$add(observeEvent(input$btSair,{
        obs$destroy()
        removeModal(session)
        callback()
   },ignoreInit = T,ignoreNULL = T))
   
   ## Clear
   obs$add(observeEvent(input$btClear, {
     updateTextInput(session,'textNameCamera', value = '')
     updateTextInput(session,'textUrlCamera',  value = '')
     updateSelectInput(session,'comboFps',selected = 5)
  }, ignoreInit = TRUE))

   ## Salvar Camera
   obs$add(observeEvent(input$btSalvar,{
 
   db$tryTransaction(function(conn){
     
      nomeCamera <- isolate(toupper(input$textNameCamera))
      urlCamera  <- isolate(input$textUrlCamera)
 
      if(stringi$stri_isempty(stringr$str_trim(nomeCamera))){
        showNotification("O nome do Câmera não foi preenchido!", type = "warning")
        return()
      }
      if(checkifExistNameCamera(conn,name = nomeCamera)){
        showNotification("O nome do Câmera já possui nos registros!", type = "warning")
        return()
      }
      if(stringi$stri_isempty(stringr$str_trim(urlCamera))){
        showNotification("O url da Câmera não foi preenchido!", type = "warning")
        return()
      }
      if(checkifExistUrlCamera(conn,urlCamera)){
        showNotification("O url da Câmera já possui nos registros!", type = "warning")
        return()
      }

      obj <- list()
      obj$NAME_CAMERA   <- nomeCamera
      obj$URL_CAMERA    <- urlCamera
      obj$FPS_CAMERA    <- isolate(input$comboFps)
      id                <- db$nextSequenciaID(conn,'CAMERA_VIEW')
      obj$CD_ID_CAMERA  <- insertNewCamera(conn,id,obj)
     
      dialogConfirm(
        session = session,
        id    = ns('dialogConfirm'),
        title = 'Câmera criado com sucesso!',
        text  = 'Deseja criar novamente um novo Câmera?')
      
      #crie so uma vez
      observeEvent(input$dialogConfirm,{
        
        status <- input$dialogConfirm

        # Limpar os campos APÓS o flush/render — garante que os inputs existam no DOM
        session$onFlushed(function() {
          updateTextInput(session,'textNameCamera', value = '')
          updateTextInput(session,'textUrlCamera',  value = '')
          updateSelectInput(session,'comboFps',selected = 5)
        }, once = TRUE)
  
        if(!status){
            obs$destroy()
            removeModal(session)
            callback()
        }
        
      },ignoreInit = TRUE,once = TRUE)

   })

   },ignoreInit = T,ignoreNULL = T))
   
 }
 
#' @export
uiEditCamera <- function(ns,input,output,session,callback = NULL){

  #open database
  obs <- newObserve()
  sliderPosition <- reactiveVal(1L)
  idSwiper       <- ns('swiperMain')

  cameras        <- reactiveVal(selectAllCameras(dbp$get_pool()))
  camera         <- reactiveVal(NULL)

  showModal(
    session = session,
    dialogModal(
      title = textOutput(ns("titleTexto")),
      size = 'm',
      swiper(id = idSwiper,width = '100%',height = '350px',
            swiperSlide(
              style = 'height: 100%; width: 100%; overflow-y: hidden; padding: 5px;',
              uiOutput(ns('slider1')) |> shinycssloaders$withSpinner(color = 'lightblue')
            ),
            swiperSlide(
              uiOutput(ns('slider2')) |> shinycssloaders$withSpinner(color = 'lightblue')
            )
      ),  
      footer = uiOutput(ns('uiFooter'))))
    
    output$uiFooter <- renderUI({
      
      current <- sliderPosition()
      
      if(current == 1){
        tagList(actionButton(ns("btSair"), label = "Sair",icon = icon("arrow-left")))
      }
      else{
        tagList(actionButton(ns("btSair"), label = "Voltar",icon = icon("arrow-left")),
        actionButton(ns('btActionUpdate'),class = "btn-warning",label = "Atualizar",icon = icon("save")))
      }
      
    })
  
    output$titleTexto <- renderText({
      
      if(sliderPosition() == 1L){
        'Registros Câmeras'
      }else{
        'Edição da câmera'
      }

    })
  
    output$slider1 <- renderUI({
      
      output$tableDinamicaCamera <- DT$renderDataTable({
     
        dataset  <- cameras()

        if(length(dataset) == 0) return(NULL)
        
        colunaNames <- c('LINHA','CAMERÂ',"URL",'VISUALIZAR / EDITAR','REMOVER')
      
        DT$datatable({
          
          dataset |> 
            mutate_if(is.POSIXct,function(x){ format(x,'%d/%m/%Y %H:%M:%S')})  |> 
            mutate_if(is.Date,function(x){ format(x,'%d/%m/%Y')}) |> 
            mutate_if(is.character,toupper) |> 
            mutate(
                  !!colunaNames[1] := 1:nrow(dataset),
                  !!colunaNames[2] :=  dataset$NAME_CAMERA,
                  !!colunaNames[3] :=  paste0(substr(dataset$URL_CAMERA, 1,10), "..."),
                  !!colunaNames[4] :=  sapply(dataset$CD_ID_CAMERA, function (x) {
                    
                   as.character(
                      actionButton(
                        paste0(ns('btEdit')),
                        label = '',
                        icon = icon('eye'),
                        onclick = paste0('Shiny.setInputValue(\"',ns("editPressedRow"),'\","',x,'",{priority: "event"})'),
                        #style = 'background-color: transparent; color: lightblue; border-solid: none;'
                      )
                    )
                  }),
                  !!colunaNames[5] :=  sapply(dataset$CD_ID_CAMERA, function (x) {
                    
                   as.character(
                      actionButton(
                        paste0(ns('btRemove')),
                        label = '',
                        icon = icon('trash'),
                        onclick = paste0('Shiny.setInputValue(\"',ns("deletePressedRow"),'\","',x,'",{priority: "event"})'),
                        #style = 'background-color: transparent; color: lightblue; border-solid: none;'
                      )
                    )
                  })
                  
            ) |> select(colunaNames) |> arrange(colunaNames[2])
        },  
        class = 'cell-border stripe',
        extensions = 'Scroller',
        options = list(
          language = list(url = 'js/table.json'),
          dom = 't',
          bSort=FALSE,
          columnDefs = list(list(visible=FALSE, targets=c(0)),list(className = 'dt-center', targets = "_all"),list(width = '75px',targets = c(1)),list(width = 'autos',targets = c(3))),
          deferRender = TRUE,
          scroller = FALSE,
          fixedHeader = TRUE,
          scrollX = TRUE,
          scrollY = '280px'
        ),
        escape = F,
        selection = 'none',
        ) |> DT$formatStyle(colunaNames, cursor = 'pointer')
        
      })
      
      div(
        style = 'border-style: solid; border-color: white; border-width: 1px; overflow-x: auto;',
        DT$dataTableOutput(outputId = ns('tableDinamicaCamera'))
      )
      
    })
  
  output$slider2 <- renderUI({

    req(camera())
    cameraSelect <- camera()

     uiMain(ns,
            valueName = cameraSelect$NAME_CAMERA,
            valueUrl = cameraSelect$URL_CAMERA,
            valueFps = cameraSelect$FPS_CAMERA
          )
  })

  obs$add(observeEvent(input$editPressedRow,{
    
    camera(isolate(cameras()) %>% filter(CD_ID_CAMERA == input$editPressedRow))

    swiperSlideNext(idSwiper)
    sliderPosition(isolate(sliderPosition()) + 1L)
    
  },ignoreInit = T))
  
  obs$add(observeEvent(input$deletePressedRow,{
    
    camera <- isolate(cameras()) |> filter(CD_ID_CAMERA == input$deletePressedRow)

    messageAlerta(
                  input,
                  ns,
                  title   = paste0('Todos os objetos ligado a esse câmera será excluido'),
                  message = paste0('Deseja realmente excluir a câmera ',camera$NAME_CAMERA,"?"),
                  callback.no = function(){
                    
                  },
                  callback.yes = function(){
                    
                    db$tryTransaction(function(conn){
                      
                      deleteCamera(conn,camera$CD_ID_CAMERA)
                      cameras.aux <- selectAllCameras(conn)
                      if(nrow(cameras.aux) == 0){
                        #destroy all observe events
                        obs$destroy()
                        removeModal(session)
                        swiperDestroy(idSwiper)
                      }else{
                        cameras(cameras.aux)
                      }
                      
                    })

                  })
    
  },ignoreInit = T))
  

  obs$add(observeEvent(input$btSair,{
    
    current <- isolate(sliderPosition())

    if(current == 1){
      #destroy all observe events
      obs$destroy()
      swiperDestroy(idSwiper)
      removeModal(session)
    }
    else{
      camera(NULL)
      swiperSlidePrevious(idSwiper)
      sliderPosition(current - 1L)
    }
    
  },ignoreInit = T))
  
  obs$add(observeEvent(input$btActionUpdate,{
    
    req(camera())
    
    db$tryTransaction(function(conn){
      
      id         <- isolate(camera()$CD_ID_CAMERA)
      nomeCamera <- isolate(toupper(input$textNameCamera))
      urlCamera  <- isolate(input$textUrlCamera)

      if(stringi$stri_isempty(stringr$str_trim(nomeCamera))){
        showNotification("O nome do Câmera não foi preenchido!", type = "warning")
        return()
      }
      
      if(checkifExistNameCameraEdit(conn,id,name = nomeCamera)){
        showNotification("O nome do Câmera já possui nos registros!", type = "warning")
      }
      if(stringi$stri_isempty(stringr$str_trim(urlCamera))){
        showNotification("O url da Câmera não foi preenchido!", type = "warning")
        return()
      }
      if(checkifExistUrlCameraEdit(conn,id,urlCamera)){
        showNotification("O url da Câmera já possui nos registros!", type = "warning")
        return()
      }
      #check if it has already data of Câmera
      obj <- list()
      obj$CD_ID_CAMERA  <- id
      obj$NAME_CAMERA   <- nomeCamera
      obj$URL_CAMERA    <- urlCamera
      obj$FPS_CAMERA    <- isolate(input$comboFps)

      updateCamera(conn,obj)
      #load todos os setores
      cameras(selectAllCameras(conn))
  
      swiperSlidePrevious(idSwiper)
      sliderPosition(isolate(sliderPosition()) - 1L)
      showNotification("câmera atualizado com sucesso!", type = "warning")
    })
    
  },ignoreInit = T))
  
}

uiMain <- function(ns,valueName = NULL,valueUrl = NULL,valueFps = 5){

  div(
        inlineCSS(paste0("#",ns("textNameCamera")," {text-transform: uppercase;}")),
        textInput(paste0(ns('textNameCamera')),label = 'Nome',placeholder = 'Digite o nome para o Câmera',value = valueName),
        br(),
        panelTitle(title = "Configuração",
                   background.color.title = 'white',
                   title.color  = 'black',
                   border.color = 'lightgray',
                   children = fluidRow(
                     style = 'padding-top: 10px; padding-left: 15px; padding-right: 15px;',
                     column(12,textInput(ns('textUrlCamera'),label = 'Url',placeholder = 'rtsp://...',value = valueUrl,width = "95%")),
                     column(12,selectizeInput(ns('comboFps'),label = 'Frame por segundos',choices = c(1,5,15,30),selected = valueFps))
                   )
        )
      )

}