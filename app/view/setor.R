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
    selectInput,
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
  .. / logic / setor_dao[...],
  stringr,
  dplyr[...],
  lubridate[...],
  dbp  = ../infra/db_pool,
  db   = ../infra/database
)

# ------------------------------------------------------------
# Estado por sessão (NUNCA global)
# ------------------------------------------------------------
.get_private <- function(session, key = "setor_private") {
  stopifnot(!is.null(session))
  if (is.null(session$userData[[key]])) {
    session$userData[[key]] <- new.env(parent = emptyenv())
  }
  session$userData[[key]]
}

# Limpa somente o estado desse módulo nessa sessão
#' @export
dispose <- function(session, key = "setor_private") {
  e <- session$userData[[key]]
  if (!is.null(e)) {
    rm(list = ls(envir = e, all.names = TRUE), envir = e)
  }
  session$userData[[key]] <- NULL
  invisible(gc())
}

# (Opcional) registra limpeza automática quando o usuário fechar a aba/navegador
.register_auto_dispose <- function(session, key = "setor_private") {
  # evita registrar múltiplas vezes
  flag <- paste0(key, "_onend_registered")
  if (isTRUE(session$userData[[flag]])) return(invisible(NULL))
  session$userData[[flag]] <- TRUE

  session$onSessionEnded(function() {
    # tenta limpar sem quebrar nada
    try(dispose(session, key), silent = TRUE)
  })

  invisible(NULL)
}


#' @export
 uiNewSetor <- function(ns,input,output,session,callback){

  .register_auto_dispose(session)

  e <- .get_private(session)

  obs <- newObserve()
 
  showModal(
      session = session,
      dialogModal(
      title = 'Novo Setor',
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
        updateTextInput(session,'textNameSetor', value = '')
        updateSelectInput(session,"comboTimer",selected = 1)
        updateSelectInput(session,"comboUnit",selected = "Minuto")
        updateSelectInput(session,"comboTimerLook",selected = 10)
        updateSelectInput(session,"comboUnitLook",selected = "Minuto")
   }, ignoreInit = TRUE))

   ## Salvar Setor
   obs$add(observeEvent(input$btSalvar,{
    
    nomeSetor <- isolate(toupper(input$textNameSetor))

    #open database
    db$tryTransaction(function(conn){

      if(stringi$stri_isempty(stringr$str_trim(nomeSetor))){
        showNotification("O nome do Setor não foi preenchido!", type = "warning")
        return()
      }
      
      if(checkifExistNameSetor(conn,name = nomeSetor)){
        showNotification("O nome do Setor já possui nos registros!", type = "warning")
      }

      #check if it has already data of Setor
      obj <- list()
      obj$NAME_SETOR                   <- nomeSetor
      obj$TEMPO_REATIVAR_SETOR         <- isolate(input$comboTimer)
      obj$TEMPO_REATIVAR_UNIDADE_SETOR <- isolate(input$comboUnit)
      obj$TEMPO_PASSADO_SETOR          <- isolate(input$comboTimerLook)
      obj$TEMPO_PASSADO_UNIDADE_SETOR  <- isolate(input$comboUnitLook)
      id                               <- db$nextSequenciaID(conn,'SETOR')
      
      insertNewSetor(conn,id,obj)

      dialogConfirm(
        session = session,
        id    = ns('dialogConfirm'),
        title = 'Setor criado com sucesso!',
        text  = 'Deseja criar novamente um novo Setor?')
      
      #crie so uma vez
      observeEvent(input$dialogConfirm,{
        
        status <- input$dialogConfirm

        # Limpar os campos APÓS o flush/render — garante que os inputs existam no DOM
        session$onFlushed(function() {
          updateTextInput(session,'textNameSetor', value = '')
          updateSelectInput(session,"comboTimer",selected = 1)
          updateSelectInput(session,"comboUnit",selected = "Minuto")
          updateSelectInput(session,"comboTimerLook",selected = 10)
          updateSelectInput(session,"comboUnitLook",selected = "Minuto")
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
uiEditSetor <- function(ns,input,output,session,callback){
  
    .register_auto_dispose(session)
    
    e <- .get_private(session)

    obs            <- newObserve()
    sliderPosition <- reactiveVal(1L)
    idSwiper       <- ns('swiperMain')
    setores        <- reactiveVal(selectAllSetors(dbp$get_pool()))
    setor          <- reactiveVal(NULL)

    showModal(
      session = session,
      dialogModal(
        title = textOutput(ns("titleTexto")),
        size = 'm',
        swiper(id = idSwiper,width = '100%',height = '350px',
              swiperSlide(
                style = 'height: 100%; width: 100%; overflow-y: hidden; padding: 5px;',
                uiOutput(ns('slider1'))
              ),
              swiperSlide(
                style = 'height: 100%; width: 100%; overflow-y: auto; overflow-x: hidden; padding: 5px;',
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
          'Registros Setor'
        }else{
          'Edição o setor'
        }

      })
    
      output$slider1 <- renderUI({
        
        output$tableDinamicaSetor <- DT$renderDataTable({
      
          dataset  <- setores()

          if(length(dataset) == 0) return(NULL)
          
          colunaNames <- c('LINHA','SETOR','VISUALIZAR / EDITAR','REMOVER')
        
          DT$datatable({
            
            dataset |> 
              mutate_if(is.POSIXct,function(x){ format(x,'%d/%m/%Y %H:%M:%S')})  |> 
              mutate_if(is.Date,function(x){ format(x,'%d/%m/%Y')}) |> 
              mutate_if(is.character,toupper) |> 
              mutate(
                    !!colunaNames[1] := 1:nrow(dataset),
                    !!colunaNames[2] :=  dataset$NAME_SETOR,
                    !!colunaNames[3] :=  sapply(dataset$CD_ID_SETOR, function (x) {
                      
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
                    !!colunaNames[4] :=  sapply(dataset$CD_ID_SETOR, function (x) {
                      
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
            columnDefs = list(list(visible=FALSE, targets=c(0)),list(className = 'dt-center', targets = "_all"),list(width = '75px',targets = c(1,4)),list(width = 'autos',targets = c(3))),
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
          DT$dataTableOutput(outputId = ns('tableDinamicaSetor'))
        )
        
      })
    
    output$slider2 <- renderUI({

      req(setor())
      
      setorSelect <- setor()
        
         uiMain(ns,
           valueName = setorSelect$NAME_SETOR,
           valueComboTimer = setorSelect$TEMPO_REATIVAR_SETOR,
           valueComboUnit =  setorSelect$TEMPO_REATIVAR_UNIDADE_SETOR,
           valueComboTimerLook = setorSelect$TEMPO_PASSADO_SETOR,
           valueComboUnitLook = setorSelect$TEMPO_PASSADO_UNIDADE_SETOR
          )
    })

    obs$add(observeEvent(input$editPressedRow,{
      
      setor(isolate(setores()) |> filter(CD_ID_SETOR == input$editPressedRow))
      
      swiperSlideNext(idSwiper)
      sliderPosition(isolate(sliderPosition()) + 1L)
      
    },ignoreInit = T))
    
    obs$add(observeEvent(input$deletePressedRow,{
      
      setor <- isolate(setores()) |> filter(CD_ID_SETOR == input$deletePressedRow)

      messageAlerta(
                    input,
                    ns,
                    title   = paste0('Todos os objetos ligado a esse setor será excluido'),
                    message = paste0('Deseja realmente excluir a setor ',setor$NAME_SETOR,"?"),
                    callback.no = function(){
                      
                    },
                    callback.yes = function(){
                      
                       db$tryTransaction(function(conn){
                        
                        deleteSetor(conn,setor$CD_ID_SETOR)
                        setores.aux <- selectAllSetors(conn)
                        if(nrow(setores.aux) == 0){
                          #destroy all observe events
                          obs$destroy()
                          removeModal(session)
                          swiperDestroy(idSwiper)
                          callback()
                        }else{
                          setores(setores.aux)
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
        callback()
      }
      else{
        setor(NULL)
        swiperSlidePrevious(idSwiper)
        sliderPosition(current - 1L)
      }
      
    },ignoreInit = T))
    
    obs$add(observeEvent(input$btActionUpdate,{
      
      req(setor())
      
      db$tryTransaction(function(conn){

        id        <- isolate(setor()$CD_ID_SETOR)
        nomeSetor <- isolate(toupper(input$textNameSetor))

        if(stringi$stri_isempty(stringr$str_trim(nomeSetor))){
          showNotification("O nome do Setor não foi preenchido!", type = "warning")
          return()
        }
        
        if(checkifExistNameSetorEdit(conn,id,name = nomeSetor)){
          showNotification("O nome do Setor já possui nos registros!", type = "warning")
        }

        #check if it has already data of Setor
        obj <- list()
        obj$CD_ID_SETOR                  <- id
        obj$NAME_SETOR                   <- nomeSetor
        obj$TEMPO_REATIVAR_SETOR         <- isolate(input$comboTimer)
        obj$TEMPO_REATIVAR_UNIDADE_SETOR <- isolate(input$comboUnit)
        obj$TEMPO_PASSADO_SETOR          <- isolate(input$comboTimerLook)
        obj$TEMPO_PASSADO_UNIDADE_SETOR  <- isolate(input$comboUnitLook)

        if(!updateSetor(conn,obj)){
          showNotification("setor não foi atualizado com sucesso erro durante processo!", type = "warning")
          return()
        }
        #load todos os setores
        setores(selectAllSetors(conn))
        
        swiperSlidePrevious(idSwiper)
        sliderPosition(isolate(sliderPosition()) - 1L)
        showNotification("setor atualizado com sucesso!", type = "warning")

      })

    },ignoreInit = T))

}

uiMain <- function(ns,valueName = NULL,
                   valueComboTimer = 1,
                   valueComboUnit  = "Minuto",
                   valueComboTimerLook = 10,
                   valueComboUnitLook  = "Minuto"
                  ){

    div(
    shinyjs::inlineCSS(paste0("#",ns("textNameSetor")," {text-transform: uppercase;}")),
    textInput(ns('textNameSetor'),label = 'Nome',placeholder = 'Digite o nome para o setor',value = valueName),
    br(),
    panelTitle(title = "Momento",
               background.color.title = 'white',
               title.color = 'black',
               border.color = 'lightgray',
               children = fluidRow(
                 style = 'padding-top: 10px; padding-left: 15px; padding-right: 15px;',
                 column(6,selectInput(ns('comboTimer'),label     = 'Tempo da Reativar',choices = 1:59,selected = valueComboTimer)),
                 column(6,selectInput(ns('comboUnit'), label     = 'Unidade',choices = c("Minuto","Hora"),selected = valueComboUnit)),
                 column(6,selectInput(ns('comboTimerLook'),label = 'Tempo do Passado',choices = 1:59,selected =valueComboTimerLook)),
                 column(6,selectInput(ns('comboUnitLook'), label = 'Unidade',choices = c("Minuto","Hora"),selected = valueComboUnitLook))
               )
    )
  )
}