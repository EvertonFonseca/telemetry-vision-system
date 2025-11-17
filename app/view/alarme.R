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
  .. / logic / Alarme_dao[...],
  stringr,
  dplyr[...],
  lubridate[...],
  dbp  = ../infra/db_pool,
  db   = ../infra/database
)

#' @export
 uiNewAlarme <- function(ns,input,output,session,callback){

  obs <- newObserve()
 
  showModal(
      session = session,
      dialogModal(
      title = 'Novo Alarme',
      size  = 'm',
      uiMain(ns),
      footer = tagList(actionButton(inputId = ns("btSair"),"Sair",icon = icon("arrow-left")),
                       actionButton(ns('btClear'), "Limpar", icon = icon("eraser")),
                       actionButton(ns('btSalvar'),'Salvar',class = "btn-primary",icon = icon("save"))
                      )))
   
     output$slider1 <- renderUI({
    
    req(currentPosition() == 1)
    
    obs2$clear()
    logica <<- NULL
    init.comondo <<- TRUE

    obs2$add(observeEvent(input[['comboBoxAlarme']],{
      
      comboAlarme <- input[['comboBoxAlarme']]
      
      index   <- which(sapply(Alarmees, function(x){ x$NAME_Alarme == comboAlarme}))
      Alarme   <<- Alarmees[index][[1]]
      objects <<- Alarme$OBJETOS
      updateSelectInput(getDefaultReactiveDomain(),"comboBoxobjects",choices = sapply(objects, function(object) object$NAME_OBJECT))
      
    }))
    
    output$containerAtributos <- renderUI({
      
      shiny::req(input$comboBoxobjects)
      
      # Alarme      <<- rlist::list.find(Alarmees,NAME_Alarme == isolate(input$comboBoxAlarme))[[1]]
      # object     <<- rlist::list.filter(Alarme$OBJETOS,NAME_OBJECT == input$comboBoxobjects)[[1]]
      # atributos  <-  rlist::list.filter(object$ATRIBUTOS,R_DATA != 'time')
      
      changetextPlaceHolder()
      
      multiInput(
        inputId = 'multiAtributos',
        width = '100%',
        options = list(
          enable_search = T,
          non_selected_header = "Atributos não selecionados",
          selected_header     = "Atributos selecionados"
        ),
        selected = isolate(input$multiAtributos),
        label = "Atributos do object",
        choices = NULL,
        choiceNames  = lapply( atributos, function(atributo)  tagList(atributo$NAME_ATRIBUTO)),
        choiceValues = lapply( atributos, function(atributo)  atributo$NAME_ATRIBUTO)
      ) |> tagAppendAttributes(style = ';height: auto; width: 100%;')
      
    })
    
    div(
      style = 'padding-5px;',
      selectInput(
        ns('comboBoxAlarme'),
        label = 'Seleciona Alarme',
        choices = AlarmeesNomes,
        selected = isolate(input[['comboBoxAlarme']])
      ),
      selectInput(
        ns('comboBoxobjects'),
        label = 'Seleciona object',
        choices = '',
        selected = isolate(input[['comboBoxobjects']])
      ),
      uiOutput(ns('containerAtributos'))
    )
    
  })
   
   # Sair 
   obs$add(observeEvent(input$btSair,{
        obs$destroy()
        removeModal(session)
        callback()
   },ignoreInit = T,ignoreNULL = T))
   
   ## Clear
   obs$add(observeEvent(input$btClear, {
        updateTextInput(session,'textNameAlarme', value = '')
        updateSelectInput(session,"comboTimer",selected = 1)
        updateSelectInput(session,"comboUnit",selected = "Minuto")
        updateSelectInput(session,"comboTimerLook",selected = 10)
        updateSelectInput(session,"comboUnitLook",selected = "Minuto")
   }, ignoreInit = TRUE))

   ## Salvar Alarme
   obs$add(observeEvent(input$btSalvar,{
    
    nomeAlarme <- isolate(toupper(input$textNameAlarme))

    #open database
    db$tryTransaction(function(conn){

      if(stringi$stri_isempty(stringr$str_trim(nomeAlarme))){
        showNotification("O nome do Alarme não foi preenchido!", type = "warning")
        return()
      }
      
      if(checkifExistNameAlarme(conn,name = nomeAlarme)){
        showNotification("O nome do Alarme já possui nos registros!", type = "warning")
      }

      #check if it has already data of Alarme
      obj <- list()
      obj$NAME_Alarme                   <- nomeAlarme
      obj$TEMPO_REATIVAR_Alarme         <- isolate(input$comboTimer)
      obj$TEMPO_REATIVAR_UNIDADE_Alarme <- isolate(input$comboUnit)
      obj$TEMPO_PASSADO_Alarme          <- isolate(input$comboTimerLook)
      obj$TEMPO_PASSADO_UNIDADE_Alarme  <- isolate(input$comboUnitLook)
      id                               <- db$nextSequenciaID(conn,'Alarme')
      
      insertNewAlarme(conn,id,obj)

      dialogConfirm(
        session = session,
        id    = ns('dialogConfirm'),
        title = 'Alarme criado com sucesso!',
        text  = 'Deseja criar novamente um novo Alarme?')
      
      #crie so uma vez
      observeEvent(input$dialogConfirm,{
        
        status <- input$dialogConfirm

        # Limpar os campos APÓS o flush/render — garante que os inputs existam no DOM
        session$onFlushed(function() {
          updateTextInput(session,'textNameAlarme', value = '')
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
uiEditAlarme <- function(ns,input,output,session,callback){
  
    obs            <- newObserve()
    sliderPosition <- reactiveVal(1L)
    idSwiper       <- ns('swiperMain')
    Alarmees        <- reactiveVal(selectAllAlarmes(dbp$get_pool()))
    Alarme          <- reactiveVal(NULL)

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
          'Registros Alarme'
        }else{
          'Edição o Alarme'
        }

      })
    
      output$slider1 <- renderUI({
        
        output$tableDinamicaAlarme <- DT$renderDataTable({
      
          dataset  <- Alarmees()

          if(length(dataset) == 0) return(NULL)
          
          colunaNames <- c('LINHA','Alarme','VISUALIZAR / EDITAR','REMOVER')
        
          DT$datatable({
            
            dataset |> 
              mutate_if(is.POSIXct,function(x){ format(x,'%d/%m/%Y %H:%M:%S')})  |> 
              mutate_if(is.Date,function(x){ format(x,'%d/%m/%Y')}) |> 
              mutate_if(is.character,toupper) |> 
              mutate(
                    !!colunaNames[1] := 1:nrow(dataset),
                    !!colunaNames[2] :=  dataset$NAME_Alarme,
                    !!colunaNames[3] :=  sapply(dataset$CD_ID_Alarme, function (x) {
                      
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
                    !!colunaNames[4] :=  sapply(dataset$CD_ID_Alarme, function (x) {
                      
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
          DT$dataTableOutput(outputId = ns('tableDinamicaAlarme'))
        )
        
      })
    
    output$slider2 <- renderUI({

      req(Alarme())
      
      AlarmeSelect <- Alarme()
        
         uiMain(ns,
           valueName = AlarmeSelect$NAME_Alarme,
           valueComboTimer = AlarmeSelect$TEMPO_REATIVAR_Alarme,
           valueComboUnit =  AlarmeSelect$TEMPO_REATIVAR_UNIDADE_Alarme,
           valueComboTimerLook = AlarmeSelect$TEMPO_PASSADO_Alarme,
           valueComboUnitLook = AlarmeSelect$TEMPO_PASSADO_UNIDADE_Alarme
          )
    })

    obs$add(observeEvent(input$editPressedRow,{
      
      Alarme(isolate(Alarmees()) |> filter(CD_ID_Alarme == input$editPressedRow))
      
      swiperSlideNext(idSwiper)
      sliderPosition(isolate(sliderPosition()) + 1L)
      
    },ignoreInit = T))
    
    obs$add(observeEvent(input$deletePressedRow,{
      
      Alarme <- isolate(Alarmees()) |> filter(CD_ID_Alarme == input$deletePressedRow)

      messageAlerta(
                    input,
                    ns,
                    title   = paste0('Todos os objetos ligado a esse Alarme será excluido'),
                    message = paste0('Deseja realmente excluir a Alarme ',Alarme$NAME_Alarme,"?"),
                    callback.no = function(){
                      
                    },
                    callback.yes = function(){
                      
                       db$tryTransaction(function(conn){
                        
                        deleteAlarme(conn,Alarme$CD_ID_Alarme)
                        Alarmees.aux <- selectAllAlarmes(conn)
                        if(nrow(Alarmees.aux) == 0){
                          #destroy all observe events
                          obs$destroy()
                          removeModal(session)
                          swiperDestroy(idSwiper)
                          callback()
                        }else{
                          Alarmees(Alarmees.aux)
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
        Alarme(NULL)
        swiperSlidePrevious(idSwiper)
        sliderPosition(current - 1L)
      }
      
    },ignoreInit = T))
    
    obs$add(observeEvent(input$btActionUpdate,{
      
      req(Alarme())
      
      db$tryTransaction(function(conn){

        id        <- isolate(Alarme()$CD_ID_Alarme)
        nomeAlarme <- isolate(toupper(input$textNameAlarme))

        if(stringi$stri_isempty(stringr$str_trim(nomeAlarme))){
          showNotification("O nome do Alarme não foi preenchido!", type = "warning")
          return()
        }
        
        if(checkifExistNameAlarmeEdit(conn,id,name = nomeAlarme)){
          showNotification("O nome do Alarme já possui nos registros!", type = "warning")
        }

        #check if it has already data of Alarme
        obj <- list()
        obj$CD_ID_Alarme                  <- id
        obj$NAME_Alarme                   <- nomeAlarme
        obj$TEMPO_REATIVAR_Alarme         <- isolate(input$comboTimer)
        obj$TEMPO_REATIVAR_UNIDADE_Alarme <- isolate(input$comboUnit)
        obj$TEMPO_PASSADO_Alarme          <- isolate(input$comboTimerLook)
        obj$TEMPO_PASSADO_UNIDADE_Alarme  <- isolate(input$comboUnitLook)

        if(!updateAlarme(conn,obj)){
          showNotification("Alarme não foi atualizado com sucesso erro durante processo!", type = "warning")
          return()
        }
        #load todos os Alarmees
        Alarmees(selectAllAlarmes(conn))
        
        swiperSlidePrevious(idSwiper)
        sliderPosition(isolate(sliderPosition()) - 1L)
        showNotification("Alarme atualizado com sucesso!", type = "warning")

      })

    },ignoreInit = T))

}

uiMain <- function(ns){
  
  swiper(id = ns('swiperMain'),
    parent.style = 'height: 100%; width: 100%;',
    width = '100%',
    height = '100%',
    swiperSlide(
      style = 'height: 100%; width: 100%; overflow-y: auto; overflow-x: hidden;',
      uiOutput(ns('slider1'),style = 'height: 100%; width: 100%;')
    ),
    swiperSlide(
      style = 'height: 100%; width: 100%; overflow-y: auto; overflow-x: hidden;',
      uiOutput(ns('slider2'),style = 'height: 100%; width: 100%;')
    ),
    swiperSlide(
      style = 'height: 100%; width: 100%; overflow-y: auto; overflow-x: hidden; padding: 10px;',
      uiOutput(ns('slider3'),style = 'height: 100%; width: 100%;')
    )
  )
}