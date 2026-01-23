box::use(
  shinyjs[inlineCSS],
  shiny[...],
  stringi,
  . / model[...],
  . /
    global[
      deleteElement,
      dialogTitleClose,
      panelTitle,
      removeModalClear,
      newObserve,
      shinySetInputValue,
      play_sound,
      debugLocal,
      console,
      messageAlerta,
      changetextPlaceHolder,
      tagAppendAttributesFind,
      set_readonly_js,
      actionWebUser
    ],
  .. / model / swiper[...],
  DT,
  shinycssloaders,
  ../ logic/estrutura_dao[...],
  stringr,
  dplyr[...],
  lubridate[...],
  shinyWidgets[multiInput,updateMultiInput,prettyToggle],
  htmlwidgets,
  ../logic/utils[...],
  purrr[map,map_df,map_chr],
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
uiNewEstrutura <- function(ns,input,output,session,callback){
  
  .register_auto_dispose(session)
  
  e <- .get_private(session)
  
  obs                <- newObserve()
  obs2               <- newObserve()
  tipoDatas          <- selectAllTipoDados(dbp$get_pool())
  attributoReactive  <- reactiveVal(modelAtributo())
  
  id       <- ns('dialogEstrutura')
  cssStyle <- list()
  cssStyle[[paste0(' #parent',id,' .modal-dialog')]]  <- paste0('height: 80% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-content')]] <- paste0('width: 100% !important; height: 100% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-body')]]    <- paste0('width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto;')
  
  showModal(
    session = session,
    div(
      id = paste0('parent', id),
      style = paste0("height: 90%;"),
      shinyjs::inlineCSS(cssStyle),
      dialogModal(
        title = "Nova Estrutura",
        size = 'm',
        uiAtributos(ns),  
        footer = uiOutput(ns('uiFooter')))))
        
        output$uiFooter <- renderUI({
          
          tagList(actionButton(ns("btSair"), label = "Voltar",icon = icon("arrow-left")),
          actionButton(ns('btSalvar'),'Salvar',class = "btn-primary",icon = icon("save")))
          
        })
        
        # ---- (Re)CONSTRUÇÃO DE UI + OBSERVERS DINÂMICOS ----
        obs$add(observeEvent(attributoReactive(), {
          
          req(attributoReactive())
          
          atributos <- attributoReactive()
          obs2$clear()  # limpa observers antigos
          len  <- nrow(atributos)
          
          output$containerAtributo <- renderUI({
            
            divLista <- div()
            for (i in 1:len) {
              
              attributo  <- atributos[i,]
              attAtivo   <- attributo$fg_ativo
              attNome    <- attributo$name_atributo
              attTipo    <- attributo$name_data
              attClasses <- attributo$value_atributo
              
              local({
                ii <- i
                obs2$add(
                  observeEvent(input[[sprintf("atributoDel_%d", ii)]], ignoreInit = TRUE, {
                    atributos <- isolate(removerAtributo(input,attributoReactive()))
                    attributoReactive(atributos[-ii,])
                  }))
                  
                  obs2$add(
                    observeEvent(input[[sprintf("comboTipodados_%d", ii)]], ignoreInit = TRUE, {
                      comboTipo <- input[[sprintf("comboTipodados_%d", ii)]]
                      visible   <- comboTipo == "QUALITATIVE"
                      elemento  <- sprintf("textoClasse_%d", ii)
                      if(visible){
                        set_readonly_js(elemento,FALSE,session)
                        updateTextAreaInput(session,elemento,placeholder = "Digite as classes separadas por vírgula.")
                      }else{
                        set_readonly_js(elemento,TRUE,session)
                        updateTextAreaInput(session,elemento,value = "",placeholder = "")
                      }
                    }))
                    
                  })
                  
                  atributoElement <- panelTitle(
                    title = paste0("Atributo: ", i),
                    background.color.title = "white",
                    title.color  = "black",
                    border.color = "lightgray",
                    children = div(
                      style = "margin-top: 10px; margin-left: 10px;",
                      inlineCSS(paste0("#",ns(sprintf("atributo_%d", i)), " {text-transform: uppercase;}")),
                      inlineCSS(paste0("#",ns(sprintf("textoClasse_%d", i)), " {text-transform: uppercase;}")),
                      br(),
                      splitLayout(
                        style = "overflow-x: auto",
                        tagList(
                          tags$label("Ativar", style = "font-size: 15px;"),
                          div( style = "margin-top: 5px;",
                          prettyToggle(
                            inputId   = ns(sprintf("checkboxAtributoAtivo_%d", i)), 
                            label_on  = "Sim",
                            label_off = "Não",
                            outline   = TRUE, plain = TRUE, value = attAtivo,
                            icon_on   = icon("thumbs-up"),
                            icon_off  = icon("thumbs-down"),
                            bigger    = TRUE, width = "auto",
                          ))
                        ),
                        textInput(
                          inputId = ns(sprintf("atributo_%d", i)),
                          label   = "Nome", width = "100%",
                          value = attNome,
                          placeholder = "Nome para atributo"
                        ),
                        selectizeInput(
                          ns(sprintf("comboTipodados_%d", i)),
                          label   = "Tipo",
                          selected = attTipo,
                          choices = tipoDatas$name_data,
                          options  = list(
                            dropdownParent = 'body',
                            openOnFocus = TRUE,
                            closeAfterSelect = TRUE
                          )
                        ),
                        actionButton(
                          ns(sprintf("atributoDel_%d", i)),
                          label = "", icon = icon("trash"),
                          style = paste0("margin-top: 25px;",ifelse(i == 1," visibility: hidden;",""))
                        ),
                        cellWidths = c("50px", "200px", "150px", "50px")
                      ),
                      textAreaInput(ns(sprintf("textoClasse_%d", i)),
                      label = "Classes",
                      value = attClasses,
                      resize = "none",
                      placeholder = "Digite as classes separadas por vírgula.",
                      width = '98%')
                    )
                  )
                  divLista <- tagAppendChildren(divLista, br(), atributoElement)
                }
                divLista
              })
              
            },ignoreInit = FALSE))
            
            # Observer do botão "Clear"
            obs$add(
              observeEvent(input$atributoClearAll, ignoreInit = TRUE, {
                actionWebUser({
                  attributoReactive(modelAtributo())
                })
              })
            )
            
            # Observer do botão "Adicionar"
            obs$add(
              observeEvent(input$atributoAdd, ignoreInit = TRUE, {
                actionWebUser({
                  atributos <- isolate(adicionarNewAtributo(input,attributoReactive()))
                  attributoReactive(atributos)
                })
              })
            )
            
            obs$add(observeEvent(input$btSair,{
              
              obs$destroy()
              obs2$destroy()
              removeModal(session)
              callback()
              
            },ignoreInit = T))
            
            ## Salvar Estrutrua
            obs$add(observeEvent(input$btSalvar,{
              
                nomeEstrutura <- isolate(toupper(input$textNameEstrutura))
                
                if(stringi$stri_isempty(stringr$str_trim(nomeEstrutura))){
                  showNotification("O nome do Estrutura não foi preenchido!", type = "warning")
                  return()
                }
                
                if(checkifExistNameEstrutura(dbp$get_pool(),name = nomeEstrutura)){
                  showNotification("O nome da Estrutura já possui nos registros!", type = "warning")
                  return()
                }
                
                atributos <- isolate(obterAllAtributos(input,attributoReactive()))
                
                if(!checkAtributoValidadao(atributos)){
                  return()
                }
   
                # try insert or roolback
                if(!db$tryTransaction(function(conn){
             
                  #check if it has already data of Câmera
                  obj <- list()
                  obj$name_estrutura   <- nomeEstrutura
                  obj$cd_id_estrutura  <- db$nextSequenciaID(conn, "estrutura", id_col = "cd_id_estrutura", schema = "public")
                
                  db$insertTable(conn,"estrutura",obj)
                 
                  #Estrutura Config
                  config <- list()
                  config$cd_id_estrutura         <- obj$cd_id_estrutura
                  config$cd_id_estrutura_config  <- db$nextSequenciaID(conn, "estrutura_config", id_col = "cd_id_estrutura_config", schema = "public")
                  db$insertTable(conn,"estrutura_config",config)
                 
                  #insert atributos do compnente
                  for(k in 1:nrow(atributos)){
                    
                    tipo_data <- tipoDatas |> filter(name_data == atributos$name_data[k])
                    objAtt    <- list()
                    objAtt$name_atributo    <- atributos$name_atributo[k] 
                    objAtt$value_atributo  <- paste0(unlist(atributos$value_atributo[k]),collapse = ",")
                    objAtt$fg_ativo         <- as.integer(atributos$fg_ativo[k])
                    objAtt$cd_id_estrutura_config  <- config$cd_id_estrutura_config
                    objAtt$cd_id_data              <- tipo_data$cd_id_data
                    
                    db$insertTable(conn,"ATRIBUTO",objAtt)
                  }
                  
                  dialogConfirm(
                    session = session,
                    id    = ns('dialogConfirm'),
                    title = 'Estrutura criado com sucesso!',
                    text  = 'Deseja criar novamente um nova Estrutura?')
                    
                    #crie so uma vez
                    observeEvent(input$dialogConfirm,{
                      
                      status <- input$dialogConfirm
                      clearPanel(session,attributoReactive)
                      
                      if(!status){
                        obs$destroy()
                        obs2$destroy()
                        removeModal(session)
                        callback()
                      }
                      
                    },ignoreInit = TRUE,once = TRUE)
                    
                  })){
                    showNotification("Não foi possivel salvar a Estrutura, durante o processo houve falha!", type = "error")
                  }
               
            },ignoreInit = T,ignoreNULL = T))          
   
 }


#' @export
uiEditEstrutura <- function(ns,input,output,session,callback){
  
  .register_auto_dispose(session)

  e <- .get_private(session)

  sliderPosition <- reactiveVal(1L)
  idSwiper       <- ns('swiperMain')
  
  estruturas     <- reactiveVal(selectAllEstrutura(dbp$get_pool()))
  estrutura      <- reactiveVal(NULL)
  obs            <- newObserve()
  obs2           <- newObserve()
  obs3           <- newObserve()
  tipoDatas          <- selectAllTipoDados(dbp$get_pool())
  attributoReactive  <- reactiveVal(modelAtributo())
  
  id       <- ns('dialogEstrutura')
  cssStyle <- list()
  cssStyle[[paste0(' #parent',id,' .modal-dialog')]]  <- paste0('height: 80% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-content')]] <- paste0('width: 100% !important; height: 100% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-body')]]    <- paste0('width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto;')
  
  showModal(
    session = session,
    div(
      id = paste0('parent', id),
      style = paste0("height: 90%;"),
      shinyjs::inlineCSS(cssStyle),
      dialogModal(
        title = textOutput(ns("titleTexto")),
        size = 'm',
        swiper(id = idSwiper,width = '100%',height = '100%',
        parent.style = "min-height: 350px !important;",
        swiperSlide(
          style = 'height: 100%; width: 100%; overflow: hidden; padding: 1px;',
          uiOutput(ns('slider1')) |> shinycssloaders$withSpinner(color = 'lightblue')
        ),
        swiperSlide(
          style = 'height: 100%; width: 100%; overflow-y: hidden; overflow-x: hidden; padding: 1px;',
          uiOutput(ns('slider2')) |> shinycssloaders$withSpinner(color = 'lightblue')
        )
      ),  
      footer = uiOutput(ns('uiFooter')))))
      
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
        
        output$tableDinamicaEstrutura <- DT$renderDataTable({
          
          dataset  <- estruturas()
          
          if(length(dataset) == 0) return(NULL)
          
          colunaNames <- c('LINHA','ESTRUTURA','VISUALIZAR / EDITAR','REMOVER')
          
          DT$datatable({
            
            dataset |> 
            mutate_if(is.POSIXct,function(x){ format(x,'%d/%m/%Y %H:%M:%S')})  |> 
            mutate_if(is.Date,function(x){ format(x,'%d/%m/%Y')}) |> 
            mutate_if(is.character,toupper) |> 
            mutate(
              !!colunaNames[1] := 1:nrow(dataset),
              !!colunaNames[2] :=  dataset$name_estrutura,
              !!colunaNames[3] :=  sapply(dataset$cd_id_estrutura, function (x) {
                
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
              !!colunaNames[4] :=  sapply(dataset$cd_id_estrutura, function (x) {
                
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
        DT$dataTableOutput(outputId = ns('tableDinamicaEstrutura'))
      )
      
    })
    
    output$slider2 <- renderUI({
      
      req(estrutura())
      estruturaSelect <- estrutura()
      attributoReactive(estruturaSelect$configs[[1]]$atributos[[1]] |> select(name_atributo,name_data,value_atributo,fg_ativo))
      
      obs2$clear()  # limpa observers antigos
      
      # ---- (Re)CONSTRUÇÃO DE UI + OBSERVERS DINÂMICOS ----
      obs2$add(observeEvent(attributoReactive(),{
        
        req(attributoReactive())
        
        obs3$clear()  # limpa observers antigos
        atributos <- attributoReactive()
        
        len  <- nrow(atributos)
        
        output$containerAtributo <- renderUI({
          
          divLista <- div()
          
          for (i in 1:len) {
            
            attributo  <- atributos[i,]
            attAtivo   <- attributo$fg_ativo
            attNome    <- attributo$name_atributo
            attTipo    <- attributo$name_data
            attClasses <- attributo$value_atributo
            
            local({
              ii <- i
              obs3$add(
                observeEvent(input[[sprintf("atributoDel_%d", ii)]], ignoreInit = TRUE, {
                  atributos <- isolate(removerAtributo(input,attributoReactive()))
                  attributoReactive(atributos[-ii,])
                }))
                
                obs3$add(
                  observeEvent(input[[sprintf("comboTipodados_%d", ii)]], ignoreInit = TRUE, {
                    comboTipo <- input[[sprintf("comboTipodados_%d", ii)]]
                    visible   <- comboTipo == "QUALITATIVE"
                    elemento  <- sprintf("textoClasse_%d", ii)
                    if(visible){
                      set_readonly_js(elemento,FALSE,session)
                      updateTextAreaInput(session,elemento,placeholder = "Digite as classes separadas por vírgula.")
                    }else{
                      set_readonly_js(elemento,TRUE,session)
                      updateTextAreaInput(session,elemento,value = "",placeholder = "")
                    }
                  }))
                  
                })
                
                atributoElement <- panelTitle(
                  title = paste0("Atributo: ", i),
                  background.color.title = "white",
                  title.color  = "black",
                  border.color = "lightgray",
                  children = div(
                    style = "margin-top: 10px; margin-left: 10px;",
                    inlineCSS(paste0("#",ns(sprintf("atributo_%d", i)), " {text-transform: uppercase;}")),
                    inlineCSS(paste0("#",ns(sprintf("textoClasse_%d", i)), " {text-transform: uppercase;}")),
                    br(),
                    splitLayout(
                      style = "overflow-x: auto",
                      tagList(
                        tags$label("Ativar", style = "font-size: 15px;"),
                        div( style = "margin-top: 5px;",
                        prettyToggle(
                          inputId   = ns(sprintf("checkboxAtributoAtivo_%d", i)), 
                          label_on  = "Sim",
                          label_off = "Não",
                          outline   = TRUE, plain = TRUE, value = attAtivo,
                          icon_on   = icon("thumbs-up"),
                          icon_off  = icon("thumbs-down"),
                          bigger    = TRUE, width = "auto",
                        ))
                      ),
                      textInput(
                        inputId = ns(sprintf("atributo_%d", i)),
                        label   = "Nome", width = "100%",
                        value = attNome,
                        placeholder = "Nome para atributo"
                      ),
                      selectizeInput(
                        ns(sprintf("comboTipodados_%d", i)),
                        label   = "Tipo",
                        selected = attTipo,
                        choices = tipoDatas$name_data,
                        options  = list(
                          dropdownParent = 'body',
                          openOnFocus = TRUE,
                          closeAfterSelect = TRUE
                        )
                      ),
                      actionButton(
                        ns(sprintf("atributoDel_%d", i)),
                        label = "", icon = icon("trash"),
                        style = paste0("margin-top: 25px;",ifelse(i == 1," visibility: hidden;",""))
                      ),
                      cellWidths = c("50px", "200px", "150px", "50px")
                    ),
                    textAreaInput(ns(sprintf("textoClasse_%d", i)),
                    label = "Classes",
                    value = attClasses,
                    resize = "none",
                    placeholder = "Digite as classes separadas por vírgula.",
                    width = '98%')
                  )
                )
                divLista <- tagAppendChildren(divLista, br(), atributoElement)
              }
              divLista
            })
            
          },ignoreInit = FALSE))
          
          # Observer do botão "Clear"
          obs2$add(
            observeEvent(input$atributoClearAll, ignoreInit = TRUE, {
              actionWebUser({
                attributoReactive(modelAtributo())
              })
            })
          )
          
          # Observer do botão "Adicionar"
          obs2$add(
            observeEvent(input$atributoAdd, ignoreInit = TRUE, {
              actionWebUser({
                atributos <- isolate(adicionarNewAtributo(input,attributoReactive()))
                attributoReactive(atributos)
              })
            })
          )
          
          uiAtributos(ns,estruturaSelect$name_estrutura)
        })
        
        obs$add(observeEvent(input$editPressedRow,{
          
          estrutura(isolate(estruturas()) |> filter(cd_id_estrutura == input$editPressedRow))
          
          swiperSlideNext(idSwiper)
          sliderPosition(isolate(sliderPosition()) + 1L)
          
        },ignoreInit = T))
        
        obs$add(observeEvent(input$deletePressedRow,{
          
          estrutura <- isolate(estruturas()) |> filter(cd_id_estrutura == input$deletePressedRow)
          
          messageAlerta(
            input,
            ns,
            title   = paste0('Todos os objetos ligado a essa estrutura será excluido'),
            message = paste0('Deseja realmente excluir a estrutura ',estrutura$name_estrutura,"?"),
            callback.no = function(){
              
            },
            callback.yes = function(){
              
              db$tryTransaction(function(conn){
                
                db$deleteTable(conn,"ESTRUTURA",where_cols = "cd_id_estrutura", where_vals = estrutura$cd_id_estrutura)

                estruturas.aux <- selectAllEstrutura(conn)
                if(nrow(estruturas.aux) == 0){
                  #destroy all observe events
                  obs$destroy()
                  obs2$destroy()
                  obs3$destroy()
                  removeModal(session)
                  swiperDestroy(idSwiper)
                  callback()
                }else{
                  estruturas(estruturas.aux)
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
              estrutura(NULL)
              swiperSlidePrevious(idSwiper)
              sliderPosition(current - 1L)
            }
            
          },ignoreInit = T))
          
          obs$add(observeEvent(input$btActionUpdate,{
            
            estruturaSelect <- isolate(estrutura())
            nomeEstrutura   <- isolate(toupper(input$textNameEstrutura))
            
            if(stringi$stri_isempty(stringr$str_trim(nomeEstrutura))){
              showNotification("O nome do Estrutura não foi preenchido!", type = "warning")
              return()
            }
            
            if(checkifExistNameEstruturaEdit(dbp$get_pool(),estruturaSelect$cd_id_estrutura,name = nomeEstrutura)){
              showNotification("O nome da Estrutura já possui nos registros!", type = "warning")
              return()
            }
            
            atributos <- isolate(obterAllAtributos(input,attributoReactive()))
            
            if(!checkAtributoValidadao(atributos)){
              return()
            }
            
            db$tryTransaction(function(conn){
              
              #check if it has already data of Câmera
              obj <- list()
              obj$name_estrutura   <- nomeEstrutura
              db$updateTable(conn,"ESTRUTURA",obj, where_cols = "cd_id_estrutura", where_vals = estruturaSelect$cd_id_estrutura)
              
              #Estrutura Config
              config <- list()
              config$cd_id_estrutura         <- estruturaSelect$cd_id_estrutura
              config$cd_id_estrutura_config  <- db$nextSequenciaID(conn, "estrutura_config", id_col = "cd_id_estrutura_config", schema = "public")

              db$insertTable(conn,"ESTRUTURA_CONFIG",config)
              
              #insert atributos do compnente
              for(k in 1:nrow(atributos)){
                
                tipo_data <- tipoDatas |> filter(name_data == atributos$name_data[k])
                objAtt    <- list()
                objAtt$name_atributo    <- atributos$name_atributo[k] 
                objAtt$value_atributo  <- paste0(unlist(atributos$value_atributo[k]),collapse = ",")
                objAtt$fg_ativo         <- as.integer(atributos$fg_ativo[k])
                objAtt$cd_id_estrutura_config  <- config$cd_id_estrutura_config
                objAtt$cd_id_data              <- tipo_data$cd_id_data
                
                db$insertTable(conn,"ATRIBUTO",objAtt)
              }
              #load todos os setores
              estruturas(selectAllEstrutura(conn))
              
              swiperSlidePrevious(idSwiper)
              sliderPosition(isolate(sliderPosition()) - 1L)
              showNotification("Estrutura atualizado com sucesso!", type = "warning")
            })
            
          },ignoreInit = T))
}
            
 uiAtributos <- function(ns,valueTextoNameEstrutura = NULL){
  
  div(
    inlineCSS(paste0("#", ns("textNameEstrutura"), " {text-transform: uppercase;}")),
    splitLayout(
      cellWidths = c("80%", "auto","auto"),
      textInput(ns("textNameEstrutura"), label = "Nome",
      placeholder = "Digite o nome para a Estrutura",width = "100%",value = valueTextoNameEstrutura ),
      actionButton(ns("atributoClearAll"), label = "", icon = icon("eraser"),style = "margin-top: 25px;"),
      actionButton(ns("atributoAdd"), label = "", icon = icon("plus"),style = "margin-top: 25px;")
    ),
    uiOutput(ns("containerAtributo"))   # <- conteúdo dinâmico entra aqui
  )
}

#template atributo
modelAtributo <- function() {
  tibble(name_atributo = "",name_data = "QUALITATIVE",value_atributo = list(NULL),fg_ativo = TRUE)
}

obterAllAtributos <- function(input,atributos){

  for(k in seq_len(nrow(atributos))){
    atributos$fg_ativo[k]         <- input[[sprintf("checkboxAtributoAtivo_%d", k)]]
    atributos$name_atributo[k]    <- toupper(input[[sprintf("atributo_%d",k)]])
    atributos$name_data[k]        <- input[[sprintf("comboTipodados_%d",k)]]
    atributos$value_atributo[k]  <- ajusteTextoClasses(toupper(input[[sprintf("textoClasse_%d",k)]]))
  }
  atributos
}

adicionarNewAtributo <- function(input,atributos){
  
  att_tmp   <- modelAtributo()
  for(k in seq_len(nrow(atributos))){
    atributos$fg_ativo[k]         <- input[[sprintf("checkboxAtributoAtivo_%d", k)]]
    atributos$name_atributo[k]    <- toupper(input[[sprintf("atributo_%d",k)]])
    atributos$name_data[k]        <- input[[sprintf("comboTipodados_%d",k)]]
    atributos$value_atributo[k]  <- toupper(input[[sprintf("textoClasse_%d",k)]])
  }
  rbind(atributos,att_tmp)
}

removerAtributo <- function(input,atributos){

  for(k in seq_len(nrow(atributos))){
    atributos$fg_ativo[k]         <- input[[sprintf("checkboxAtributoAtivo_%d", k)]]
    atributos$name_atributo[k]    <- toupper(input[[sprintf("atributo_%d",k)]])
    atributos$name_data[k]        <- input[[sprintf("comboTipodados_%d",k)]]
    atributos$value_atributo[k]  <- toupper(input[[sprintf("textoClasse_%d",k)]])
  }
  atributos
}

checkAtributoValidadao <- function(atributos){
  
  #checa e analisar os componentes
  for(i in 1:nrow(atributos)){
    att <- atributos[i,]
    if(any(stringi$stri_isempty(att$name_atributo))){
      showNotification("Alguns atributos não esta com nome preenchido!", type = "warning")
      return(FALSE)
    }else if(any(duplicated(att$name_atributo))){
      showNotification("Alguns nomes de atributo possui duplicação!", type = "warning")
      return(FALSE)
    }else if(!any(att$fg_ativo)){
      showNotification("Pelo menos deve possuir algum atributo ativo!", type = "warning")
      return(FALSE)
    }else if(stringi$stri_isempty(att$value_atributo) && att$name_data == "QUALITATIVE"){
      showNotification(paste0("Nenhuma classe foi preenchida para o atributo ",toupper(att$name_atributo)), type = "warning")
      return(FALSE)
    }
    
  }
  return(TRUE)
}

clearPanel <- function(session,attributoReactive){
  # Limpar os campos APÓS o flush/render — garante que os inputs existam no DOM
  session$onFlushed(function() {
    updateTextInput(session,'textNameEstrutura', value = '')
    updateTextInput(session,sprintf("atributo_%d",1L), value = '')
    updateTextAreaInput(session,sprintf("textoClasse_%d",1L), value = '')
    updateSelectizeInput(session,sprintf("comboTipodados_%d",1L),selected = "QUALITATIVE")
    attributoReactive(modelAtributo())
  }, once = TRUE)
}

ajusteTextoClasses <- function(texto){
  texto <- stringr$str_trim(stringr$str_split(texto,",")[[1]])
  paste(texto, collapse = ",")
}