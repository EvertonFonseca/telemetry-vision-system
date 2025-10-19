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
  db = .. / logic / database,
  .. / logic/objeto_dao[...],
  .. / logic/camera_dao[...],
  .. / logic/setor_dao[...],
  .. / logic/plot_dao[...],
  stringr,
  dplyr[...],
  lubridate[...],
  shinyWidgets[multiInput,updateMultiInput,prettyToggle],
  magick[...],
  htmlwidgets,
  base64enc,
  jsonlite,
  ../logic/utils[...],
  purrr[map,map_df,map_chr,map_vec,map_lgl],
  sf[st_as_sf]
)

#' @export
 uiNewPlot <- function(ns,input,output,session,callback){
   
  db$tryResetConnection(function(con){

    obs        <- newObserve()
    obs2       <- newObserve()
    obs3       <- newObserve()
    setores    <- selectAllSetors(con)
    typedatas  <- selectAllTipoDados(con)
    tipoPlots  <- selectTypesPlots(con)
    index_objs   <- FALSE
    context.plot <- NULL

    objetos         <- selectAllObjetos(con,fg.ativo = TRUE)
    sliderPosition  <- reactiveVal(1L)
    tipo.plot       <- NULL
    datatable       <- NULL

    idSwiper <- ns('swiperMain')
    id       <- ns('dialogObj')
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
              ),
              swiperSlide(
                style = 'height: 100%; width: 100%; overflow-x: hidden; overflow-y: hidden;  padding: 1px;',
                uiOutput(ns('slider3')) |> shinycssloaders$withSpinner(color = 'lightblue')
              )
        ),  
        footer = uiOutput(ns('uiFooter')))))
    
      output$uiFooter <- renderUI({
      
        current <- sliderPosition()
        
        if(current == 1){
          tagList(actionButton(inputId = "btSair","Sair"),actionButton(ns('btSalvar'),'Avançar'))
        }
        else{
          tagList(actionButton(inputId = "btSair","Voltar"),actionButton(ns('btSalvar'),'Salvar'))
        }
      
    })

     output$titleTexto <- renderText({
        
        current <- sliderPosition()

        if(current == 1L){
          'Novo Grafico'
        }
        else if(current == 2L){
          'Seleção de Objetos'
        }else{
          'Seleção de atributos'
        }

      })
    
    output$slider1 <- renderUI({

      output$containerFormat <- renderUI({
        
        req(input$plotType)
        
        tipo.plot <<- tipoPlots|> filter(NAME_TIPO == input$plotType) 

        x <- NULL
        y <- NULL
        
        if(tipo.plot$CD_ID_TIPO == 1 || tipo.plot$CD_ID_TIPO == 2){ #scatter and line
          
          y <- c('QUANTITATIVE')#typedatas|>  filter(R_DATA == 'numeric' | R_DATA == 'integer')
          x <- c('QUANTITATIVE','TIME')#typedatas|>  filter(R_DATA == 'numeric' | R_DATA == 'time' | R_DATA == 'date')
          
        }
        else if(tipo.plot$CD_ID_TIPO == 3){ # box
          
          y <- c('QUANTITATIVE')
          x <- c('QUALITATIVE','TIME')
          
        }
        else if(tipo.plot$CD_ID_TIPO == 4){ # hist
          
          y <- c('QUANTITATIVE')
          x <- c('QUANTITATIVE')
          
        }
        else if(tipo.plot$CD_ID_TIPO == 5){ # dens
          
          y <- c('QUANTITATIVE')
          x <- c('QUANTITATIVE')
          
        }
        else if(tipo.plot$CD_ID_TIPO == 6){ # bar
          
          y <- c('QUANTITATIVE')
          x <- c('QUALITATIVE')
          
        }
        else if(tipo.plot$CD_ID_TIPO == 7){ # pie
          
          y <- c('QUANTITATIVE')
          x <- c('QUALITATIVE')
          
        }
        else if(tipo.plot$CD_ID_TIPO == 8){ # Area
          
          y <- c('QUANTITATIVE')
          x <- c('QUALITATIVE','TIME')
          
        }

        y.txt    <- column(6,textInput(ns('textEixoY'),label  = 'Texto Eixo Y',placeholder = 'Digite o text para Eixo Y',value = isolate(input$textEixoY)))
        y.format <- column(6,selectizeInput(ns('comboEixoY'),label  = 'Tipo de dado Eixo Y',choices = toupper(y),options  = list(
                      dropdownParent = 'body',
                      openOnFocus = TRUE,
                      closeAfterSelect = TRUE
                    )))
        x.txt    <- column(6,textInput(ns('textEixoX'),label  = 'Texto Eixo X',placeholder = 'Digite o text para Eixo X',value = isolate(input$textEixoX)))
        x.format <- column(6,selectizeInput(ns('comboEixoX'),label  = 'Tipo de dado Eixo X',choices = toupper(x),options  = list(
                      dropdownParent = 'body',
                      openOnFocus = TRUE,
                      closeAfterSelect = TRUE
                    )))
        
      fluidRow(
        y.txt,
        y.format,
        x.txt,
        x.format
      )
    
      })
      
      tagList(
        fluidRow(
          column(6,fluidRow(
            shinyjs::inlineCSS("#textTitlePlot {text-transform: uppercase;}"),
            shinyjs::inlineCSS("#textEixoY {text-transform: uppercase;}"),
            shinyjs::inlineCSS("#textEixoX {text-transform: uppercase;}"),
            shinyjs::inlineCSS("#textLegend {text-transform: uppercase;}"),
            column(12,selectizeInput(
              ns('comboBoxSetor'),
              label = 'Seleciona Setor',
              choices = setores$NAME_SETOR,
              selected = isolate(input$comboBoxSetor)
            )),
            column(12,textInput(ns('textTitlePlot'),label = 'Titulo',placeholder = 'Digite o titulo para grafico',value = '')),
          ))
        ),    
        br(),
        panelTitle(title = 'Configuração',
                        background.color.title = 'white',
                        title.color = 'black',
                        border.color = 'lightgray',
                        children = fluidRow(style = 'padding: 10px',column(12,selectizeInput(ns('plotType'),label = 'Tipo de grafico',choices =  tipoPlots$NAME_TIPO,width = '100%',options  = list(
                                              dropdownParent = 'body',
                                              openOnFocus = TRUE,
                                              closeAfterSelect = TRUE
                                            ))),
                                            column(12,uiOutput(ns('containerFormat'))),
                                            column(12,textInput(ns('textLegend'),label = 'Texto Legenda',placeholder = 'Digite o titulo para legenda',value = '',width = '100%'))
                                            
                        ))
        
      )# end tagList
    })


   output$slider2 <- renderUI({
  
      req(sliderPosition() == 2)
     
      output$containerObjetos <- renderUI({
        
        changetextPlaceHolder()

        estruturas.tmp <- typedatas |> filter(NAME_DATA %in% input$multiTipoStrutcs)

        #objetos para selecao do plot
        objetos.tmp <- objetos$NAME_OBJETO[index_objs]

        if(tipo.plot$CD_ID_TIPO != 7){
        
          multiInput(
            inputId = ns('multiTipoObjects'),
            width = '100%',
            options = list(
              enable_search = T,
              non_selected_header = "Objetos não selecionados",
              selected_header     = "Objetos selecionados"
            ),
            selected =  NULL,
            label = "Objeto",
            choices = NULL,
            choiceNames  = lapply(objetos.tmp, function(x)  tagList(x)),
            choiceValues = lapply(objetos.tmp, function(x)  x)
          ) |> tagAppendAttributes(style = ';height: auto; width: 100%;')
        }
        else{
          
          selectizeInput(
            inputId = ns('multiTipoObjects'),
            label   = 'Objeto selecionado',
            width   = '100%',
            choices = objetos.tmp,
            options = list(
                      dropdownParent = 'body',
                      openOnFocus = TRUE,
                      closeAfterSelect = TRUE
                    )
          )
        }
  
      })

      div(
        style = 'height: auto;',
        uiOutput(ns('containerEstruturas')),
        uiOutput(ns('containerObjetos'))
      )
    
   }) # end slider 2
    
   output$slider3 <- renderUI({

    req(sliderPosition() == 3)
    
    objetos.tmp    <- objetos |> filter(NAME_OBJETO %in% isolate(input$multiTipoObjects))
    component.y    <- NULL

    obs2$clear()
    
    if(is.null(datatable))
    {
      datatable   <<- reactiveVal({
        x <- as.data.frame(matrix(nrow = 0,ncol = 4))
        colnames(x) <- c(context.plot$EIXO.Y,context.plot$EIXO.X,"LEGENDA","REMOVER")
        x
      })
    }else{
      isolate(datatable({
        x <- as.data.frame(matrix(nrow = 0,ncol = 4))
        colnames(x) <- c(context.plot$EIXO.Y,context.plot$EIXO.X,"LEGENDA","REMOVER")
        x
      }))
    }
    
    obs2$add(observeEvent(input$deletePressedRow,{
      
      id    <- as.integer(input$deletePressedRow)
      table <- isolate(datatable())
      table <- table[-id,]
      
      if(nrow(table) == 0){
        datatable({
          x <- as.data.frame(matrix(nrow = 0,ncol = 4))
          colnames(x) <- c(context.plot$EIXO.Y,context.plot$EIXO.X,"LEGENDA","REMOVER")
          x
        })
      }else{
        datatable(table)
      }
      
      
    },ignoreInit = TRUE,ignoreNULL = TRUE))
    
    obs2$add(observeEvent(input$btInsertTable,{
  
      table     <- isolate(datatable())

      table.new <- insertTablePlot(table          = table,
                                   context.plot   = context.plot,
                                   objetos.tmp    = objetos.tmp,
                                   atributos.tmp  = atributos.tmp,
                                   nameObject.y   = isolate(input$comboObjetoY),
                                   nameObject.x   = isolate(input$comboObjetoX),
                                   nameAtriburo.y = isolate(input$comboAtributoY),
                                   nameAtriburo.x = isolate(input$comboAtributoX)
                                   )

      datatable(rbind(table,table.new))
      
    },ignoreInit = TRUE,ignoreNULL = TRUE))

    obs2$add(observeEvent(input$comboObjetoY,{
      objeto         <- objetos.tmp |> filter(NAME_OBJETO    == input$comboObjetoY)
      atributo       <- atributos.tmp |> filter((CD_ID_STRUCT == objeto$CD_ID_STRUCT & context.plot$FORMAT.Y == R_DATA) | !is.na(CD_ID_QUAT))  
      updateSelectInput(inputId = 'comboAtributoY',choices = unique(atributo$NAME_ATRIBUTO))
      
    },ignoreNULL = TRUE))
    
    obs2$add(observeEvent(input$comboObjetoX,{
      objeto         <- objetos.tmp |> filter(NAME_OBJETO    == input$comboObjetoX)
      atributo       <- atributos.tmp |> filter((CD_ID_STRUCT == objeto$CD_ID_STRUCT & context.plot$FORMAT.X == R_DATA) | !is.na(CD_ID_QUAT))
      updateSelectInput(inputId = 'comboAtributoX',choices = unique(atributo$NAME_ATRIBUTO))    
      
    },ignoreNULL = TRUE))

    output$tableDataframe <- DT::renderDataTable({
      
      datas       <- datatable()  
      colunaNames <- c({if(context.plot$BINARY)context.plot$EIXO.Y else NULL},context.plot$EIXO.X,"LEGENDA","REMOVER")

      if(nrow(datas) > 0){
        
        if(context.plot$BINARY){

          datas <-  datas |> 
            mutate_if(is.character,toupper) |> 
            mutate(
              !!colunaNames[1] :=  datas[[context.plot$EIXO.Y]],
              !!colunaNames[2] :=  datas[[context.plot$EIXO.X]],
              !!colunaNames[3] :=  datas$LEGENDA,
              !!colunaNames[4] :=  sapply(1:nrow(datas),function(x){
                
                as.character(
                  actionButton(
                    paste0(ns('btRemove')),
                    label = '',
                    icon = icon('trash'),
                    onclick = paste0('Shiny.setInputValue(\"',ns("deletePressedRow"),'\","',x,'",{priority: "event"})'),
                    #style = 'background-color: transparent; color: lightblue; border-solid: none;'
                  )
                )
                
              }))  |> select(colunaNames)
          
        }else{

          datas <-  datas |> 
            mutate_if(is.character,toupper) |> 
            mutate(
              !!colunaNames[1] :=  datas[[context.plot$EIXO.X]],
              !!colunaNames[2] :=  datas$LEGENDA,
              !!colunaNames[3] :=  sapply(1:nrow(datas),function(x){
                
                as.character(
                  actionButton(
                    paste0(ns('btRemove')),
                    label = '',
                    icon = icon('trash'),
                    onclick = paste0('Shiny.setInputValue(\"',ns("deletePressedRow"),'\","',x,'",{priority: "event"})'),
                    #style = 'background-color: transparent; color: lightblue; border-solid: none;'
                  )
                )
                
              }))  |> select(colunaNames)
          
        }
      }
      else{
        datas <-  datas |> select(colunaNames)
      }
      
      DT::datatable({datas}, 
                    class = 'cell-border stripe',
                    extensions = 'Scroller',
                    options = list(
                      language = list(url = 'js/table/translate.json'),
                      dom = 't',
                      bSort=FALSE,
                      columnDefs = list(list(visible=FALSE, targets=c(0)),list(className = 'dt-center', targets = "_all"),list(width = 'autos',targets = 0:3)),
                      deferRender = TRUE,
                      scroller = FALSE
                    ),
                    escape = F,
                    selection = 'none',
      )  |> DT::formatStyle(colunaNames, cursor = 'pointer')
      
    })
    
    if(context.plot$BINARY)
    {
      component.y    <-   tagList(panelTitle(title = 'Eixo Y',
                                             background.color.title = 'white',
                                             title.color = 'black',
                                             border.color = 'lightgray',
                                             children = div(style = 'padding: 10px',
                                                            splitLayout(
                                                              selectInput(ns('comboObjetoY'),'Objeto',choices = objetos.tmp$NAME_OBJETO,selected = isolate(input$comboObjetoY),width = '100%'),
                                                              selectInput(ns('comboAtributoY'),'Atributo',choices = '',width = '100%'),
                                                              cellWidths = c('50%','50%')
                                                            ))),br())
    }
     
    div(
      style = 'padding-left: 15px; padding-right: 15px; padding-bottom: 10px',
      br(),
      component.y,
      panelTitle(title = 'Eixo X',
                 background.color.title = 'white',
                 title.color = 'black',
                 border.color = 'lightgray',
                 children = div(style = 'padding: 10px',
                                splitLayout(
                                  selectInput(ns('comboObjetoX'),'Objeto',choices = objetos.tmp$NAME_OBJETO,selected = isolate(input$comboObjetoX),width = '100%'),
                                  selectInput(ns('comboAtributoX'),'Atributo',choices = '',width = '100%'),
                                  cellWidths = c('50%','50%')
                                ))),
      br(),
      span('Adicionar',style = 'font-size: 16px;'),
      actionButton(ns('btInsertTable'),'',icon = icon('arrow-down'),width = '100%',style = 'font-size: 18px;'),
      br(),
      div(
        style = 'width: 100%; height: auto; padding: 5px;',
        DT::dataTableOutput(ns('tableDataframe'))
      ))
    
   })
    
      ## Clear
    obs$add(observeEvent(input$btClear, {
      updateTextInput(session,'textNameObjeto', value = '')
      updateMultiInput(session,'multiCameras',choices = '')
    }, ignoreInit = TRUE))

    ## Salvar Objeto
    obs$add(observeEvent(input$btSalvar,{

      current <- isolate(sliderPosition())

      if(current == 1L){

        textTitle     <- toupper(isolate(input$textTitlePlot))
        textEixoY     <- toupper(isolate(input$textEixoY))
        textEixoX     <- toupper(isolate(input$textEixoX))
        textLegend    <- toupper(isolate(input$textLegend))
        binary.status <- FALSE
        
        if(tipo.plot$CD_ID_TIPO  == 1 || tipo.plot$CD_ID_TIPO  == 2 || tipo.plot$CD_ID_TIPO  == 3 || tipo.plot$CD_ID_TIPO  == 8){ #scatter and line

          binary.status <- TRUE
          
          if(checkifTextEmpty(textTitle) || 
              checkifTextEmpty(textEixoY) ||
              checkifTextEmpty(textEixoX) ||
              checkifTextEmpty(textLegend)){
            
            showNotification("Existe campos que não foram preenchido!", type = "warning")
            
            return()
          }
          
          if(stringr::str_trim(textEixoY) == stringr::str_trim(textEixoX)){
            
            showNotification("O texto do Eixo Y e X não podem ser iguais!", type = "warning")
            
            return()
          }
        }
        else{
          
          binary.status <- FALSE
          
          if(checkifTextEmpty(textTitle) || 
            checkifTextEmpty(textEixoX) ||
            checkifTextEmpty(textLegend)){
            
            showNotification("Existe campos que não foram preenchido!", type = "warning")
            
            return()
          }
          
        }
        
        context.plot <<- list(
          TITLE    = textTitle,
          EIXO.X   = textEixoX,
          EIXO.Y   = textEixoY,
          LEGEND   = textLegend,
          FORMAT.X = isolate(input$comboEixoX),
          FORMAT.Y = isolate(input$comboEixoY),
          BINARY   = binary.status
        )

        nome_estrutura <- isolate({c(input$comboEixoY,input$comboEixoX)})
        index_objs     <<- findAllObjetosEstruturaToPlot(objetos,nome_estrutura)

        if(!any(index_objs)){
          showNotification("Nenhum objeto possui esses tipos estrutura!", type = "warning")
        }else{
          sliderPosition(isolate(sliderPosition()) + 1L)
          swiperSlideNext(idSwiper)
        }
      }else if(current == 2L){
        selecao_objetos <- isolate(input$multiTipoObjects)
      
        if(length(selecao_objetos) == 0){
          showNotification("Nenhum objeto foi selecionado para grafico!", type = "warning")
        }else{
          sliderPosition(isolate(sliderPosition()) + 1L)
          swiperSlideNext(idSwiper)
        }
      }


    },ignoreInit = T,ignoreNULL = T))

  }) # end banco
 }
 
insertNewPlotComponent <- function(plot,componentPlot){
  
  child         <- paste0('plot', plot$CD_ID_PLOT)
  boxid         <- paste0('plotbox-',plot$CD_ID_PLOT)
  
  div(
    id = paste0('child-',child),
    box(
      id = boxid,
      solidHeader = T,
      collapsible = T,
      title = tags$span(plot$TITLE_PLOT,style = 'font-size: 16px;'),
      width = plot$WIDTH_PLOT,
      absolutePanel(
        height = 45,
        width = 'auto',
        top   = 5,
        right = 35,
        div(
          actionButton(inputId = paste0('btEraser', plot$CD_ID_PLOT),
                       label = '',
                       icon = icon('eraser')
          ),
          actionButton(inputId = paste0('bteye', plot$CD_ID_PLOT),
                       label = '',
                       icon = icon('window-maximize')
          )#,
          # shinyWidgets::dropdownButton(
          #   inputId = paste0('btgears', plot$CD_ID_PLOT),
          #   tags$h2("List of Input"),
          #   selectizeInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
          #   selectizeInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
          #   sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
          #   circle = FALSE,
          #   icon = icon("gear"),
          #   width = "300px",
          #   tooltip = tooltipOptions(title = "Configuração extras"))|> 
          #   tagAppendAttributes( style = 'float: right; margin-left: 5px;')|> 
          #   tagAppendAttributesFind(2,style = 'margin-left: -250px; border-color: gray;')
        )
      ),
      div(style = 'padding: 15px; height: auto; width: 100%;',componentPlot)
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
  )|>   htmlwidgets::onRender("function(el,x){
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

findAllObjetosEstruturaToPlot <- function(objetos,nome_estrutura){

   map_vec(objetos$CONFIG,function(config){
           any(map_lgl(config$COMPONENTES,function(componente){
              any(map_lgl(componente$ATRIBUTOS,function(att){
                   any(att$NAME_DATA %in% nome_estrutura)
               }))
            }))
   })
}

insertTablePlot <- function(table,context.plot,objetos.tmp,atributos.tmp,nameObject.y,nameObject.x,nameAtriburo.y,nameAtriburo.x){
  
  x <- NULL
  
  if(context.plot$BINARY){
    
    object.y   <- objetos.tmp |> filter(NAME_OBJETO == nameObject.y)
    object.x   <- objetos.tmp |> filter(NAME_OBJETO == nameObject.x)
    atributo.y <- atributos.tmp |> filter(CD_ID_STRUCT == object.y$CD_ID_STRUCT) |> filter(NAME_ATRIBUTO == nameAtriburo.y)
    atributo.x <- atributos.tmp |> filter(CD_ID_STRUCT == object.x$CD_ID_STRUCT) |> filter(NAME_ATRIBUTO == nameAtriburo.x)
    
    x <- as.data.frame(matrix(nrow = 1,ncol = 3))
    colnames(x) <- c(context.plot$EIXO.Y,context.plot$EIXO.X,"LEGENDA")
    x[context.plot$EIXO.Y] <- unique(atributo.y$NAME_ATRIBUTO)
    x[context.plot$EIXO.X] <- unique(atributo.x$NAME_ATRIBUTO)
    x["LEGENDA"] <- {
      
      if(object.y$NAME_OBJETO == object.x$NAME_OBJETO){
        object.y$NAME_OBJETO
      }else{
        paste0(object.y$NAME_OBJETO,' - ',object.x$NAME_OBJETO)
        
      }
      
    }
    
    x$OBJECT.Y   <- object.y$CD_ID_OBJECT
    x$OBJECT.X   <- object.x$CD_ID_OBJECT
    x$ATRIBUTO.Y <- unique(atributo.y$CD_ID_ATRIBUTO)
    x$ATRIBUTO.X <- unique(atributo.x$CD_ID_ATRIBUTO)
    x$FLAG       <- FALSE
    x$CD_ID      <- 0

    if(nrow(table) > 0)
    {
      status <- sapply(1:nrow(table), function(i){
        
        data <- table[i,]
        
        return(all(data[context.plot$EIXO.Y] ==  x[context.plot$EIXO.Y],
                   data[context.plot$EIXO.X] ==  x[context.plot$EIXO.X],
                   data["LEGENDA"] ==  x["LEGENDA"]))
        
      })
      
      if(status)
        x <- NULL
    }
    
  }else{
    
    object.x   <- objetos.tmp |> filter(NAME_OBJETO == nameObject.x)
    atributo.x <- atributos.tmp |> filter(CD_ID_STRUCT == object.x$CD_ID_STRUCT) |> filter(NAME_ATRIBUTO == nameAtriburo.x)
    
    x <- as.data.frame(matrix(nrow = 1,ncol = 2))
    colnames(x) <- c(context.plot$EIXO.X,"LEGENDA")
    x[context.plot$EIXO.X] <- unique(atributo.x$NAME_ATRIBUTO)
    x["LEGENDA"] <- object.x$NAME_OBJETO
    x$OBJECT.X   <- object.x$CD_ID_OBJECT
    x$ATRIBUTO.X <- unique(atributo.x$CD_ID_ATRIBUTO)
    x$FLAG       <- FALSE
    x$CD_ID      <- 0

    if(nrow(table) > 0)
    {
      status <- sapply(1:nrow(table), function(i){
        
        data <- table[i,]
        
        return(all(data[context.plot$EIXO.X] ==  x[context.plot$EIXO.X], data["LEGENDA"] ==  x["LEGENDA"]))
        
      })
      
      if(status)
        x <- NULL
    }
   
  }
  return(x)
}

insertTablePlotInit <- function(table,context.plot,objetos.tmp,atributos.tmp,dataplot){
  
  x <- NULL
  
  if(context.plot$BINARY){
    
    object.y <- objetos.tmp |> filter(CD_ID_OBJECT == dataplot$CD_ID_OBJECT_Y)
    object.x <- objetos.tmp |> filter(CD_ID_OBJECT == dataplot$CD_ID_OBJECT_X)

    if(nrow(object.x) == 0 || nrow(object.y) == 0) return(NULL)
    
    atributo.y <- atributos.tmp |> filter(CD_ID_STRUCT == object.y$CD_ID_STRUCT) |> filter(CD_ID_ATRIBUTO == dataplot$CD_ID_ATRIBUTO_Y)
    atributo.x <- atributos.tmp |> filter(CD_ID_STRUCT == object.x$CD_ID_STRUCT) |> filter(CD_ID_ATRIBUTO == dataplot$CD_ID_ATRIBUTO_X)
    
    x <- as.data.frame(matrix(nrow = 1,ncol = 3))
    colnames(x) <- c(context.plot$EIXO.Y,context.plot$EIXO.X,"LEGENDA")
    x[context.plot$EIXO.Y] <- unique(atributo.y$NAME_ATRIBUTO)
    x[context.plot$EIXO.X] <- unique(atributo.x$NAME_ATRIBUTO)
    x["LEGENDA"] <- {
      
      if(object.y$NAME_OBJETO == object.x$NAME_OBJETO){
        object.y$NAME_OBJETO
      }else{
        paste0(object.y$NAME_OBJETO,' - ',object.x$NAME_OBJETO)
        
      }
      
    }
    
    x$OBJECT.Y   <- object.y$CD_ID_OBJECT
    x$OBJECT.X   <- object.x$CD_ID_OBJECT
    x$ATRIBUTO.Y <- unique(atributo.y$CD_ID_ATRIBUTO)
    x$ATRIBUTO.X <- unique(atributo.x$CD_ID_ATRIBUTO)
    x$FLAG       <- TRUE
    x$CD_ID      <- dataplot$CD_ID_DATAPLOT
    
    if(nrow(table) > 0)
    {
      status <- sapply(1:nrow(table), function(i){
        
        data <- table[i,]
        
        return(all(data[context.plot$EIXO.Y] ==  x[context.plot$EIXO.Y],
                   data[context.plot$EIXO.X] ==  x[context.plot$EIXO.X],
                   data["LEGENDA"] ==  x["LEGENDA"]))
        
      })
      
      if(status)
        x <- NULL
    }
    
  }else{
    
    object.x   <- objetos.tmp |> filter(CD_ID_OBJECT == dataplot$CD_ID_OBJECT_X)
    
    if(nrow(object.x) == 0) return(NULL)
    
    atributo.x <- atributos.tmp |> filter(CD_ID_STRUCT == object.x$CD_ID_STRUCT) |> filter(CD_ID_ATRIBUTO == dataplot$CD_ID_ATRIBUTO_X)
    
    x <- as.data.frame(matrix(nrow = 1,ncol = 2))
    colnames(x) <- c(context.plot$EIXO.X,"LEGENDA")
    x[context.plot$EIXO.X] <- unique(atributo.x$NAME_ATRIBUTO)
    x["LEGENDA"] <- object.x$NAME_OBJETO
    x$OBJECT.X   <- object.x$CD_ID_OBJECT
    x$ATRIBUTO.X <- unique(atributo.x$CD_ID_ATRIBUTO)
    x$FLAG       <- FALSE
    x$CD_ID      <- 0
    x$FLAG       <- TRUE
    x$CD_ID      <- dataplot$CD_ID_DATAPLOT
    
    if(nrow(table) > 0)
    {
      status <- sapply(1:nrow(table), function(i){
        
        data <- table[i,]
        
        return(all(data[context.plot$EIXO.X] ==  x[context.plot$EIXO.X], data["LEGENDA"] ==  x["LEGENDA"]))
        
      })
      
      if(status)
        x <- NULL
    }
   
  }
  return(x)
}

checkifTextEmpty <- function(text){
  stringi$stri_isempty(stringr$str_trim(text))
}