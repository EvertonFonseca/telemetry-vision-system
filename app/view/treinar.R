box::use(
   shiny[...],
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
    ]
  )

#' @export
uiNewTreinar <- function(ns,input,output,session,callback){
  
  id       <- ns('dialogObj')
  cssStyle <- list()
  cssStyle[[paste0(' #parent',id,' .modal-dialog')]]  <- paste0('width: 95% !important; height: 90% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-content')]] <- paste0('width: 100% !important; height: 100% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-body')]]    <- paste0('width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto;')
  
  showModal(
    session = session,
    div(
      id = paste0('parent', id),
      style = paste0("height: 80%; overflow: hidden;"),
      shinyjs::inlineCSS(cssStyle),
      dialogModal(
        title = textOutput(ns("titleTexto")),
        size = 'm',
        div(),  
        footer = uiOutput(ns('uiFooter')))))
        
        output$uiFooter <- renderUI({
          
          tagList(actionButton(ns("btSair"), label = "Voltar",icon = icon("arrow-left")),actionButton(ns('btSalvar'),class = "btn-warning",label = "Atualizar",icon = icon("save")))
          
       })
        
}