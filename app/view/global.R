box::use(
 shiny[div,splitLayout,tags,absolutePanel,removeModal,insertUI,br,actionButton,observeEvent,removeUI],
 shinydashboard[menuSubItem],
 stringi[stri_isempty],
 shinyjs[onclick,runjs,delay],
 vov,
 R6
)

#' Coleção de observers com ciclo de vida controlado
#'
#' Mantém uma lista de objetos retornados por observe/observeEvent, permitindo
#' adicionar, remover, limpar e destruir (chamar $destroy() em todos).
#'
#' Métodos:
#'  - $add(o)         : adiciona um observer (ignora NULL)
#'  - $clear()        : destroy() em todos e esvazia a lista
#'  - $remove(obs)    : remove por referência (identical) ou por índice numérico
#'  - $destroy()      : alias para $clear(); usado para desalocar
#'  - $items (active) : acesso somente-leitura à lista interna
#'

#' @export
ObserversBag <- R6$R6Class(
  "ObserversBag",
  public = list(
    initialize = function() {
      private$listobserves <- list()
    },

    add = function(o) {
      if (!is.null(o)) private$listobserves <- append(private$listobserves, list(o))
      invisible(self)
    },

    clear = function() {
      lapply(private$listobserves, function(x) try(x$destroy(), silent = TRUE))
      private$listobserves <- list()
      invisible(self)
    },

    remove = function(o) {
      idx <- integer()
      if (is.numeric(o)) {
        idx <- intersect(as.integer(o), seq_along(private$listobserves))
      } else {
        idx <- which(vapply(private$listobserves, function(x) identical(x, o), logical(1)))
      }
      if (length(idx)) {
        lapply(idx, function(i) try(private$listobserves[[i]]$destroy(), silent = TRUE))
        private$listobserves <- private$listobserves[-idx]
      }
      invisible(self)
    },

    destroy = function() {
      self$clear()
      invisible(NULL)
    }
  ),

  private = list(
    listobserves = list(),

    # finalize agora é PRIVATE (requisito do R6 >= 2.4.0)
    finalize = function() {
      lapply(private$listobserves, function(x) try(x$destroy(), silent = TRUE))
    }
  )
)

# Factory preservando a sua assinatura atual
#' @export
newObserve <- function() ObserversBag$new()

#' @export
MenuSubItem  <- function(id = NULL,
                         text,
                         tabName = NULL,
                         href = NULL,
                         newtab = TRUE,
                         icon = shiny::icon("angle-double-right"),
                         selected = NULL,
                         hide = FALSE) {
  
  styleinline <- 'margin-left: 8px; padding: 5px; height: 35px;'
  styleinline <- if(hide) paste0(styleinline," display:none !important;")
  component     <- div(
    id = id,
    style = styleinline,
    splitLayout(
      cellWidths = 'auto',
      tags$span(icon),
      div(style = 'margin-left: 5px;',
          menuSubItem(
            text,
            tabName = tabName,
            href = href,
            newtab = newtab,
            icon = NULL,
            selected = selected
          )
      )
    )
  )
  return(component)
}

#' @export
panelTitle <- function(id = '',
                       display = 'block',
                       title = '',
                       width = 'auto',
                       height = 'auto',
                       id.tooltip = '',
                       title.tooltip = '',
                       title.color   = 'gray',
                       background.color.title = 'white',
                       border.color = 'gray',
                       children) {
  
  if(!stri_isempty(id.tooltip))
    tooltipComponent <- div(
      id = id.tooltip,
      style = 'float: left; margin-left: 5px',
      icon('info-circle'),
      bsTooltip(
        id.tooltip,
        title.tooltip,
        "right",
        options = list(container = "body")
      )
    )
  else
    tooltipComponent <- NULL
  
  return(
    div(id = id,
        style = paste0('position: relative; ', 'display: ', display, '; height: ',height,'; width: ',width,';','border-color: ',border.color,'; border-style: solid; border-width: 1px;'),
        div(class = 'panelTitle',
            div(tags$span(style = 'float: left;',paste0(" ",title," ")),tooltipComponent,
                style = paste0(' position: absolute; top:-10px; left: 10px; background-color: ',background.color.title,'; color: ',title.color,';')),
            div(style = 'margin-top: 5px; z-index: 99999999;',children))))
}

#' @export
dialogTitleClose <- function(id,title,active.bt.close = TRUE){
  
  btcloser <- NULL
  
  if(active.bt.close)
    btcloser <- absolutePanel(id = id,right = 15,top = 10,'x',style = 'cursor: pointer; font-weight: bold;')
  
  container <- div(title,btcloser)
  return(container)
}

#' @export
removeModalClear <- function(session){
  removeModal(session)
  runjs('$(".modal-body").html("");')
}

#' @export
shinySetInputValue <- function(id,value = NULL){

  value <- ifelse(is.null(value),'null',value)
  value <- ifelse(stringi::stri_isempty(value),"''",value)
  
  if(is.vector(id))
  {
   js <- sapply(id, function(x)  paste0("Shiny.setInputValue('",x,"',",value,",{priority:'event'});"))
   runjs(paste0(js,collapse = '\n '))
    
  }else{
    runjs(paste0("Shiny.setInputValue('",id,"',",value,",{priority:'event'});"))
  }
}

#' @export
play_sound <- function(session,id_local) {
  session$sendCustomMessage("play-audio", list(id = id_local))
}

#' @export
debugLocal <- function(expr) {
  
   debug(expr)
   x <- expr()
   undebug(expr)
   return(x)
}

#' @export
console <- function(text){
  runjs(paste0("console.log('",text,"');"))
}

#Cria um dialog dinamico
#' @export
messageAlerta <- function(input,ns,title = 'Mensagem de Alerta!',message,callback.yes,callback.no) {
  
  obs <- newObserve()

  insertUI(
    selector = "body",
    ui = div(
      id    = 'notificationWarning',
      style = "position: fixed!important; top: 0; left: 0; bottom: 0; right: 0; background-color: rgba(45,55,65,0.6); z-index: 999999; color: black; font-size: 15px;",
      vov$slide_in_right(absolutePanel(
        top = 25,
        right = 25,
        class = 'dialogChip',
        width = 300,
        div(
          style = 'padding: 15px',
          div(
            style = 'padding: 5px; border-style : solid; border-color: gray; border-width: 1px; border-top: none;  border-left: none;  border-right: none;',
            title
          ),
          br(),
          div(message),
          br(),
          div(
            style = 'padding: 5px; border-style : solid; border-color: gray; border-width: 1px; border-bottom: none;  border-left: none;  border-right: none;',
            div(
              style = 'float: right; padding-bottom: 5px;',
              actionButton(ns('btYesnotification'),'Sim'),
              actionButton(ns('btNonotification'),'Não')
            )
          )
        )
      ),duration = 'faster')
    )
  )

  obs$add(observeEvent(input$btYesnotification,{

    runjs("document.getElementById('notificationWarning').remove();")
    obs$destroy()
    callback.yes()
    
  },ignoreInit = TRUE))
  
  obs$add(observeEvent(input$btNonotification,{

    runjs("document.getElementById('notificationWarning').remove();")
    obs$destroy()
    callback.no()

  },ignoreInit = TRUE))

}
# Change Text's multiInput
#' @export
changetextPlaceHolder <- function(text = "Filtro ..."){
  
  delay(100,{
    runjs(paste0('
                  let lista = document.getElementsByClassName("search-input");\n
                  for(i = 0; i < lista.length; i++){ lista[i].placeholder = "',text,'"; }
                '))
  })
}

#' @export
tagAppendChildFind <- function(tag,target,child){
  
  tag$children[[target]]$children <- list.append(tag$children[[target]]$children,child)
  tag
}
#' @export
tagAppendAttributesFind <- function(tag,target,...){
  
  tag$children[[target]]$attribs <- c(tag$children[[target]]$attribs,...)
  tag
}
#' @export
tagAppendAttributesFindSub <- function(tag,target,...){
  
  if(length(target) == 2)
  {
    tag$children[[target[1]]]$children[[target[2]]]$attribs <- c(tag$children[[target[1]]]$children[[target[2]]]$attribs,...)
    tag
  }
  else if(length(target) == 3)
  {
    tag$children[[target[1]]]$children[[target[2]]]$children[[target[3]]]$attribs <- c(tag$children[[target[1]]]$children[[target[2]]]$children[[target[3]]]$attribs,...)
    tag
  }
}

#' @export
deleteElement <- function(id){
   runjs(paste0("document.getElementById('",id,"').remove();"))
}

#' @export
set_readonly_js <- function(id, readonly = FALSE, session = shiny::getDefaultReactiveDomain()){
  full_id <- if (!is.null(session)) session$ns(id) else id
  if (isTRUE(readonly)) {
    js <- sprintf("
      (function(){
        var el = $('#%s');
        if (!el.length) return;
        el.attr('readonly', true)
          .attr('tabindex','-1')
          .addClass('textarea-ro')
          .trigger('blur');
        // Se por acaso ganhar foco, perde imediatamente
        el.off('focus.readonly').on('focus.readonly', function(){ this.blur(); });
      })();", full_id)
  } else {
    js <- sprintf("
      (function(){
        var el = $('#%s');
        if (!el.length) return;
        el.removeAttr('readonly')
          .removeAttr('tabindex')
          .removeClass('textarea-ro');
        el.off('focus.readonly');
      })();", full_id)
  }
  runjs(js)
}
#' @export
newProgressLoader <- function(){
  
   insertUI(
      selector = 'body',
      where = 'afterBegin',
      ui =
        div(id = 'progressoLoader',class = "modalLoader",
            div(
              id = 'loadInit',
              class = "divLoader",
              div(class = "loaderBar")
            )),
      immediate = F
    )
  
}
#' @export
removeProgressLoader <- function(timer = 1000,callback = NULL){
  
  if(timer > 0){
    
    delay(timer,{
      
      if(!is.null(callback))
        callback()
      #remove loader
      runjs("try{document.getElementById('progressoLoader').remove();}catch(e){}")
      assign('status.loader',value = FALSE,immediate = T,envir = .GlobalEnv)
    })
    
  }
  else{
    
    if(!is.null(callback))
       callback()
    
    #remove loader
    runjs("try{document.getElementById('progressoLoader').remove();}catch(e){}")
    assign('status.loader',value = FALSE,immediate = T,envir = .GlobalEnv)

  }

}

#' @export
actionWebUser <- function(callback,auto.remove = TRUE,new.progess = TRUE,delay = 500){
  
  if(new.progess)
  {
    newProgressLoader()
    
    delay(delay,{
      try(callback,silent = TRUE)
      if(auto.remove)
        removeProgressLoader(delay)
    })
  }
  else{
    try(callback,silent = TRUE)
    if(auto.remove)
      removeProgressLoader(delay)
  }

}

#' @export
moveScrollToUp <- function(){
    runjs(
    "var el = document.querySelector('#shiny-modal .modal-body');
     if (el) el.scrollTo({top: 0, behavior: 'auto'});"
  )
}

#' @export
.init_reports_path <- local({
  done <- FALSE
  function(base_subdir = file.path("app", "www", "reports")) {
    if (done) return(invisible(NULL))

    app_base   <- getwd()
    reports_dir <- normalizePath(file.path(app_base, base_subdir),
                                 winslash = "/", mustWork = FALSE)
    dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

    # Evita erro se já existir um path "reports"
    try(shiny::removeResourcePath("reports"), silent = TRUE)
    shiny::addResourcePath("reports", reports_dir)

    options(.reports_dir = reports_dir)  # guardamos pra reuso
    done <<- TRUE
    invisible(NULL)
  }
})

