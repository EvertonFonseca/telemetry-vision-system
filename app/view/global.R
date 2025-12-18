box::use(
  shiny[div, splitLayout, tags, absolutePanel, removeModal, insertUI, br,
        actionButton, observeEvent, removeUI],
  shinydashboard[menuSubItem],
  stringi[stri_isempty],
  shinyjs[onclick, runjs, delay],
  vov,
  R6
)

#' Coleção de observers com ciclo de vida controlado
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
        style = paste0('position: relative; ', 'display: ', display, '; height: ',height,'; width: ',width,';',
                       'border-color: ',border.color,'; border-style: solid; border-width: 1px;'),
        div(class = 'panelTitle',
            div(tags$span(style = 'float: left;',paste0(" ",title," ")),tooltipComponent,
                style = paste0(' position: absolute; top:-10px; left: 10px; background-color: ',
                               background.color.title,'; color: ',title.color,';')),
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

# =========================================================
# ✅ MELHORIA 1: newProgressLoader idempotente (não recria se já estiver live)
# =========================================================
#' @export
newProgressLoader <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(invisible(FALSE))

  if (is.null(session$userData$loader_count)) session$userData$loader_count <- 0L
  if (is.null(session$userData$loader_live))  session$userData$loader_live  <- FALSE

  # Já está live -> só incrementa contador e não reinsere DOM/runjs
  if (isTRUE(session$userData$loader_live)) {
    session$userData$loader_count <- session$userData$loader_count + 1L
    return(invisible(TRUE))
  }

  # Primeira ativação
  session$userData$loader_count <- session$userData$loader_count + 1L
  session$userData$loader_live  <- TRUE

  shiny::withReactiveDomain(session, {
    shinyjs::runjs("
      try{
        if(!document.getElementById('progressoLoader')){
          const div = document.createElement('div');
          div.id = 'progressoLoader';
          div.className = 'modalLoader';
          div.innerHTML = \"<div id='loadInit' class='divLoader'><div class='loaderBar'></div></div>\";
          document.body.insertBefore(div, document.body.firstChild);
        }
      }catch(e){}
    ")
  })

  invisible(TRUE)
}

# =========================================================
# ✅ MELHORIA 2: removeProgressLoader libera locks do actionWebUser e derruba loader_live
# =========================================================
#' @export
removeProgressLoader <- function(timer_ms = 0, callback = NULL,
                                 session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(invisible(FALSE))

  if (is.null(session$userData$loader_count)) session$userData$loader_count <- 0L
  if (is.null(session$userData$loader_live))  session$userData$loader_live  <- FALSE

  session$userData$loader_count <- max(0L, session$userData$loader_count - 1L)

  # ainda tem "usuários" do loader -> não remove
  if (session$userData$loader_count > 0L) return(invisible(TRUE))

  alive <- TRUE
  session$onSessionEnded(function() alive <<- FALSE)

  later::later(function() {
    if (!isTRUE(alive)) return(invisible(NULL))
    shiny::withReactiveDomain(session, {

      if (!is.null(callback)) try(callback(), silent = TRUE)

      shinyjs::runjs("try{document.getElementById('progressoLoader')?.remove();}catch(e){}")

      # ✅ flag live OFF (agora realmente não está mais ativo)
      session$userData$loader_live <- FALSE

      # ✅ libera locks do actionWebUser (tudo que estava travado por essa sessão)
      lock_keys <- session$userData$.actionWebUser_lock_keys
      if (length(lock_keys)) {
        for (k in lock_keys) session$userData[[k]] <- FALSE
      }
      session$userData$.actionWebUser_lock_keys <- character()

    })
  }, timer_ms / 1000)

  invisible(TRUE)
}

# =========================================================
# ✅ MELHORIA 3: actionWebUser com lock por sessão/usuário (não duplica callback)
# - só libera quando removeProgressLoader zerar (loader_count==0)
# =========================================================
#' @export
actionWebUser <- function(callback,
                          session = shiny::getDefaultReactiveDomain(),
                          auto.remove = TRUE,
                          new.progress = TRUE,
                          delay = 500,
                          lock_id = "default",
                          on_busy = c("ignore","notify")) {

  on_busy <- match.arg(on_busy)

  # Se for chamado fora de sessão (boot do processo), não tenta UI
  if (is.null(session)) {
    try(callback(), silent = TRUE)
    return(invisible(FALSE))
  }

  # infra de lock por sessão
  if (is.null(session$userData$.actionWebUser_lock_keys))
    session$userData$.actionWebUser_lock_keys <- character()

  lock_key <- paste0(".actionWebUser_busy__", lock_id)

  # Se já tem ação pendente/rodando -> não agenda outra
  if (isTRUE(session$userData[[lock_key]])) {
    if (identical(on_busy, "notify")) {
      try(shiny::showNotification(
        "Ação já em execução. Aguarde finalizar…",
        type = "message", duration = 2
      ), silent = TRUE)
    }
    return(invisible(FALSE))
  }

  # marca busy imediatamente (bloqueia spam antes do later)
  session$userData[[lock_key]] <- TRUE
  session$userData$.actionWebUser_lock_keys <- unique(c(session$userData$.actionWebUser_lock_keys, lock_key))

  alive <- TRUE
  session$onSessionEnded(function() alive <<- FALSE)

  # cria loader (idempotente agora)
  if (isTRUE(new.progress)) {
    newProgressLoader(session)
  } else {
    # se não há loader, não faz sentido travar até removeProgressLoader
    session$userData[[lock_key]] <- FALSE
    session$userData$.actionWebUser_lock_keys <- setdiff(session$userData$.actionWebUser_lock_keys, lock_key)
  }

  later::later(function() {
    if (!isTRUE(alive)) return(invisible(NULL))

    shiny::withReactiveDomain(session, {
      try(callback(), silent = TRUE)

      if (isTRUE(auto.remove) && isTRUE(new.progress)) {
        # removeProgressLoader vai liberar locks quando loader_count chegar em 0
        removeProgressLoader(timer_ms = 0, session = session)
      }
    })
  }, delay / 1000)

  invisible(TRUE)
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

    app_base    <- getwd()
    reports_dir <- normalizePath(file.path(app_base, base_subdir),winslash = "/", mustWork = FALSE)

    if (dir.exists(reports_dir)) {
      unlink(list.files(reports_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
             recursive = TRUE, force = TRUE)
    } else {
      dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)
    }

    dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

    # Evita erro se já existir um path "reports"
    try(shiny::removeResourcePath("reports"), silent = TRUE)
    shiny::addResourcePath("reports", reports_dir)

    options(.reports_dir = reports_dir)  # guardamos pra reuso
    done <<- TRUE
    invisible(NULL)
  }
})
