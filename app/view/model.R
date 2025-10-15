box::use(shinyWidgets[confirmSweetAlert],
   shiny[observeEvent,modalDialog],
   htmltools[tags],
   shinyjs[...],
   ./global[play_sound])

#' @export
dialogModal <- function(title,size = 'm',...,footer){

   modalDialog(
      title = title,
      ...,
      size = size,
      footer = footer
   )
}

#' @export
dialogConfirm <- function(session,id,title,text,type = 'success',bt_labels = c("Não","Sim")){

   play_sound(session,"soundSessionDialog")

    confirmSweetAlert(
      session = session,
      inputId = id,
      type = type,
      title = title,
      text = text,
      btn_labels = bt_labels,
      closeOnClickOutside = FALSE,
      showCloseButton = FALSE
   )
   
   runjs("window.scrollTo(0,0);")
   
   return(invisible())
}
