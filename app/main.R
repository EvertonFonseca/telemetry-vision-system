box::use(
  ./view/init[layout_ui = ui,layout_server = server]
)


#' @export
ui <- function(id){
  layout_ui(id)
} 

#' @export
server <- function(id){
  layout_server(id)
}
