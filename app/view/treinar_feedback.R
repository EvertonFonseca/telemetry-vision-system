box::use(
  treinar = ./treinar[uiNewTreinar, dispose]
)

#' @export
uiFeedbackTreinar <- function(ns, input, output, session, callback) {
  treinar$uiNewTreinar(
    ns = ns,
    input = input,
    output = output,
    session = session,
    callback = callback,
    dialogTitle = "Feedback Human"
  )
}

#' @export
dispose <- function(session, key = "setor_private") {
  treinar$dispose(session, key = key)
}
