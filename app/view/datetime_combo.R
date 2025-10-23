# app/components/datetime_combo.R
# Componente datetime (dateInput + shinyTime::timeInput)
# Uso: box::use(app/components/datetime_combo[...])

box::use(
  shiny[...],
  shinyTime
)

#' UI do componente
#' @export
ui <- function(ns, id, label_date = "Data:", label_time = "Hora:",
               value = NULL, tz = "UTC", width = "100%", seconds = FALSE) {
  # resolve valor inicial
  if (is.null(value)) {
    d <- Sys.Date()
    t <- strptime("00:00:00", "%H:%M:%S")
  } else if (inherits(value, "POSIXt")) {
    d <- as.Date(value, tz = tz)
    t <- strptime(format(value, if (seconds) "%H:%M:%S" else "%H:%M", tz = tz),
                  if (seconds) "%H:%M:%S" else "%H:%M")
  } else if (inherits(value, "Date")) {
    d <- value
    t <- strptime("00:00:00", "%H:%M:%S")
  } else {
    # assume "dd/mm/yyyy HH:MM[:SS]"
    fmts <- c("%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%Y-%m-%d %H:%M:%S")
    dt <- NA
    for (f in fmts) {
      dt <- try(as.POSIXct(strptime(value, f), tz = tz), silent = TRUE)
      if (!inherits(dt, "try-error") && !is.na(dt)) break
    }
    if (is.na(dt)) {
      d <- Sys.Date()
      t <- strptime("00:00:00", "%H:%M:%S")
    } else {
      d <- as.Date(dt, tz = tz)
      t <- strptime(format(dt, if (seconds) "%H:%M:%S" else "%H:%M"),
                    if (seconds) "%H:%M:%S" else "%H:%M")
    }
  }

  shiny::tagList(
    shiny::dateInput(
      ns(paste0(id, "_date")), label_date,
      value = d, format = "dd/mm/yyyy", language = "pt-BR", width = width
    ),
    shinyTime::timeInput(
      ns(paste0(id, "_time")), label_time,
      value = t, seconds = seconds, width = width
    )
  )
}

#' Lê como POSIXct (UTC por padrão)
#' @export
get <- function(input, ns, id, tz = "UTC") {
  d <- input[[ns(paste0(id, "_date"))]]
  t <- input[[ns(paste0(id, "_time"))]]
  if (is.null(d) || is.null(t)) return(NA)

  # shinyTime pode entregar POSIXt ou string "HH:MM[:SS]"
  t_str <- if (inherits(t, "POSIXt")) format(t, "%H:%M:%S") else as.character(t)
  fmt   <- if (nchar(t_str) == 5) "%d/%m/%Y %H:%M" else "%d/%m/%Y %H:%M:%S"

  as.POSIXct(strptime(
    paste(format(as.Date(d), "%d/%m/%Y"), t_str), fmt
  ), tz = tz)
}

#' Atualiza valores e limites
#' @export
update <- function(session, ns, id, value, tz = "UTC",
                   minDate = NULL, maxDate = NULL) {
  if (is.null(value) || is.na(value)) return(invisible())
  d <- as.Date(value, tz = tz)
  shiny::updateDateInput(session, ns(paste0(id, "_date")),
                         value = d, min = minDate, max = maxDate)
  shinyTime::updateTimeInput(
    session, ns(paste0(id, "_time")),
    value = strptime(format(value, "%H:%M:%S", tz = tz), "%H:%M:%S")
  )
}

#' Validação simples (texto de erro ou NULL)
#' @export
validate_range <- function(dt_begin, dt_end) {
  if (is.na(dt_begin) || is.na(dt_end)) return("Datas inválidas.")
  if (dt_end < dt_begin) return("A data/hora final deve ser maior ou igual à inicial.")
  NULL
}
