# setor_agenda.R
box::use(
  shiny[
    NS,
    showModal, removeModal,
    fluidRow, column, tagList, div, br, tags,
    selectInput, actionButton, uiOutput, renderUI,
    checkboxGroupInput, textInput,
    observeEvent, reactiveVal,
    updateSelectInput, updateTextInput, updateCheckboxGroupInput,
    showNotification, req, icon
  ],
  shinyjs[inlineCSS],
  shinyWidgets[airDatepickerInput, timepickerOptions, updateAirDateInput,prettyToggle],
  DT,
  dplyr[...],
  stringr,
  stringi,
  jsonlite,
  lubridate,
  . / model[...],
  ./ global[
    dialogTitleClose,
    panelTitle,
    removeModalClear,
    newObserve,
    messageAlerta
  ],
  stats[...],
  dbp = ../infra/db_pool,
  db  = ../infra/database,
  .. / logic / setor_dao[selectAllSetors]
)

# ------------------------------------------------------------
# util: null-coalesce (evita depender de rlang)
# ------------------------------------------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

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

# ID simples pra agenda (por sessão)
.next_agenda_id <- function(df) {
  if (is.null(df) || !nrow(df)) return(1L)
  suppressWarnings(max(df$ID_AGENDA, na.rm = TRUE)) + 1L
}

.empty_agenda_df <- function() {
  data.frame(
    ID_AGENDA    = integer(),
    CD_ID_SETOR  = integer(),
    NAME_SETOR   = character(),
    TITULO       = character(),
    ATIVO        = logical(),      # <<< NOVO: controlado pelo toggle (DT)
    TIPO         = character(),    # "ONCE" / "WEEKLY"
    DT_BEGIN     = as.POSIXct(character(), tz = "UTC"),
    DT_END       = as.POSIXct(character(), tz = "UTC"),
    DAYS_WEEK    = character(),
    WINDOWS_JSON = character(),
    CREATED_AT   = as.POSIXct(character(), tz = "UTC"),
    UPDATED_AT   = as.POSIXct(character(), tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

# garante compatibilidade com df antigo
.normalize_agenda_df <- function(df) {
  if (is.null(df) || !nrow(df)) return(.empty_agenda_df())

  need_cols <- names(.empty_agenda_df())
  for (nm in need_cols) {
    if (!nm %in% names(df)) {
      df[[nm]] <- .empty_agenda_df()[[nm]]
    }
  }

  df$ID_AGENDA    <- as.integer(df$ID_AGENDA)
  df$CD_ID_SETOR  <- as.integer(df$CD_ID_SETOR)
  df$NAME_SETOR   <- as.character(df$NAME_SETOR)
  df$TITULO       <- as.character(df$TITULO)
  df$ATIVO        <- as.logical(df$ATIVO)
  df$TIPO         <- as.character(df$TIPO)
  df$DAYS_WEEK    <- as.character(df$DAYS_WEEK)
  df$WINDOWS_JSON <- as.character(df$WINDOWS_JSON)

  df$DT_BEGIN   <- as.POSIXct(df$DT_BEGIN, tz = "UTC")
  df$DT_END     <- as.POSIXct(df$DT_END, tz = "UTC")
  df$CREATED_AT <- as.POSIXct(df$CREATED_AT, tz = "UTC")
  df$UPDATED_AT <- as.POSIXct(df$UPDATED_AT, tz = "UTC")

  # default: TRUE
  df$ATIVO[is.na(df$ATIVO)] <- TRUE
  df
}

.week_labels <- function() c("SEG","TER","QUA","QUI","SEX","SAB","DOM")

# ------------------------------------------------------------
# TZ + helpers (date-only no UI, storage em UTC)
# ------------------------------------------------------------
.app_tz <- "America/Sao_Paulo"

.as_date_safe <- function(x) {
  if (is.null(x) || identical(x, "")) return(as.Date(NA))
  as.Date(x)
}

.date_begin_utc <- function(d) {
  lubridate::with_tz(lubridate::as_datetime(d, tz = .app_tz), "UTC")
}

.date_end_utc <- function(d) {
  lubridate::with_tz(
    lubridate::as_datetime(d, tz = .app_tz) + lubridate::days(1) - lubridate::seconds(1),
    "UTC"
  )
}

.format_date_br <- function(x_utc) {
  if (is.null(x_utc) || is.na(x_utc)) return("")
  format(lubridate::with_tz(x_utc, .app_tz), "%d/%m/%Y")
}

.clean_title <- function(x) {
  x <- stringr::str_squish(stringr::str_trim(x %||% ""))
  if (!nzchar(x)) return("")
  toupper(x)
}

# ------------------------------------------------------------
# WINDOWS_JSON robusto (evita "$ operator is invalid for atomic vectors")
# ------------------------------------------------------------
.parse_windows_json <- function(x) {
  if (is.null(x) || !nzchar(x)) return(list())

  w <- tryCatch(
    jsonlite::fromJSON(x, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(w)) return(list())

  # Caso 1: lista de objetos
  if (is.list(w) && length(w) && is.list(w[[1]])) {
    out <- lapply(w, function(z) {
      list(
        begin = as.character(z$begin %||% ""),
        end   = as.character(z$end   %||% "")
      )
    })
    return(out)
  }

  # Caso 2: lista colunar
  if (is.list(w) && !is.null(w$begin) && !is.null(w$end)) {
    b <- w$begin
    e <- w$end
    n <- min(length(b), length(e))
    out <- lapply(seq_len(n), function(i) {
      list(
        begin = as.character(b[[i]] %||% b[i] %||% ""),
        end   = as.character(e[[i]] %||% e[i] %||% "")
      )
    })
    return(out)
  }

  # Caso 3: data.frame
  if (is.data.frame(w) && all(c("begin","end") %in% names(w))) {
    out <- lapply(seq_len(nrow(w)), function(i) {
      list(begin = as.character(w$begin[i] %||% ""), end = as.character(w$end[i] %||% ""))
    })
    return(out)
  }

  list()
}

.windows_to_text <- function(wlist) {
  if (!length(wlist)) return("")
  parts <- vapply(wlist, function(z) {
    b <- as.character(z$begin %||% "")
    e <- as.character(z$end   %||% "")
    if (!nzchar(b) || !nzchar(e)) return("")
    paste0(b, "-", e)
  }, character(1))
  parts <- parts[nzchar(parts)]
  paste(parts, collapse = "; ")
}

# ------------------------------------------------------------
# API pública: obter dataframe de agendamentos dessa sessão
# ------------------------------------------------------------
#' @export
get_agenda_df <- function(session, key = "setor_private") {
  e <- .get_private(session, key)
  df <- e$agenda_df
  if (is.null(df)) df <- .empty_agenda_df()
  .normalize_agenda_df(df)
}

# ------------------------------------------------------------
# Modal principal: Setor Agenda
# ------------------------------------------------------------
#' @export
uiSetorAgenda <- function(ns, input, output, session, callback = function() {}) {

  e   <- .get_private(session)
  obs <- newObserve()

  setores_df <- selectAllSetors(dbp$get_pool())

  if (is.null(e$agenda_df)) e$agenda_df <- .empty_agenda_df()
  e$agenda_df <- .normalize_agenda_df(e$agenda_df)

  rv_agenda  <- shiny::reactiveVal(e$agenda_df)
  rv_windows <- shiny::reactiveVal(list())
  rv_edit_id <- shiny::reactiveVal(NA_integer_)

  # registra observers do toggle por id (por sessão)
  if (is.null(e$watch_active_ids)) e$watch_active_ids <- new.env(parent = emptyenv())

  .persist <- function(df) {
    df <- .normalize_agenda_df(df)
    e$agenda_df <- df
    rv_agenda(df)
  }

  .reset_editor <- function() {
    rv_edit_id(NA_integer_)
    rv_windows(list())
    updateTextInput(session, "titulo", value = "")
    updateSelectInput(session, "tipo", selected = "ONCE")
    updateCheckboxGroupInput(session, "dias_week", selected = c("SEG","TER","QUA","QUI","SEX"))
    updateTextInput(session, "win_begin", value = "08:00")
    updateTextInput(session, "win_end", value = "17:00")
    shinyWidgets::updateAirDateInput(session, "dt_begin", value = Sys.Date())
    shinyWidgets::updateAirDateInput(session, "dt_end", value = NULL)
  }

  # ----------------------------
  # Editor UI
  # ----------------------------
  uiEditor <- function(ns) {
    tagList(
      fluidRow(
        column(
          6,
          selectInput(
            ns("tipo"),
            "Tipo",
            choices = c("UMA VEZ" = "ONCE", "SEMANAL" = "WEEKLY"),
            selected = "ONCE"
          )
        ),
        column(
          6,
          div(
            style = "margin-top: 24px; text-align:right;",
            actionButton(ns("btLimparEditor"), "Limpar editor", icon = icon("eraser")),
            actionButton(ns("btSalvarAgenda"), "Salvar", class = "btn-success", icon = icon("save"))
          )
        )
      ),

      br(),

      fluidRow(
        column(
          12,
          textInput(
            inputId = ns("titulo"),
            label   = "Título do agendamento",
            value   = "",
            placeholder = "Ex: Manutenção preventiva / Setup / Inspeção / etc."
          ),
          tags$small(style="opacity:.75;", "O Ativar/Desativar é controlado direto na tabela (toggle).")
        )
      ),

      br(),

      fluidRow(
        column(
          6,
          shinyWidgets::airDatepickerInput(
            inputId    = ns("dt_begin"),
            label      = "DT_BEGIN (início)",
            value      = Sys.Date(),
            timepicker = FALSE,
            language   = "pt-BR",
            dateFormat = "dd/MM/yyyy",
            autoClose  = TRUE,
            todayButton = TRUE,
            clearButton = FALSE,
            addon = "none"
          )
        ),
        column(
          6,
          shinyWidgets::airDatepickerInput(
            inputId    = ns("dt_end"),
            label      = "DT_END (fim) — opcional",
            value      = NULL,
            timepicker = FALSE,
            language   = "pt-BR",
            dateFormat = "dd/MM/yyyy",
            autoClose  = TRUE,
            todayButton = TRUE,
            clearButton = TRUE,
            addon = "none"
          )
        )
      ),

      br(),
      uiOutput(ns("uiTipoBody"))
    )
  }

  # ----------------------------
  # modal
  # ----------------------------
  id       <- ns('dialogAgendaSetor')
  cssStyle <- list()
  cssStyle[[paste0(' #parent',id,' .modal-dialog')]]  <- paste0('height: 80% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-content')]] <- paste0('width: 100% !important; height: 100% !important;')
  cssStyle[[paste0(' #parent',id,' .modal-body')]]    <- paste0('width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto;')
  
  showModal(
    session = session,
    div(
      id = paste0('parent', id),
      style = paste0("height: 90%;"),
      inlineCSS(cssStyle),
      dialogModal(
        title = "Agenda do Setor",
        size  = "l",
        div(
          shinyjs::inlineCSS(paste0("
            #", ns("setor_id"), " { text-transform: uppercase; }
            .modal-title { font-weight: 800; }
            .shiny-input-container label { font-weight: 700; }
            .dataTables_wrapper .dataTable tbody td { padding: 8px 10px; }
            .badge-soft { background:#f2f4f8; border:1px solid #e5e7eb; padding:2px 8px; border-radius:999px; }

            #", ns("tbAgenda"), "_wrapper .dataTables_scrollBody {
              max-height: 45vh !important;
              height: 45vh !important;
            }
          ")),
          fluidRow(
            column(
              width = 12,
              panelTitle(
                title = "Selecionar setor",
                background.color.title = "white",
                title.color = "black",
                border.color = "lightgray",
                children = tagList(
                  selectInput(
                    inputId = ns("setor_id"),
                    label   = "Setor",
                    choices = setNames(setores_df$CD_ID_SETOR, toupper(setores_df$NAME_SETOR))
                  ),
                  br(),
                  actionButton(ns("btNovo"), "Novo agendamento", icon = icon("plus"), class = "btn-primary"),
                  actionButton(ns("btFechar"), "Fechar", icon = icon("times"))
                )
              )
            ),
            column(
              width = 12,
              uiOutput(ns("uiBody"))
            )
          )
        ),
        footer = tagList(
          actionButton(inputId = ns("btSair"),  "Sair",   icon = icon("arrow-left")),
          actionButton(inputId = ns("btClear"), "Limpar", icon = icon("eraser")),
          actionButton(inputId = ns("btSalvar"),"Salvar", class = "btn-primary", icon = icon("save"))
        )
      )
    )
  )

  # ----------------------------
  # Body
  # ----------------------------
  output$uiBody <- renderUI({
    req(input$setor_id)
    tagList(
      br(),
      panelTitle(
        title = "Agendamentos cadastrados",
        background.color.title = "white",
        title.color = "black",
        border.color = "lightgray",
        children = div(style = "border: 1px solid #fff; overflow-x: auto;",
                       DT::dataTableOutput(ns("tbAgenda")))
      ),
      br(),
      panelTitle(
        title = "Editor de agendamento",
        background.color.title = "white",
        title.color = "black",
        border.color = "lightgray",
        children = uiEditor(ns)
      )
    )
  })

  # ----------------------------
  # Tipo body
  # ----------------------------
  output$uiTipoBody <- renderUI({
    req(input$tipo)

    if (input$tipo == "ONCE") {
      div(style = "opacity:.9; font-size: 12px;",
          icon("info-circle"),
          " Uma vez: use DT_BEGIN e (opcionalmente) DT_END.")
    } else {
      tagList(
        checkboxGroupInput(
          inputId = ns("dias_week"),
          label   = "Dias da semana",
          choices = .week_labels(),
          selected = c("SEG","TER","QUA","QUI","SEX"),
          inline = TRUE
        ),
        br(),
        fluidRow(
          column(5, textInput(ns("win_begin"), "Horário início (HH:MM)", value = "08:00")),
          column(5, textInput(ns("win_end"),   "Horário fim (HH:MM)",    value = "17:00")),
          column(2, div(style = "margin-top: 24px;",
                        actionButton(ns("btAddWindow"), "Adicionar", icon = icon("plus"))))
        ),
        br(),
        div(
          style = "border: 1px solid #ddd; border-radius: 12px; padding: 10px;",
          uiOutput(ns("uiWindowsList"))
        )
      )
    }
  })

  output$uiWindowsList <- renderUI({
    w <- rv_windows()
    if (!length(w)) return(div(style="opacity:.7;", icon("clock"), " Nenhum horário adicionado ainda."))

    tags$div(
      tags$strong("Horários do agendamento:"),
      tags$ul(
        lapply(seq_along(w), function(i){
          it <- w[[i]]
          tags$li(
            tags$span(class = "badge-soft", paste0(it$begin, " → ", it$end)),
            tags$a(
              href = "#",
              onclick = paste0(
                'Shiny.setInputValue("', ns("rmWindowIdx"), '", ', i, ', {priority:"event"}); return false;'
              ),
              style = "margin-left:10px; color:#b00;",
              icon("trash")
            )
          )
        })
      )
    )
  })

  # ----------------------------
  # DT (toggle ATIVAR no lugar do ID)
  # ----------------------------
  output$tbAgenda <- DT::renderDataTable({

    setor_id <- input$setor_id
    req(setor_id)

    df <- rv_agenda() |> dplyr::filter(.data$CD_ID_SETOR == as.integer(setor_id))
    if (!nrow(df)) return(NULL)

    DT::datatable({
      df2 <- df |>
        dplyr::mutate(
          `INÍCIO` = vapply(.data$DT_BEGIN, .format_date_br, character(1)),
          `FIM`    = ifelse(is.na(.data$DT_END), "", vapply(.data$DT_END, .format_date_br, character(1))),
          `DIAS`   = ifelse(.data$TIPO == "WEEKLY", .data$DAYS_WEEK, ""),
          `HORÁRIOS` = ifelse(
            .data$TIPO == "WEEKLY",
            vapply(.data$WINDOWS_JSON, function(x) {
              wl <- .parse_windows_json(x)
              .windows_to_text(wl)
            }, character(1)),
            ""
          ),
          `ATIVAR` = vapply(.data$ID_AGENDA, function(id_ag) {
            valueAtivo <- isTRUE(df$ATIVO[df$ID_AGENDA == id_ag][1])
            as.character(
              div(style = "margin-top: 5px;",
              shinyWidgets::prettyCheckbox(
                inputId = ns(paste0("checkboxAtivoAgenda_", id_ag)),
                label   = "",
                value   = valueAtivo,
                bigger  = TRUE,
                status  = "success",
                outline = TRUE
              )
            )
          )
          }, character(1)),
          `EDITAR` = vapply(.data$ID_AGENDA, function(id_ag) {
            as.character(actionButton(
              inputId = ns(paste0("btEditRow_", id_ag)),
              label = "",
              icon = icon("pen"),
              class = "btn btn-default btn-sm",
              onclick = paste0(
                'Shiny.setInputValue("', ns("editPressed"), '", "', id_ag, '", {priority:"event"})'
              )
            ))
          }, character(1)),
          `REMOVER` = vapply(.data$ID_AGENDA, function(id_ag) {
            as.character(actionButton(
              inputId = ns(paste0("btDelRow_", id_ag)),
              label = "",
              icon = icon("trash"),
              class = "btn btn-default btn-sm",
              onclick = paste0(
                'Shiny.setInputValue("', ns("delPressed"), '", "', id_ag, '", {priority:"event"})'
              )
            ))
          }, character(1))
        ) |>
        dplyr::transmute(
          ATIVAR    = .data$`ATIVAR`,
          `TÍTULO`  = .data$TITULO,
          TIPO      = ifelse(.data$TIPO == "WEEKLY", "SEMANAL", "UMA VEZ"),
          `INÍCIO`  = .data$`INÍCIO`,
          `FIM`     = .data$`FIM`,
          DIAS      = .data$`DIAS`,
          HORÁRIOS  = .data$`HORÁRIOS`,
          EDITAR    = .data$`EDITAR`,
          REMOVER   = .data$`REMOVER`
        )

      df2
    },
    class = "cell-border stripe",
    options = list(
      dom = "t",
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      scrollY = "45vh",
      scrollX = TRUE,
      scrollCollapse = TRUE,
      preDrawCallback = DT::JS("function() {
        Shiny.unbindAll(this.api().table().node());
      }"),
          
          drawCallback = DT::JS("function() {
        Shiny.bindAll(this.api().table().node());
      }"),
      columnDefs = list(
        list(className = "dt-center", targets = "_all"),
        list(width = "160px", targets = c(0)), # ATIVAR toggle
        list(width = "260px", targets = c(1)), # TÍTULO
        list(width = "90px",  targets = c(2)), # TIPO
        list(width = "120px", targets = c(3,4)),# INÍCIO/FIM
        list(width = "140px", targets = c(5)), # DIAS
        list(width = "520px", targets = c(6)), # <<< HORÁRIOS maior
        list(width = "120px", targets = c(7,8))# EDITAR/REMOVER
      )
    ),
    escape = FALSE,
    selection = "none",
    callback = DT::JS("
      table.on('draw.dt', function() {
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());
      });
    ")
    )
  })

  # ------------------------------------------------------------
  # Observers do toggle ATIVO (cria 1 observer por ID)
  # ------------------------------------------------------------
  obs$add(observeEvent(rv_agenda(), {
    df_all <- rv_agenda()
    if (is.null(df_all) || !nrow(df_all)) return()

    ids <- df_all$ID_AGENDA
    for (id_ag in ids) {

      key <- paste0("checkboxAtivoAgenda_", id_ag)

      if (isTRUE(e$watch_active_ids[[key]])) next
      e$watch_active_ids[[key]] <- TRUE

      local({
        id_local  <- id_ag
        key_local <- key

        obs$add(observeEvent(input[[key_local]], {
          val <- isTRUE(input[[key_local]])

          df2 <- rv_agenda()
          idx <- df2$ID_AGENDA == id_local
          if (!any(idx)) return()

          if (isTRUE(df2$ATIVO[idx][1]) == val) return()

          df2$ATIVO[idx] <- val
          df2$UPDATED_AT[idx] <- lubridate::with_tz(Sys.time(), "UTC")

          .persist(df2)
        }, ignoreInit = TRUE))
      })
    }
  }, ignoreInit = TRUE))

  # ------------------------------------------------------------
  # Ações do editor
  # ------------------------------------------------------------
  obs$add(observeEvent(input$btNovo, { .reset_editor() }, ignoreInit = TRUE))
  obs$add(observeEvent(input$btLimparEditor, { .reset_editor() }, ignoreInit = TRUE))
  obs$add(observeEvent(input$btClear, { .reset_editor() }, ignoreInit = TRUE))

  # Add janela (weekly)
  obs$add(observeEvent(input$btAddWindow, {
    begin <- stringr::str_trim(input$win_begin %||% "")
    end   <- stringr::str_trim(input$win_end   %||% "")

    ok_time <- function(x) grepl("^([01]\\d|2[0-3]):[0-5]\\d$", x)

    if (!ok_time(begin) || !ok_time(end)) {
      showNotification("Horário inválido. Use HH:MM (ex: 08:30).", type = "warning")
      return()
    }
    if (begin >= end) {
      showNotification("Horário fim deve ser maior que início.", type = "warning")
      return()
    }

    w <- rv_windows()
    w[[length(w) + 1L]] <- list(begin = begin, end = end)
    rv_windows(unname(w))
  }, ignoreInit = TRUE))

  # Remover janela
  obs$add(observeEvent(input$rmWindowIdx, {
    idx <- as.integer(input$rmWindowIdx)
    w <- rv_windows()
    if (is.na(idx) || idx < 1L || idx > length(w)) return()
    w[[idx]] <- NULL
    rv_windows(unname(w))
  }, ignoreInit = TRUE))

  # Editar registro
  obs$add(observeEvent(input$editPressed, {

    id_ag <- as.integer(input$editPressed)
    df_all <- rv_agenda()
    row <- df_all |> dplyr::filter(.data$ID_AGENDA == id_ag)
    if (!nrow(row)) return()

    rv_edit_id(id_ag)

    updateTextInput(session, "titulo", value = row$TITULO[1] %||% "")
    updateSelectInput(session, "tipo", selected = row$TIPO[1])

    d_begin <- as.Date(lubridate::with_tz(row$DT_BEGIN[1], .app_tz))
    d_end   <- if (is.na(row$DT_END[1])) as.Date(NA) else as.Date(lubridate::with_tz(row$DT_END[1], .app_tz))

    shinyWidgets::updateAirDateInput(session, "dt_begin", value = d_begin)
    shinyWidgets::updateAirDateInput(session, "dt_end",   value = if (is.na(d_end)) NULL else d_end)

    if (row$TIPO[1] == "WEEKLY") {
      dias <- strsplit(row$DAYS_WEEK[1] %||% "", ",", fixed = TRUE)[[1]]
      dias <- dias[nzchar(dias)]
      updateCheckboxGroupInput(session, "dias_week", selected = dias)

      wl <- .parse_windows_json(row$WINDOWS_JSON[1] %||% "")
      rv_windows(unname(wl))
    } else {
      rv_windows(list())
    }

  }, ignoreInit = TRUE))

  # Remover registro
  obs$add(observeEvent(input$delPressed, {

    id_ag <- as.integer(input$delPressed)
    df_all <- rv_agenda()
    row <- df_all |> dplyr::filter(.data$ID_AGENDA == id_ag)
    if (!nrow(row)) return()

    messageAlerta(
      input,
      ns,
      title   = "Remover agendamento",
      message = paste0("Deseja remover o agendamento: ", row$TITULO[1], " ?"),
      callback.no = function(){},
      callback.yes = function(){
        df2 <- df_all |> dplyr::filter(.data$ID_AGENDA != id_ag)
        .persist(df2)

        if (!is.na(rv_edit_id()) && rv_edit_id() == id_ag) {
          .reset_editor()
        }
      }
    )

  }, ignoreInit = TRUE))

  # ------------------------------------------------------------
  # Salvar (novo ou edição) - ATIVO controlado no DT
  # ------------------------------------------------------------
  .do_save <- function() {

    req(input$setor_id)
    req(input$tipo)

    setor_id <- as.integer(input$setor_id)
    setor_nm <- setores_df |>
      dplyr::filter(.data$CD_ID_SETOR == setor_id) |>
      dplyr::pull(.data$NAME_SETOR)
    setor_nm <- toupper(setor_nm %||% "")

    titulo <- .clean_title(input$titulo)
    if (!nzchar(titulo)) {
      showNotification("Informe um Título do agendamento.", type = "warning")
      return()
    }

    d_b <- .as_date_safe(input$dt_begin)
    if (is.na(d_b)) {
      showNotification("DT_BEGIN é obrigatório.", type = "warning")
      return()
    }

    d_e <- .as_date_safe(input$dt_end)

    dtb <- .date_begin_utc(d_b)
    if (!is.na(d_e)) {
      if (d_e < d_b) {
        showNotification("DT_END deve ser maior ou igual a DT_BEGIN.", type = "warning")
        return()
      }
      dte <- .date_end_utc(d_e)
    } else {
      dte <- as.POSIXct(NA, tz = "UTC")
    }

    tipo <- input$tipo

    days_week <- ""
    windows_json <- ""

    if (tipo == "WEEKLY") {
      dias <- input$dias_week %||% character()
      if (!length(dias)) {
        showNotification("Selecione pelo menos 1 dia da semana.", type = "warning")
        return()
      }

      w <- rv_windows()
      if (!length(w)) {
        showNotification("Adicione pelo menos 1 horário (janela).", type = "warning")
        return()
      }

      days_week <- paste0(dias, collapse = ",")
      w <- unname(w)
      windows_json <- as.character(jsonlite::toJSON(w, auto_unbox = TRUE))
    }

    df_all <- rv_agenda()
    now_utc <- lubridate::with_tz(Sys.time(), "UTC")
    edit_id <- rv_edit_id()

    if (is.na(edit_id)) {

      new_id <- .next_agenda_id(df_all)

      new_row <- data.frame(
        ID_AGENDA    = new_id,
        CD_ID_SETOR  = setor_id,
        NAME_SETOR   = setor_nm,
        TITULO       = titulo,
        ATIVO        = TRUE,      # <<< padrão: ativo ao criar (toggle muda depois)
        TIPO         = tipo,
        DT_BEGIN     = dtb,
        DT_END       = dte,
        DAYS_WEEK    = days_week,
        WINDOWS_JSON = windows_json,
        CREATED_AT   = now_utc,
        UPDATED_AT   = now_utc,
        stringsAsFactors = FALSE
      )

      df2 <- dplyr::bind_rows(df_all, new_row)
      .persist(df2)
      showNotification("Agendamento criado com sucesso!", type = "message")

    } else {

      if (!any(df_all$ID_AGENDA == edit_id)) {
        showNotification("Registro não encontrado para edição.", type = "warning")
        return()
      }

      df2 <- df_all
      idx <- df2$ID_AGENDA == edit_id

      df2$TITULO[idx]       <- titulo
      df2$TIPO[idx]         <- tipo
      df2$DT_BEGIN[idx]     <- dtb
      df2$DT_END[idx]       <- dte
      df2$DAYS_WEEK[idx]    <- days_week
      df2$WINDOWS_JSON[idx] <- windows_json
      df2$UPDATED_AT[idx]   <- now_utc
      # ATIVO NÃO muda aqui (controlado no DT)

      .persist(df2)
      showNotification("Agendamento atualizado com sucesso!", type = "message")
    }

    .reset_editor()
    updateSelectInput(session, "tipo", selected = "ONCE")
    shinyWidgets::updateAirDateInput(session, "dt_end", value = NULL)
  }

  obs$add(observeEvent(input$btSalvarAgenda, { .do_save() }, ignoreInit = TRUE))
  obs$add(observeEvent(input$btSalvar,       { .do_save() }, ignoreInit = TRUE))

  # ------------------------------------------------------------
  # Fechar / Sair
  # ------------------------------------------------------------
  .do_close <- function() {
    obs$destroy()
    shiny::removeModal()
    callback()
  }

  obs$add(observeEvent(input$btFechar, { .do_close() }, ignoreInit = TRUE))
  obs$add(observeEvent(input$btSair,   { .do_close() }, ignoreInit = TRUE))

  .reset_editor()
}
