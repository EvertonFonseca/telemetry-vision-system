# setor_agenda.R
box::use(
  shiny[...],
  shinyjs[inlineCSS],
  shinyWidgets[airDatepickerInput, timepickerOptions],
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
  max(df$ID_AGENDA, na.rm = TRUE) + 1L
}

.empty_agenda_df <- function() {
  data.frame(
    ID_AGENDA   = integer(),
    CD_ID_SETOR = integer(),
    NAME_SETOR  = character(),
    ACAO        = character(),  # "ATIVAR" / "DESATIVAR"
    TIPO        = character(),  # "ONCE" / "WEEKLY"
    DT_BEGIN    = as.POSIXct(character()),
    DT_END      = as.POSIXct(character()),
    DAYS_WEEK   = character(),  # "SEG,TER" etc (vazio no ONCE)
    WINDOWS_JSON= character(),  # lista de janelas no WEEKLY
    CREATED_AT  = as.POSIXct(character()),
    UPDATED_AT  = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
}

# Helpers: dias semana
.week_labels <- function() c("SEG","TER","QUA","QUI","SEX","SAB","DOM")

# ------------------------------------------------------------
# API pública: obter dataframe de agendamentos dessa sessão
# ------------------------------------------------------------
#' @export
get_agenda_df <- function(session, key = "setor_private") {
  e <- .get_private(session, key)
  df <- e$agenda_df
  if (is.null(df)) df <- .empty_agenda_df()
  df
}

# ------------------------------------------------------------
# Modal principal: Setor Agenda
# ------------------------------------------------------------
#' @export
uiSetorAgenda <- function(ns, input, output, session, callback = function() {}) {

  e   <- .get_private(session)
  obs <- newObserve()

  # carrega setores (db) e agenda (sessão)
  setores_df <- selectAllSetors(dbp$get_pool())
  if (is.null(e$agenda_df)) e$agenda_df <- .empty_agenda_df()

  rv_agenda   <- shiny::reactiveVal(e$agenda_df)
  rv_windows  <- shiny::reactiveVal(list())    # janelas do editor WEEKLY
  rv_edit_id  <- shiny::reactiveVal(NA_integer_)# ID_AGENDA em edição (NA = novo)

  .persist <- function(df) {
    e$agenda_df <- df
    rv_agenda(df)
  }

  # UI
  showModal(
    session = session,
    dialogModal(
      title = "Agenda do Setor",
      size  = "l",
      div(
        shinyjs::inlineCSS(paste0("#", ns("setor_id"), " {text-transform: uppercase;}")),
        fluidRow(
          column(
            width = 4,
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
            ),
            br(),
            panelTitle(
              title = "Regras rápidas",
              background.color.title = "white",
              title.color = "black",
              border.color = "lightgray",
              children = div(
                style = "font-size: 12px; opacity: .9;",
                tags$ul(
                  tags$li("DT_END vazio = começa e nunca termina."),
                  tags$li("Semanal: escolha dias + adicione 1 ou mais horários."),
                  tags$li("Uma vez: período único (DT_BEGIN → DT_END).")
                )
              )
            )
          ),
          column(
            width = 8,
            uiOutput(ns("uiBody"))
          )
        )
      ),
       footer = tagList(actionButton(inputId = ns("btSair"),"Sair",icon = icon("arrow-left")),
                       actionButton(ns('btClear'), "Limpar", icon = icon("eraser")),
                       actionButton(ns('btSalvar'),'Salvar',class = "btn-primary",icon = icon("save"))
                      )
    )
  )

  # corpo: tabela + editor
  output$uiBody <- renderUI({

    setor_id <- input$setor_id
    req(setor_id)

    df <- rv_agenda() |> dplyr::filter(.data$CD_ID_SETOR == as.integer(setor_id))

    tagList(
      panelTitle(
        title = "Agendamentos cadastrados",
        background.color.title = "white",
        title.color = "black",
        border.color = "lightgray",
        children = tagList(
          div(
            style = "border: 1px solid #fff; overflow-x: auto;",
            DT::dataTableOutput(ns("tbAgenda"))
          )
        )
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

  # tabela
  output$tbAgenda <- DT::renderDataTable({

    setor_id <- input$setor_id
    req(setor_id)

    df <- rv_agenda() |> dplyr::filter(.data$CD_ID_SETOR == as.integer(setor_id))
    if (!nrow(df)) return(NULL)

    coluna <- c("ID","AÇÃO","TIPO","INÍCIO","FIM","DIAS","HORÁRIOS","EDITAR","REMOVER")

    DT::datatable({
      df2 <- df |>
        mutate(
          `INÍCIO`  = format(.data$DT_BEGIN, "%d/%m/%Y %H:%M"),
          `FIM`     = ifelse(is.na(.data$DT_END), "", format(.data$DT_END, "%d/%m/%Y %H:%M")),
          `DIAS`    = ifelse(.data$TIPO == "WEEKLY", .data$DAYS_WEEK, ""),
          `HORÁRIOS`= ifelse(
            .data$TIPO == "WEEKLY",
            sapply(.data$WINDOWS_JSON, function(x){
              w <- try(jsonlite::fromJSON(x), silent = TRUE)
              if (inherits(w, "try-error") || is.null(w) || !length(w)) return("")
              paste0(vapply(w, function(z) paste0(z$begin,"-",z$end), character(1)), collapse = "; ")
            }),
            ""
          ),
          `EDITAR` = sapply(.data$ID_AGENDA, function(id){
            as.character(actionButton(
              inputId = ns("btEditRow"),
              label = "",
              icon = icon("pen"),
              onclick = paste0(
                'Shiny.setInputValue("', ns("editPressed"), '", "', id, '", {priority:"event"})'
              )
            ))
          }),
          `REMOVER` = sapply(.data$ID_AGENDA, function(id){
            as.character(actionButton(
              inputId = ns("btDelRow"),
              label = "",
              icon = icon("trash"),
              onclick = paste0(
                'Shiny.setInputValue("', ns("delPressed"), '", "', id, '", {priority:"event"})'
              )
            ))
          })
        ) |>
        transmute(
          ID       = .data$ID_AGENDA,
          AÇÃO     = .data$ACAO,
          TIPO     = ifelse(.data$TIPO == "WEEKLY", "SEMANAL", "UMA VEZ"),
          `INÍCIO` = .data$`INÍCIO`,
          `FIM`    = .data$`FIM`,
          DIAS     = .data$`DIAS`,
          HORÁRIOS = .data$`HORÁRIOS`,
          EDITAR   = .data$`EDITAR`,
          REMOVER  = .data$`REMOVER`
        )

      df2
    },
    class = "cell-border stripe",
    options = list(
      dom = "t",
      paging = FALSE,
      searching = FALSE,
      ordering = FALSE,
      scrollY = "210px",
      scrollX = TRUE,
      columnDefs = list(
        list(className = "dt-center", targets = "_all"),
        list(width = "70px", targets = c(0,1,2)),
        list(width = "140px", targets = c(3,4)),
        list(width = "90px", targets = c(7,8))
      )
    ),
    escape = FALSE,
    selection = "none"
    )
  })

  # editor UI
  uiEditor <- function(ns) {
    tagList(
      fluidRow(
        column(
          4,
          radioButtons(
            ns("acao"),
            "Ação",
            choices = c("ATIVAR","DESATIVAR"),
            selected = "ATIVAR",
            inline = TRUE
          )
        ),
        column(
          4,
          selectInput(
            ns("tipo"),
            "Tipo",
            choices = c("UMA VEZ" = "ONCE", "SEMANAL" = "WEEKLY"),
            selected = "ONCE"
          )
        ),
        column(
          4,
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
          6,
          shinyWidgets::airDatepickerInput(
            inputId = ns("dt_begin"),
            label   = "DT_BEGIN (início)",
            timepicker = TRUE,
            timepickerOpts = shinyWidgets::timepickerOptions(timeFormat = "hh:ii"),
            value = Sys.time(),
            dateFormat = "dd/MM/yyyy",
            addon = "none"
          )
        ),
        column(
          6,
          shinyWidgets::airDatepickerInput(
            inputId = ns("dt_end"),
            label   = "DT_END (fim) — opcional",
            timepicker = TRUE,
            timepickerOpts = shinyWidgets::timepickerOptions(timeFormat = "hh:ii"),
            value = NULL,
            dateFormat = "dd/MM/yyyy",
            addon = "none"
          )
        )
      ),
      
      br(),

      uiOutput(ns("uiTipoBody"))
    )
  }

  output$uiTipoBody <- renderUI({

    req(input$tipo)

    if (input$tipo == "ONCE") {
      div(
        style = "opacity:.9; font-size: 12px;",
        icon("info-circle"), " Uma vez: use DT_BEGIN e (opcionalmente) DT_END."
      )
    } else {

      tagList(
        fluidRow(
          column(
            12,
            checkboxGroupInput(
              inputId = ns("dias_week"),
              label   = "Dias da semana",
              choices = .week_labels(),
              selected = c("SEG","TER","QUA","QUI","SEX"),
              inline = TRUE
            )
          )
        ),
        br(),
        fluidRow(
          column(
            5,
            textInput(ns("win_begin"), "Horário início (HH:MM)", value = "08:00")
          ),
          column(
            5,
            textInput(ns("win_end"), "Horário fim (HH:MM)", value = "17:00")
          ),
          column(
            2,
            div(
              style = "margin-top: 24px;",
              actionButton(ns("btAddWindow"), "Adicionar", icon = icon("plus"))
            )
          )
        ),
        br(),
        div(
          style = "border: 1px solid #ddd; border-radius: 8px; padding: 10px;",
          uiOutput(ns("uiWindowsList"))
        )
      )
    }
  })

  output$uiWindowsList <- renderUI({
    w <- rv_windows()
    if (!length(w)) {
      return(div(style="opacity:.7;", icon("clock"), " Nenhum horário adicionado ainda."))
    }

    # lista simples (bonita e leve)
    tags$div(
      tags$strong("Horários do agendamento:"),
      tags$ul(
        lapply(seq_along(w), function(i){
          it <- w[[i]]
          tags$li(
            paste0(it$begin, " → ", it$end, "  "),
            tags$a(
              href = "#",
              onclick = paste0(
                'Shiny.setInputValue("', ns("rmWindowIdx"), '", ', i, ', {priority:"event"}); return false;'
              ),
              style = "margin-left:8px; color:#b00;",
              icon("trash")
            )
          )
        })
      )
    )
  })

  # ------------------------------------------------------------
  # Ações do editor
  # ------------------------------------------------------------

  # Novo agendamento (limpa editor)
  obs$add(observeEvent(input$btNovo, {
    rv_edit_id(NA_integer_)
    rv_windows(list())
    updateRadioButtons(session, "acao", selected = "ATIVAR")
    updateSelectInput(session, "tipo", selected = "ONCE")
    updateTextInput(session, "win_begin", value = "08:00")
    updateTextInput(session, "win_end", value = "17:00")
    # dt_begin volta para agora; dt_end vazio
    shinyWidgets::updateAirDateInput(session, "dt_begin", value = Sys.time())
    shinyWidgets::updateAirDateInput(session, "dt_end", value = NULL)
  }, ignoreInit = TRUE))

  # Limpar editor
  obs$add(observeEvent(input$btLimparEditor, {
    rv_edit_id(NA_integer_)
    rv_windows(list())
    updateRadioButtons(session, "acao", selected = "ATIVAR")
    updateSelectInput(session, "tipo", selected = "ONCE")
    updateCheckboxGroupInput(session, "dias_week", selected = c("SEG","TER","QUA","QUI","SEX"))
    updateTextInput(session, "win_begin", value = "08:00")
    updateTextInput(session, "win_end", value = "17:00")
    shinyWidgets::updateAirDateInput(session, "dt_begin", value = Sys.time())
    shinyWidgets::updateAirDateInput(session, "dt_end", value = NULL)
  }, ignoreInit = TRUE))

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
    rv_windows(w)

  }, ignoreInit = TRUE))

  # Remover janela
  obs$add(observeEvent(input$rmWindowIdx, {
    idx <- as.integer(input$rmWindowIdx)
    w <- rv_windows()
    if (is.na(idx) || idx < 1L || idx > length(w)) return()
    w[[idx]] <- NULL
    rv_windows(w)
  }, ignoreInit = TRUE))

  # Editar registro
  obs$add(observeEvent(input$editPressed, {

    id <- as.integer(input$editPressed)
    df <- rv_agenda()
    row <- df |> dplyr::filter(.data$ID_AGENDA == id)
    if (!nrow(row)) return()

    rv_edit_id(id)

    updateRadioButtons(session, "acao", selected = row$ACAO[1])
    updateSelectInput(session, "tipo", selected = row$TIPO[1])

    shinyWidgets::updateAirDateInput(session, "dt_begin", value = row$DT_BEGIN[1])
    shinyWidgets::updateAirDateInput(session, "dt_end",   value = if (is.na(row$DT_END[1])) NULL else row$DT_END[1])

    if (row$TIPO[1] == "WEEKLY") {
      dias <- strsplit(row$DAYS_WEEK[1] %||% "", ",", fixed = TRUE)[[1]]
      dias <- dias[nzchar(dias)]
      updateCheckboxGroupInput(session, "dias_week", selected = dias)

      w <- try(jsonlite::fromJSON(row$WINDOWS_JSON[1]), silent = TRUE)
      if (inherits(w, "try-error") || is.null(w)) {
        rv_windows(list())
      } else {
        # garante lista de listas
        rv_windows(lapply(seq_len(nrow(w)), function(i) list(begin = w$begin[i], end = w$end[i])))
      }
    } else {
      rv_windows(list())
    }

  }, ignoreInit = TRUE))

  # Remover registro
  obs$add(observeEvent(input$delPressed, {

    id <- as.integer(input$delPressed)
    df <- rv_agenda()
    row <- df |> dplyr::filter(.data$ID_AGENDA == id)
    if (!nrow(row)) return()

    messageAlerta(
      input,
      ns,
      title   = "Remover agendamento",
      message = paste0("Deseja remover o agendamento #", id, " do setor ", row$NAME_SETOR[1], "?"),
      callback.no = function(){},
      callback.yes = function(){
        df2 <- df |> dplyr::filter(.data$ID_AGENDA != id)
        .persist(df2)

        # se estava editando, limpa
        if (isTRUE(!is.na(rv_edit_id())) && rv_edit_id() == id) {
          rv_edit_id(NA_integer_)
          rv_windows(list())
        }
      }
    )

  }, ignoreInit = TRUE))

  # Salvar (novo ou edição)
  obs$add(observeEvent(input$btSalvarAgenda, {

    req(input$setor_id)
    req(input$tipo)
    req(input$acao)

    setor_id <- as.integer(input$setor_id)
    setor_nm <- setores_df |> dplyr::filter(.data$CD_ID_SETOR == setor_id) |> dplyr::pull(.data$NAME_SETOR)
    setor_nm <- toupper(setor_nm %||% "")

    # datas
    dtb <- input$dt_begin
    if (is.null(dtb)) {
      showNotification("DT_BEGIN é obrigatório.", type = "warning")
      return()
    }

    # normaliza para UTC (padrão)
    dtb <- lubridate::with_tz(as.POSIXct(dtb), tzone = "UTC")

    dte <- input$dt_end
    if (!is.null(dte) && !identical(dte, "")) {
      dte <- lubridate::with_tz(as.POSIXct(dte), tzone = "UTC")
      if (dte <= dtb) {
        showNotification("DT_END deve ser maior que DT_BEGIN.", type = "warning")
        return()
      }
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

      windows_json <- jsonlite::toJSON(w, auto_unbox = TRUE)

    } else {
      # ONCE: pode deixar DAYS/WINDOWS vazios
      days_week <- ""
      windows_json <- ""
    }

    df <- rv_agenda()

    now_utc <- lubridate::with_tz(Sys.time(), "UTC")
    edit_id <- rv_edit_id()

    if (is.na(edit_id)) {
      # novo
      new_id <- .next_agenda_id(df)
      new_row <- data.frame(
        ID_AGENDA    = new_id,
        CD_ID_SETOR  = setor_id,
        NAME_SETOR   = setor_nm,
        ACAO         = input$acao,
        TIPO         = tipo,
        DT_BEGIN     = dtb,
        DT_END       = dte,
        DAYS_WEEK    = days_week,
        WINDOWS_JSON = windows_json,
        CREATED_AT   = now_utc,
        UPDATED_AT   = now_utc,
        stringsAsFactors = FALSE
      )
      df2 <- dplyr::bind_rows(df, new_row)
      .persist(df2)

      showNotification("Agendamento criado com sucesso!", type = "message")

    } else {
      # update
      if (!any(df$ID_AGENDA == edit_id)) {
        showNotification("Registro não encontrado para edição.", type = "warning")
        return()
      }

      df2 <- df |>
        mutate(
          ACAO         = ifelse(.data$ID_AGENDA == edit_id, input$acao, .data$ACAO),
          TIPO         = ifelse(.data$ID_AGENDA == edit_id, tipo, .data$TIPO),
          DT_BEGIN     = ifelse(.data$ID_AGENDA == edit_id, dtb, .data$DT_BEGIN),
          DT_END       = ifelse(.data$ID_AGENDA == edit_id, dte, .data$DT_END),
          DAYS_WEEK    = ifelse(.data$ID_AGENDA == edit_id, days_week, .data$DAYS_WEEK),
          WINDOWS_JSON = ifelse(.data$ID_AGENDA == edit_id, windows_json, .data$WINDOWS_JSON),
          UPDATED_AT   = ifelse(.data$ID_AGENDA == edit_id, now_utc, .data$UPDATED_AT)
        )

      .persist(df2)
      showNotification("Agendamento atualizado com sucesso!", type = "message")
    }

    # após salvar: limpa editor (boa UX)
    rv_edit_id(NA_integer_)
    rv_windows(list())
    updateSelectInput(session, "tipo", selected = "ONCE")
    shinyWidgets::updateAirDateInput(session, "dt_end", value = NULL)

  }, ignoreInit = TRUE))

  # Fechar modal
  obs$add(observeEvent(input$btFechar, {
    obs$destroy()
    removeModal(session)
    callback()
  }, ignoreInit = TRUE))
}
