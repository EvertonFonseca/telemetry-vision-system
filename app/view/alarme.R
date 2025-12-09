box::use(
  shinyjs[inlineCSS],
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
    ],
  .. / model / swiper[...],
  DT,
  shinycssloaders,
  .. / logic / alarme_dao[...],
  stringr,
  dplyr[...],
  lubridate[...],
  dbp  = ../infra/db_pool,
  db   = ../infra/database,
  stats[...]
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ------------------------------------------------------------
# Estado por sessão (NUNCA global)
# ------------------------------------------------------------
.get_private <- function(session, key = "alarme_private") {
  stopifnot(!is.null(session))
  if (is.null(session$userData[[key]])) {
    session$userData[[key]] <- new.env(parent = emptyenv())
  }
  session$userData[[key]]
}
#' @export
dispose <- function(session, key = "alarme_private") {
  e <- session$userData[[key]]
  if (!is.null(e)) rm(list = ls(envir = e, all.names = TRUE), envir = e)
  session$userData[[key]] <- NULL
  invisible(gc())
}

.register_auto_dispose <- function(session, key = "alarme_private") {
  flag <- paste0(key, "_onend_registered")
  if (isTRUE(session$userData[[flag]])) return(invisible(NULL))
  session$userData[[flag]] <- TRUE
  session$onSessionEnded(function() {
    try(dispose(session, key), silent = TRUE)
  })
  invisible(NULL)
}

# ----------------------------
# Builder helpers
# ----------------------------
.new_rule <- function(fields) {
  # regra default
  list(
    field = if (nrow(fields) > 0) fields$field[1] else NULL,
    op    = "==",
    value = ""
  )
}

.new_group <- function(fields) {
  list(
    joiner = "AND",
    rules  = list(.new_rule(fields))
  )
}

# sincroniza inputs -> estado (pra não perder ao rerender)
.sync_state_from_inputs <- function(session, ns, st) {
  if (is.null(st)) return(st)

  for (gi in seq_along(st$groups)) {
    g <- st$groups[[gi]]

    join_id <- ns(paste0("g_join_", gi))
    if (!is.null(session$input[[sub("^.*\\-", "", join_id)]])) {
      # não confiável; vamos pelo input[[...]] direto é chato com ns.
    }

    for (ri in seq_along(g$rules)) {
      id_field <- paste0("r_field_", gi, "_", ri)
      id_op    <- paste0("r_op_",    gi, "_", ri)
      id_val   <- paste0("r_val_",   gi, "_", ri)

      field_v <- session$input[[ns(id_field)]]
      op_v    <- session$input[[ns(id_op)]]
      val_v   <- session$input[[ns(id_val)]]

      if (!is.null(field_v)) st$groups[[gi]]$rules[[ri]]$field <- field_v
      if (!is.null(op_v))    st$groups[[gi]]$rules[[ri]]$op    <- op_v
      if (!is.null(val_v))   st$groups[[gi]]$rules[[ri]]$value <- val_v
    }

    join_v <- session$input[[ns(paste0("g_join_", gi))]]
    if (!is.null(join_v)) st$groups[[gi]]$joiner <- join_v
  }

  top_join <- session$input[[ns("top_joiner")]]
  if (!is.null(top_join)) st$top_joiner <- top_join

  st
}

# monta JSON expr a partir do estado (já sincronizado)
.build_expr <- function(st) {
  top <- tolower(st$top_joiner %||% "OR")

  groups_expr <- lapply(st$groups, function(g) {
    list(
      type = tolower(g$joiner %||% "AND"),
      children = lapply(g$rules, function(r) {
        list(
          field = r$field %||% "",
          op    = r$op %||% "==",
          value = r$value %||% ""
        )
      })
    )
  })

  jsonlite::toJSON(
    list(type = top, children = groups_expr),
    auto_unbox = TRUE, null = "null"
  )
}

# ----------------------------
# UI do builder (não depende de inputs -> não rerenderiza digitando)
# ----------------------------
.ui_builder <- function(ns, st, fields_df) {

  field_choices <- setNames(fields_df$field, fields_df$field)
  if (length(field_choices) == 0) field_choices <- c("SEM_ATRIBUTOS" = "SEM_ATRIBUTOS")

  tagList(
    panelTitle(
      title = "Composição do alarme",
      background.color.title = "white",
      title.color = "black",
      border.color = "lightgray",
      children = div(
        style = "padding: 10px;",
        selectInput(
          ns("top_joiner"),
          "Como combinar GRUPOS (parênteses):",
          choices = c("OR","AND"),
          selected = st$top_joiner %||% "OR"
        ),
        div(
          style = "display:flex; gap:8px; align-items:center;",
          tags$button(
            type  = "button",
            class = "btn btn-success",
            onclick = sprintf(
              'Shiny.setInputValue("%s", "addGroup", {priority:"event"})',
              ns("alarm_evt")
            ),
            tags$i(class="fa fa-plus"), " Adicionar grupo"
          )
        )
      )
    ),

    lapply(seq_along(st$groups), function(gi) {
      g <- st$groups[[gi]]
      
      tagList(
        br(),
        panelTitle(
          title = paste0("Grupo ", gi, " (", g$joiner %||% "AND", ")"),
          background.color.title = "white",
          title.color = "black",
          border.color = "lightgray",
          children = div(
            style = "padding: 10px;",
            div(
              style = "display:flex; gap:10px; align-items:flex-end;",
              selectInput(
                ns(paste0("g_join_", gi)),
                "Combinar regras do grupo:",
                choices  = c("AND","OR"),
                selected = g$joiner %||% "AND",
                width = "240px"
              ),
              div(
                style = "margin-bottom: 15px !important;",
                tags$button(
                type  = "button",
                class = "btn btn-success",
                onclick = sprintf(
                  'Shiny.setInputValue("%s", "%s", {priority:"event"})',
                  ns("alarm_evt"),
                  paste0("addRule:", gi)
                ),
                tags$i(class="fa fa-plus"), " Regra"
              )),
              div(
                style = "margin-bottom: 15px !important;",
                tags$button(
                type  = "button",
                class = "btn btn-danger",
                onclick = sprintf(
                  'Shiny.setInputValue("%s", "%s", {priority:"event"})',
                  ns("alarm_evt"),
                  paste0("rmGroup:", gi)
                ),
                tags$i(class="fa fa-trash"), " Grupo"
              )
              )
            ),
            
            br(),
            
            div(
              style = "border:1px solid rgba(255,255,255,0.2); padding:8px; border-radius:8px;",
              lapply(seq_along(g$rules), function(ri) {
                r <- g$rules[[ri]]
                
                id_field <- ns(paste0("r_field_", gi, "_", ri))
                id_op    <- ns(paste0("r_op_",    gi, "_", ri))
                id_val   <- ns(paste0("r_val_",   gi, "_", ri))
                
                # dtype pelo field atual (pra escolher textInput/numericInput)
                dtype <- fields_df |> dplyr::filter(field == (r$field %||% "")) |> dplyr::pull(dtype)
                dtype <- dtype[1] %||% "text"
                
                op_choices <- c("==","!=","<",">","<=",">=","CONTAINS","IN","NOT IN","IS NULL","IS NOT NULL")
                
                div(
                  style = "display:flex; gap:10px; align-items:flex-end; padding:6px; flex-wrap:wrap;",
                  selectInput(id_field, "Campo", choices = field_choices, selected = r$field, width = "260px"),
                  selectInput(id_op, "Operação", choices = op_choices, selected = r$op %||% "==", width = "160px"),
                  
                  # valor (não re-renderiza ao digitar; só troca quando o usuário muda field/op e salvar/recarregar)
                  if ((r$op %||% "==") %in% c("IS NULL","IS NOT NULL")) {
                    div(style="min-width:220px;", tags$small("Sem valor (NULL check)"))
                  } else if (dtype == "numeric") {
                    shiny::numericInput(id_val, "Valor", value = suppressWarnings(as.numeric(r$value)) %||% NA_real_, width = "220px")
                  } else {
                    textInput(id_val, "Valor", value = r$value %||% "", width = "220px")
                  },
                  div(
                    style = "margin-bottom: 15px !important;",
                    tags$button(
                      type  = "button",
                      class = "btn btn-danger",
                      onclick = sprintf(
                        'Shiny.setInputValue("%s", "%s", {priority:"event"})',
                        ns("alarm_evt"),
                        paste0("rmRule:", gi, ":", ri)
                      ),
                      tags$i(class="fa fa-times")
                    ))
                  )
                })
            )
          )
        )
      )
    })
  )
}

# ==========================================================
# MODAL: NOVO ALARME
# ==========================================================
#' @export
uiNewAlarme <- function(ns, input, output, session, callback, cd_id_setor = NULL, cd_id_objeto = NULL) {

  .register_auto_dispose(session)

  e   <- .get_private(session)
  obs <- newObserve()

  # carrega campos (atributos) do banco
  fields_df <- NULL
  db$tryTransaction(function(conn){
    fields_df <<- listAlarmFields(conn, only_active = FALSE)
  })

  # estado do builder
  st <- reactiveVal(list(
    top_joiner = "OR",
    groups = list(.new_group(fields_df))
  ))

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
      inlineCSS(cssStyle),
      dialogModal(
        title = "Novo Alarme",
        size  = "l",
        tagList(
          inlineCSS(paste0("#", ns("alarm_name"), " {text-transform: uppercase;}"))
          ,
          div(
            style = "padding:10px;",
            textInput(ns("alarm_name"), "Nome do alarme", placeholder = "Ex.: ALARME OPERANDO INDEVIDO"),
            textInput(ns("alarm_desc"), "Descrição", placeholder = "Opcional"),
            div(style="display:flex; gap:10px; flex-wrap:wrap;",
            selectInput(ns("alarm_sev"), "Severidade", choices=c("INFO","LOW","MEDIUM","HIGH","CRITICAL"), selected="MEDIUM", width="220px"),
            checkboxInput(ns("alarm_active"), "Ativo", value = TRUE)
          ),
          uiOutput(ns("ui_builder"))
        )
      ),
      footer = tagList(
        actionButton(ns("btSair"),  "Sair",   icon = shiny::icon("arrow-left")),
        actionButton(ns("btClear"), "Limpar", icon = shiny::icon("eraser")),
        actionButton(ns("btSalvar"),"Salvar", class = "btn-primary", icon = shiny::icon("save"))
      )
    )
  ))

  output$ui_builder <- renderUI({
    .ui_builder(ns, st(), fields_df)
  })

  # ----------------------------
  # Eventos do builder (add/remove grupo/regra)
  # ----------------------------
  obs$add(observeEvent(input$alarm_evt, {
    req(input$alarm_evt)

    # sincroniza antes de mexer pra não perder o que o usuário digitou
    cur <- isolate(st())
    cur <- .sync_state_from_inputs(session, ns, cur)

    parts <- strsplit(as.character(input$alarm_evt), ":", fixed = TRUE)[[1]]
    cmd <- parts[1]

    if (cmd == "addGroup") {
      cur$groups <- c(cur$groups, list(.new_group(fields_df)))
    }

    if (cmd == "rmGroup") {
      gi <- as.integer(parts[2])
      if (!is.na(gi) && gi >= 1 && gi <= length(cur$groups)) {
        if (length(cur$groups) == 1) {
          showNotification("Você precisa ter pelo menos 1 grupo.", type = "warning")
        } else {
          cur$groups <- cur$groups[-gi]
        }
      }
    }

    if (cmd == "addRule") {
      gi <- as.integer(parts[2])
      if (!is.na(gi) && gi >= 1 && gi <= length(cur$groups)) {
        cur$groups[[gi]]$rules <- c(cur$groups[[gi]]$rules, list(.new_rule(fields_df)))
      }
    }

    if (cmd == "rmRule") {
      gi <- as.integer(parts[2])
      ri <- as.integer(parts[3])
      if (!is.na(gi) && !is.na(ri) && gi >= 1 && gi <= length(cur$groups)) {
        rules <- cur$groups[[gi]]$rules
        if (length(rules) == 1) {
          showNotification("Você precisa ter pelo menos 1 regra no grupo.", type = "warning")
        } else if (ri >= 1 && ri <= length(rules)) {
          cur$groups[[gi]]$rules <- rules[-ri]
        }
      }
    }

    st(cur)
  }, ignoreInit = TRUE, ignoreNULL = TRUE))

  # ----------------------------
  # Clear
  # ----------------------------
  obs$add(observeEvent(input$btClear, {
    updateTextInput(session, "alarm_name", value = "")
    updateTextInput(session, "alarm_desc", value = "")
    updateSelectInput(session, "alarm_sev", selected = "MEDIUM")
    session$sendInputMessage(ns("alarm_active"), list(value = TRUE))

    st(list(
      top_joiner = "OR",
      groups = list(.new_group(fields_df))
    ))
  }, ignoreInit = TRUE))

  # ----------------------------
  # Sair
  # ----------------------------
  obs$add(observeEvent(input$btSair, {
    obs$destroy()
    removeModal(session)
    callback()
  }, ignoreInit = TRUE))

  # ----------------------------
  # Salvar
  # ----------------------------
  obs$add(observeEvent(input$btSalvar, {

    nome <- isolate(toupper(input$alarm_name))
    if (stringi::stri_isempty(stringr::str_trim(nome))) {
      showNotification("O nome do alarme não foi preenchido!", type = "warning")
      return()
    }

    cur <- isolate(st())
    cur <- .sync_state_from_inputs(session, ns, cur)
    expr_json <- .build_expr(cur)

    obj <- list(
      NAME_ALARME = nome,
      DS_ALARME   = isolate(input$alarm_desc) %||% NULL,
      CD_ID_SETOR = cd_id_setor %||% NULL,
      CD_ID_OBJETO= cd_id_objeto %||% NULL,
      SEVERITY    = isolate(input$alarm_sev) %||% "MEDIUM",
      FG_ATIVO    = isTRUE(isolate(input$alarm_active)),
      JSON_EXPR   = expr_json
    )

    db$tryTransaction(function(conn){

      insertNewAlarme(conn, obj)

      showNotification("Alarme criado com sucesso!", type = "message")

      obs$destroy()
      removeModal(session)
      callback()
    })

  }, ignoreInit = TRUE, ignoreNULL = TRUE))
}

# ==========================================================
# MODAL: EDITAR/REMOVER ALARMES (lista -> editar)
# (mantive simples, sem swiper de edição profunda aqui;
#  se quiser, eu transformo em 2 slides igual teu Setor)
# ==========================================================
#' @export
uiListAlarmes <- function(ns, input, output, session, callback, cd_id_setor = NULL, cd_id_objeto = NULL) {

  .register_auto_dispose(session)
  obs <- newObserve()

  alarmes <- reactiveVal(data.frame())

  .reload <- function() {
    db$tryTransaction(function(conn){
      alarmes(selectAllAlarmes(conn, cd_id_setor = cd_id_setor, cd_id_objeto = cd_id_objeto))
    })
  }

  .reload()

  showModal(
    session = session,
    dialogModal(
      title = "Alarmes",
      size = "l",
      div(
        style="padding:10px;",
        actionButton(ns("btNovo"), "Novo alarme", class="btn-primary", icon=shiny::icon("plus")),
        br(), br(),
        DT::dataTableOutput(ns("tbAlarmes")) |> shinycssloaders$withSpinner(color = "lightblue")
      ),
      footer = tagList(
        actionButton(ns("btSair"), "Sair", icon=shiny::icon("arrow-left"))
      )
    )
  )

  output$tbAlarmes <- DT::renderDataTable({
    df <- alarmes()
    if (nrow(df) == 0) return(NULL)

    df2 <- df |>
      mutate(
        REMOVER = sapply(CD_ID_ALARME, function(id) {
          as.character(tags$button(
            type="button",
            class="btn btn-danger btn-sm",
            onclick = sprintf('Shiny.setInputValue("%s","%s",{priority:"event"})', ns("rm_alarm"), id),
            shiny::icon("trash")
          ))
        })
      ) |>
      select(CD_ID_ALARME, NAME_ALARME, SEVERITY, FG_ATIVO, DT_HR_LOCAL, REMOVER)

    DT::datatable(
      df2,
      escape = FALSE,
      selection = "none",
      options = list(dom="t", scrollX=TRUE)
    )
  })

  obs$add(observeEvent(input$btNovo, {
    removeModal(session)
    obs$destroy()
    uiNewAlarme(ns, input, output, session, callback = function(){
      callback()
    }, cd_id_setor = cd_id_setor, cd_id_objeto = cd_id_objeto)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$rm_alarm, {
    id <- as.integer(input$rm_alarm)
    if (is.na(id)) return()

    messageAlerta(
      input, ns,
      title = "Remover alarme",
      message = paste0("Deseja remover o alarme #", id, "?"),
      callback.no = function(){},
      callback.yes = function(){
        db$tryTransaction(function(conn){
          deleteAlarme(conn, id)
          .reload()
          showNotification("Alarme removido.", type="message")
        })
      }
    )
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btSair, {
    obs$destroy()
    removeModal(session)
    callback()
  }, ignoreInit = TRUE))
}
