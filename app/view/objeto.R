box::use(
  shinyjs[inlineCSS,disable],
  shiny[...],
  stringi,
  . / model[...],
  . /
    global[
      deleteElement,
      dialogTitleClose,
      panelTitle,
      removeModalClear,
      newObserve,
      shinySetInputValue,
      play_sound,
      debugLocal,
      console,
      messageAlerta,
      changetextPlaceHolder,
      tagAppendAttributesFind,
      dtProfessionalOptions,
      dtProfessionalOutput,
      set_readonly_js,
      actionWebUser
    ],
  ../model/Swiper[...],
  DT,
  shinycssloaders,
  .. / logic/objeto_dao[...],
  .. / logic/camera_dao[...],
  .. / logic/setor_dao[...],
  ../ logic/estrutura_dao[...],
  stringr,
  dplyr[...],
  lubridate[...],
  shinyWidgets[multiInput,updateMultiInput,prettyToggle,airDatepickerInput,timepickerOptions,updateAirDateInput],
  leaflet[...],
  leaflet.extras[...],
  magick[...],
  htmlwidgets,
  base64enc,
  jsonlite,
  ../logic/utils[...],
  purrr[map,map_df,map_chr],
  sf[st_as_sf],
  dbp  = ../infra/db_pool,
  db   = ../infra/database
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

# Limpa somente o estado desse módulo nessa sessão
#' @export
dispose <- function(session, key = "setor_private") {
  e <- session$userData[[key]]
  if (!is.null(e)) {
    rm(list = ls(envir = e, all.names = TRUE), envir = e)
  }
  session$userData[[key]] <- NULL
  invisible(gc())
}

# (Opcional) registra limpeza automática quando o usuário fechar a aba/navegador
.register_auto_dispose <- function(session, key = "setor_private") {
  # evita registrar múltiplas vezes
  flag <- paste0(key, "_onend_registered")
  if (isTRUE(session$userData[[flag]])) return(invisible(NULL))
  session$userData[[flag]] <- TRUE

  session$onSessionEnded(function() {
    # tenta limpar sem quebrar nada
    try(dispose(session, key), silent = TRUE)
  })

  invisible(NULL)
}

initMap <- FALSE
MAP_GROUP_COMPONENT_POLYGON <- "poligonos_componentes"
MAP_GROUP_COMPONENT_NAMES   <- "componentes_nomes"

.object_flag_value <- function(df, col, default = FALSE) {
  if (is.null(df) || !is.data.frame(df) || !(col %in% names(df))) {
    return(default)
  }

  value <- df[[col]]
  if (!length(value)) return(default)

  isTRUE(as.logical(value[[1]]))
}

.object_integer_value <- function(df, col, default = NA_integer_) {
  if (is.null(df) || !is.data.frame(df) || !(col %in% names(df))) {
    return(default)
  }

  value <- df[[col]]
  if (!length(value)) return(default)

  value <- suppressWarnings(as.integer(value[[1]]))
  if (!length(value) || is.na(value)) return(default)

  value
}

.DEFAULT_FRAME_WIDTH  <- 512L
.DEFAULT_FRAME_HEIGHT <- 512L

.camera_has_frame <- function(camera) {
  if (is.null(camera) || !is.data.frame(camera) || !"has_frame" %in% names(camera) || !nrow(camera)) {
    return(FALSE)
  }

  isTRUE(as.logical(camera$has_frame[[1]]))
}

.camera_missing_frame_text <- function(camera_name = NULL) {
  camera_name <- as.character(camera_name %||% "")
  camera_name <- stringr::str_trim(camera_name)

  if (!nzchar(camera_name)) {
    return("Esta câmera não possui frame. Um frame preto será usado no editor para desenhar os polígonos.")
  }

  paste0(
    "A câmera ", camera_name,
    " não possui frame. Um frame preto será usado no editor para desenhar os polígonos."
  )
}

.camera_choice_label <- function(camera) {
  nm <- as.character(camera$name_camera[[1]])

  if (.camera_has_frame(camera)) {
    return(tags$span(style = "pointer-events:none;", nm))
  }

  tags$span(
    title = .camera_missing_frame_text(nm),
    style = "display:inline-flex; align-items:center; gap:6px; pointer-events:none;",
    icon("exclamation-triangle", class = "text-warning"),
    tags$span(nm),
    tags$small("(sem frame)", style = "color:#f0ad4e; font-weight:600;")
  )
}

.camera_choice_names <- function(cameras) {
  if (is.null(cameras) || !is.data.frame(cameras) || !nrow(cameras)) {
    return(list())
  }

  lapply(seq_len(nrow(cameras)), function(i) .camera_choice_label(cameras[i, , drop = FALSE]))
}

.camera_selection_help <- function(cameras) {
  if (is.null(cameras) || !is.data.frame(cameras) || !nrow(cameras) || !"has_frame" %in% names(cameras)) {
    return(NULL)
  }

  has_frame <- as.logical(cameras$has_frame)
  has_frame[is.na(has_frame)] <- FALSE
  if (!any(!has_frame)) return(NULL)

  tags$div(
    style = "margin-top:8px; display:flex; align-items:flex-start; gap:8px; color:#f0ad4e;",
    icon("exclamation-triangle"),
    tags$span("Câmeras com aviso estão sem frame. Se selecionadas, o editor abrirá um frame preto para permitir o desenho dos polígonos.")
  )
}

.camera_frame_alert <- function(frame_data, camera) {
  if (is.null(frame_data) || !is.data.frame(frame_data) || !nrow(frame_data) || is.null(camera) || !is.data.frame(camera) || !nrow(camera)) {
    return(NULL)
  }

  data <- frame_data |> filter(.data$id == camera$cd_id_camera[[1]])
  if (!nrow(data) || !"frame_is_placeholder" %in% names(data) || !isTRUE(data$frame_is_placeholder[[1]])) {
    return(NULL)
  }

  msg <- as.character(data$frame_warning[[1]] %||% "")
  if (is.na(msg) || !nzchar(msg)) {
    msg <- .camera_missing_frame_text(camera$name_camera[[1]])
  }

  tags$div(
    style = paste(
      "margin:8px 0 10px 0; padding:10px 12px; border-radius:6px;",
      "background:#fff7e6; border:1px solid #f0ad4e; color:#8a6d3b;"
    ),
    tags$div(
      style = "display:flex; align-items:flex-start; gap:8px;",
      icon("exclamation-triangle"),
      tags$span(msg)
    )
  )
}

.empty_componentes_df <- function() {
  tibble::tibble(
    cd_id_componente   = integer(0),
    name_componente    = character(0),
    cd_id_camera       = integer(0),
    poligno_componente = list(),
    estrutura          = list(),
    color_componente   = character(0)
  )
}

.blank_frame_image <- function(width = .DEFAULT_FRAME_WIDTH, height = .DEFAULT_FRAME_HEIGHT) {
  image_blank(width = as.integer(width), height = as.integer(height), color = "black")
}

.frame_warning_text <- function(camera_name, frame, img_ok) {
  if (nrow(frame) == 0L || is.null(frame$data_frame[[1]])) {
    return(.camera_missing_frame_text(camera_name))
  }

  if (!isTRUE(img_ok)) {
    return(paste0(
      "Não foi possível carregar o último frame da câmera ", camera_name,
      ". Um frame preto será usado no editor para desenhar os polígonos."
    ))
  }

  NULL
}

.objeto_contexto_tz <- function() {
  tz <- tryCatch(Sys.timezone(), error = function(e) "")
  tz <- as.character(tz)[1]
  if (is.na(tz) || !nzchar(tz)) tz <- "America/Sao_Paulo"
  tz
}

.objeto_contexto_to_utc <- function(x, tz_local = .objeto_contexto_tz()) {
  if (is.null(x) || !length(x) || all(is.na(x))) return(NULL)

  x <- as.POSIXct(x)
  tz_in <- attr(x, "tzone")
  tz_in <- as.character(tz_in)[1]

  if (is.na(tz_in) || !nzchar(tz_in)) {
    x <- lubridate::force_tz(x, tzone = tz_local)
  }

  lubridate::with_tz(x, tzone = "UTC")
}

.objeto_contexto_empty_choice <- function() {
  stats::setNames("", "")
}

.objeto_contexto_choices_setor <- function(setores) {
  c(
    .objeto_contexto_empty_choice(),
    stats::setNames(as.character(setores$cd_id_setor), toupper(as.character(setores$name_setor)))
  )
}

.objeto_contexto_choices_objeto <- function(objetos) {
  if (is.null(objetos) || !is.data.frame(objetos) || !nrow(objetos)) {
    return(.objeto_contexto_empty_choice())
  }

  c(
    .objeto_contexto_empty_choice(),
    stats::setNames(as.character(objetos$cd_id_objeto), toupper(as.character(objetos$name_objeto)))
  )
}

.objeto_contexto_dt_empty <- function() {
  data.frame(
    CONTEXTO = character(0),
    MOMENTO  = character(0),
    stringsAsFactors = FALSE
  )
}

#' @export
uiObjetoContexto <- function(ns, input, output, session, callback) {

  .register_auto_dispose(session)

  obs <- newObserve()
  tz_local <- .objeto_contexto_tz()
  setores <- selectAllSetors(dbp$get_pool())
  objetos_lookup <- reactiveVal(data.frame())
  contextos <- reactiveVal(.objeto_contexto_dt_empty())

  id <- ns("dialogObjetoContexto")
  cssStyle <- list()
  cssStyle[[paste0(" #parent", id, " .modal-dialog")]]  <- "width: 96% !important; height: 90% !important;"
  cssStyle[[paste0(" #parent", id, " .modal-content")]] <- "width: 100% !important; height: 100% !important;"
  cssStyle[[paste0(" #parent", id, " .modal-body")]]    <- "width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto; overflow-x: hidden;"

  .set_objetos_do_setor <- function(cd_id_setor = NULL, selected = "") {
    setor_id <- suppressWarnings(as.integer(cd_id_setor))
    objs_all <- objetos_lookup()
    objs <- if (is.data.frame(objs_all)) objs_all[0, , drop = FALSE] else data.frame()

    if (!is.na(setor_id) && is.data.frame(objs_all) && nrow(objs_all)) {
      objs <- objs_all |> filter(.data$cd_id_setor == setor_id)
    }

    updateSelectizeInput(
      session,
      "comboObjetoContexto",
      choices = .objeto_contexto_choices_objeto(objs),
      selected = selected,
      server = TRUE
    )
  }

  .reload_objetos_lookup <- function(cd_id_setor = NULL, selected = "") {
    setor_id <- suppressWarnings(as.integer(cd_id_setor))

    ok <- db$tryTransaction(function(conn) {
      objetos_lookup(selectObjetosLookup(conn, cd_id_setor = setor_id))
    })

    if (!isTRUE(ok)) {
      objetos_lookup(data.frame())
      updateSelectizeInput(
        session,
        "comboObjetoContexto",
        choices = .objeto_contexto_empty_choice(),
        selected = "",
        server = TRUE
      )
      showNotification("Nao foi possivel carregar os objetos do setor.", type = "error")
      return(invisible(NULL))
    }

    .set_objetos_do_setor(cd_id_setor = setor_id, selected = selected)
    invisible(NULL)
  }

  .load_contextos <- function(use_loader = FALSE) {
    objeto_id <- suppressWarnings(as.integer(isolate(input$comboObjetoContexto)))
    if (is.na(objeto_id)) {
      contextos(.objeto_contexto_dt_empty())
      return(invisible(NULL))
    }

    dt_de_utc <- .objeto_contexto_to_utc(isolate(input$dtDeContexto), tz_local = tz_local)
    dt_ate_utc <- .objeto_contexto_to_utc(isolate(input$dtAteContexto), tz_local = tz_local)

    runner <- function() {
      ok <- db$tryTransaction(function(conn) {
        df <- selectObjetoContexto(
          conn,
          cd_id_objeto = objeto_id,
          dt_de_utc = dt_de_utc,
          dt_ate_utc = dt_ate_utc,
          tz_local = tz_local
        )

        df_tbl <- if (nrow(df)) {
          data.frame(
            CONTEXTO = as.character(df$contexto),
            MOMENTO  = format(df$momento, "%d/%m/%Y %H:%M:%S"),
            stringsAsFactors = FALSE
          )
        } else {
          .objeto_contexto_dt_empty()
        }

        contextos(df_tbl)
      })

      if (!isTRUE(ok)) {
        showNotification("Nao foi possivel carregar o contexto do objeto.", type = "error")
      }
    }

    if (isTRUE(use_loader)) {
      actionWebUser(runner, delay = 0, lock_id = "objeto_contexto_load")
    } else {
      runner()
    }

    invisible(NULL)
  }

  if (nrow(setores) == 0) {
    showNotification("Nenhum registro de setor foi encontrado!", type = "error")
    callback()
    return(invisible(NULL))
  }

  showModal(
    session = session,
    div(
      id = paste0("parent", id),
      style = "height: 90%;",
      inlineCSS(cssStyle),
      dialogModal(
        title = "Contexto do Objeto",
        size = "l",
        div(
          style = "padding: 14px;",
          shinyjs::inlineCSS(paste0(
            "#", ns("tbObjetoContexto"), "_wrapper .dataTables_scrollBody {",
            "max-height: 52vh !important; height: 52vh !important;}",
            "#", ns("uiResumoContexto"), " {margin-bottom: 10px;}"
          )),
          fluidRow(
            column(
              3,
              selectizeInput(
                ns("comboSetorContexto"),
                label = "Setor",
                choices = .objeto_contexto_choices_setor(setores),
                selected = "",
                options = list(
                  placeholder = "Selecione o setor",
                  openOnFocus = TRUE
                )
              )
            ),
            column(
              3,
              selectizeInput(
                ns("comboObjetoContexto"),
                label = "Objeto",
                choices = .objeto_contexto_empty_choice(),
                selected = "",
                options = list(
                  placeholder = "Selecione o objeto",
                  openOnFocus = TRUE
                )
              )
            ),
            column(
              2,
              airDatepickerInput(
                inputId = ns("dtDeContexto"),
                label = "De",
                value = NULL,
                timepicker = TRUE,
                autoClose = TRUE,
                clearButton = TRUE,
                readonly = TRUE,
                dateFormat = "dd/MM/yyyy",
                language = "pt-BR",
                timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 1),
                placeholder = "Sem filtro",
                addon = "none"
              )
            ),
            column(
              2,
              airDatepickerInput(
                inputId = ns("dtAteContexto"),
                label = "Ate",
                value = NULL,
                timepicker = TRUE,
                autoClose = TRUE,
                clearButton = TRUE,
                readonly = TRUE,
                dateFormat = "dd/MM/yyyy",
                language = "pt-BR",
                timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 1),
                placeholder = "Sem filtro",
                addon = "none"
              )
            ),
            column(
              2,
              div(
                style = "padding-top: 25px; display:flex; gap:6px; justify-content:flex-end;",
                actionButton(
                  ns("btAtualizarContexto"),
                  label = NULL,
                  icon = icon("search"),
                  class = "btn btn-default",
                  title = "Atualizar tabela"
                ),
                actionButton(
                  ns("btLimparPeriodoContexto"),
                  label = NULL,
                  icon = icon("eraser"),
                  class = "btn btn-warning",
                  title = "Limpar periodo"
                ),
                actionButton(
                  ns("btExcluirPeriodoContexto"),
                  label = NULL,
                  icon = icon("trash"),
                  class = "btn btn-danger",
                  title = "Excluir periodo filtrado"
                )
              )
            )
          ),
          uiOutput(ns("uiResumoContexto")),
          uiOutput(ns("uiTabelaContexto"))
        ),
        footer = tagList(
          actionButton(ns("btSairContexto"), "Sair", icon = icon("arrow-left"))
        )
      )
    )
  )

  output$uiResumoContexto <- renderUI({
    objeto_id <- suppressWarnings(as.integer(input$comboObjetoContexto))
    qtd <- nrow(contextos())

    msg <- if (is.na(objeto_id)) {
      "Selecione um setor e um objeto para carregar o contexto."
    } else if (qtd == 0) {
      "Nenhum contexto encontrado para o filtro atual."
    } else {
      paste0(qtd, " registro(s) carregado(s).")
    }

    div(
      style = "display:flex; justify-content:space-between; align-items:center; gap:12px;",
      tags$span(msg),
      tags$small(style = "color:#6b7280;", paste0("Horario exibido em ", tz_local))
    )
  })

  output$uiTabelaContexto <- renderUI({
    objeto_id <- suppressWarnings(as.integer(input$comboObjetoContexto))
    if (is.na(objeto_id)) {
      return(NULL)
    }

    dtProfessionalOutput(ns, "tbObjetoContexto") |>
      shinycssloaders$withSpinner(color = "lightblue", proxy.height = "50px")
  })

  output$tbObjetoContexto <- DT$renderDataTable({
    df <- contextos()
    if (is.null(df)) {
      df <- .objeto_contexto_dt_empty()
    }

    DT$datatable(
      df,
      class = "cell-border stripe",
      extensions = "Scroller",
      options = dtProfessionalOptions(
        columnDefs = list(
          list(className = "dt-left", targets = c(0)),
          list(className = "dt-center", targets = c(1)),
          list(width = "auto", targets = c(0)),
          list(width = "10%", targets = c(1))
        ),
        scrollY = "420px",
        search_placeholder = "Pesquisar contexto ou momento"
      ),
      escape = TRUE,
      selection = "none"
    ) |> DT$formatStyle(names(df), cursor = "pointer")
  })

  obs$add(observeEvent(input$comboSetorContexto, {
    .reload_objetos_lookup(input$comboSetorContexto, selected = "")
    contextos(.objeto_contexto_dt_empty())
  }, ignoreInit = TRUE, ignoreNULL = FALSE))

  obs$add(observeEvent(input$comboObjetoContexto, {
    .load_contextos()
  }, ignoreInit = TRUE, ignoreNULL = FALSE))

  obs$add(observeEvent(list(input$dtDeContexto, input$dtAteContexto), {
    objeto_sel <- as.character(input$comboObjetoContexto)
    objeto_sel <- if (length(objeto_sel)) objeto_sel[[1]] else ""
    if (!is.na(objeto_sel) && nzchar(objeto_sel)) {
      .load_contextos()
    }
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btAtualizarContexto, {
    .load_contextos(use_loader = TRUE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btLimparPeriodoContexto, {
    updateAirDateInput(session, "dtDeContexto", clear = TRUE)
    updateAirDateInput(session, "dtAteContexto", clear = TRUE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btExcluirPeriodoContexto, {
    objeto_id <- suppressWarnings(as.integer(isolate(input$comboObjetoContexto)))
    if (is.na(objeto_id)) {
      showNotification("Selecione um objeto para excluir o contexto.", type = "warning")
      return()
    }

    dt_de_local <- isolate(input$dtDeContexto)
    dt_ate_local <- isolate(input$dtAteContexto)

    if (is.null(dt_de_local) || any(is.na(dt_de_local)) || is.null(dt_ate_local) || any(is.na(dt_ate_local))) {
      showNotification("Informe os filtros De e Ate para excluir o periodo.", type = "warning")
      return()
    }

    dt_de_utc <- .objeto_contexto_to_utc(dt_de_local, tz_local = tz_local)
    dt_ate_utc <- .objeto_contexto_to_utc(dt_ate_local, tz_local = tz_local)

    if (is.null(dt_de_utc) || is.null(dt_ate_utc) || dt_de_utc > dt_ate_utc) {
      showNotification("O periodo informado e invalido.", type = "warning")
      return()
    }

    qtd <- nrow(isolate(contextos()))
    if (qtd == 0) {
      showNotification("Nenhum contexto encontrado para o periodo informado.", type = "warning")
      return()
    }

    nome_objeto <- objetos_lookup() |>
      filter(.data$cd_id_objeto == objeto_id) |>
      pull(.data$name_objeto)
    nome_objeto <- if (length(nome_objeto)) toupper(as.character(nome_objeto[[1]])) else paste0("#", objeto_id)

    messageAlerta(
      input,
      ns,
      title = "Excluir contexto do objeto",
      message = paste0(
        "Deseja remover ", qtd, " registro(s) do objeto ", nome_objeto,
        " entre ",
        format(as.POSIXct(dt_de_local), "%d/%m/%Y %H:%M:%S"),
        " e ",
        format(as.POSIXct(dt_ate_local), "%d/%m/%Y %H:%M:%S"),
        "?"
      ),
      callback.no = function() {
      },
      callback.yes = function() {
        actionWebUser(function() {
          ok <- db$tryTransaction(function(conn) {
            deleteObjetoContextoByPeriodo(
              conn,
              cd_id_objeto = objeto_id,
              dt_de_utc = dt_de_utc,
              dt_ate_utc = dt_ate_utc
            )
          })

          if (!isTRUE(ok)) {
            showNotification("Nao foi possivel excluir o periodo selecionado.", type = "error")
            return()
          }

          showNotification("Contexto excluido com sucesso!", type = "message")
          .load_contextos(use_loader = FALSE)
        }, delay = 0, lock_id = "objeto_contexto_delete")
      }
    )
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btSairContexto, {
    obs$destroy()
    removeModal(session)
    callback()
  }, ignoreInit = TRUE))
}

#' @export
 uiNewObjeto <- function(ns,input,output,session,callback){
  
  .register_auto_dispose(session)
  
  e <- .get_private(session)
  
  
  obs       <- newObserve()
  obs2      <- newObserve()
  obs3      <- newObserve()
  setores   <- selectAllSetors(dbp$get_pool())
  cameras   <- selectAllCamerasWithFrameStatus(dbp$get_pool())
  camerasSelected <- reactiveVal(NULL)
  tiposObjeto     <- selectTipoObjeto(dbp$get_pool())
  deleting        <- reactiveVal(FALSE)
  editing         <- reactiveVal(FALSE)
  sliderPosition  <- reactiveVal(1L)
  idSwiper        <- ns('swiperMain')
  frame_data         <- NULL
  componenteReactive <- reactiveVal(NULL)
  estruturas         <- selectAllEstrutura(dbp$get_pool())
  updateObjDynamic   <- reactiveVal(FALSE)
  visiveisPrevPorCamera <- reactiveVal(list())
   
   if(nrow(cameras) == 0){
    obs$destroy()
    obs2$destroy()
    obs3$destroy()
    updateObjDynamic(FALSE)
    if(!is.null(frame_data)) unlink(frame_data$img_path)
    swiperDestroy(idSwiper)
    showNotification("Nenhum registro de câmera foi encontrado!", type = "error")
    callback()
  }else if(nrow(setores) == 0){
    obs$destroy()
    obs2$destroy()
    obs3$destroy()
    updateObjDynamic(FALSE)
    if(!is.null(frame_data)) unlink(frame_data$img_path)
    swiperDestroy(idSwiper)
    showNotification("Nenhum registro de setor foi encontrado!", type = "error")
    callback()
  }else if(nrow(estruturas) == 0){
    obs$destroy()
    obs2$destroy()
    obs3$destroy()
    updateObjDynamic(FALSE)
    if(!is.null(frame_data)) unlink(frame_data$img_path)
    swiperDestroy(idSwiper)
    showNotification("Nenhum registro de estrutura foi encontrado!", type = "error")
    callback()
  }
  
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
        title = textOutput(ns("titleTexto")),
        size = 'm',
        swiper(id = idSwiper,width = '100%',height = '100%',
        parent.style = "min-height: 350px !important;",
        swiperSlide(
          style = 'height: 100%; width: 100%; overflow: hidden; padding: 1px;',
          uiOutput(ns('slider1')) |> shinycssloaders$withSpinner(color = 'lightblue')
        ),
        swiperSlide(
          style = 'height: 100%; width: 100%; overflow-y: hidden; overflow-x: hidden; padding: 1px;',
          uiOutput(ns('slider2')) |> shinycssloaders$withSpinner(color = 'lightblue')
        ),
        swiperSlide(
          style = 'height: 100%; width: 100%; overflow-x: hidden; overflow-y: hidden;  padding: 1px;',
          uiOutput(ns('slider3')) |> shinycssloaders$withSpinner(color = 'lightblue')
        )
      ),  
      footer = uiOutput(ns('uiFooter')))))
      
      output$uiFooter <- renderUI({
        
        current <- sliderPosition()
        
        if(!is.null(componenteReactive())){
          tagList(actionButton(ns("btSair"), label = "Selecionar",icon = icon("arrow-left")))
        }
        else if(current == 1){
          tagList(actionButton(ns("btSair"), label = "Sair",icon = icon("arrow-left")),
          actionButton(ns('btClear'), "Limpar", icon = icon("eraser")),
          actionButton(ns('btSalvar'),class = "btn-success",label = "Avançar",icon = icon("arrow-right")))
        }
        else{
          tagList(actionButton(ns("btSair"), label = "Voltar",icon = icon("arrow-left")),
          actionButton(ns('btSalvar'),'Salvar',class = "btn-primary",icon = icon("save")))
        }
        
      })
      
      output$titleTexto <- renderText({
        
        if(!is.null(componenteReactive())){
          'Seleciona a estrutura'
        }
        else if(sliderPosition() == 1L){
          'Novo Objeto'
        }else{
          'Novos Componentes'
        }
        
      })

      output$uiCameraFrameAlert <- renderUI({
        req(sliderPosition() == 2L, !is.null(frame_data), !is.null(input$comboCameras))

        camera <- cameras |> filter(name_camera == input$comboCameras)
        .camera_frame_alert(frame_data, camera)
      })
      
      output$slider1 <- renderUI({
        
        uiMain(ns,setores,cameras,tiposObjeto)
      })
      
      output$slider2 <- renderUI({
        
        req(sliderPosition() == 2L)
        camerasTargets <- isolate(input$multiCameras) 
        tipoObjeto     <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
       
        obs2$clear()
        
        update_visible_componentes <- function() {
          if (tipoObjeto$cd_id_objeto_tipo == 2L) return(invisible(NULL))
          
          if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) {
            updateCheckboxGroupInput(session, "checkComponentesVisiveis", choices = character(0), selected = character(0))
            return(invisible(NULL))
          }
          
          camera <- cameras |> filter(name_camera == input$comboCameras)
          if (nrow(camera) != 1L) return(invisible(NULL))
          camera_key <- as.character(camera$cd_id_camera)
          
          info_choices <- .component_visibility_choices(frame_data$componente[[1]], camera$cd_id_camera)
          prev_map <- isolate(visiveisPrevPorCamera())
          prev_ids <- prev_map[[camera_key]]
          if (is.null(prev_ids)) prev_ids <- character(0)
          selected_ids <- isolate(input$checkComponentesVisiveis)
          if (is.null(selected_ids)) {
            selected_ids <- info_choices$ids
          } else {
            selected_ids <- intersect(as.character(selected_ids), info_choices$ids)
            ids_novos <- setdiff(info_choices$ids, prev_ids)
            selected_ids <- unique(c(selected_ids, ids_novos))
          }
          
          updateCheckboxGroupInput(
            session,
            "checkComponentesVisiveis",
            choices = info_choices$choices,
            selected = selected_ids
          )
          
          prev_map[[camera_key]] <- info_choices$ids
          visiveisPrevPorCamera(prev_map)
        }
        
        # -------------------------------------------------------------------
        # BLOQUEAR / LIBERAR CLIQUES durante DELETE e EDIT
        # -------------------------------------------------------------------
        obs2$add(observeEvent(input$mapFrame_draw_deletestart, {
          deleting(TRUE)
        }, ignoreInit = TRUE))
        
        # Alguns fluxos disparam draw_stop, outros deletestop; trate os dois.
        obs2$add(observeEvent(input$mapFrame_draw_deletestop, {
          deleting(FALSE)
        }, ignoreInit = TRUE))
        
        obs2$add(observeEvent(input$mapFrame_draw_stop, {
          # Só libera se o modo stop era "remove"
          if (identical(input$mapFrame_draw_stop$mode, "remove")) deleting(FALSE)
        }, ignoreInit = TRUE))
        
        obs2$add(observeEvent(input$mapFrame_draw_editstart, {
          editing(TRUE)
        }, ignoreInit = TRUE))
        
        obs2$add(observeEvent(input$mapFrame_draw_editstop, {
          editing(FALSE)
        }, ignoreInit = TRUE))
        
        # -------------------------------------------------------------------
        # CRIAÇÃO DE POLÍGONO
        # -------------------------------------------------------------------
        obs2$add(observeEvent(input$mapFrame_draw_new_feature, {
          req(sliderPosition() == 2L)

          feat <- input$mapFrame_draw_new_feature
          if (is.null(feat) || !identical(feat$geometry$type, "Polygon")) return()
          
          cameraTarget <- cameras |> filter(name_camera == input$comboCameras)
          req(nrow(cameraTarget) == 1)  # garante correspondência única
          
          coords <- feat$geometry$coordinates[[1]]
          lng    <- vapply(coords, function(x) x[[1]], numeric(1))
          lat    <- vapply(coords, function(x) x[[2]], numeric(1))
          poly   <- .drop_dup_last(tibble::tibble(x = lng, y = lat))
          
          componentes_atual <- NULL
          if (!is.null(frame_data$componente)) {
            componentes_atual <- frame_data$componente[[1]]
          }
          componentes_atual <- .ensure_component_colors(componentes_atual)
          used_colors <- if (is.null(componentes_atual)) character(0) else componentes_atual$color_componente

          row_new <- tibble::tibble(
            cd_id_componente    = feat$properties$`_leaflet_id`,
            name_componente     = "",
            cd_id_camera        = cameraTarget$cd_id_camera,
            poligno_componente  = list(poly),
            estrutura           = list(NULL),
            color_componente    = .next_component_color(used_colors)
          )

          if (is.null(componentes_atual) || nrow(componentes_atual) == 0L) {
            frame_data$componente[[1]] <<- row_new
          } else {
            frame_data$componente[[1]] <<- bind_rows(componentes_atual, row_new)
          }
          update_visible_componentes()
        
          #objetos dinamicos apenas 1 compomentes
          if(tipoObjeto$cd_id_objeto_tipo == 2L && nrow(frame_data$componente[[1]]) == 1L){
            camera          <- cameras |> filter(name_camera == input$comboCameras)
            componentes     <- frame_data$componente[[1]]   
            updateObjDynamic(TRUE)
            output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = TRUE)})
          }else{
            camera          <- cameras |> filter(name_camera == input$comboCameras)
            componentes     <- frame_data$componente[[1]]
            output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = FALSE)})
          }
        }, ignoreInit = TRUE))
        
        # -------------------------------------------------------------------
        # EDIÇÃO DE POLÍGONOS (mover vértices, etc.)
        # -------------------------------------------------------------------
        obs2$add(observeEvent(input$mapFrame_draw_edited_features, {
          req(sliderPosition() == 2L)
          
          feats <- input$mapFrame_draw_edited_features
          if (is.null(feats$features) || length(feats$features) == 0) return()
          if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) return()
          
          df <- frame_data$componente[[1]]
          
          for (feat in feats$features) {
            # aceita Polygon/MultiPolygon com 1 anel
            if (is.null(feat$geometry$type) || !(feat$geometry$type %in% c("Polygon","MultiPolygon"))) next
            
            id_ <- feat$properties$`_leaflet_id`
            if (!is.null(feat$properties$layerId)) id_ <- feat$properties$layerId
            
            coords <- feat$geometry$coordinates[[1]]
            if (is.null(coords) || length(coords) < 3) next
            
            lng  <- vapply(coords, function(x) x[[1]], numeric(1))
            lat  <- vapply(coords, function(x) x[[2]], numeric(1))
            poly <- .drop_dup_last(tibble::tibble(x = lng, y = lat))
            
            idx <- which(df$cd_id_componente == id_)
            if (length(idx) != 1L) next  # evita sobrescrita incorreta
            
            df$poligno_componente[[idx]] <- poly
          }
          
          df <- .ensure_component_colors(df)
          frame_data$componente[[1]] <<- df
          update_visible_componentes()

          camera          <- cameras |> filter(name_camera == input$comboCameras)
          componentes     <- frame_data$componente[[1]]
          is_dynamic      <- updateObjDynamic()
          output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = is_dynamic)})
        }, ignoreInit = TRUE))
        
        # -------------------------------------------------------------------
        # REMOÇÃO DE POLÍGONOS
        # -------------------------------------------------------------------
        obs2$add(observeEvent(input$mapFrame_draw_deleted_features, {
          req(sliderPosition() == 2L)
          
          feats <- input$mapFrame_draw_deleted_features
          if (is.null(feats$features) || length(feats$features) == 0) return()
          if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) return()
          
          df <- frame_data$componente[[1]]
          
          for (feat in feats$features) {
            id_ <- feat$properties$`_leaflet_id`
            if (!is.null(feat$properties$layerId)) id_ <- feat$properties$layerId
            
            # Remoção segura (mantém list-cols inteiras)
            df <- filter(df, .data$cd_id_componente != id_)
          }
          
          frame_data$componente[[1]] <<- .ensure_component_colors(df)
          update_visible_componentes()

          camera          <- cameras |> filter(name_camera == input$comboCameras)
          componentes     <- frame_data$componente[[1]]
          is_dynamic      <- updateObjDynamic()
          output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = is_dynamic)})
          
          if(nrow(df) == 0){
            if(isolate(updateObjDynamic())){
              camera          <- cameras |> filter(name_camera == input$comboCameras)
              componentes     <- frame_data$componente[[1]]   
              updateObjDynamic(FALSE)
              output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = FALSE)})
            }
          }

        }, ignoreInit = TRUE))
        
        # -------------------------------------------------------------------
        # CLICKS EM SHAPES (somente se NÃO estiver editando nem deletando)
        # -------------------------------------------------------------------
        obs2$add(observeEvent(input$mapFrame_shape_poligonos_componentes_click, {
          req(sliderPosition() == 2L, !isTRUE(deleting()), !isTRUE(editing()))
          ev <- input$mapFrame_shape_poligonos_componentes_click
          if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) return()
          
          if(isolate(updateObjDynamic())) return()
          
          target <- frame_data$componente[[1]] |> filter(.data$cd_id_componente == ev$id)
          if (nrow(target) == 1) {
            componenteReactive(target)
            swiperSlideNext(idSwiper)
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE))
        
        # Se você já tem shape_draw_click, pode dispensar o shape_click
        # para evitar "duplo disparo". Se quiser manter, mantenha o mesmo req().
        obs2$add(observeEvent(input$mapFrame_shape_click, {
          req(sliderPosition() == 2L, !isTRUE(deleting()), !isTRUE(editing()))
          ev <- input$mapFrame_shape_click
          if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) return()
          
          if(isolate(updateObjDynamic())) return()
          
          target <- frame_data$componente[[1]] |> filter(.data$cd_id_componente == ev$id)
          if (nrow(target) == 1) {
            componenteReactive(target)
            swiperSlideNext(idSwiper)
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE))

        obs2$add(observeEvent(input$comboCameras,{
          update_visible_componentes()
          camera          <- cameras |> filter(name_camera == input$comboCameras)
          componentes     <- frame_data$componente[[1]]   
          is_dynamic      <- updateObjDynamic()
          output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = is_dynamic)})
        },ignoreNULL = TRUE))
        
        obs2$add(observeEvent(input$checkComponentesVisiveis,{
          req(sliderPosition() == 2L)
          if(tipoObjeto$cd_id_objeto_tipo == 2L) return()
          camera <- cameras |> filter(name_camera == input$comboCameras)
          if (nrow(camera) != 1L) return()
          componentes <- frame_data$componente[[1]]
          is_dynamic  <- updateObjDynamic()
          output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = is_dynamic)})
        },ignoreNULL = FALSE, ignoreInit = TRUE))

        estruturas_dinamicos <- NULL
        visibilidade_estaticos <- NULL
        if(tipoObjeto$cd_id_objeto_tipo == 2L){

          changetextPlaceHolder()
          estruturas_dinamicos <- tagList(
            br(),
            inlineCSS(paste0("#", ns("textNameComponente"), " {text-transform: uppercase;}")),
            textInput(ns("textNameComponente"), label = "Nome",
            placeholder = "Digite o nome para o componente",value = ""),
            multiInput(
              inputId = ns('multiEstruturaComp'),
              width = '100%',
              options = list(
                enable_search = T,
                non_selected_header = "Estrutura não selecionados",
                selected_header     = "Estrutura selecionados"
              ),
              label = "Estruturas ativas",
              choices = NULL,
              choiceNames  = estruturas$name_estrutura,
              choiceValues = estruturas$name_estrutura
            ) |> tagAppendAttributes(style = ';height: auto; width: 100%;'))
        }else{
          visibilidade_estaticos <- checkboxGroupInput(
            ns("checkComponentesVisiveis"),
            label = "Mostrar/Ocultar poligonos por componente",
            choices = NULL,
            selected = NULL
          )
        }
        
        tagList(
          selectizeInput(ns('comboCameras'),label = 'Câmera',choices = camerasTargets,options  = list(
            dropdownParent = 'body',
            openOnFocus = TRUE,
            closeAfterSelect = TRUE
          )),
          uiOutput(ns("uiCameraFrameAlert")),
          leafletOutput(ns("mapFrame"), height = "512px", width = "100%"),
          visibilidade_estaticos,
          estruturas_dinamicos,
          br()
        )
        
      })
      
      # ---- CASCA SUPERIOR (apenas UI) ----
      output$slider3 <- renderUI({
        
        req(componenteReactive())
        
        componente <- componenteReactive()
       
        obs3$clear()
        obs3$add(observeEvent(input$comboEstrutura,{

          estrutura <- estruturas |> filter(name_estrutura == input$comboEstrutura)
          atributos <- map_df(estrutura$configs[[1]]$atributos,~ .x)
          width_nm  <- max(nchar(atributos$name_atributo %||% ""))

          nm  <- stringr::str_pad(atributos$name_atributo,width = width_nm,side  = "right")

          prefix_b <- ifelse(atributos$name_data == "QUALITATIVE","[ ","")
          prefix_e <- ifelse(atributos$name_data == "QUALITATIVE"," ]","")
          linhas <- paste0(nm, " = ",prefix_b,atributos$value_atributo,prefix_e)
          txt    <- paste(linhas, collapse = "\n")
          updateTextAreaInput(session,"info_estrutura",value = txt)

        },ignoreNULL = TRUE))

        estrutura  <- componente$estrutura[[1]]
        uiEstrutura(ns,componente$name_componente,estruturas,estrutura)
      })
      
      
      # Sair 
      obs$add(observeEvent(input$btSair,{
        
        current <- isolate(sliderPosition())
        
        if(current == 1L){
          obs$destroy()
          obs2$destroy()
          obs3$destroy()
          updateObjDynamic(FALSE)
          if(!is.null(frame_data)) unlink(frame_data$img_path)
          removeModal(session)
          callback()
          swiperDestroy(idSwiper)
        }else{
          status   <- is.null(isolate(componenteReactive()))
          if(!status){
            df_poly    <- frame_data$componente[[1]]
            componente <- isolate(componenteReactive())
            index      <- which(df_poly$cd_id_componente == componente$cd_id_componente)
            df_p       <- df_poly[index,]
            df_p$name_componente <- toupper(isolate(input$textNameComponente))
            estrutura  <- isolate(estruturas |> filter(name_estrutura == input$comboEstrutura))
            df_p$estrutura[[1]] <- estrutura
            frame_data$componente[[1]][index,] <<- df_p
            
            camera      <- cameras |> filter(name_camera == isolate(input$comboCameras))
            componentes <- frame_data$componente[[1]] 
            if(!isTRUE(isolate(updateObjDynamic())) && nrow(camera) == 1L){
              info_choices <- .component_visibility_choices(componentes, camera$cd_id_camera)
              selected_ids <- isolate(input$checkComponentesVisiveis)
              if (is.null(selected_ids)) {
                selected_ids <- info_choices$ids
              } else {
                selected_ids <- intersect(as.character(selected_ids), info_choices$ids)
              }
              updateCheckboxGroupInput(session, "checkComponentesVisiveis", choices = info_choices$choices, selected = selected_ids)
            }
            #update mapa
            #proxy_update_componentes(map_id = ns("mapFrame"),ns = ns, camera = camera,componentes = componentes)  
            #proxy_update_componentes(session,ns,ns("mapFrame"),camera, componentes)           
            output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis))})
            componenteReactive(NULL)
          }else{
            if(!is.null(frame_data)) unlink(frame_data$img_path)
            sliderPosition(isolate(sliderPosition()) - 1L)
          }
          deleting(FALSE)
          editing(FALSE)
          swiperSlidePrevious(idSwiper)    
        }
        
      },ignoreInit = T,ignoreNULL = T))
      
      ## Clear
      obs$add(observeEvent(input$btClear, {
        updateTextInput(session,'textNameObjeto', value = '')
        updateMultiInput(session,'multiCameras',choices = '')
        updateNumericInput(session, "numericIdGrupoObjeto", value = NA_integer_)
      }, ignoreInit = TRUE))
      
      ## Salvar Objeto
      obs$add(observeEvent(input$btSalvar,{
        
        current <- isolate(sliderPosition())
        
        if(current == 1L){
          nomeObjeto     <- isolate(toupper(input$textNameObjeto))
          camerasTargets <- isolate(input$multiCameras)
          idGrupoObjeto  <- suppressWarnings(as.integer(isolate(input$numericIdGrupoObjeto)))
          tipoObjeto     <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
          
          if(tipoObjeto$cd_id_objeto_tipo == 2 && length( camerasTargets) > 1){
            showNotification("Não foi possível avançar. Para objetos dinâmicos, selecione apenas uma câmera.", type = "warning")
            return()
          }

          if(stringi$stri_isempty(stringr$str_trim(nomeObjeto))){
            showNotification("O nome do Objeto não foi preenchido!", type = "warning")
            return()
          }
          
          if(length(camerasTargets) == 0){
            showNotification("Nenhuma câmera foi selecionada para objeto!", type = "warning")
            return()
          }

          if(is.na(idGrupoObjeto)){
            showNotification("O ID do grupo nÃ£o foi preenchido!", type = "warning")
            return()
          }
          
          if(checkifExistNameObjeto(dbp$get_pool(),nomeObjeto)){
            showNotification("O nome do Objeto já possui nos registros!", type = "warning")
            return()
          }
          
          frame_data <<- searchFramesByCamerasSelected(dbp$get_pool(),camerasTargets,cameras)
          
          sliderPosition(isolate(sliderPosition()) + 1L)
          swiperSlideNext(idSwiper)
          
        }else if(current == 2L){

          tipoObjeto  <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
          componentes <- frame_data$componente[[1]]
          
          if(tipoObjeto$cd_id_objeto_tipo == 2L){
            componentes$name_componente <- toupper(isolate(input$textNameComponente))
            multiEstruturas <- estruturas |> filter(name_estrutura %in% isolate(input$multiEstruturaComp))
            if(nrow(multiEstruturas) == 0){
              showNotification("Nenhuma estrutura foi selecionada para o componente!", type = "warning")
              return()
            }
            componentes$estrutura <- list(multiEstruturas)
          }
          
          if(is.null(componentes)){
            showNotification("Nenhum desenho de poligno foi encontrado!", type = "warning")
            return()
          }else if(nrow(componentes) == 0){
            showNotification("Nenhum desenho de poligno foi encontrado!", type = "warning")
            return()
          }
          
          actionWebUser({
            if(!db$tryTransaction(function(conn){
            
            if(any(stringi$stri_isempty(componentes$name_componente))){
              showNotification("Existe componente com nomes vazios!", type = "warning")
              return()
            }else if(any(duplicated(componentes$name_componente))){
              showNotification("Existe componente com nomes duplicados!", type = "warning")
              return()
            }

            #check if it has already data of Câmera
            nomeObjeto     <- isolate(toupper(input$textNameObjeto))
            ativoObjeto    <- isolate(input$checkboxAtivoObjeto)
            isDevObjeto    <- isolate(input$checkboxIsDevObjeto)
            grupoObjeto    <- isolate(input$checkboxGrupoObjeto)
            idGrupoObjeto  <- suppressWarnings(as.integer(isolate(input$numericIdGrupoObjeto)))
            tipoObjeto     <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
            setor          <- setores |> filter(name_setor == isolate(input$comboSetor))

            if(is.na(idGrupoObjeto)){
              showNotification("O ID do grupo nÃ£o foi preenchido!", type = "warning")
              return()
            }
            
            # try insert or roolback
            obj               <- list()
            obj$name_objeto   <- nomeObjeto
            obj$fg_ativo      <- as.integer(ativoObjeto)
            obj$is_dev        <- as.integer(isDevObjeto)
            obj$grupo         <- as.integer(grupoObjeto)
            obj$id_grupo      <- idGrupoObjeto
            obj$cd_id_setor   <- setor$cd_id_setor
            obj$cd_id_objeto_tipo <- tipoObjeto$cd_id_objeto_tipo
            obj$timeline_context_sec <- 5L #isolate(input$sliderTimeContexto)
            id_obj             <- db$nextSequenciaID(conn, "objeto", id_col = "cd_id_objeto", schema = "public")
            obj$cd_id_objeto   <- insertNewObjeto(conn,id_obj,obj)
            
            id_obj_config      <- db$nextSequenciaID(conn, "objeto_config", id_col = "cd_id_obj_conf", schema = "public")
            insertNewObjetoConfig(conn,id_obj_config,obj)
            
            for(i in 1:nrow(componentes)){
              #insert componente do objeto
              comp       <- componentes[i,]
              estruturas <- comp$estrutura[[1]]
              
              for(k in 1:nrow(estruturas)){
                estrutura <- estruturas[k,]
                poligno   <- jsonlite$toJSON(comp$poligno_componente[[1]],auto_unbox = T)
                
                objComp <- list()
                objComp$name_componente    <- comp$name_componente
                objComp$poligno_componente <- poligno
                objComp$cd_id_obj_conf     <- id_obj_config
                objComp$cd_id_camera       <- comp$cd_id_camera
                objComp$cd_id_componente   <- db$nextSequenciaID(conn, "componente", id_col = "cd_id_componente", schema = "public")
                
                objComp$cd_id_estrutura    <- estrutura$cd_id_estrutura
                
                db$insertTable(conn,"COMPONENTE",objComp)
              }
            }
            
            dialogConfirm(
              session = session,
              id    = ns('dialogConfirm'),
              title = 'Objeto criado com sucesso!',
              text  = 'Deseja criar novamente um novo Objeto?')
              
              #crie so uma vez
              observeEvent(input$dialogConfirm,{
                
                status <- input$dialogConfirm
                
                # Limpar os campos APÓS o flush/render — garante que os inputs existam no DOM
                session$onFlushed(function() {
                  updateTextInput(session,'textNameObjeto', value = '')
                  updateMultiInput(session,'multiCameras',selected = NULL)
                  updateSelectizeInput(session,'comboTipoObjeto',selected = NULL)
                  updateNumericInput(session, "numericIdGrupoObjeto", value = NA_integer_)
                },once = TRUE)
                
                if(!status){
                  obs$destroy()
                  removeModal(session)
                  callback()
                  swiperDestroy(idSwiper)
                }else{
                  deleting(FALSE)
                  editing(FALSE)
                  sliderPosition(isolate(sliderPosition()) - 1L)
                  swiperSlidePrevious(idSwiper)
                }
                
              },ignoreInit = TRUE,once = TRUE)
              
            })){
              showNotification("Não foi possivel salvar o objeto, durante o processo houve falha!", type = "error")
            }
          }, delay = 0, lock_id = "objeto_create_save")
          }
          
        },ignoreInit = T,ignoreNULL = T))
        
 }

 
#' @export
uiEditObjeto <- function(ns,input,output,session,callback){

  .register_auto_dispose(session)

  e <- .get_private(session)

  objetos         <- reactiveVal(selectAllObjetos(dbp$get_pool()))
  objeto          <- reactiveVal(NULL)
  obs             <- newObserve()
  obs2            <- newObserve()
  obs3            <- newObserve()
  setores         <- selectAllSetors(dbp$get_pool())
  cameras         <- selectAllCamerasWithFrameStatus(dbp$get_pool())
  camerasSelected <- reactiveVal(NULL)
  tiposObjeto     <- selectTipoObjeto(dbp$get_pool())
  deleting        <- reactiveVal(FALSE)
  editing         <- reactiveVal(FALSE)
  sliderPosition  <- reactiveVal(1L)
  idSwiper        <- ns('swiperMain')
  frame_data         <- NULL
  componenteReactive <- reactiveVal(NULL)
  estruturas         <- selectAllEstrutura(dbp$get_pool())  
  updateObjDynamic   <- reactiveVal(FALSE)
  visiveisPrevPorCamera <- reactiveVal(list())
  
  if(nrow(cameras) == 0){
    obs$destroy()
    obs2$destroy()
    obs3$destroy()
    updateObjDynamic(FALSE)
    if(!is.null(frame_data)) unlink(frame_data$img_path)
    swiperDestroy(idSwiper)
    showNotification("Nenhum registro de câmera foi encontrado!", type = "error")
    callback()
  }else if(nrow(setores) == 0){
    obs$destroy()
    obs2$destroy()
    obs3$destroy()
    updateObjDynamic(FALSE)
    if(!is.null(frame_data)) unlink(frame_data$img_path)
    swiperDestroy(idSwiper)
    showNotification("Nenhum registro de setor foi encontrado!", type = "error")
    callback()
  }else if(nrow(estruturas) == 0){
    obs$destroy()
    obs2$destroy()
    obs3$destroy()
    updateObjDynamic(FALSE)
    if(!is.null(frame_data)) unlink(frame_data$img_path)
    swiperDestroy(idSwiper)
    showNotification("Nenhum registro de estrutura foi encontrado!", type = "error")
    callback()
  }
  
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
        title = textOutput(ns("titleTexto")),
        size = 'm',
        swiper(id = idSwiper,width = '100%',height = '100%',
        parent.style = "min-height: 350px !important;",
        swiperSlide(
          style = 'height: 100%; width: 100%; overflow: hidden; padding: 1px;',
          selectizeInput(ns('comboSetor'),label = 'Setor',choices = setores$name_setor),
          uiOutput(ns('slider1')) |> shinycssloaders$withSpinner(color = 'lightblue')
        ),
        swiperSlide(
          style = 'height: 100%; width: 100%; overflow: hidden; padding: 1px;',
          uiOutput(ns('slider2')) |> shinycssloaders$withSpinner(color = 'lightblue')
        ),
        swiperSlide(
          style = 'height: 100%; width: 100%; overflow: hidden; padding: 1px;',
          uiOutput(ns('slider3')) |> shinycssloaders$withSpinner(color = 'lightblue')
        ),
        swiperSlide(
          style = 'height: 100%; width: 100%; overflow: hidden; padding: 1px;',
          uiOutput(ns('slider4')) |> shinycssloaders$withSpinner(color = 'lightblue')
        )
      ),  
      footer = uiOutput(ns('uiFooter')))))
      
      output$uiFooter <- renderUI({
        
        current <- sliderPosition()
        
        if(!is.null(componenteReactive())){
          tagList(actionButton(ns("btSair"), label = "Selecionar",icon = icon("arrow-left")))
        }
        else if(current == 1){
          tagList(actionButton(ns("btSair"), label = "Sair",icon = icon("arrow-left")))
        }
        else if(current == 2){
          tagList(actionButton(ns("btSair"), label = "Sair",icon = icon("arrow-left")),
          actionButton(ns('btClear'), "Limpar", icon = icon("eraser")),
          actionButton(ns('btSalvar'),class = "btn-success",label = "Avançar",icon = icon("arrow-right")))
        }
        else if(current == 3){
          tagList(actionButton(ns("btSair"), label = "Voltar",icon = icon("arrow-left")),actionButton(ns('btSalvar'),class = "btn-warning",label = "Atualizar",icon = icon("save")))
        }
        
      })
      
      output$titleTexto <- renderText({
        
        if(sliderPosition() == 1L){
          'Registros Objetos'
        }else{
          'Edição do Objeto'
        }

      })

      output$uiCameraFrameAlert <- renderUI({
        req(sliderPosition() == 3L, !is.null(frame_data), !is.null(input$comboCameras))

        camera <- cameras |> filter(name_camera == input$comboCameras)
        .camera_frame_alert(frame_data, camera)
      })
      
      output$slider1 <- renderUI({
        
        dataset  <- objetos()
        
        setor    <- setores |> filter(name_setor == input$comboSetor)
        dataset  <- dataset |> filter(cd_id_setor == setor$cd_id_setor)
        if(nrow(dataset) == 0){
          div_tmp <- div(
            style = "margin-top: 50px;display: flex; justify-content: center; align-items: center; height: 100%; width: 100%;",
            "Não há registro de objetos"
          )
          return(div_tmp)
        }
        
        output$tableDinamicaObjeto <- DT$renderDataTable({
          
          colunaNames <- c('LINHA','OBJETO','TIPO','ID GRUPO','VISUALIZAR / EDITAR','REMOVER')
          
          DT$datatable({
            
            dataset |> 
            mutate_if(is.POSIXct,function(x){ format(x,'%d/%m/%Y %H:%M:%S')})  |> 
            mutate_if(is.Date,function(x){ format(x,'%d/%m/%Y')}) |> 
            mutate_if(is.character,toupper) |> 
            mutate(
              !!colunaNames[1] := 1:nrow(dataset),
              !!colunaNames[2] :=  dataset$name_objeto,
              !!colunaNames[3] :=  dataset$name_objeto_tipo,
              !!colunaNames[4] := if ("id_grupo" %in% names(dataset)) dplyr::coalesce(as.character(dataset$id_grupo), "") else rep("", nrow(dataset)),
              !!colunaNames[5] :=  sapply(dataset$cd_id_objeto, function (x) {
                
                as.character(
                  actionButton(
                    paste0(ns('btEdit')),
                    label = '',
                    icon = icon('eye'),
                    onclick = paste0('Shiny.setInputValue(\"',ns("editPressedRow"),'\","',x,'",{priority: "event"})'),
                    #style = 'background-color: transparent; color: lightblue; border-solid: none;'
                  )
                )
              }),
              !!colunaNames[6] :=  sapply(dataset$cd_id_objeto,function (x) {
                
                as.character(
                  actionButton(
                    paste0(ns('btRemove')),
                    label = '',
                    icon = icon('trash'),
                    onclick = paste0('Shiny.setInputValue(\"',ns("deletePressedRow"),'\","',x,'",{priority: "event"})'),
                    #style = 'background-color: transparent; color: lightblue; border-solid: none;'
                  )
                )
              })
              
            ) |> select(colunaNames) |> arrange(colunaNames[2])
          },  
          class = 'cell-border stripe',
          extensions = 'Scroller',
          options = dtProfessionalOptions(
            columnDefs = list(
              list(visible = FALSE, targets = c(0)),
              list(className = 'dt-center', targets = "_all"),
              list(width = '75px', targets = c(1, 3, 4, 5)),
              list(width = 'auto', targets = c(2))
            ),
            search_placeholder = "Pesquisar objeto, tipo ou ID grupo"
          ),
          escape = F,
          selection = 'none',
        ) |> DT$formatStyle(colunaNames, cursor = 'pointer')
        
      })
      
      dtProfessionalOutput(ns, 'tableDinamicaObjeto')
      
    })
    
    output$slider2 <- renderUI({
      
      req(objeto())
      objetoSelect <- objeto()
      
      cameraComponetes <- unique(unlist(map(objetoSelect$config[[1]]$componentes,~ map(.x$cameras,~ .x$name_camera))))
      
      session$onFlushed(function(){
        disable(ns("comboTipoObjeto"))
      }, once = TRUE)

      uiMain(ns,
        setores,
        cameras,
        tiposObjeto,
        valueComboSetor  = objetoSelect$name_setor,
        valueAtivo       = as.logical(objetoSelect$fg_ativo),
        valueIsDev       = .object_flag_value(objetoSelect, "is_dev"),
        valueGrupo       = .object_flag_value(objetoSelect, "grupo"),
        valueIdGrupo     = .object_integer_value(objetoSelect, "id_grupo"),
        valueTextName    = objetoSelect$name_objeto,
        valueTipoObjeto  = objetoSelect$name_objeto_tipo,
        valueMultiCamera = cameraComponetes,
        valueTempoContexto  = objetoSelect$timeline_context_sec
      )
      
    })
    
    output$slider3 <- renderUI({
      
      req(sliderPosition() == 3L)
      
      camerasTargets <- isolate(input$multiCameras) 
      objetoSelect   <- isolate(objeto())
      tipoObjeto     <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
      
      obs2$clear()
      
      update_visible_componentes <- function() {
        if (tipoObjeto$cd_id_objeto_tipo == 2L) return(invisible(NULL))
        
        if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) {
          updateCheckboxGroupInput(session, "checkComponentesVisiveis", choices = character(0), selected = character(0))
          return(invisible(NULL))
        }
        
        camera <- cameras |> filter(name_camera == input$comboCameras)
        if (nrow(camera) != 1L) return(invisible(NULL))
        camera_key <- as.character(camera$cd_id_camera)
        
        info_choices <- .component_visibility_choices(frame_data$componente[[1]], camera$cd_id_camera)
        prev_map <- isolate(visiveisPrevPorCamera())
        prev_ids <- prev_map[[camera_key]]
        if (is.null(prev_ids)) prev_ids <- character(0)
        selected_ids <- isolate(input$checkComponentesVisiveis)
        if (is.null(selected_ids)) {
          selected_ids <- info_choices$ids
        } else {
          selected_ids <- intersect(as.character(selected_ids), info_choices$ids)
          ids_novos <- setdiff(info_choices$ids, prev_ids)
          selected_ids <- unique(c(selected_ids, ids_novos))
        }
        
        updateCheckboxGroupInput(
          session,
          "checkComponentesVisiveis",
          choices = info_choices$choices,
          selected = selected_ids
        )
        
        prev_map[[camera_key]] <- info_choices$ids
        visiveisPrevPorCamera(prev_map)
      }
      
      # --- novo polígono ---
      obs2$add(observeEvent(input$mapFrame_draw_new_feature, {
        feat <- input$mapFrame_draw_new_feature
        if (is.null(feat) || !identical(feat$geometry$type, "Polygon")) return()
        
        cameraTarget <- cameras |> filter(name_camera == input$comboCameras)
        req(nrow(cameraTarget) == 1)  # CHANGE: garante 1 camera
        
        coords <- feat$geometry$coordinates[[1]]
        lng    <- vapply(coords, function(x) x[[1]], numeric(1))
        lat    <- vapply(coords, function(x) x[[2]], numeric(1))
        poly   <- .drop_dup_last(tibble::tibble(x = lng, y = lat))  # CHANGE: helper
        
        componentes_atual <- NULL
        if (!is.null(frame_data$componente)) {
          componentes_atual <- frame_data$componente[[1]]
        }
        componentes_atual <- .ensure_component_colors(componentes_atual)
        used_colors <- if (is.null(componentes_atual)) character(0) else componentes_atual$color_componente

        row_new <- tibble::tibble(
          cd_id_componente    = feat$properties$`_leaflet_id`,
          name_componente     = "",
          cd_id_camera        = cameraTarget$cd_id_camera,
          poligno_componente  = list(poly),
          estrutura           = list(NULL),
          color_componente    = .next_component_color(used_colors)
        )
        
        if (is.null(componentes_atual) || nrow(componentes_atual) == 0L) {
          frame_data$componente[[1]] <<- row_new
        } else {
          frame_data$componente[[1]] <<- bind_rows(componentes_atual, row_new)
        }
        update_visible_componentes()
        #objetos dinamicos apenas 1 compomentes
        if(tipoObjeto$cd_id_objeto_tipo == 2L && nrow(frame_data$componente[[1]]) == 1L){
          camera          <- cameras |> filter(name_camera == input$comboCameras)
          componentes     <- frame_data$componente[[1]]   
          updateObjDynamic(TRUE)
          output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = TRUE)})
        }else{
          camera          <- cameras |> filter(name_camera == input$comboCameras)
          componentes     <- frame_data$componente[[1]]
          output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = FALSE)})
        }

      }, ignoreInit = TRUE))
      
      # --- edições (mover vértices etc.) ---
      obs2$add(observeEvent(input$mapFrame_draw_edited_features, {
        feats        <- input$mapFrame_draw_edited_features
        cameraTarget <- cameras |> filter(name_camera == input$comboCameras)  # mantido
        if (is.null(feats$features) || length(feats$features) == 0) return()
        if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) return()
        
        df <- frame_data$componente[[1]]
        
        for (i in seq_along(feats$features)) {
          feat <- feats$features[[i]]
          id_  <- feat$properties$`_leaflet_id`
          if (!is.null(feat$properties$layerId)) id_ <- feat$properties$layerId
          
          coords <- feat$geometry$coordinates[[1]]
          lng  <- vapply(coords, function(x) x[[1]], numeric(1))
          lat  <- vapply(coords, function(x) x[[2]], numeric(1))
          poly <- .drop_dup_last(tibble::tibble(x = lng, y = lat))
          
          idx <- match(id_, df$cd_id_componente)  # CHANGE: match() é mais seguro
          if (!is.na(idx)) {
            df$poligno_componente[[idx]] <- poly
          } # se não achar, ignora silenciosamente
        }
        
        df <- .ensure_component_colors(df)
        frame_data$componente[[1]] <<- df
        update_visible_componentes()

        camera          <- cameras |> filter(name_camera == input$comboCameras)
        componentes     <- frame_data$componente[[1]]
        is_dynamic      <- updateObjDynamic()
        output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = is_dynamic)})
      }, ignoreInit = TRUE))
      
      # --- deleções ---
      obs2$add(observeEvent(input$mapFrame_draw_deleted_features, {
        feats <- input$mapFrame_draw_deleted_features
        if (is.null(feats$features) || length(feats$features) == 0) return()
        if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) return()
        
        df <- frame_data$componente[[1]]
        for (i in seq_along(feats$features)) {
          feat <- feats$features[[i]]
          id_  <- feat$properties$`_leaflet_id`
          if (!is.null(feat$properties$layerId)) id_ <- feat$properties$layerId
          
          # CHANGE: filtro seguro (evita [-which()] quando não encontra)
          df <- filter(df, .data$cd_id_componente != id_)
        }
        frame_data$componente[[1]] <<- .ensure_component_colors(df)
        update_visible_componentes()

        camera          <- cameras |> filter(name_camera == input$comboCameras)
        componentes     <- frame_data$componente[[1]]
        is_dynamic      <- updateObjDynamic()
        output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = is_dynamic)})
        
        if(nrow(df) == 0){
          if(isolate(updateObjDynamic())){
            camera          <- cameras |> filter(name_camera == input$comboCameras)
            componentes     <- frame_data$componente[[1]]   
            updateObjDynamic(FALSE)
            output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = FALSE)})
          }
        }
      }, ignoreInit = TRUE))
      
      # --- clique nos shapes (mantidos exatamente seus inputs) ---
      obs2$add(observeEvent(input$mapFrame_shape_poligonos_componentes_click, {
        req(!deleting())  # mantido
        ev <- input$mapFrame_shape_poligonos_componentes_click

        if(isolate(updateObjDynamic())) return()
        
        if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) return()
        target <- frame_data$componente[[1]] |> filter(.data$cd_id_componente == ev$id)
        
        if (nrow(target) == 1) {  # CHANGE: evita passar df vazio
          componenteReactive(target)
          swiperSlideNext(idSwiper)
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE))
      
      obs2$add(observeEvent(input$mapFrame_shape_click, {
        req(!deleting())  # mantido
        ev <- input$mapFrame_shape_click

        if(isolate(updateObjDynamic())) return()
        
        if (is.null(frame_data$componente) || is.null(frame_data$componente[[1]])) return()
        target <- frame_data$componente[[1]] |> filter(.data$cd_id_componente == ev$id)
        
        if (nrow(target) == 1) {  # CHANGE: idem
          componenteReactive(target)
          swiperSlideNext(idSwiper)
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE))
      
      # --- estados de delete (mantidos seus inputs) ---
      obs2$add(observeEvent(input$mapFrame_draw_deletestart, {
        deleting(TRUE)
      }, ignoreInit = TRUE))
      obs2$add(observeEvent(input$mapFrame_draw_deletestop, {
        deleting(FALSE)
      }, ignoreInit = TRUE))
      
      obs2$add(observeEvent(input$mapFrame_draw_stop, {
        if (identical(input$mapFrame_draw_stop$mode, "remove")) {
          deleting(FALSE)
        }
      }, ignoreInit = TRUE))
      
      obs2$add(observeEvent(input$comboCameras,{
        update_visible_componentes()
        camera          <- cameras |> filter(name_camera == input$comboCameras)
        componentes     <- frame_data$componente[[1]]   
        is_dynamic      <- updateObjDynamic()
        output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = is_dynamic)})
      },ignoreNULL = TRUE))
      
      obs2$add(observeEvent(input$checkComponentesVisiveis,{
        req(sliderPosition() == 3L)
        if(tipoObjeto$cd_id_objeto_tipo == 2L) return()
        camera <- cameras |> filter(name_camera == input$comboCameras)
        if (nrow(camera) != 1L) return()
        componentes <- frame_data$componente[[1]]
        is_dynamic  <- updateObjDynamic()
        output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis),is_dynamic = is_dynamic)})
      },ignoreNULL = FALSE, ignoreInit = TRUE))
      
      estruturas_dinamicos <- NULL
      visibilidade_estaticos <- NULL
      if(tipoObjeto$cd_id_objeto_tipo == 2L){

        componente     <- objetoSelect$config[[1]]$componentes[[1]]
        estruturasComp <- purrr::map_df(componente$estrutura,~ .x)
     
        changetextPlaceHolder()
        estruturas_dinamicos <- tagList(
          br(),
          inlineCSS(paste0("#", ns("textNameComponente"), " {text-transform: uppercase;}")),
          textInput(ns("textNameComponente"), label = "Nome",
          placeholder = "Digite o nome para o componente",value = componente$name_componente),
          multiInput(
            inputId = ns('multiEstruturaComp'),
            width = '100%',
            options = list(
              enable_search = T,
              non_selected_header = "Estrutura não selecionados",
              selected_header     = "Estrutura selecionados"
            ),
            label = "Estruturas ativas",
            selected = estruturasComp$name_estrutura,
            choices = NULL,
            choiceNames  = estruturas$name_estrutura,
            choiceValues = estruturas$name_estrutura
          ) |> tagAppendAttributes(style = ';height: auto; width: 100%;'))
        }else{
          visibilidade_estaticos <- checkboxGroupInput(
            ns("checkComponentesVisiveis"),
            label = "Mostrar/Ocultar poligonos por componente",
            choices = NULL,
            selected = NULL
          )
        }
        
        tagList(
          selectizeInput(ns('comboCameras'),label = 'Câmera',choices = camerasTargets,options  = list(
            dropdownParent = 'body',
            openOnFocus = TRUE,
            closeAfterSelect = TRUE
          )),
          uiOutput(ns("uiCameraFrameAlert")),
          leafletOutput(ns("mapFrame"), height = "512px", width = "100%"),
          visibilidade_estaticos,
          estruturas_dinamicos,
          br()
        )
      
    })
    
    # ---- CASCA SUPERIOR (apenas UI) ----
    output$slider4 <- renderUI({
      
      req(componenteReactive())
      
      componente <- componenteReactive()
      obs3$clear()
      obs3$add(observeEvent(input$comboEstrutura,{

          estrutura <- estruturas |> filter(name_estrutura == input$comboEstrutura)
          atributos <- map_df(estrutura$configs[[1]]$atributos,~ .x)
          width_nm  <- max(nchar(atributos$name_atributo %||% ""))
          
          nm  <- stringr::str_pad(atributos$name_atributo,width = width_nm,side  = "right")
          
          prefix_b <- ifelse(atributos$name_data == "QUALITATIVE","[ ","")
          prefix_e <- ifelse(atributos$name_data == "QUALITATIVE"," ]","")
          linhas <- paste0(nm, " = ",prefix_b,atributos$value_atributo,prefix_e)
          txt    <- paste(linhas, collapse = "\n")
          updateTextAreaInput(session,"info_estrutura",value = txt)
        
      },ignoreNULL = TRUE))
        
        estrutura  <- componente$estrutura[[1]]
        uiEstrutura(ns,componente$name_componente,estruturas,estrutura)
      })
    
    obs$add(observeEvent(input$editPressedRow,{
      
      obj <- isolate(objetos()) |> filter(cd_id_objeto == input$editPressedRow)
      objeto(obj)
      
      updateObjDynamic(obj$cd_id_objeto_tipo == 2L)
      swiperSlideNext(idSwiper)
      sliderPosition(isolate(sliderPosition()) + 1L)
      
    },ignoreInit = T))
    
    obs$add(observeEvent(input$deletePressedRow,{
      
      objeto <- isolate(objetos()) |> filter(cd_id_objeto == input$deletePressedRow)
      
      messageAlerta(
        input,
        ns,
        title   = paste0('Todos os objetos ligado a esse camerâ será excluido'),
        message = paste0('Deseja realmente excluir a camerâ ',objeto$name_camera,"?"),
        callback.no = function(){
          
        },
        callback.yes = function(){
          
           db$tryTransaction(function(conn){
            
            db$deleteTable(conn,"OBJETO","cd_id_objeto",objeto$cd_id_objeto)
            
            objetos.aux <- selectAllObjetos(conn)
            
            if(nrow(objetos.aux) == 0){
              #destroy all observe events
              obs$destroy()
              obs2$destroy()
              obs3$destroy()
              removeModal(session)
              callback()
              swiperDestroy(idSwiper)
            }else{
              objetos(objetos.aux)
            }
            
          })
          
        })
        
      },ignoreInit = T))
      
      
      obs$add(observeEvent(input$btSair,{
        
        current <- isolate(sliderPosition())
        
        if(current == 1L){
          obs$destroy()
          obs2$destroy()
          obs3$destroy()
          if(!is.null(frame_data)) unlink(frame_data$img_path)
          removeModal(session)
          callback()
          swiperDestroy(idSwiper)
        }else{
          status   <- is.null(isolate(componenteReactive()))
          if(!status){
            
            df_poly    <- frame_data$componente[[1]]
            componente <- isolate(componenteReactive())
            index      <- which(df_poly$cd_id_componente == componente$cd_id_componente)
            df_p       <- df_poly[index,]
            df_p$name_componente <- toupper(isolate(input$textNameComponente))
            estrutura  <- isolate(estruturas |> filter(name_estrutura == input$comboEstrutura))
            df_p$estrutura[[1]] <- estrutura
            frame_data$componente[[1]][index,] <<- df_p
            
            camera      <- cameras |> filter(name_camera == isolate(input$comboCameras))
            componentes <- frame_data$componente[[1]] 
            if(!isTRUE(isolate(updateObjDynamic())) && nrow(camera) == 1L){
              info_choices <- .component_visibility_choices(componentes, camera$cd_id_camera)
              selected_ids <- isolate(input$checkComponentesVisiveis)
              if (is.null(selected_ids)) {
                selected_ids <- info_choices$ids
              } else {
                selected_ids <- intersect(as.character(selected_ids), info_choices$ids)
              }
              updateCheckboxGroupInput(session, "checkComponentesVisiveis", choices = info_choices$choices, selected = selected_ids)
            }
            #update mapa
            #proxy_update_componentes(map_id = ns("mapFrame"),ns = ns, camera = camera,componentes = componentes)  
            #proxy_update_componentes(session,ns,ns("mapFrame"),camera, componentes)           
            output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,componentes_visiveis = isolate(input$checkComponentesVisiveis))})
            componenteReactive(NULL)
          }else{
            sliderPosition(isolate(sliderPosition()) - 1L)
          }
          
          if(current == 2L){
            if(!is.null(frame_data)) unlink(frame_data$img_path)
            objeto(NULL)
          }
          deleting(FALSE)
          editing(FALSE)
          swiperSlidePrevious(idSwiper)
          
        }
        
      },ignoreInit = T))
      
      ## Clear
      obs$add(observeEvent(input$btClear, {
        updateTextInput(session,'textNameObjeto', value = '')
        updateMultiInput(session,'multiCameras',choices = '')
        updateNumericInput(session, "numericIdGrupoObjeto", value = NA_integer_)
      },ignoreInit = TRUE))
      
      obs$add(observeEvent(input$btSalvar,{
        
        req(objeto())
        
        objetoSelect <- isolate(objeto())
        current      <- isolate(sliderPosition())
        
        if(current == 2L){
          
          nomeObjeto     <- isolate(toupper(input$textNameObjeto))
          camerasTargets <- isolate(input$multiCameras)
          idGrupoObjeto  <- suppressWarnings(as.integer(isolate(input$numericIdGrupoObjeto)))
          tipoObjeto     <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
          
          if(tipoObjeto$cd_id_objeto_tipo == 2 && length( camerasTargets) > 1){
            showNotification("Não foi possível avançar. Para objetos dinâmicos, selecione apenas uma câmera.", type = "warning")
            return()
          }
          
          if(stringi$stri_isempty(stringr$str_trim(nomeObjeto))){
            showNotification("O nome do Objeto não foi preenchido!", type = "warning")
            return()
          }
          
          if(length(camerasTargets) == 0){
            showNotification("Nenhuma câmera foi selecionada para objeto!", type = "warning")
            return()
          }
          
          if(is.na(idGrupoObjeto)){
            showNotification("O ID do grupo nÃ£o foi preenchido!", type = "warning")
            return()
          }

          if(checkifExistNameObjetoEdit(dbp$get_pool(),objetoSelect$cd_id_objeto,nomeObjeto)){
            showNotification("O nome do Objeto já possui nos registros!", type = "warning")
            return()
          }
          
          frame_data   <<- searchFramesByCamerasSelected(dbp$get_pool(),camerasTargets,cameras,objetoSelect)
          
          sliderPosition(isolate(sliderPosition()) + 1L)
          swiperSlideNext(idSwiper)
       
        }else if(current == 3L){

          tipoObjeto  <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
          componentes <- frame_data$componente[[1]]
          
          if(tipoObjeto$cd_id_objeto_tipo == 2L){
            componentes$name_componente <- toupper(isolate(input$textNameComponente))
            multiEstruturas <- estruturas |> filter(name_estrutura %in% isolate(input$multiEstruturaComp))
            if(nrow(multiEstruturas) == 0){
              showNotification("Nenhuma estrutura foi selecionada para o componente!", type = "warning")
              return()
            }
            componentes$estrutura <- list(multiEstruturas)
          }
          
          if(is.null(componentes)){
            showNotification("Nenhum desenho de poligno foi encontrado!", type = "warning")
            return()
          }else if(nrow(componentes) == 0){
            showNotification("Nenhum desenho de poligno foi encontrado!", type = "warning")
            return()
          }
          
          actionWebUser({
            if(!db$tryTransaction(function(conn){

            #check if it has already data of Câmera
            nomeObjeto     <- isolate(toupper(input$textNameObjeto))
            ativoObjeto    <- isolate(input$checkboxAtivoObjeto)
            isDevObjeto    <- isolate(input$checkboxIsDevObjeto)
            grupoObjeto    <- isolate(input$checkboxGrupoObjeto)
            idGrupoObjeto  <- suppressWarnings(as.integer(isolate(input$numericIdGrupoObjeto)))
            tipoObjeto     <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
            setor          <- setores |> filter(name_setor == isolate(input$comboSetor))

            if(is.na(idGrupoObjeto)){
              showNotification("O ID do grupo nÃ£o foi preenchido!", type = "warning")
              return()
            }
       
            obj                      <- list()
            obj$name_objeto          <- nomeObjeto
            obj$fg_ativo             <- as.integer(ativoObjeto)
            obj$is_dev               <- as.integer(isDevObjeto)
            obj$grupo                <- as.integer(grupoObjeto)
            obj$id_grupo             <- idGrupoObjeto
            obj$cd_id_setor          <- setor$cd_id_setor
            obj$timeline_context_sec <- 5L #isolate(input$sliderTimeContexto)
        
            db$updateTable(conn,"OBJETO",obj,"cd_id_objeto",objetoSelect$cd_id_objeto)
     
            id_obj_config  <- db$nextSequenciaID(conn, "objeto_config", id_col = "cd_id_obj_conf", schema = "public")
       
            insertNewObjetoConfig(conn,id_obj_config,objetoSelect)
            
            for(i in 1:nrow(componentes)){
              #insert componente do objeto
              comp       <- componentes[i,]
              estruturas <- comp$estrutura[[1]]
              
              for(k in 1:nrow(estruturas)){
                estrutura <- estruturas[k,]
                poligno   <- jsonlite$toJSON(comp$poligno_componente[[1]],auto_unbox = T)
                
                objComp <- list()
                objComp$name_componente    <- comp$name_componente
                objComp$poligno_componente <- poligno
                objComp$cd_id_obj_conf     <- id_obj_config
                objComp$cd_id_camera       <- comp$cd_id_camera
                objComp$cd_id_componente   <- db$nextSequenciaID(conn, "componente", id_col = "cd_id_componente", schema = "public")
                objComp$cd_id_estrutura    <- estrutura$cd_id_estrutura
                
                db$insertTable(conn,"COMPONENTE",objComp)
              }
            }
     
            #load todos os setores
            objetos(selectAllObjetos(conn))
            # volta para init
            swiperSlideTo(idSwiper,0)
            sliderPosition(1L)
            deleting(FALSE)
            editing(FALSE)
            showNotification("objeto atualizado com sucesso!", type = "warning")
            
          })){
            showNotification("Não foi possivel salvar o objeto, durante o processo houve falha!", type = "error")
          }
          }, delay = 0, lock_id = "objeto_update_save")
        }
      },ignoreInit = T))
}



uiMain <- function(ns,
                   setores,
                   cameras,
                   tiposObjeto,
                   valueComboSetor = NULL,
                   valueAtivo      = TRUE,
                   valueIsDev      = FALSE,
                   valueGrupo      = FALSE,
                   valueIdGrupo    = NA_integer_,
                   valueTextName   = NULL,
                   valueTipoObjeto = NULL,
                   valueMultiCamera = NULL,
                   valueTempoContexto = 5
                  ){

     valueAtivo <- isTRUE(as.logical(valueAtivo))
     valueIsDev <- isTRUE(as.logical(valueIsDev))
     valueGrupo <- isTRUE(as.logical(valueGrupo))
     valueIdGrupo <- suppressWarnings(as.integer(valueIdGrupo[[1]]))
     if (!length(valueIdGrupo) || is.na(valueIdGrupo)) valueIdGrupo <- NA_integer_

     changetextPlaceHolder()
      cameras
      div(
          inlineCSS(paste0("#",ns("textNameObjeto")," {text-transform: uppercase;}")),
          fluidRow(
           column(12,selectizeInput(ns('comboSetor'),label = 'Setor',choices = setores$name_setor,selected = valueComboSetor))
           #column(6,sliderInput(ns('sliderTimeContexto'),label = 'Tempo Contexto Segundo',min = 1,step = 1,max = 10,round = TRUE,value = valueTempoContexto))
          ),
          div(
            style = "display: flex; flex-wrap: wrap; gap: 12px; align-items: flex-end;",
            div(
              style = "flex: 0 1 120px;",
              tags$label("Ativar", style = "font-size: 15px;"),
              div(style = "margin-top: 5px;",
              prettyToggle(
                inputId   = ns("checkboxAtivoObjeto"), 
                label_on  = "Sim",
                label_off = "Não",
                outline   = TRUE, plain = TRUE, value = valueAtivo,
                icon_on   = icon("thumbs-up"),
                icon_off  = icon("thumbs-down"),
                bigger    = TRUE, width = "auto",
              ))),
            div(
              style = "flex: 0 1 160px;",
              tags$label("Desenvolvimento", style = "font-size: 15px;"),
              div(
                style = "margin-top: 5px;",
                prettyToggle(
                  inputId   = ns("checkboxIsDevObjeto"),
                  label_on  = "Sim",
                  label_off = "NÃO",
                  outline   = TRUE, plain = TRUE, value = valueIsDev,
                  icon_on   = icon("check"),
                  icon_off  = icon("times"),
                  bigger    = TRUE, width = "auto"
                )
              )
            ),
            div(
              style = "flex: 0 1 175px;",
              tags$label("Grupado componentes", style = "font-size: 15px;"),
              div(
                style = "margin-top: 5px;",
                prettyToggle(
                  inputId   = ns("checkboxGrupoObjeto"),
                  label_on  = "Sim",
                  label_off = "NÃO",
                  outline   = TRUE, plain = TRUE, value = valueGrupo,
                  icon_on   = icon("check"),
                  icon_off  = icon("times"),
                  bigger    = TRUE, width = "auto"
                )
              )
            ),
            div(
              style = "flex: 1 1 280px; min-width: 240px;",
              textInput(
                paste0(ns('textNameObjeto')),
                label = 'Nome',
                placeholder = 'Digite o nome para o Objeto',
                width = "100%",
                value = valueTextName
              )
            ),
            div(
              style = "flex: 0 1 150px; min-width: 130px;",
              numericInput(
                ns("numericIdGrupoObjeto"),
                label = "ID Grupo",
                value = valueIdGrupo,
                min = 0,
                step = 1,
                width = "100%"
              )
            ),
            div(
              style = "flex: 0 1 140px; min-width: 120px;",
              selectizeInput(
                ns('comboTipoObjeto'),
                label = 'Tipo',
                choices = tiposObjeto$name_objeto_tipo,
                width = "100%",
                selected = valueTipoObjeto
              )
            ),
          ),
          multiInput(
            inputId = ns('multiCameras'),
            width = '100%',
            options = list(
              enable_search = T,
              non_selected_header = "Câmeras não selecionados",
              selected_header     = "Câmeras selecionados"
            ),
            selected = valueMultiCamera,
            label = "Câmeras ativas",
            choices = NULL,
            choiceNames  = .camera_choice_names(cameras),
            choiceValues = as.character(cameras$name_camera)
          ) |> tagAppendAttributes(style = ';height: auto; width: 100%;'),
          .camera_selection_help(cameras)
        )
}


.component_name_for_map <- function(name_componente, component_id) {
  nm <- as.character(name_componente)[1]
  if (length(nm) == 0 || is.na(nm) || stringi::stri_isempty(stringr::str_trim(nm))) {
    return(paste0("COMP-", component_id))
  }
  nm
}

.next_component_color <- function(used_colors = character(0)) {
  used <- unique(used_colors[!is.na(used_colors) & nzchar(used_colors)])
  idx <- length(used) + 1L

  repeat {
    hue <- ((idx - 1L) * 137.508) %% 360
    sat <- 0.78 - (((idx - 1L) %/% 360L) %% 3L) * 0.08
    val <- 0.92 - (((idx - 1L) %/% (360L * 3L)) %% 3L) * 0.08
    candidate <- grDevices::hsv(
      h = hue / 360,
      s = max(min(sat, 1), 0.55),
      v = max(min(val, 1), 0.55)
    )

    if (!(candidate %in% used)) return(candidate)
    idx <- idx + 1L
  }
}

.ensure_component_colors <- function(componentes) {
  if (is.null(componentes) || !is.data.frame(componentes)) return(componentes)
  has_color_col <- "color_componente" %in% names(componentes)

  if (nrow(componentes) == 0L) {
    if (!has_color_col) {
      componentes[["color_componente"]] <- character(0)
    }
    return(componentes)
  }

  if (!has_color_col) {
    componentes[["color_componente"]] <- rep(NA_character_, nrow(componentes))
  } else {
    componentes[["color_componente"]] <- as.character(componentes[["color_componente"]])
  }

  used <- unique(componentes[["color_componente"]][!is.na(componentes[["color_componente"]]) & nzchar(componentes[["color_componente"]])])

  for (i in seq_len(nrow(componentes))) {
    cor <- componentes[["color_componente"]][[i]]
    if (is.na(cor) || !nzchar(cor)) {
      cor <- .next_component_color(used)
      componentes[["color_componente"]][[i]] <- cor
      used <- c(used, cor)
    }
  }

  componentes
}

.polygon_center <- function(poly) {
  if (is.null(poly) || nrow(poly) == 0L) return(NULL)
  list(
    lng = mean(poly$x, na.rm = TRUE),
    lat = mean(poly$y, na.rm = TRUE)
  )
}

.component_visibility_choices <- function(componentes, camera_id) {
  if (is.null(componentes) || !is.data.frame(componentes) || nrow(componentes) == 0L) {
    return(list(choices = character(0), ids = character(0)))
  }

  comps <- componentes |> filter(.data$cd_id_camera == camera_id)
  if (nrow(comps) == 0L) {
    return(list(choices = character(0), ids = character(0)))
  }

  ids <- as.character(comps$cd_id_componente)
  labels <- vapply(
    seq_len(nrow(comps)),
    function(i) .component_name_for_map(comps$name_componente[[i]], comps$cd_id_componente[[i]]),
    character(1)
  )

  list(
    choices = stats::setNames(ids, labels),
    ids = ids
  )
}

.filter_componentes_visible <- function(componentes, camera_id, visible_ids = NULL) {
  if (is.null(componentes) || !is.data.frame(componentes) || nrow(componentes) == 0L) return(componentes)
  if (is.null(visible_ids)) return(componentes)

  camera_chr <- as.character(camera_id)
  ids_visiveis <- as.character(visible_ids)
  keep <- as.character(componentes$cd_id_camera) != camera_chr |
    as.character(componentes$cd_id_componente) %in% ids_visiveis

  componentes[keep, , drop = FALSE]
}

uiMapa <-function(ns,camera,cameras,frame_data,componentes = NULL,is_dynamic = FALSE,componentes_visiveis = NULL){

  data     <- frame_data |> filter(id == camera$cd_id_camera)
  data_uri <- base64enc$dataURI(file = data$img_path, mime = "image/png")
  
  mapa <- leaflet(options = leafletOptions(
          crs = leafletCRS(crsClass = "L.CRS.Simple"),
          zoomSnap  = 0,        # permite zoom fracionário
          zoomDelta = 0.25      # passo de zoom ao usar scroll
        )) |>
        addDrawToolbar(
          targetGroup = MAP_GROUP_COMPONENT_POLYGON,
          polylineOptions      = FALSE,
          circleMarkerOptions  = FALSE,
          markerOptions        = FALSE,
          # HABILITA polígono
          polygonOptions = if(!is_dynamic){
            drawPolygonOptions(
              shapeOptions = drawShapeOptions(fillOpacity = 0.2, weight = 2),
              showArea = FALSE
            )
          }else{
            FALSE
          },
          # HABILITA retângulo
          rectangleOptions = if(!is_dynamic){
            drawRectangleOptions(
              shapeOptions = drawShapeOptions(fillOpacity = 0.2, weight = 2),
              showArea = FALSE
            )
          }else{
            FALSE
          },
          # HABILITA círculo
          circleOptions = FALSE,
          editOptions = editToolbarOptions(
            selectedPathOptions = selectedPathOptions()
          )
        ) |>
        htmlwidgets$onRender(
          "
            function(el, x, data){
              var map = this;
              var bounds = [[0,0], [data.h, data.w]];   // [[0,0],[512,512]]

              // overlay da imagem
              L.imageOverlay(data.imageUrl, bounds, {opacity: 1}).addTo(map);

              // função que calcula o zoom para caber 100% da imagem (largura e altura)
              function fitWholeImage(){
                // tamanho do viewport atual
                var size  = map.getSize();
                var scaleX = size.x / data.w;  // quanto cabe na largura
                var scaleY = size.y / data.h;  // quanto cabe na altura
                var scale  = Math.min(scaleX, scaleY);

                // CRS.Simple usa potências de 2
                var targetZoom = Math.log2(scale);
                if (!isFinite(targetZoom)) targetZoom = 0;

                // centraliza e aplica zoom
                var center = [data.h/2, data.w/2];
                map.setView(center, targetZoom, {animate:false});

                // prende o mapa aos bounds da imagem
                map.setMaxBounds(bounds);
                map.options.maxBoundsViscosity = 1.0;

                // impede 'zoom out' que faria sobrar bordas além da imagem
                map.setMinZoom(targetZoom);

                // garante cálculo correto após render
                setTimeout(function(){ map.invalidateSize(); }, 0);
              }

              fitWholeImage();
              map.on('resize', fitWholeImage); // reencaixa se a janela mudar de tamanho
            }
          ",
          data = list(imageUrl = data_uri, w = data$w, h = data$h)
        )
   
  if(!is.null(componentes)){
    if (!isTRUE(is_dynamic)) {
      componentes <- .filter_componentes_visible(componentes, camera$cd_id_camera, componentes_visiveis)
    }
    componentes <- .ensure_component_colors(componentes)

    for(i in seq_len(nrow(componentes))){

      comp    <- componentes[i,]
      
      if(comp$cd_id_camera != camera$cd_id_camera) next

      poligno   <- comp$poligno_componente[[1]]
      estrutura <- comp$estrutura[[1]]
      comp_nome <- .component_name_for_map(comp$name_componente, comp$cd_id_componente)
      comp_cor  <- comp$color_componente

      estrutura_nome <- ""
      if (!is.null(estrutura) && nrow(estrutura) > 0 && !is.null(estrutura$name_estrutura)) {
        estrutura_nome <- paste(unique(estrutura$name_estrutura), collapse = ", ")
      }

      label <- HTML(
        paste0(
          "<strong>COMPONENTE:</strong> ", comp_nome,
          "<br><strong>estrutura:</strong> ", estrutura_nome
        )
      )

      mapa <- mapa |> addPolygons(
        group       = MAP_GROUP_COMPONENT_POLYGON,
        lng         = poligno$x,
        lat         = poligno$y,
        layerId     = comp$cd_id_componente,
        weight      = 2,
        color       = comp_cor,
        fillColor   = comp_cor,
        fillOpacity = 0.2,
        label       = label
      )

      centro <- .polygon_center(poligno)
      if (!is.null(centro)) {
        mapa <- mapa |>
          addLabelOnlyMarkers(
            lng = centro$lng,
            lat = centro$lat,
            label = comp_nome,
            group = MAP_GROUP_COMPONENT_NAMES,
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "center",
              textOnly = TRUE,
              style = list(
                "font-size" = "12px",
                "font-weight" = "700",
                "color" = comp_cor,
                "text-shadow" = "0 0 3px #FFFFFF, 0 0 6px #FFFFFF"
              )
            )
          )
      }
    }
  }
  mapa <- mapa |>
    addLayersControl(
      overlayGroups = c(MAP_GROUP_COMPONENT_POLYGON, MAP_GROUP_COMPONENT_NAMES),
      options = layersControlOptions(collapsed = FALSE),
      position = "topright"
    )
   mapa
}

# 2) Atualiza os componentes via PROXY (chame sempre que 'componentes' mudar)
proxy_update_componentes <- function(session,ns,map_id,camera, componentes){
  comps <- componentes |> filter(.data$cd_id_camera == camera$cd_id_camera)
  comps <- .ensure_component_colors(comps)

  prx <- leafletProxy(mapId = map_id,session = session) |>
    clearGroup(MAP_GROUP_COMPONENT_POLYGON) |>
    clearGroup(MAP_GROUP_COMPONENT_NAMES)

  if (nrow(comps) == 0) return(invisible(TRUE))

  for(i in seq_len(nrow(comps))){
    comp <- comps[i,]
    poly <- comp$poligno_componente[[1]]
    if (is.character(poly)) {
      poly <- jsonlite::fromJSON(poly)
    }

    comp_nome <- .component_name_for_map(comp$name_componente, comp$cd_id_componente)
    comp_cor  <- comp$color_componente
    centro    <- .polygon_center(poly)

    prx <- prx |>
      addPolygons(
        lng         = poly$x,
        lat         = poly$y,
        group       = MAP_GROUP_COMPONENT_POLYGON,
        layerId     = as.character(comp$cd_id_componente),
        weight      = 2,
        color       = comp_cor,
        fillColor   = comp_cor,
        fillOpacity = 0.2,
        label       = comp_nome
      )

    if (!is.null(centro)) {
      prx <- prx |>
        addLabelOnlyMarkers(
          lng = centro$lng,
          lat = centro$lat,
          label = comp_nome,
          group = MAP_GROUP_COMPONENT_NAMES,
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "font-size" = "12px",
              "font-weight" = "700",
              "color" = comp_cor,
              "text-shadow" = "0 0 3px #FFFFFF, 0 0 6px #FFFFFF"
            )
          )
        )
    }
  }

  invisible(TRUE)
}

searchFramesByCamerasSelected <- function(conn,camerasTargets,cameras,objeto = NULL){

  df <- map_df(camerasTargets,function(camera){

       cam_id <- cameras |> filter(name_camera == camera)
       if (nrow(cam_id) != 1L) return(NULL)

       frame     <- selectLastFrameById(conn, cam_id$cd_id_camera)
       img       <- NULL
       img_ok    <- FALSE

       if (nrow(frame) > 0L && !is.null(frame$data_frame[[1]])) {
         img <- tryCatch(
           image_read(frame$data_frame[[1]]),
           error = function(e) NULL
         )
         img_ok <- !is.null(img)
       }

       frame_warning <- .frame_warning_text(cam_id$name_camera[[1]], frame, img_ok)
       frame_is_placeholder <- !img_ok

       if (is.null(img)) {
         img <- .blank_frame_image()
       }

       info      <- image_info(img)
       w         <- as.integer(info$width[[1]])
       h         <- as.integer(info$height[[1]])
      componentes <- NULL

      if(!is.null(objeto)){
        comp <- objeto$config[[1]]$componentes[[1]]
        comp <- comp |> filter(cd_id_camera == cam_id$cd_id_camera)
        
        if(nrow(comp) > 0){
          componentes <- comp
        }
      }
       # Gera um PNG em temp e um dataURI (para overlay no Leaflet)
       tmp_png <- tempfile(fileext = ".png")
       image_write(img, path = tmp_png, format = "png")
       img_path <- tmp_png
       tibble(id     = cam_id$cd_id_camera,
               frame = list(frame),
               img   = list(img),
               info  = list(info),
               w     = w,
               h     = h,
               img_path   = img_path,
               has_frame  = .camera_has_frame(cam_id),
               frame_is_placeholder = frame_is_placeholder,
               frame_warning = frame_warning %||% NA_character_,
               componente = list(componentes)
               )
    })

  componentes_list <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, df$componente)
  if (length(componentes_list)) {
    df$componente[[1]] <- bind_rows(componentes_list)
  } else {
    df$componente[[1]] <- .empty_componentes_df()
  }

  df$componente[[1]] <- .ensure_component_colors(df$componente[[1]])
  df
}

uiEstrutura <- function(ns,nameComp,estruturas,estrutura){
  
  div(
    inlineCSS(paste0("#", ns("textNameComponente"), " {text-transform: uppercase;}")),
    textInput(ns("textNameComponente"), label = "Nome",
    placeholder = "Digite o nome para o componente",value = nameComp),
    selectizeInput(ns('comboEstrutura'),label = 'Estrutura',choices = estruturas$name_estrutura,
    selected = estrutura$name_estrutura,
    options  = list(
      dropdownParent = 'body',
      openOnFocus = TRUE,
      closeAfterSelect = TRUE
    )),
    textAreaInput(
      ns("info_estrutura"),
      label = "Atributos",
      resize = "none",
      width = "100%") |>
      shiny::tagAppendAttributes(style = 'width: 100%;') |> 
      tagAppendAttributesFind(2,style = 'text-align: center;',readonly = 'readonly')
)
}

# --- util: checa se polígono fechado repete o 1º ponto no final ---
.drop_dup_last <- function(poly) {
  if (nrow(poly) >= 2 && isTRUE(all(poly[1, ] == poly[nrow(poly), ]))) {
    poly[-nrow(poly), ]
  } else poly
}


