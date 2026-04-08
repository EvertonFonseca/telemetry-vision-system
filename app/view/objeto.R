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
  treinar = ./treinar[uiNewTreinar],
  ../model/Swiper[...],
  DT,
  shinycssloaders,
  .. / logic/objeto_dao[...],
  .. / logic/camera_dao[...],
  .. / logic/setor_dao[...],
  ../ logic/estrutura_dao[...],
  ../logic/treinar_dao[selectAllTypesPacote],
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

.objeto_contexto_tz <- function(tz = NULL) {
  tz <- trimws(as.character(tz)[1])
  tz_choices <- OlsonNames()
  tz_default <- "America/Sao_Paulo"

  if (is.na(tz) || !nzchar(tz) || !(tz %in% tz_choices)) {
    return(tz_default)
  }

  tz
}

.objeto_contexto_input_is_blank <- function(x) {
  if (is.null(x) || !length(x)) return(TRUE)
  if (inherits(x, "POSIXt")) return(all(is.na(x)))

  x_chr <- trimws(as.character(x))
  !length(x_chr) || all(is.na(x_chr) | !nzchar(x_chr))
}

.objeto_contexto_parse_datetime <- function(x, tz_local = .objeto_contexto_tz()) {
  if (.objeto_contexto_input_is_blank(x)) return(NULL)

  if (inherits(x, "POSIXt")) {
    out <- as.POSIXct(x[[1]])
  } else {
    txt <- trimws(as.character(x[[1]]))
    txt_norm <- gsub("T", " ", txt, fixed = TRUE)
    txt_norm <- sub("Z$", "", txt_norm, ignore.case = TRUE)
    txt_norm <- sub("\\s+UTC$", "", txt_norm, ignore.case = TRUE)

    attempts <- list(
      suppressWarnings(dmy_hms(txt_norm, tz = tz_local)),
      suppressWarnings(dmy_hm(txt_norm, tz = tz_local)),
      suppressWarnings(dmy(txt_norm, tz = tz_local)),
      suppressWarnings(ymd_hms(txt_norm, tz = tz_local)),
      suppressWarnings(ymd_hm(txt_norm, tz = tz_local)),
      suppressWarnings(ymd(txt_norm, tz = tz_local)),
      suppressWarnings(as.POSIXct(txt, tz = tz_local)),
      suppressWarnings(as.POSIXct(txt_norm, tz = tz_local))
    )

    out <- NULL
    for (candidate in attempts) {
      if (!is.null(candidate) && length(candidate) && !all(is.na(candidate))) {
        out <- as.POSIXct(candidate[[1]])
        break
      }
    }
  }

  if (is.null(out) || !length(out) || any(is.na(out))) return(NULL)

  tz_in <- attr(out, "tzone")
  tz_in <- as.character(tz_in)[1]
  if (is.na(tz_in) || !nzchar(tz_in)) {
    out <- lubridate::force_tz(out, tzone = tz_local)
  }

  out
}

.objeto_contexto_format_local <- function(x, tz_local = .objeto_contexto_tz()) {
  dt <- .objeto_contexto_parse_datetime(x, tz_local = tz_local)
  if (is.null(dt) || !length(dt) || any(is.na(dt))) return("")

  format(lubridate::with_tz(dt, tzone = tz_local), "%d/%m/%Y %H:%M:%S")
}

.objeto_contexto_to_utc <- function(x, tz_local = .objeto_contexto_tz()) {
  x <- .objeto_contexto_parse_datetime(x, tz_local = tz_local)
  if (is.null(x) || !length(x) || any(is.na(x))) return(NULL)

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

.objeto_contexto_raw_empty <- function() {
  data.frame(
    cd_id_oc = integer(0),
    contexto = character(0),
    momento = as.POSIXct(character(0), tz = "UTC"),
    cd_id_frame = character(0),
    cd_id_camera = character(0),
    stringsAsFactors = FALSE
  )
}

.objeto_contexto_parse_ids <- function(x) {
  if (is.null(x) || !length(x)) return(integer(0))

  x <- as.character(x[[1]])
  if (is.na(x) || !nzchar(x)) return(integer(0))

  vals <- stringr::str_extract_all(x, "\\d+")[[1]]
  vals <- suppressWarnings(as.integer(vals))
  vals <- vals[is.finite(vals)]

  unique(vals)
}

.objeto_contexto_detect_mime <- function(raw) {
  if (is.null(raw) || !length(raw)) return("application/octet-stream")
  if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
  if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)))) return("image/png")
  "application/octet-stream"
}

.objeto_contexto_to_data_url <- function(raw) {
  if (is.null(raw) || !length(raw)) return(NULL)
  paste0(
    "data:",
    .objeto_contexto_detect_mime(raw),
    ";base64,",
    base64enc::base64encode(raw)
  )
}

.objeto_contexto_first <- function(x, default = NULL) {
  if (is.null(x) || !length(x)) return(default)
  x[[1]]
}

.objeto_contexto_first_chr <- function(x, default = "") {
  as.character(.objeto_contexto_first(x, default))
}

.objeto_contexto_train_attr_empty <- function() {
  data.frame(
    cd_id_componente = integer(0),
    cd_id_atributo = integer(0),
    name_componente = character(0),
    name_atributo = character(0),
    name_data = character(0),
    VALUE = character(0),
    stringsAsFactors = FALSE
  )
}

.objeto_contexto_normalize_id_piece <- function(x) {
  x <- as.character(x)
  x <- gsub("\\s+", "_", x)
  gsub("[^A-Za-z0-9_\\-]", "_", x)
}

.objeto_contexto_make_attr_ids <- function(prefix, comp_id, attr_id, idx) {
  comp_id <- as.character(comp_id)
  attr_id <- as.character(attr_id)
  idx <- as.character(idx)

  raw_id <- paste0(prefix, "_", comp_id, "_", attr_id, "_", idx)
  norm_id <- paste0(
    prefix, "_",
    .objeto_contexto_normalize_id_piece(comp_id), "_",
    .objeto_contexto_normalize_id_piece(attr_id), "_",
    .objeto_contexto_normalize_id_piece(idx)
  )

  list(raw = raw_id, norm = norm_id)
}

.objeto_contexto_component_values <- function(values_map, comp_id, comp_name) {
  if (is.null(values_map) || !length(values_map)) {
    return(list())
  }

  by_id <- values_map$by_component_id %||% list()
  by_name <- values_map$by_component_name %||% list()

  out <- NULL
  if (length(comp_id) == 1L && is.finite(comp_id)) {
    out <- by_id[[as.character(comp_id)]]
  }
  if (is.null(out)) {
    out <- by_name[[toupper(as.character(comp_name))]]
  }
  if (is.null(out)) out <- list()

  out
}

.objeto_contexto_parse_inference_values <- function(contexto_txt) {
  empty <- list(by_component_id = list(), by_component_name = list())

  txt <- .objeto_contexto_first_chr(contexto_txt, "")
  if (is.na(txt) || !nzchar(trimws(txt))) {
    return(empty)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed) || !is.list(parsed) || !length(parsed)) {
    return(empty)
  }

  merge_values <- function(target, key, values) {
    key <- .objeto_contexto_first_chr(key, "")
    if (!nzchar(key) || is.null(values) || !length(values)) {
      return(target)
    }

    current <- target[[key]]
    if (is.null(current)) current <- list()

    for (nm in names(values)) {
      current[[as.character(nm)]] <- values[[nm]]
    }

    target[[key]] <- current
    target
  }

  normalize_value <- function(x) {
    if (is.null(x) || !length(x)) return(NA_character_)
    if (is.list(x) && length(x) == 1L) x <- x[[1]]
    if (length(x) > 1L) x <- paste(as.character(unlist(x, use.names = FALSE)), collapse = ", ")

    if (is.logical(x)) {
      return(ifelse(is.na(x[[1]]), NA_character_, ifelse(isTRUE(x[[1]]), "TRUE", "FALSE")))
    }
    if (is.numeric(x)) {
      return(as.character(x[[1]]))
    }

    as.character(x[[1]])
  }

  process_entry <- function(entry, acc) {
    if (is.null(entry) || !is.list(entry) || !length(entry)) {
      return(acc)
    }

    comp_id_raw <- .objeto_contexto_first(
      entry$ID,
      .objeto_contexto_first(
        entry$id,
        .objeto_contexto_first(
          entry$cd_id_componente,
          .objeto_contexto_first(entry$CD_ID_COMPONENTE, NA_integer_)
        )
      )
    )
    comp_id <- suppressWarnings(as.integer(comp_id_raw))
    has_comp_id <- length(comp_id) == 1L && is.finite(comp_id)
    comp_names <- setdiff(names(entry), c("ID", "id", "cd_id_componente", "CD_ID_COMPONENTE"))
    if (!length(comp_names)) {
      return(acc)
    }

    for (comp_name in comp_names) {
      attrs <- entry[[comp_name]]
      if (is.null(attrs) || !is.list(attrs) || !length(attrs)) next

      values <- list()
      for (attr_name in names(attrs)) {
        values[[as.character(attr_name)]] <- normalize_value(attrs[[attr_name]])
      }

      if (isTRUE(has_comp_id)) {
        acc$by_component_id <- merge_values(acc$by_component_id, as.character(comp_id), values)
      }
      acc$by_component_name <- merge_values(acc$by_component_name, toupper(as.character(comp_name)), values)
    }

    acc
  }

  entries <- if (!is.null(names(parsed)) && any(nzchar(names(parsed)))) {
    list(parsed)
  } else {
    parsed
  }

  out <- empty
  for (entry in entries) {
    out <- process_entry(entry, out)
  }

  out
}

.objeto_contexto_train_attrs_ui <- function(ns, objeto, values_map = NULL, id_prefix = "objCtxTrain") {
  if (is.null(objeto) || !is.data.frame(objeto) || !nrow(objeto)) {
    return(tags$em("Nenhum objeto foi encontrado para carregar os atributos."))
  }

  componentes <- objeto$config[[1]]$componentes[[1]]
  if (is.null(componentes) || !is.data.frame(componentes) || !nrow(componentes)) {
    return(tags$em("Nenhum componente foi encontrado para este objeto."))
  }

  tagList(lapply(seq_len(nrow(componentes)), function(i) {
    comp <- componentes[i, , drop = FALSE]
    comp_id <- suppressWarnings(as.integer(comp$cd_id_componente[[1]]))
    comp_name <- .objeto_contexto_first_chr(comp$name_componente, paste0("Componente ", i))

    estrutura <- if ("estrutura" %in% names(comp) && length(comp$estrutura)) comp$estrutura[[1]] else NULL
    estrutura_nome <- ""
    attrs_df <- NULL

    if (!is.null(estrutura) && is.data.frame(estrutura) && nrow(estrutura)) {
      estrutura_nome <- .objeto_contexto_first_chr(estrutura$name_estrutura, "")
      if (!is.null(estrutura$configs) && length(estrutura$configs) &&
          !is.null(estrutura$configs[[1]]$atributos) && length(estrutura$configs[[1]]$atributos)) {
        attrs_df <- estrutura$configs[[1]]$atributos[[1]]
      }
    }

    comp_values <- .objeto_contexto_component_values(values_map, comp_id, comp_name)
    inputs_ui <- if (is.null(attrs_df) || !is.data.frame(attrs_df) || !nrow(attrs_df)) {
      tags$em("Sem atributos configurados para esta estrutura.")
    } else {
      tagList(lapply(seq_len(nrow(attrs_df)), function(k) {
        att <- attrs_df[k, , drop = FALSE]
        att_id <- suppressWarnings(as.integer(att$cd_id_atributo[[1]]))
        att_name <- .objeto_contexto_first_chr(att$name_atributo, paste0("Atributo ", k))
        att_type <- .objeto_contexto_first_chr(att$name_data, "")
        att_vals <- .objeto_contexto_first_chr(att$value_atributo, "")
        input_ids <- .objeto_contexto_make_attr_ids(id_prefix, comp_id, att_id, k)

        cur_value <- comp_values[[att_name]]
        if (is.null(cur_value)) cur_value <- comp_values[[toupper(att_name)]]
        if (is.null(cur_value)) cur_value <- comp_values[[as.character(att_id)]]
        cur_value_first <- .objeto_contexto_first(cur_value, NULL)

        if (identical(att_type, "QUALITATIVE")) {
          choices <- stringr::str_split(att_vals, ",")[[1]]
          choices <- trimws(choices)
          choices <- choices[nzchar(choices)]

          return(
            selectizeInput(
              ns(input_ids$raw),
              label = att_name,
              choices = choices,
              selected = if (!is.null(cur_value_first) && nzchar(as.character(cur_value_first))) as.character(cur_value_first) else NULL,
              options = list(dropdownParent = "body", openOnFocus = TRUE, closeAfterSelect = TRUE)
            )
          )
        }

        cur_num <- suppressWarnings(as.numeric(.objeto_contexto_first(cur_value, NA_real_)))
        numericInput(
          ns(input_ids$raw),
          label = att_name,
          value = if (is.finite(cur_num)) cur_num else NA_real_,
          step = 1
        )
      }))
    }

    panelTitle(
      title = comp_name,
      background.color.title = "white",
      title.color = "black",
      border.color = "lightgray",
      children = div(
        style = "padding: 12px;",
        if (nzchar(estrutura_nome)) {
          tags$div(
            style = "margin-bottom:10px; color:#6b7280; font-size:12px; font-weight:600;",
            paste0("Estrutura: ", estrutura_nome)
          )
        },
        inputs_ui
      )
    )
  }))
}

.objeto_contexto_collect_train_attrs <- function(input_source, objeto, id_prefix = "objCtxTrain") {
  if (is.null(objeto) || !is.data.frame(objeto) || !nrow(objeto)) {
    return(.objeto_contexto_train_attr_empty())
  }

  componentes <- objeto$config[[1]]$componentes[[1]]
  if (is.null(componentes) || !is.data.frame(componentes) || !nrow(componentes)) {
    return(.objeto_contexto_train_attr_empty())
  }

  read_input <- function(id_candidates) {
    ids <- unlist(id_candidates, use.names = FALSE)
    for (cid in ids) {
      if (cid %in% names(input_source)) {
        return(input_source[[cid]])
      }
    }
    NA
  }

  rows <- list()

  for (i in seq_len(nrow(componentes))) {
    comp <- componentes[i, , drop = FALSE]
    estrutura <- if ("estrutura" %in% names(comp) && length(comp$estrutura)) comp$estrutura[[1]] else NULL
    attrs_df <- NULL

    if (!is.null(estrutura) && is.data.frame(estrutura) && nrow(estrutura) &&
        !is.null(estrutura$configs) && length(estrutura$configs) &&
        !is.null(estrutura$configs[[1]]$atributos) && length(estrutura$configs[[1]]$atributos)) {
      attrs_df <- estrutura$configs[[1]]$atributos[[1]]
    }

    if (is.null(attrs_df) || !is.data.frame(attrs_df) || !nrow(attrs_df)) next

    comp_id <- suppressWarnings(as.integer(comp$cd_id_componente[[1]]))
    comp_name <- .objeto_contexto_first_chr(comp$name_componente, paste0("Componente ", i))

    for (k in seq_len(nrow(attrs_df))) {
      att <- attrs_df[k, , drop = FALSE]
      att_id <- suppressWarnings(as.integer(att$cd_id_atributo[[1]]))
      att_name <- .objeto_contexto_first_chr(att$name_atributo, paste0("Atributo ", k))
      att_type <- .objeto_contexto_first_chr(att$name_data, "")
      ids <- .objeto_contexto_make_attr_ids(id_prefix, comp_id, att_id, k)
      value <- read_input(ids)

      if (is.numeric(value)) {
        value <- as.character(value[[1]])
      } else if (is.logical(value)) {
        value <- ifelse(is.na(value[[1]]), NA_character_, ifelse(isTRUE(value[[1]]), "TRUE", "FALSE"))
      } else {
        value <- if (is.null(value) || !length(value)) NA_character_ else as.character(value[[1]])
      }

      rows[[length(rows) + 1L]] <- data.frame(
        cd_id_componente = comp_id,
        cd_id_atributo = att_id,
        name_componente = comp_name,
        name_atributo = att_name,
        name_data = att_type,
        VALUE = value,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) {
    return(.objeto_contexto_train_attr_empty())
  }

  do.call(rbind, rows)
}

.objeto_contexto_validate_train_attrs <- function(attrs_df) {
  if (is.null(attrs_df) || !is.data.frame(attrs_df) || !nrow(attrs_df)) {
    return(list(ok = FALSE, missing = "Nenhum atributo foi encontrado para salvar."))
  }

  values <- as.character(attrs_df$VALUE)
  missing <- unique(as.character(attrs_df$name_atributo[is.na(values) | !nzchar(trimws(values))]))
  missing <- missing[!is.na(missing) & nzchar(missing)]

  list(ok = !length(missing), missing = missing)
}

.objeto_contexto_build_output_json <- function(attrs_df) {
  if (is.null(attrs_df) || !is.data.frame(attrs_df) || !nrow(attrs_df)) {
    return("[]")
  }

  groups <- split(attrs_df, factor(attrs_df$cd_id_componente, levels = unique(attrs_df$cd_id_componente)))
  items <- lapply(groups, function(df_comp) {
    comp_name <- .objeto_contexto_first_chr(df_comp$name_componente, "COMPONENTE")
    comp_id <- suppressWarnings(as.integer(df_comp$cd_id_componente[[1]]))

    attrs <- stats::setNames(vector("list", nrow(df_comp)), as.character(df_comp$name_atributo))
    for (i in seq_len(nrow(df_comp))) {
      row <- df_comp[i, , drop = FALSE]
      if (identical(as.character(row$name_data[[1]]), "QUALITATIVE")) {
        attrs[[as.character(row$name_atributo[[1]])]] <- as.character(row$VALUE[[1]])
      } else {
        num_value <- suppressWarnings(as.numeric(row$VALUE[[1]]))
        attrs[[as.character(row$name_atributo[[1]])]] <- if (is.finite(num_value)) num_value else as.character(row$VALUE[[1]])
      }
    }

    item <- list()
    item[[comp_name]] <- attrs
    item$ID <- comp_id
    item
  })

  jsonlite::toJSON(items, auto_unbox = TRUE, null = "null")
}

.objeto_contexto_build_input_ia <- function(objeto) {
  if (is.null(objeto) || !is.data.frame(objeto) || !nrow(objeto)) {
    return("OBJETO:  TIPO: ")
  }

  objeto_nome <- .objeto_contexto_first_chr(objeto$name_objeto, "")
  tipo_nome <- .objeto_contexto_first_chr(objeto$name_objeto_tipo, "")
  paste0("OBJETO: ", objeto_nome, " TIPO: ", tipo_nome)
}

.objeto_contexto_blob_frame_info <- function(blob,
                                             default_width = .DEFAULT_FRAME_WIDTH,
                                             default_height = .DEFAULT_FRAME_HEIGHT) {
  width <- as.integer(default_width)
  height <- as.integer(default_height)

  raw_blob <- NULL
  if (!is.null(blob) && length(blob)) {
    raw_blob <- blob[[1]]
  }
  if (is.null(raw_blob)) {
    return(list(width = width, height = height))
  }

  img <- tryCatch(image_read(raw_blob), error = function(e) NULL)
  if (is.null(img)) {
    return(list(width = width, height = height))
  }

  info <- tryCatch(image_info(img), error = function(e) NULL)
  if (is.data.frame(info) && nrow(info)) {
    info_w <- suppressWarnings(as.integer(info$width[[1]]))
    info_h <- suppressWarnings(as.integer(info$height[[1]]))

    if (is.finite(info_w) && info_w > 0L) width <- info_w
    if (is.finite(info_h) && info_h > 0L) height <- info_h
  }

  list(width = width, height = height)
}

.objeto_contexto_clip_components_by_camera <- function(objeto, frame_info_by_camera = list()) {
  out <- list()

  if (is.null(objeto) || !is.data.frame(objeto) || !nrow(objeto)) {
    return(out)
  }

  componentes <- tryCatch(objeto$config[[1]]$componentes[[1]], error = function(e) NULL)
  if (is.null(componentes) || !is.data.frame(componentes) || !nrow(componentes)) {
    return(out)
  }

  componentes <- .ensure_component_colors(componentes)

  for (i in seq_len(nrow(componentes))) {
    comp <- componentes[i, , drop = FALSE]
    cam_id <- suppressWarnings(as.integer(comp$cd_id_camera[[1]]))
    if (!is.finite(cam_id)) next

    poly_df <- tryCatch(.component_polygon_as_df(comp$poligno_componente[[1]]), error = function(e) NULL)
    if (is.null(poly_df) || !nrow(poly_df)) next

    frame_info <- frame_info_by_camera[[as.character(cam_id)]]
    frame_h <- suppressWarnings(as.numeric(frame_info$height %||% NA_real_))
    if (is.finite(frame_h) && frame_h > 0) {
      poly_df$y <- frame_h - poly_df$y
    }

    estrutura_nome <- ""
    if ("estrutura" %in% names(comp) && length(comp$estrutura)) {
      estrutura <- comp$estrutura[[1]]
      if (is.data.frame(estrutura) && nrow(estrutura) && "name_estrutura" %in% names(estrutura)) {
        estrutura_nome <- .objeto_contexto_first_chr(estrutura$name_estrutura, "")
      }
    }

    cor <- if ("color_componente" %in% names(comp)) {
      as.character(comp$color_componente[[1]] %||% "")
    } else {
      ""
    }
    if (is.na(cor) || !nzchar(cor)) cor <- "#38BDF8"

    item <- list(
      id = suppressWarnings(as.integer(comp$cd_id_componente[[1]])),
      name = .objeto_contexto_first_chr(comp$name_componente, paste0("Componente ", i)),
      structure = estrutura_nome,
      color = cor,
      points = lapply(seq_len(nrow(poly_df)), function(j) {
        list(
          x = as.numeric(poly_df$x[[j]]),
          y = as.numeric(poly_df$y[[j]])
        )
      })
    )

    key <- as.character(cam_id)
    if (is.null(out[[key]])) out[[key]] <- list()
    out[[key]][[length(out[[key]]) + 1L]] <- item
  }

  out
}

.objeto_contexto_train_overlay_ui <- function(ns, payload, objeto, tipos_pacote, values_map = NULL) {
  overlay_id <- ns("objetoContextoFinningTunnelOverlay")
  tipos_choices <- if (is.data.frame(tipos_pacote) && nrow(tipos_pacote)) {
    as.character(tipos_pacote$name_tipo_pacote)
  } else {
    character(0)
  }

  div(
    id = overlay_id,
    style = paste(
      "position: fixed; inset: 0; background: rgba(0,0,0,.5); z-index: 1060;",
      "display: flex; align-items: center; justify-content: center;"
    ),
    div(
      style = paste(
        "background:#fff; border-radius:10px; width:min(1080px,96%);",
        "height:90vh; min-height:420px; box-shadow:0 12px 30px rgba(0,0,0,.25);",
        "display:flex; flex-direction:column;"
      ),
      div(
        style = "padding:16px 18px; border-bottom:1px solid #eee; flex:0 0 auto;",
        tags$h4("Finning Tunnel", style = "margin:0;"),
        tags$div(
          style = "margin-top:6px; color:#6b7280;",
          "Corrija os valores inferidos e grave este clip como pacote de treino."
        )
      ),
      div(
        style = "padding:14px 16px; flex:1 1 auto; min-height:0; overflow-y:auto; overflow-x:hidden;",
        fluidRow(
          column(
            6,
            textInput(
              ns("finningTunnelTitulo"),
              label = "Titulo do clip",
              value = as.character(payload$title_default %||% "Correcao do contexto")
            )
          ),
          column(
            6,
            selectInput(
              ns("finningTunnelTipoPacote"),
              label = "Tipo de pacote",
              choices = tipos_choices,
              selected = if (length(tipos_choices)) tipos_choices[[1]] else character(0)
            )
          )
        ),
        panelTitle(
          title = "Resumo",
          background.color.title = "white",
          title.color = "black",
          border.color = "lightgray",
          children = div(
            style = "padding: 12px;",
            tags$div(tags$b("Objeto: "), .objeto_contexto_first_chr(objeto$name_objeto, "-")),
            tags$div(tags$b("Tipo: "), .objeto_contexto_first_chr(objeto$name_objeto_tipo, "-")),
            tags$div(tags$b("Momento do contexto: "), as.character(payload$moment_label %||% "-")),
            tags$div(tags$b("Periodo do clip: "), as.character(payload$clip_period_label %||% "-"))
          )
        ),
        br(),
        panelTitle(
          title = "Atributos da Estrutura",
          background.color.title = "white",
          title.color = "black",
          border.color = "lightgray",
          children = div(
            style = "padding: 12px;",
            .objeto_contexto_train_attrs_ui(
              ns = ns,
              objeto = objeto,
              values_map = values_map,
              id_prefix = "objCtxTrain"
            )
          )
        )
      ),
      div(
        style = "padding:12px 18px; border-top:1px solid #eee; text-align:right; flex:0 0 auto;",
        actionButton(ns("btFecharFinningTunnel"), "Fechar", class = "btn btn-default btn-sm"),
        actionButton(ns("btSalvarFinningTunnel"), "Salvar", class = "btn btn-primary btn-sm", icon = icon("save"))
      )
    )
  )
}

.objeto_contexto_clip_ui <- function(ns, payload, missing_cameras = character(0)) {
  if (is.null(payload) || !length(payload$cameras)) return(NULL)

  player_id <- ns("objetoContextoClipPlayer")
  root_json <- jsonlite::toJSON(player_id, auto_unbox = TRUE)
  payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  has_components <- isTRUE(any(vapply(payload$cameras, function(cam) {
    length(cam$components %||% list()) > 0L
  }, logical(1L))))
  close_js <- sprintf(
    "window.tvsObjCtxPlayerStop && window.tvsObjCtxPlayerStop(%s);",
    root_json
  )

  tags$div(
    id = player_id,
    tags$style(HTML(sprintf(
      "
      #%1$s {
        margin: 0 0 14px 0;
        padding: 14px;
        border: 1px solid #dbe3ea;
        border-radius: 12px;
        background: linear-gradient(180deg, #fbfdff 0%%, #f3f8fc 100%%);
      }
      #%1$s .tvs-ctxclip-head {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 12px;
        flex-wrap: wrap;
        margin-bottom: 12px;
      }
      #%1$s .tvs-ctxclip-title {
        font-size: 16px;
        font-weight: 700;
        color: #1f2d3d;
        line-height: 1.35;
        word-break: break-word;
      }
      #%1$s .tvs-ctxclip-titlebar {
        display: flex;
        align-items: flex-start;
        gap: 10px;
        flex-wrap: wrap;
      }
      #%1$s .tvs-ctxclip-momento {
        font-size: 12px;
        font-weight: 600;
        color: #475569;
        background: #e8f1f8;
        border: 1px solid #cdddea;
        border-radius: 999px;
        padding: 4px 10px;
        white-space: nowrap;
      }
      #%1$s .tvs-ctxclip-subtitle {
        font-size: 12px;
        color: #6b7280;
        margin-top: 2px;
      }
      #%1$s .tvs-ctxclip-actions {
        display: flex;
        gap: 8px;
        align-items: center;
        flex-wrap: wrap;
      }
      #%1$s .tvs-ctxclip-status {
        min-width: 110px;
        font-weight: 600;
        color: #374151;
      }
      #%1$s .tvs-ctxclip-repeat {
        min-width: 110px;
      }
      #%1$s .tvs-ctxclip-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
        gap: 12px;
      }
      #%1$s .tvs-ctxclip-card {
        border: 1px solid #d8e2ea;
        border-radius: 10px;
        overflow: hidden;
        background: #fff;
        box-shadow: 0 3px 12px rgba(15, 23, 42, 0.04);
      }
      #%1$s .tvs-ctxclip-cardhead {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 10px;
        padding: 10px 12px;
        border-bottom: 1px solid #edf2f7;
        background: #f9fbfd;
      }
      #%1$s .tvs-ctxclip-camname {
        font-weight: 700;
        color: #1f2d3d;
      }
      #%1$s .tvs-ctxclip-meta {
        font-size: 12px;
        color: #6b7280;
        text-align: right;
      }
      #%1$s .tvs-ctxclip-stage {
        background: #0f172a;
        aspect-ratio: 16 / 9;
        display: flex;
        align-items: center;
        justify-content: center;
        position: relative;
        overflow: hidden;
      }
      #%1$s .tvs-ctxclip-media {
        position: relative;
        width: 100%%;
        height: 100%%;
      }
      #%1$s .tvs-ctxclip-stage img,
      #%1$s .tvs-ctxclip-overlay {
        position: absolute;
        inset: 0;
        width: 100%%;
        height: 100%%;
      }
      #%1$s .tvs-ctxclip-stage img {
        object-fit: contain;
        display: block;
      }
      #%1$s .tvs-ctxclip-overlay {
        pointer-events: none;
      }
      #%1$s .tvs-ctxclip-slider {
        margin-top: 12px;
      }
      #%1$s .tvs-ctxclip-note {
        margin-bottom: 12px;
        padding: 10px 12px;
        border-radius: 8px;
        background: #fff7e6;
        border: 1px solid #f0ad4e;
        color: #8a6d3b;
      }
      ",
      player_id
    ))),
    tags$div(
      class = "tvs-ctxclip-head",
      tags$div(
        tags$div(
          class = "tvs-ctxclip-titlebar",
          tags$div(class = "tvs-ctxclip-title", as.character(payload$context_label %||% "Clip do contexto")),
          if (!is.null(payload$moment_label) && nzchar(as.character(payload$moment_label))) {
            tags$span(class = "tvs-ctxclip-momento", as.character(payload$moment_label))
          }
        ),
        tags$div(
          class = "tvs-ctxclip-subtitle",
          paste0(payload$total_steps, " frame(s) carregado(s) para a inferencia.")
        )
      ),
      tags$div(
        class = "tvs-ctxclip-actions",
        tags$span(id = paste0(player_id, "_status"), class = "tvs-ctxclip-status"),
        tags$button(
          type = "button",
          class = "btn btn-default btn-sm",
          title = "Frame anterior",
          onclick = sprintf("window.tvsObjCtxClipControl && window.tvsObjCtxClipControl(%s, 'prev');", root_json),
          icon("step-backward")
        ),
        tags$button(
          type = "button",
          class = "btn btn-default btn-sm",
          title = "Play",
          onclick = sprintf("window.tvsObjCtxClipControl && window.tvsObjCtxClipControl(%s, 'play');", root_json),
          icon("play")
        ),
        tags$button(
          type = "button",
          class = "btn btn-default btn-sm",
          title = "Pause",
          onclick = sprintf("window.tvsObjCtxClipControl && window.tvsObjCtxClipControl(%s, 'pause');", root_json),
          icon("pause")
        ),
        tags$button(
          type = "button",
          class = "btn btn-default btn-sm",
          title = "Restart",
          onclick = sprintf("window.tvsObjCtxClipControl && window.tvsObjCtxClipControl(%s, 'restart');", root_json),
          icon("undo")
        ),
        tags$button(
          id = paste0(player_id, "_repeat"),
          type = "button",
          class = "btn btn-success btn-sm tvs-ctxclip-repeat",
          title = "Repeat",
          onclick = sprintf("window.tvsObjCtxClipControl && window.tvsObjCtxClipControl(%s, 'repeat');", root_json),
          "Repeat: On"
        ),
        if (isTRUE(has_components)) {
          tags$button(
            id = paste0(player_id, "_components"),
            type = "button",
            class = "btn btn-info btn-sm",
            title = "Mostrar ou ocultar componentes",
            onclick = sprintf("window.tvsObjCtxClipControl && window.tvsObjCtxClipControl(%s, 'components');", root_json),
            "Componentes: TRUE"
          )
        },
        tags$button(
          type = "button",
          class = "btn btn-default btn-sm",
          title = "Proximo frame",
          onclick = sprintf("window.tvsObjCtxClipControl && window.tvsObjCtxClipControl(%s, 'next');", root_json),
          icon("step-forward")
        ),
        tags$span("FPS"),
        tags$input(
          id = paste0(player_id, "_fps"),
          type = "number",
          value = as.integer(payload$fps_default),
          min = 1,
          max = 60,
          step = 1,
          style = "width: 80px;"
        ),
        if (isTRUE(payload$show_finning_tunnel)) {
          actionButton(
            ns("btFinningTunnel"),
            label = "Finning Tunnel",
            icon = icon("sliders"),
            class = "btn btn-primary btn-sm"
          )
        },
        actionButton(
          ns("btFecharContextoClip"),
          label = "Fechar clip",
          icon = icon("times"),
          class = "btn btn-default btn-sm",
          onclick = close_js
        )
      )
    ),
    if (length(missing_cameras)) {
      tags$div(
        class = "tvs-ctxclip-note",
        paste0(
          "Algumas cameras nao retornaram frames para este contexto: ",
          paste(missing_cameras, collapse = ", "),
          "."
        )
      )
    },
    tags$div(
      class = "tvs-ctxclip-grid",
      lapply(payload$cameras, function(cam) {
        first_src <- ""
        first_label <- "Sem frame"
        if (length(cam$frames)) {
          first_src <- as.character(cam$frames[[1]]$src)
          first_src <- if (length(first_src) && !is.na(first_src[[1]])) first_src[[1]] else ""
          first_label_tmp <- as.character(cam$frames[[1]]$label)
          if (length(first_label_tmp) && !is.na(first_label_tmp[[1]]) && nzchar(first_label_tmp[[1]])) {
            first_label <- first_label_tmp[[1]]
          }
        }

        tags$div(
          class = "tvs-ctxclip-card",
          tags$div(
            class = "tvs-ctxclip-cardhead",
            tags$span(class = "tvs-ctxclip-camname", cam$name),
            tags$span(id = paste0(player_id, "_meta_", cam$id), class = "tvs-ctxclip-meta", first_label)
          ),
          tags$div(
            class = "tvs-ctxclip-stage",
            tags$div(
              class = "tvs-ctxclip-media",
              tags$img(
                id = paste0(player_id, "_img_", cam$id),
                src = first_src,
                alt = cam$name
              ),
              tags$svg(
                id = paste0(player_id, "_overlay_", cam$id),
                class = "tvs-ctxclip-overlay",
                viewBox = "0 0 1 1",
                preserveAspectRatio = "xMidYMid meet"
              )
            )
          )
        )
      })
    ),
    tags$div(
      class = "tvs-ctxclip-slider",
      tags$input(
        id = paste0(player_id, "_slider"),
        type = "range",
        min = 1,
        max = max(1L, as.integer(payload$total_steps)),
        value = 1,
        step = 1,
        style = "width: 100%;"
      )
    ),
    tags$script(HTML(sprintf(
      "
      (function() {
        const rootId = %1$s;
        const payload = %2$s;

        window.tvsObjCtxPlayers = window.tvsObjCtxPlayers || {};

        if (window.tvsObjCtxPlayers[rootId] && window.tvsObjCtxPlayers[rootId].timer) {
          clearInterval(window.tvsObjCtxPlayers[rootId].timer);
        }

        const state = {
          index: 0,
          timer: null,
          payload: payload,
          repeat: payload.repeat_default !== false,
          componentsVisible: payload.components_visible_default !== false
        };

        function totalSteps() {
          return Math.max(1, Number(payload.total_steps || 1));
        }

        function clampIndex(i) {
          return Math.max(0, Math.min(totalSteps() - 1, Number(i || 0)));
        }

        function readFps() {
          const fpsEl = document.getElementById(rootId + '_fps');
          const value = fpsEl ? Number(fpsEl.value) : Number(payload.fps_default || 5);
          if (!Number.isFinite(value)) return 5;
          return Math.max(1, Math.min(60, Math.round(value)));
        }

        function frameAt(cam, idx) {
          if (!cam || !cam.frames || !cam.frames.length) return null;
          const pos = Math.max(0, Math.min(cam.frames.length - 1, idx));
          return cam.frames[pos];
        }

        function svgEl(name) {
          return document.createElementNS('http://www.w3.org/2000/svg', name);
        }

        function componentPoints(comp) {
          const pts = Array.isArray(comp && comp.points) ? comp.points : [];
          return pts
            .map(function(pt) {
              const x = Number(pt && pt.x);
              const y = Number(pt && pt.y);
              if (!Number.isFinite(x) || !Number.isFinite(y)) return null;
              return String(x) + ',' + String(y);
            })
            .filter(Boolean)
            .join(' ');
        }

        function syncOverlay(cam, frame, img, overlay) {
          if (!overlay) return;

          const comps = Array.isArray(cam && cam.components) ? cam.components : [];
          overlay.style.display = (state.componentsVisible && comps.length) ? 'block' : 'none';
          if (!state.componentsVisible || !comps.length) return;

          const w = Number((frame && frame.width) || (cam && cam.frame_width) || (img && img.naturalWidth) || 0);
          const h = Number((frame && frame.height) || (cam && cam.frame_height) || (img && img.naturalHeight) || 0);
          if (!Number.isFinite(w) || !Number.isFinite(h) || w <= 0 || h <= 0) return;

          overlay.setAttribute('viewBox', '0 0 ' + w + ' ' + h);
          overlay.setAttribute('preserveAspectRatio', 'xMidYMid meet');
          while (overlay.firstChild) overlay.removeChild(overlay.firstChild);

          comps.forEach(function(comp) {
            const points = componentPoints(comp);
            if (!points) return;

            const color = comp && comp.color ? String(comp.color) : '#38BDF8';
            const poly = svgEl('polygon');
            poly.setAttribute('points', points);
            poly.setAttribute('fill', color);
            poly.setAttribute('fill-opacity', '0.10');
            poly.setAttribute('stroke', color);
            poly.setAttribute('stroke-width', '2');
            poly.setAttribute('vector-effect', 'non-scaling-stroke');

            const title = svgEl('title');
            const compName = comp && comp.name ? String(comp.name) : 'Componente';
            const structName = comp && comp.structure ? String(comp.structure) : '';
            title.textContent = structName ? (compName + ' | ' + structName) : compName;
            poly.appendChild(title);

            overlay.appendChild(poly);
          });
        }

        function render() {
          state.index = clampIndex(state.index);

          (payload.cameras || []).forEach(function(cam) {
            const frame = frameAt(cam, state.index);
            const img = document.getElementById(rootId + '_img_' + cam.id);
            const meta = document.getElementById(rootId + '_meta_' + cam.id);
            const overlay = document.getElementById(rootId + '_overlay_' + cam.id);

            if (img) {
              if (frame && frame.src) {
                img.src = frame.src;
                img.style.opacity = '1';
              } else {
                img.removeAttribute('src');
                img.style.opacity = '0.35';
              }

              img.onload = function() {
                syncOverlay(cam, frame, img, overlay);
              };
            }

            if (meta) {
              meta.textContent = frame && frame.label ? frame.label : 'Sem frame';
            }

            syncOverlay(cam, frame, img, overlay);
          });

          const status = document.getElementById(rootId + '_status');
          if (status) {
            status.textContent = 'Frame ' + (state.index + 1) + ' / ' + totalSteps();
          }

          const repeatBtn = document.getElementById(rootId + '_repeat');
          if (repeatBtn) {
            repeatBtn.textContent = state.repeat ? 'Repeat: On' : 'Repeat: Off';
            repeatBtn.className = state.repeat
              ? 'btn btn-success btn-sm tvs-ctxclip-repeat'
              : 'btn btn-default btn-sm tvs-ctxclip-repeat';
          }

          const componentsBtn = document.getElementById(rootId + '_components');
          if (componentsBtn) {
            componentsBtn.textContent = state.componentsVisible
              ? 'Componentes: TRUE'
              : 'Componentes: FALSE';
            componentsBtn.className = state.componentsVisible
              ? 'btn btn-info btn-sm'
              : 'btn btn-default btn-sm';
          }

          const slider = document.getElementById(rootId + '_slider');
          if (slider) {
            slider.max = String(totalSteps());
            slider.value = String(state.index + 1);
          }
        }

        function pause() {
          if (state.timer) {
            clearInterval(state.timer);
            state.timer = null;
          }
        }

        function play() {
          pause();
          state.timer = setInterval(function() {
            if (state.index >= totalSteps() - 1) {
              if (state.repeat) {
                state.index = 0;
                render();
                return;
              }
              pause();
              return;
            }
            state.index += 1;
            render();
          }, Math.max(16, Math.round(1000 / readFps())));
        }

        const slider = document.getElementById(rootId + '_slider');
        if (slider) {
          slider.oninput = function() {
            pause();
            state.index = clampIndex(Number(this.value || 1) - 1);
            render();
          };
        }

        window.tvsObjCtxClipControl = function(target, action) {
          const st = window.tvsObjCtxPlayers[target];
          if (!st || target !== rootId) return;

          if (action === 'play') {
            play();
            return;
          }

          if (action === 'pause') {
            pause();
            return;
          }

          if (action === 'restart') {
            pause();
            state.index = 0;
            render();
            return;
          }

          if (action === 'repeat') {
            state.repeat = !state.repeat;
            render();
            return;
          }

          if (action === 'components') {
            state.componentsVisible = !state.componentsVisible;
            render();
            return;
          }

          pause();
          if (action === 'prev') {
            state.index = clampIndex(state.index - 1);
          } else if (action === 'next') {
            state.index = clampIndex(state.index + 1);
          }
          render();
        };

        window.tvsObjCtxPlayerStop = function(target) {
          const st = window.tvsObjCtxPlayers[target];
          if (st && st.timer) {
            clearInterval(st.timer);
            st.timer = null;
          }
        };

        window.tvsObjCtxPlayers[rootId] = state;
        render();
      })();
      ",
      root_json,
      payload_json
    )))
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
  contextos_raw <- reactiveVal(.objeto_contexto_raw_empty())
  busca_realizada <- reactiveVal(FALSE)
  contexto_clip_payload <- reactiveVal(NULL)
  contexto_clip_meta <- reactiveVal(NULL)
  contexto_clip_missing <- reactiveVal(character(0))
  contexto_page_length <- 100L
  tipos_pacote_contexto <- tryCatch(
    selectAllTypesPacote(dbp$get_pool()),
    error = function(e) data.frame()
  )

  id <- ns("dialogObjetoContexto")
  cssStyle <- list()
  cssStyle[[paste0(" #parent", id, " .modal-dialog")]]  <- "width: 96% !important; height: 90% !important;"
  cssStyle[[paste0(" #parent", id, " .modal-content")]] <- "width: 100% !important; height: 100% !important;"
  cssStyle[[paste0(" #parent", id, " .modal-body")]]    <- "width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto; overflow-x: hidden;"

  .clear_contexto_training_overlay <- function() {
    try(
      removeUI(
        selector = paste0("#", ns("objetoContextoFinningTunnelOverlay")),
        multiple = TRUE,
        immediate = TRUE
      ),
      silent = TRUE
    )
    invisible(NULL)
  }

  .clear_contexto_clip <- function(reset_table_selection = TRUE) {
    shinyjs::runjs(sprintf(
      "window.tvsObjCtxPlayerStop && window.tvsObjCtxPlayerStop(%s);",
      jsonlite::toJSON(ns("objetoContextoClipPlayer"), auto_unbox = TRUE)
    ))

    contexto_clip_payload(NULL)
    contexto_clip_meta(NULL)
    contexto_clip_missing(character(0))
    .clear_contexto_training_overlay()

    if (isTRUE(reset_table_selection)) {
      try(
        DT::selectRows(
          DT::dataTableProxy("tbObjetoContexto", session = session),
          NULL
        ),
        silent = TRUE
      )
    }

    invisible(NULL)
  }

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
    tz_use <- tz_local
    objeto_id <- suppressWarnings(as.integer(isolate(input$comboObjetoContexto)))
    if (is.na(objeto_id)) {
      busca_realizada(FALSE)
      contextos_raw(.objeto_contexto_raw_empty())
      contextos(.objeto_contexto_dt_empty())
      .clear_contexto_clip(reset_table_selection = TRUE)
      return(invisible(NULL))
    }

    dt_de_raw <- isolate(input$dtDeContexto)
    dt_ate_raw <- isolate(input$dtAteContexto)
    dt_de_local <- .objeto_contexto_parse_datetime(dt_de_raw, tz_local = tz_use)
    dt_ate_local <- .objeto_contexto_parse_datetime(dt_ate_raw, tz_local = tz_use)

    invalid_fields <- character(0)
    if (!.objeto_contexto_input_is_blank(dt_de_raw) && is.null(dt_de_local)) invalid_fields <- c(invalid_fields, "De")
    if (!.objeto_contexto_input_is_blank(dt_ate_raw) && is.null(dt_ate_local)) invalid_fields <- c(invalid_fields, "Ate")

    if (length(invalid_fields)) {
      showNotification(
        paste0(
          "Campo(s) ",
          paste(invalid_fields, collapse = " e "),
          " com data/hora invalida. Use o formato dd/MM/aaaa HH:mm."
        ),
        type = "warning"
      )
      return(invisible(NULL))
    }

    if (!is.null(dt_de_local) && !is.null(dt_ate_local) && dt_de_local > dt_ate_local) {
      showNotification("O campo De nao pode ser maior que Ate.", type = "warning")
      return(invisible(NULL))
    }

    dt_de_utc <- .objeto_contexto_to_utc(dt_de_local, tz_local = tz_use)
    dt_ate_utc <- .objeto_contexto_to_utc(dt_ate_local, tz_local = tz_use)

    runner <- function() {
      ok <- db$tryTransaction(function(conn) {
        df <- selectObjetoContexto(
          conn,
          cd_id_objeto = objeto_id,
          dt_de_utc = dt_de_utc,
          dt_ate_utc = dt_ate_utc,
          tz_local = tz_use
        )

        contextos_raw(df)
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
        .clear_contexto_clip(reset_table_selection = TRUE)
        busca_realizada(TRUE)
      })

      if (!isTRUE(ok)) {
        busca_realizada(FALSE)
        contextos_raw(.objeto_contexto_raw_empty())
        contextos(.objeto_contexto_dt_empty())
        .clear_contexto_clip(reset_table_selection = TRUE)
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
            "#", ns("tbObjetoContexto"), "_wrapper .tvs-dt-footer {",
            "display:flex; justify-content:space-between; align-items:center; gap:12px;",
            "margin-top:12px; flex-wrap:wrap;}",
            "#", ns("tbObjetoContexto"), "_wrapper .tvs-dt-footer .dataTables_info,",
            "#", ns("tbObjetoContexto"), "_wrapper .tvs-dt-footer .dataTables_paginate {",
            "float:none !important; margin:0; padding-top:0;}",
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
                timepickerOpts = timepickerOptions(
                  timeFormat = "HH:mm",
                  minHours = 0,
                  maxHours = 23,
                  minMinutes = 0,
                  maxMinutes = 59,
                  hoursStep = 1,
                  minutesStep = 1
                ),
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
                timepickerOpts = timepickerOptions(
                  timeFormat = "HH:mm",
                  minHours = 0,
                  maxHours = 23,
                  minMinutes = 0,
                  maxMinutes = 59,
                  hoursStep = 1,
                  minutesStep = 1
                ),
                placeholder = "Sem filtro",
                addon = "none"
              )
            )
          ),
          fluidRow(
            column(
              12,
              div(
                style = "padding-top: 10px; display:flex; gap:6px; justify-content:flex-end;",
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
          uiOutput(ns("uiObjetoContextoClip")),
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
    } else if (!isTRUE(busca_realizada())) {
      "Clique na lupa para carregar o contexto."
    } else if (qtd == 0) {
      "Nenhum contexto encontrado para o filtro atual."
    } else if (qtd > contexto_page_length) {
      paste0(
        qtd,
        " registro(s) carregado(s), paginados em lotes de ",
        contexto_page_length,
        ". Use Proximo/Anterior no rodape da tabela."
      )
    } else {
      paste0(qtd, " registro(s) carregado(s).")
    }

    div(msg)
  })

  output$uiObjetoContextoClip <- renderUI({
    payload <- contexto_clip_payload()
    if (is.null(payload) || !length(payload$cameras)) {
      return(NULL)
    }

    .objeto_contexto_clip_ui(
      ns,
      payload,
      missing_cameras = contexto_clip_missing()
    )
  })

  output$uiTabelaContexto <- renderUI({
    objeto_id <- suppressWarnings(as.integer(input$comboObjetoContexto))
    if (is.na(objeto_id) || !isTRUE(busca_realizada())) {
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
      rownames = TRUE,
      class = "cell-border stripe",
      options = dtProfessionalOptions(
        columnDefs = list(
          list(className = "dt-center", targets = c(0)),
          list(className = "dt-left", targets = c(1)),
          list(className = "dt-center", targets = c(2)),
          list(width = "6%", targets = c(0)),
          list(width = "74%", targets = c(1)),
          list(width = "20%", targets = c(2))
        ),
        scrollY = "420px",
        search_placeholder = "Pesquisar contexto ou momento",
        extra_options = list(
          autoWidth = FALSE,
          dom = '<"tvs-dt-toolbar"f>t<"tvs-dt-footer"ip>',
          paging = TRUE,
          pagingType = "simple_numbers",
          pageLength = contexto_page_length,
          scrollCollapse = TRUE
        )
      ),
      escape = TRUE,
      selection = list(mode = "single", target = "row", selected = NULL)
    ) |> DT$formatStyle(names(df), cursor = "pointer")
  }, server = TRUE)

  obs$add(observeEvent(input$comboSetorContexto, {
    .reload_objetos_lookup(input$comboSetorContexto, selected = "")
    busca_realizada(FALSE)
    contextos_raw(.objeto_contexto_raw_empty())
    contextos(.objeto_contexto_dt_empty())
    .clear_contexto_clip(reset_table_selection = TRUE)
  }, ignoreInit = TRUE, ignoreNULL = FALSE))

  obs$add(observeEvent(input$comboObjetoContexto, {
    busca_realizada(FALSE)
    contextos_raw(.objeto_contexto_raw_empty())
    contextos(.objeto_contexto_dt_empty())
    .clear_contexto_clip(reset_table_selection = TRUE)
  }, ignoreInit = TRUE, ignoreNULL = FALSE))

  obs$add(observeEvent(list(input$dtDeContexto, input$dtAteContexto), {
    busca_realizada(FALSE)
    contextos_raw(.objeto_contexto_raw_empty())
    contextos(.objeto_contexto_dt_empty())
    .clear_contexto_clip(reset_table_selection = TRUE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btAtualizarContexto, {
    .load_contextos(use_loader = TRUE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$tbObjetoContexto_rows_selected, {
    tz_use <- tz_local
    sel <- suppressWarnings(as.integer(input$tbObjetoContexto_rows_selected))
    if (!length(sel) || is.na(sel[[1]])) {
      return(invisible(NULL))
    }

    rows <- isolate(contextos_raw())
    if (!is.data.frame(rows) || !nrow(rows) || sel[[1]] < 1L || sel[[1]] > nrow(rows)) {
      .clear_contexto_clip(reset_table_selection = TRUE)
      return(invisible(NULL))
    }

    row_sel <- rows[sel[[1]], , drop = FALSE]
    .clear_contexto_clip(reset_table_selection = FALSE)
    objeto_id_contexto <- suppressWarnings(as.integer(isolate(input$comboObjetoContexto)))

    actionWebUser(function() {
      payload_local <- NULL
      meta_local <- NULL
      missing_local <- character(0)

      ok <- db$tryTransaction(function(conn) {
        frame_ids <- .objeto_contexto_parse_ids(row_sel$cd_id_frame)
        camera_ids_hint <- .objeto_contexto_parse_ids(row_sel$cd_id_camera)
        objeto_ctx <- if (is.finite(objeto_id_contexto)) {
          selectObjetoById(conn, objeto_id_contexto)
        } else {
          data.frame()
        }
        moment_ref <- row_sel$momento[[1]]

        if (!inherits(moment_ref, "POSIXct")) {
          moment_ref <- as.POSIXct(moment_ref, tz = "UTC")
        }

        seqs_by_camera <- list()
        loaded_order <- integer(0)

        if (length(frame_ids)) {
          for (frame_id in frame_ids) {
            df_cam <- tryCatch(
              get_frames_window(
                conn,
                cd_id_frame = frame_id,
                janela = 35L,
                include_blob = TRUE,
                ascending = TRUE
              ),
              error = function(e) NULL
            )

            if (is.null(df_cam) || !nrow(df_cam)) next

            cam_id <- suppressWarnings(as.integer(df_cam$cd_id_camera[[1]]))
            if (!is.finite(cam_id)) next

            seqs_by_camera[[as.character(cam_id)]] <- df_cam
            loaded_order <- c(loaded_order, cam_id)
          }
        }

        if (!length(seqs_by_camera) && length(camera_ids_hint)) {
          for (cam_id in camera_ids_hint) {
            df_cam <- tryCatch(
              selectFramesByCamera(
                conn,
                camera = cam_id,
                janela = 35L,
                date_time = moment_ref,
                chronological = TRUE
              ),
              error = function(e) NULL
            )

            if (is.null(df_cam) || !nrow(df_cam)) {
              missing_local <<- c(missing_local, as.character(cam_id))
              next
            }

            seqs_by_camera[[as.character(cam_id)]] <- df_cam
            loaded_order <- c(loaded_order, cam_id)
          }
        }

        if (!length(seqs_by_camera)) {
          stop("Nenhum frame foi encontrado para o contexto selecionado.")
        }

        camera_ids_loaded <- suppressWarnings(as.integer(names(seqs_by_camera)))
        camera_ids_loaded <- camera_ids_loaded[is.finite(camera_ids_loaded)]

        all_cameras <- selectAllCameras(conn)
        cam_names <- character(0)
        if (is.data.frame(all_cameras) && nrow(all_cameras)) {
          cam_names <- stats::setNames(
            as.character(all_cameras$name_camera),
            as.character(all_cameras$cd_id_camera)
          )
        }

        if (length(camera_ids_hint)) {
          missing_local <<- unique(c(
            missing_local,
            as.character(setdiff(camera_ids_hint, camera_ids_loaded))
          ))
        }

        if (length(missing_local)) {
          missing_local <<- unique(vapply(missing_local, function(cam_ref) {
            cam_ref_chr <- as.character(cam_ref)
            cam_ref_id <- suppressWarnings(as.integer(cam_ref_chr))
            if (is.finite(cam_ref_id)) {
              return(unname(cam_names[[as.character(cam_ref_id)]] %||% paste0("Camera ", cam_ref_id)))
            }
            cam_ref_chr
          }, character(1L)))
        }

        display_order <- unique(c(camera_ids_hint, loaded_order, camera_ids_loaded))
        display_order <- display_order[is.finite(display_order)]
        frame_info_by_camera <- lapply(seqs_by_camera, function(df_cam) {
          if (is.null(df_cam) || !is.data.frame(df_cam) || !nrow(df_cam) || !("data_frame" %in% names(df_cam))) {
            return(.objeto_contexto_blob_frame_info(NULL))
          }

          idx_blob <- which(vapply(df_cam$data_frame, function(blob) {
            !is.null(blob) && length(blob) > 0L && !all(is.na(blob))
          }, logical(1L)))

          blob <- if (length(idx_blob)) df_cam$data_frame[idx_blob[[1]]] else NULL
          .objeto_contexto_blob_frame_info(blob)
        })
        components_by_camera <- .objeto_contexto_clip_components_by_camera(
          objeto_ctx,
          frame_info_by_camera = frame_info_by_camera
        )

        cameras_payload <- Filter(Negate(is.null), lapply(display_order, function(cam_id) {
          df_cam <- seqs_by_camera[[as.character(cam_id)]]
          if (is.null(df_cam) || !nrow(df_cam)) {
            return(NULL)
          }

          cam_label <- unname(cam_names[[as.character(cam_id)]] %||% paste0("Camera ", cam_id))
          frame_info <- frame_info_by_camera[[as.character(cam_id)]] %||% .objeto_contexto_blob_frame_info(NULL)

          frames_payload <- lapply(seq_len(nrow(df_cam)), function(i) {
            ts_frame <- as.POSIXct(df_cam$dt_hr_local[[i]], tz = "UTC")
            ts_frame <- lubridate::with_tz(ts_frame, tzone = tz_use)
            list(
              src = .objeto_contexto_to_data_url(df_cam$data_frame[[i]]),
              label = format(ts_frame, "%d/%m/%Y %H:%M:%S"),
              width = as.integer(frame_info$width %||% .DEFAULT_FRAME_WIDTH),
              height = as.integer(frame_info$height %||% .DEFAULT_FRAME_HEIGHT)
            )
          })

          has_blob <- any(vapply(frames_payload, function(frame) {
            src <- frame$src
            !is.null(src) && length(src) && !is.na(src[[1]]) && nzchar(src[[1]])
          }, logical(1L)))

          if (!isTRUE(has_blob)) {
            missing_local <<- c(missing_local, cam_label)
            return(NULL)
          }

          list(
            id = as.integer(cam_id),
            name = cam_label,
            frame_width = as.integer(frame_info$width %||% .DEFAULT_FRAME_WIDTH),
            frame_height = as.integer(frame_info$height %||% .DEFAULT_FRAME_HEIGHT),
            frames = frames_payload,
            components = components_by_camera[[as.character(cam_id)]] %||% list()
          )
        }))

        if (!length(cameras_payload)) {
          stop("Os blobs dos frames nao estao disponiveis para o contexto selecionado.")
        }

        context_label <- .objeto_contexto_first_chr(row_sel$contexto, "")
        if (is.na(context_label) || !nzchar(context_label)) {
          context_label <- "Clip do contexto"
        }

        moment_label <- format(as.POSIXct(row_sel$momento[[1]]), "%d/%m/%Y %H:%M:%S")
        clip_moments_num <- unlist(lapply(seqs_by_camera, function(df_cam) {
          as.numeric(as.POSIXct(df_cam$dt_hr_local, tz = "UTC"))
        }), use.names = FALSE)
        clip_moments_num <- clip_moments_num[is.finite(clip_moments_num)]
        clip_start_utc <- if (length(clip_moments_num)) {
          as.POSIXct(min(clip_moments_num), origin = "1970-01-01", tz = "UTC")
        } else {
          as.POSIXct(moment_ref, tz = "UTC")
        }
        clip_end_utc <- if (length(clip_moments_num)) {
          as.POSIXct(max(clip_moments_num), origin = "1970-01-01", tz = "UTC")
        } else {
          as.POSIXct(moment_ref, tz = "UTC")
        }
        clip_start_local <- lubridate::with_tz(clip_start_utc, tzone = tz_use)
        clip_end_local <- lubridate::with_tz(clip_end_utc, tzone = tz_use)
        clip_period_label <- paste0(
          format(clip_start_local, "%d/%m/%Y %H:%M:%S"),
          " ate ",
          format(clip_end_local, "%d/%m/%Y %H:%M:%S")
        )
        objeto_nome_ctx <- if (is.data.frame(objeto_ctx) && nrow(objeto_ctx)) {
          .objeto_contexto_first_chr(objeto_ctx$name_objeto, paste0("#", objeto_id_contexto))
        } else {
          paste0("#", objeto_id_contexto)
        }
        objeto_tipo_ctx <- if (is.data.frame(objeto_ctx) && nrow(objeto_ctx)) {
          suppressWarnings(as.integer(objeto_ctx$cd_id_objeto_tipo[[1]]))
        } else {
          NA_integer_
        }

        payload_local <<- list(
          context_label = context_label,
          moment_label = moment_label,
          fps_default = 5L,
          repeat_default = TRUE,
          components_visible_default = TRUE,
          clip_period_label = clip_period_label,
          show_finning_tunnel = isTRUE(is.finite(objeto_tipo_ctx) && objeto_tipo_ctx %in% c(1L, 2L)),
          total_steps = max(vapply(cameras_payload, function(cam) length(cam$frames), integer(1L))),
          cameras = cameras_payload
        )

        meta_local <<- list(
          objeto = objeto_ctx,
          contexto = .objeto_contexto_first_chr(row_sel$contexto, ""),
          moment_label = moment_label,
          clip_start_utc = clip_start_utc,
          clip_end_utc = clip_end_utc,
          clip_period_label = clip_period_label,
          title_default = paste0(
            "Contexto ",
            toupper(as.character(objeto_nome_ctx)),
            " ",
            format(as.POSIXct(row_sel$momento[[1]]), "%d/%m/%Y %H:%M:%S")
          )
        )
      })

      .clear_contexto_clip(reset_table_selection = TRUE)

      if (!isTRUE(ok) || is.null(payload_local) || !length(payload_local$cameras)) {
        showNotification("Nao foi possivel carregar o clip do contexto.", type = "warning")
        return(invisible(NULL))
      }

      contexto_clip_missing(unique(missing_local))
      contexto_clip_payload(payload_local)
      contexto_clip_meta(meta_local)
    }, delay = 0, lock_id = "objeto_contexto_clip_load")
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btFecharContextoClip, {
    .clear_contexto_clip(reset_table_selection = TRUE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btFinningTunnel, {
    meta <- isolate(contexto_clip_meta())
    if (is.null(meta) || is.null(meta$objeto) || !is.data.frame(meta$objeto) || !nrow(meta$objeto)) {
      showNotification("Nao foi possivel identificar o objeto do clip selecionado.", type = "warning")
      return(invisible(NULL))
    }

    objeto_ctx <- meta$objeto
    tipo_objeto <- suppressWarnings(as.integer(objeto_ctx$cd_id_objeto_tipo[[1]]))
    if (!is.finite(tipo_objeto) || !(tipo_objeto %in% c(1L, 2L))) {
      showNotification("Finning Tunnel esta disponivel apenas para objetos estaticos e dinamicos.", type = "warning")
      return(invisible(NULL))
    }

    if (isTRUE(tipo_objeto == 2L)) {
      treinar$uiNewTreinar(
        ns,
        input,
        output,
        session,
        callback = function() {},
        dialogTitle = paste0(
          "Finning Tunnel - ",
          .objeto_contexto_first_chr(objeto_ctx$name_objeto, "Objeto Dinamico")
        ),
        context_prefill = list(
          objeto_id = suppressWarnings(as.integer(objeto_ctx$cd_id_objeto[[1]])),
          dt_begin = as.POSIXct(meta$clip_start_utc, tz = "UTC"),
          dt_end = as.POSIXct(meta$clip_end_utc, tz = "UTC")
        )
      )

      showNotification(
        "Treinar aberto com o objeto e o periodo do contexto preenchidos. Clique na lupa para carregar os frames e seguir com o tracking.",
        type = "message",
        duration = 7
      )
      return(invisible(NULL))
    }

    if (!is.data.frame(tipos_pacote_contexto) || !nrow(tipos_pacote_contexto)) {
      showNotification("Nao foi possivel carregar os tipos de pacote.", type = "error")
      return(invisible(NULL))
    }

    values_map <- .objeto_contexto_parse_inference_values(meta$contexto)
    .clear_contexto_training_overlay()

    insertUI(
      selector = paste0("#parent", id, " .modal-content"),
      where = "beforeEnd",
      ui = .objeto_contexto_train_overlay_ui(
        ns = ns,
        payload = meta,
        objeto = objeto_ctx,
        tipos_pacote = tipos_pacote_contexto,
        values_map = values_map
      )
    )
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btFecharFinningTunnel, {
    .clear_contexto_training_overlay()
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btSalvarFinningTunnel, {
    meta <- isolate(contexto_clip_meta())
    if (is.null(meta) || is.null(meta$objeto) || !is.data.frame(meta$objeto) || !nrow(meta$objeto)) {
      showNotification("Nenhum clip valido foi encontrado para salvar.", type = "warning")
      return(invisible(NULL))
    }

    objeto_ctx <- meta$objeto
    attrs_df <- .objeto_contexto_collect_train_attrs(
      input_source = isolate(shiny::reactiveValuesToList(input)),
      objeto = objeto_ctx,
      id_prefix = "objCtxTrain"
    )
    attrs_status <- .objeto_contexto_validate_train_attrs(attrs_df)

    if (!isTRUE(attrs_status$ok)) {
      showNotification(
        paste0(
          "Preencha todos os atributos antes de salvar: ",
          paste(unique(attrs_status$missing), collapse = ", ")
        ),
        type = "error"
      )
      return(invisible(NULL))
    }

    titulo <- trimws(as.character(isolate(input$finningTunnelTitulo) %||% ""))
    if (!nzchar(titulo)) {
      showNotification("Informe um titulo para o clip.", type = "warning")
      return(invisible(NULL))
    }

    tipo_nome <- as.character(isolate(input$finningTunnelTipoPacote) %||% "")
    tipo_pacote <- tipos_pacote_contexto |>
      dplyr::filter(.data$name_tipo_pacote == tipo_nome)
    if (!nrow(tipo_pacote)) {
      showNotification("Selecione um tipo de pacote valido.", type = "warning")
      return(invisible(NULL))
    }

    dt_begin <- as.POSIXct(meta$clip_start_utc, tz = "UTC")
    dt_end <- as.POSIXct(meta$clip_end_utc, tz = "UTC")
    if (is.na(dt_begin) || is.na(dt_end) || dt_begin > dt_end) {
      showNotification("O periodo do clip selecionado e invalido.", type = "error")
      return(invisible(NULL))
    }

    output_ia <- .objeto_contexto_build_output_json(attrs_df)
    input_ia <- .objeto_contexto_build_input_ia(objeto_ctx)

    actionWebUser(function() {
      save_result <- db$tryTransaction(function(conn) {
        objPacote <- list(
          cd_id_objeto = as.integer(objeto_ctx$cd_id_objeto[[1]]),
          titulo_ia = titulo,
          input_ia = input_ia,
          cd_id_tipo_pacote = as.integer(tipo_pacote$cd_id_tipo_pacote[[1]]),
          output_ia = as.character(output_ia),
          dt_hr_local_begin = dt_begin,
          dt_hr_local_end = dt_end
        )

        db$insertTable(conn, "pacote_ia", objPacote)
      })

      if (!isTRUE(save_result)) {
        save_err <- attr(save_result, "error_message", exact = TRUE)
        msg <- "Nao foi possivel salvar a correcao do clip."
        if (!is.null(save_err) && nzchar(save_err)) {
          msg <- paste0(msg, " ", save_err)
        }
        showNotification(msg, type = "error")
        return(invisible(NULL))
      }

      showNotification("Clip corrigido e salvo como pacote de treino.", type = "message")
      .clear_contexto_training_overlay()
    }, delay = 0, lock_id = "objeto_contexto_finning_save")
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btLimparPeriodoContexto, {
    updateAirDateInput(session, "dtDeContexto", clear = TRUE)
    updateAirDateInput(session, "dtAteContexto", clear = TRUE)
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$btExcluirPeriodoContexto, {
    tz_use <- tz_local
    objeto_id <- suppressWarnings(as.integer(isolate(input$comboObjetoContexto)))
    if (is.na(objeto_id)) {
      showNotification("Selecione um objeto para excluir o contexto.", type = "warning")
      return()
    }

    dt_de_raw <- isolate(input$dtDeContexto)
    dt_ate_raw <- isolate(input$dtAteContexto)
    dt_de_local <- .objeto_contexto_parse_datetime(dt_de_raw, tz_local = tz_use)
    dt_ate_local <- .objeto_contexto_parse_datetime(dt_ate_raw, tz_local = tz_use)

    if (.objeto_contexto_input_is_blank(dt_de_raw) || .objeto_contexto_input_is_blank(dt_ate_raw)) {
      showNotification("Informe os filtros De e Ate para excluir o periodo.", type = "warning")
      return()
    }

    if (is.null(dt_de_local) || is.null(dt_ate_local)) {
      showNotification("Informe os filtros De e Ate em um formato valido: dd/MM/aaaa HH:mm.", type = "warning")
      return()
    }

    dt_de_utc <- .objeto_contexto_to_utc(dt_de_local, tz_local = tz_use)
    dt_ate_utc <- .objeto_contexto_to_utc(dt_ate_local, tz_local = tz_use)

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
        .objeto_contexto_format_local(dt_de_local, tz_local = tz_use),
        " e ",
        .objeto_contexto_format_local(dt_ate_local, tz_local = tz_use),
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
    .clear_contexto_clip(reset_table_selection = TRUE)
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
        uiEstrutura(ns,componente$name_componente,estruturas,estrutura,componente$poligno_componente[[1]])
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
            poly_text  <- isolate(input$textPolygonComponente)
            poly_new   <- tryCatch(
              .component_polygon_from_string(poly_text),
              error = function(e) {
                showNotification(conditionMessage(e), type = "warning")
                NULL
              }
            )
            if (is.null(poly_new)) return(invisible())
            df_p$name_componente <- toupper(isolate(input$textNameComponente))
            df_p$poligno_componente[[1]] <- poly_new
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
          
          colunaNames <- c('LINHA','OBJETO','TIPO','ID GRUPO','ATIVO','VISUALIZAR / EDITAR','REMOVER')
          
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
              !!colunaNames[5] := ifelse(as.logical(dataset$fg_ativo), "SIM", "NAO"),
              !!colunaNames[6] :=  sapply(dataset$cd_id_objeto, function (x) {
                
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
              !!colunaNames[7] :=  sapply(dataset$cd_id_objeto,function (x) {
                
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
              list(width = '75px', targets = c(1, 3, 4, 5, 6)),
              list(width = 'auto', targets = c(2))
            ),
            search_placeholder = "Pesquisar objeto, tipo, ID grupo ou status"
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
        uiEstrutura(ns,componente$name_componente,estruturas,estrutura,componente$poligno_componente[[1]])
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
            poly_text  <- isolate(input$textPolygonComponente)
            poly_new   <- tryCatch(
              .component_polygon_from_string(poly_text),
              error = function(e) {
                showNotification(conditionMessage(e), type = "warning")
                NULL
              }
            )
            if (is.null(poly_new)) return(invisible())
            df_p$name_componente <- toupper(isolate(input$textNameComponente))
            df_p$poligno_componente[[1]] <- poly_new
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

.component_polygon_as_df <- function(poly) {
  if (is.null(poly)) return(NULL)

  if (is.character(poly)) {
    txt <- trimws(as.character(poly)[1])
    if (!nzchar(txt)) return(NULL)
    poly <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  }

  if (is.null(poly)) return(NULL)
  if (!is.data.frame(poly)) poly <- as.data.frame(poly, stringsAsFactors = FALSE)
  if (!all(c("x", "y") %in% names(poly))) {
    if (ncol(poly) < 2L) return(NULL)
    names(poly)[1:2] <- c("x", "y")
  }

  out <- tibble::tibble(
    x = suppressWarnings(as.numeric(poly$x)),
    y = suppressWarnings(as.numeric(poly$y))
  )
  out <- out[is.finite(out$x) & is.finite(out$y), , drop = FALSE]
  if (!nrow(out)) return(NULL)

  .drop_dup_last(out)
}

.format_polygon_number <- function(x) {
  format(as.numeric(x), trim = TRUE, scientific = FALSE, digits = 15)
}

.component_polygon_to_string <- function(poly) {
  poly_df <- .component_polygon_as_df(poly)
  if (is.null(poly_df) || !nrow(poly_df)) return("")

  paste(
    vapply(seq_len(nrow(poly_df)), function(i) {
      paste0(
        .format_polygon_number(poly_df$x[[i]]),
        ",",
        .format_polygon_number(poly_df$y[[i]])
      )
    }, character(1)),
    collapse = "; "
  )
}

.component_polygon_from_string <- function(text) {
  txt <- trimws(as.character(text)[1])
  if (!nzchar(txt)) {
    stop("Informe a string do poligono do componente.", call. = FALSE)
  }

  poly_json <- tryCatch(.component_polygon_as_df(txt), error = function(e) NULL)
  if (!is.null(poly_json) && nrow(poly_json) >= 3L) {
    return(poly_json)
  }

  tokens <- unlist(strsplit(txt, "(;|\\||\\r\\n|\\n|\\r)+", perl = TRUE))
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]

  if (!length(tokens)) {
    stop("Nao foi possivel interpretar a string do poligono.", call. = FALSE)
  }

  coords <- lapply(tokens, function(token) {
    clean <- gsub("^[\\[{(\\s]+|[\\]})\\s]+$", "", token)
    clean <- trimws(clean)
    if (!nzchar(clean)) return(NULL)

    parts <- if (grepl(",", clean, fixed = TRUE)) {
      strsplit(clean, ",", fixed = TRUE)[[1]]
    } else {
      strsplit(clean, "\\s+")[[1]]
    }

    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    if (length(parts) != 2L) {
      stop("Use o formato x,y; x,y; x,y para o poligono.", call. = FALSE)
    }

    x <- suppressWarnings(as.numeric(parts[[1]]))
    y <- suppressWarnings(as.numeric(parts[[2]]))
    if (!is.finite(x) || !is.finite(y)) {
      stop("Cada ponto do poligono deve ter coordenadas numericas validas.", call. = FALSE)
    }

    tibble::tibble(x = x, y = y)
  })

  poly_df <- dplyr::bind_rows(coords)
  poly_df <- .drop_dup_last(poly_df)

  if (nrow(poly_df) < 3L) {
    stop("O poligono do componente precisa ter pelo menos 3 pontos.", call. = FALSE)
  }

  poly_df
}

uiEstrutura <- function(ns,nameComp,estruturas,estrutura,poligno = NULL){
  
  div(
    inlineCSS(paste0("#", ns("textNameComponente"), " {text-transform: uppercase;}")),
    textInput(ns("textNameComponente"), label = "Nome",
    placeholder = "Digite o nome para o componente",value = nameComp),
    textAreaInput(
      ns("textPolygonComponente"),
      label = "String do poligono",
      value = .component_polygon_to_string(poligno),
      placeholder = "Ex.: 10,10; 120,10; 120,80; 10,80",
      resize = "vertical",
      width = "100%",
      rows = 4
    ) |>
      shiny::tagAppendAttributes(style = "width: 100%;"),
    tags$div(
      style = "margin-top:-10px; margin-bottom:10px; color:#666; font-size:12px;",
      "Formato: x,y; x,y; x,y"
    ),
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


