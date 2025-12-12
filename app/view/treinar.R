# ============================================
# Novo Treinamento (refatorado e organizado)
# - Player com later::later (sem reentrância)
# - Cache LRU + Prefetch por lote
# - Múltiplos leafletOutput por câmera
# - Handler JS 'set_frame_to' (id alvo)
# - Timezone consistente (UTC) p/ chaves/queries
# - Clip com duração máxima (default 5 min)
# - Player de clip dentro do modal do clip (Preview independente)
# ============================================

box::use(
  shiny[...],
  shinyjs[inlineCSS, delay],
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
    messageAlerta,
    actionWebUser,
    removeProgressLoader
  ],
  dplyr[...],
  tidyr[...],
  DT,
  shinyWidgets[airDatepickerInput, timepickerOptions],
  shinycssloaders,
  .. / logic/objeto_dao[...],
  .. / logic/camera_dao[...],
  .. / logic/setor_dao[...],
  .. / logic/treinar_dao[...],
  base64enc[...],
  dbp = ../infra/db_pool,
  db  = ../infra/database,
  later,
  leaflet[...],
  purrr[map, map_df,map_chr],
  utils[...]
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

# ---------- Parâmetros ----------
PREFETCH_AHEAD  <- 16L
MAX_CACHE_ITEMS <- 400L

# ==================================================
# Utils: MIME / Data URL / Dimensões de imagem
# ==================================================
detect_mime <- function(raw) {
  if (length(raw) >= 2 && raw[1] == as.raw(0xFF) && raw[2] == as.raw(0xD8)) return("image/jpeg")
  if (length(raw) >= 8 && all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return("image/png")
  "application/octet-stream"
}
to_data_url <- function(raw) paste0("data:", detect_mime(raw), ";base64,", base64enc::base64encode(raw))

be32 <- function(v) sum(as.integer(v) * c(256^3, 256^2, 256, 1))
png_dims <- function(raw) {
  if (length(raw) < 24) return(c(NA_integer_, NA_integer_))
  if (!all(raw[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))) return(c(NA_integer_, NA_integer_))
  w <- be32(raw[17:20]); h <- be32(raw[21:24]); c(w, h)
}
jpeg_dims <- function(raw) {
  n <- length(raw); i <- 3L
  while (i + 8L <= n) {
    if (raw[i] != as.raw(0xFF)) { i <- i + 1L; next }
    marker <- as.integer(raw[i + 1L])
    if (marker == 0xD8 || marker == 0xD9) { i <- i + 2L; next }
    if (i + 3L > n) break
    len <- as.integer(raw[i + 2L]) * 256L + as.integer(raw[i + 3L])
    if (len < 2L) break
    if (marker >= 0xC0 && marker <= 0xC3) {
      if (i + 8L > n) break
      h <- as.integer(raw[i + 5L]) * 256L + as.integer(raw[i + 6L])
      w <- as.integer(raw[i + 7L]) * 256L + as.integer(raw[i + 8L])
      return(c(w, h))
    }
    i <- i + 2L + len
  }
  c(NA_integer_, NA_integer_)
}
img_dims <- function(raw) {
  mime <- detect_mime(raw)
  if (identical(mime, "image/png"))  return(png_dims(raw))
  if (identical(mime, "image/jpeg")) return(jpeg_dims(raw))
  c(NA_integer_, NA_integer_)
}

# ==================================================
# Utils: tempo/chaves/formatos
# ==================================================
key_of <- function(cam, ts_utc) sprintf(
  "%s|%s", as.character(cam), format(ts_utc, "%Y-%m-%d %H:%M:%OS", tz = "UTC")
)
fmt_pt <- function(x, tz) format(x, tz = tz, format = "%d/%m/%y %H:%M:%S")

validaDateFormat <- function(text){
  grepl(
    "^(0[1-9]|[12][0-9]|3[01])/(0[1-9]|1[0-2])/[0-9]{2} ([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$",
    text
  )
}

# ==================================================
# LRU Cache (env + funções puras)
# ==================================================
new_lru_cache <- function(max_items = MAX_CACHE_ITEMS) {
  cache <- new.env(parent = emptyenv())
  order <- character(0)
  list(
    get = function(k) {
      if (!exists(k, envir = cache, inherits = FALSE)) return(NULL)
      idx <- which(order == k); if (length(idx)) order <<- c(order[-idx], k)
      get(k, envir = cache, inherits = FALSE)
    },
    put = function(k, val) {
      assign(k, val, envir = cache)
      idx <- which(order == k); if (length(idx)) order <<- order[-idx]
      order <<- c(order, k)
      if (length(order) > max_items) {
        drop_keys <- order[seq_len(length(order) - max_items)]
        rm(list = drop_keys, envir = cache)
        order <<- tail(order, max_items)
      }
    },
    clear = function() {
      rm(list = ls(envir = cache), envir = cache)
      order <<- character(0)
    },
    keys = function() order
  )
}

# ==================================================
# PlayerContext (render + prefetch), independente do módulo
# ==================================================
new_player_ctx <- function(session, pool, lru_cache) {
  env <- new.env(parent = emptyenv())
  env$session <- session
  env$pool    <- pool
  env$cache   <- lru_cache

  env$fetch_dataurl_single <- function(cam, ts_utc) {
    k <- key_of(cam, ts_utc)
    hit <- env$cache$get(k)
    if (!is.null(hit)) return(hit)

    res <- db_fetch_frame_raw(env$pool, cam, ts_utc)
    if (!nrow(res) || is.null(res$DATA_FRAME[[1]])) return(NULL)
    raw <- res$DATA_FRAME[[1]]
    uri <- to_data_url(raw)
    env$cache$put(k, uri)
    uri
  }


 env$render_current <- function(seq_df, i, w, h, id_map, fit_bounds = FALSE) {
    if (is.null(seq_df) || !nrow(seq_df)) return(list(ok = FALSE, w = w, h = h))
    i <- max(1L, min(nrow(seq_df), as.integer(i)))

    # Timestamp de referência do "passo" atual
    ts_ref <- as.POSIXct(seq_df$DT_HR_LOCAL[i], tz = "UTC")

    # Todas as linhas do mesmo timestamp (potencialmente várias câmeras)
    same_ts <- seq_df[seq_df$DT_HR_LOCAL == ts_ref, , drop = FALSE]
    if (!nrow(same_ts)) return(list(ok = FALSE, w = w, h = h))

    ok_any <- FALSE

    # Renderiza cada câmera desse timestamp
    for (row in seq_len(nrow(same_ts))) {
      cam    <- as.integer(same_ts$CD_ID_CAMERA[row])
      dom_id <- id_map[[as.character(cam)]]
      if (is.null(dom_id) || is.na(dom_id)) next

      uri <- env$fetch_dataurl_single(cam, ts_ref)
      if (is.null(uri)) next

      env$session$sendCustomMessage("set_frame_to", list(
        id  = dom_id,
        url = uri,
        w   = as.integer(w),
        h   = as.integer(h),
        fit = isTRUE(fit_bounds)
      ))
      ok_any <- TRUE
    }

    list(ok = ok_any, w = w, h = h)
  }

  env$prefetch_ahead_batch <- function(seq_df, i, N = PREFETCH_AHEAD) {
    if (is.null(seq_df) || !nrow(seq_df)) return(invisible(FALSE))
    n <- nrow(seq_df); i <- as.integer(i)
    if (i >= n) return(invisible(FALSE))

    # Mantém seu prefetch por índice, mas isso já cobre multi-câmera
    tgt_idx <- seq.int(i + 1L, min(n, i + N))
    if (!length(tgt_idx)) return(invisible(FALSE))
    seg <- seq_df[tgt_idx, , drop = FALSE]
    seg$k <- mapply(key_of, seg$CD_ID_CAMERA, seg$DT_HR_LOCAL)
    seg <- seg[!vapply(seg$k, function(z) !is.null(env$cache$get(z)), logical(1L)), , drop = FALSE]
    if (!nrow(seg)) return(invisible(TRUE))

    groups <- split(seg, seg$CD_ID_CAMERA)
    for (cam_str in names(groups)) {
      g      <- groups[[cam_str]]
      cam    <- as.integer(cam_str)
      ts_vec <- as.POSIXct(g$DT_HR_LOCAL, tz = "UTC")

      res <- tryCatch(db_fetch_many_frames(env$pool, cam, ts_vec), error = function(e) NULL)
      if (!is.null(res) && nrow(res)) {
        res$DT_HR_LOCAL <- as.POSIXct(res$DT_HR_LOCAL, tz = "UTC")
        idx_map <- match(ts_vec, res$DT_HR_LOCAL)
        for (j in seq_along(ts_vec)) {
          if (!is.na(idx_map[j])) {
            raw <- res$DATA_FRAME[[idx_map[j]]]
            if (!is.null(raw)) env$cache$put(key_of(cam, ts_vec[j]), to_data_url(raw))
          }
        }
      }
      for (j in seq_along(ts_vec)) {
        if (is.null(env$cache$get(key_of(cam, ts_vec[j])))) {
          invisible(env$fetch_dataurl_single(cam, ts_vec[j]))
        }
      }
    }

    invisible(TRUE)
  }

  env
}

# ==================================================
# Clip Manager util
# ==================================================
clips_update_title <- function(df,id,title){
  if (!nrow(df)) return(NULL)
  idx <- which(df$id == id)
  if (length(idx)) { df$title[idx] <- title; df }
}

clips_update_date_time_t0 <- function(df,id,time){
  if (!nrow(df)) return(NULL)
  idx <- which(df$id == id)
  new_time <- as.POSIXct(as.POSIXct(time,format = "%d/%m/%y %H:%M:%S"),tz = "UTC")
  if (length(idx)) { df$t0[idx] <- new_time; df }
}
clips_update_date_time_t1 <- function(df,id,time){
  if (!nrow(df)) return(NULL)
  idx <- which(df$id == id)
  new_time <- as.POSIXct(as.POSIXct(time,format = "%d/%m/%y %H:%M:%S"),tz = "UTC")
  if (length(idx)) { df$t1[idx] <- new_time; df }
}

clip_start <- function(rv) {
  ts0 <- rv_get_current_ts(rv)
  if (is.null(ts0)) return(FALSE)
  rv$clip_active <- TRUE
  rv$clip_t0     <- ts0
  rv$clip_i0     <- rv$i
  TRUE
}
clip_limit_exceeded <- function(rv, max_min, now_ts = NULL) {
  if (!isTRUE(rv$clip_active)) return(FALSE)
  if (is.null(rv$clip_t0))     return(FALSE)
  if (is.null(now_ts)) now_ts <- rv_get_current_ts(rv)
  if (is.null(now_ts)) return(FALSE)
  max_sec <- (if (is.finite(max_min) && max_min > 0) max_min else 5) * 60
  elapsed <- as.numeric(difftime(now_ts, rv$clip_t0, units = "secs"))
  is.finite(elapsed) && elapsed >= max_sec
}

# ==================================================
# Overlay resumo do clip + PLAYER DO CLIP
# ==================================================
# - preview do frame atual do clip
# - botões Reverse / Prev / Play / Pause / Next
# - numericInput pra velocidade (ms)
#
# OBS IMPORTANTE:
# Esse overlay só desenha UI. Ele NÃO move frame sozinho.
# Quem controla o playback (loop later) é o server usando clipOverlayPlayer.
clip_summary_overlay <- function(ns, session, input, objeto,id_clip,ts_start, ts_end, n_frames,tz_local = Sys.timezone()) {

  if (is.null(ts_start) || is.null(ts_end)) return(invisible())

  codigo_date <- paste0(id_clip,"_")
  overlay_id  <- ns("clip_summary_overlay")
  parent_sel  <- paste0("#parent", ns("dialogTrain"), " .modal-content")

  try(removeUI(selector = paste0("#", overlay_id), multiple = TRUE, immediate = TRUE), silent = TRUE)

  # --- Formulário de atributos (igual ao teu) ---
  camera_ids    <- NULL
  camera_names  <- NULL

  atributos_mem <- collect_clip_attributes(input,objeto,id_clip,ts_start,ts_end)
  componentes   <- objeto$CONFIG[[1]]$COMPONENTES[[1]]
  divLista      <- fluidRow()

  for (i in seq_len(nrow(componentes))) {

    comp          <- componentes[i,]
    id_comp       <- comp$CD_ID_COMPONENTE
    estrutura     <- comp$ESTRUTURA[[1]]
    atributos     <- estrutura$CONFIGS[[1]]$ATRIBUTOS[[1]]
    camera_ids    <- c(camera_ids,comp$CD_ID_CAMERA)
    listAtributos <- tagList()

    for (k in seq_len(nrow(atributos))) {
      atributo  <- atributos[k,]
      id_html   <- ns(paste0(codigo_date,comp$CD_ID_COMPONENTE, "_", atributo$CD_ID_ATRIBUTO, "_", k))
      att_tmp   <- atributos_mem |> filter(CD_ID_COMPONENTE == id_comp) |> filter(CD_ID_ATRIBUTO == atributo$CD_ID_ATRIBUTO)
     
      if(any(is.na(att_tmp$VALUE))) value <- NULL

      if (atributo$NAME_DATA == "QUALITATIVE") {
        classes       <- stringr::str_split(atributo$VALUE_ATRIBUTO, ",")[[1]]
        listAtributos <- tagAppendChildren(
          listAtributos,
          selectizeInput(id_html,
            label   = atributo$NAME_ATRIBUTO,
            choices = classes,
            selected = att_tmp$VALUE,
            options = list(dropdownParent = 'body', openOnFocus = TRUE, closeAfterSelect = TRUE)
          )
        )
      } else {
        listAtributos <- tagAppendChildren(
          listAtributos,
          numericInput(id_html, label = atributo$NAME_ATRIBUTO, value = att_tmp$VALUE)
        )
      }
    }

    divLista <- tagAppendChildren(
      divLista,
      panelTitle(
        title = comp$NAME_COMPONENTE,
        background.color.title = "white",
        title.color  = "black",
        border.color = "lightgray",
        children = div(style = "padding: 10px;", listAtributos)
      ),
      br()
    )
  }

  # --- PREVIEW MULTI-CÂMERA ---
  # ids e nomes
  camera_ids   <- as.integer(unique(camera_ids))
  if (is.null(camera_names)) {
    cam_tbl <- purrr::map_df(componentes$CAMERA, ~ .x) |>
               dplyr::distinct(CD_ID_CAMERA, NAME_CAMERA)
    camera_names <- vapply(camera_ids, function(cid){
      nm <- cam_tbl$NAME_CAMERA[cam_tbl$CD_ID_CAMERA == cid]
      if (length(nm)) nm[[1]] else paste("Câmera", cid)
    }, character(1))
  }

  # cards: título + <img id="clipOverlayImg_<cam>">
  img_cards <- tagList()
  n         <- length(camera_ids)
  for (k in seq_along(camera_ids)) {
    cid  <- camera_ids[k]
    name <- camera_names[k]
    img_cards <- tagAppendChildren(
      img_cards,
      column(
        width = ifelse(n > 1,6,12),
        panelTitle(
          title = paste0("Preview – ", name, " (", cid, ")"),
          background.color.title = "white",
          title.color  = "black",
          border.color = "lightgray",
          children = div(
            style = "padding: 10px; text-align:center;",
            tags$img(
              id = ns(paste0("clipOverlayImg_", cid)),
              style = "max-width:100%; max-height:300px; border:1px solid #ccc; border-radius:6px; background:#000;"
            )
          )
        )
      )
    )
  }

  # bloco de botões (mantido)
  controls_block <- div(
    style = "padding: 10px; text-align:center;",
    splitLayout(
      cellWidths = c("auto","auto","auto","auto","auto","120px"),
      tags$button(
        class="btn btn-outline-secondary btn-sm",
        title="Reverse",
        onclick = sprintf("Shiny.setInputValue('%s',{action:'reverse',nonce:Math.random()},{priority:'event'})",
                          ns("clipOverlay_action")),
        shiny::icon("backward"), " Reverse"
      ),
      tags$button(
        class="btn btn-outline-secondary btn-sm",
        title="Prev",
        onclick = sprintf("Shiny.setInputValue('%s',{action:'prev',nonce:Math.random()},{priority:'event'})",
                          ns("clipOverlay_action")),
        shiny::icon("step-backward"), " Prev"
      ),
      tags$button(
        class="btn btn-outline-secondary btn-sm",
        title="Play",
        onclick = sprintf("Shiny.setInputValue('%s',{action:'play',nonce:Math.random()},{priority:'event'})",
                          ns("clipOverlay_action")),
        shiny::icon("play"), " Play"
      ),
      tags$button(
        class="btn btn-outline-secondary btn-sm",
        title="Pause",
        onclick = sprintf("Shiny.setInputValue('%s',{action:'pause',nonce:Math.random()},{priority:'event'})",
                          ns("clipOverlay_action")),
        shiny::icon("pause"), " Pause"
      ),
      tags$button(
        class="btn btn-outline-secondary btn-sm",
        title="Next",
        onclick = sprintf("Shiny.setInputValue('%s',{action:'next',nonce:Math.random()},{priority:'event'})",
                          ns("clipOverlay_action")),
        shiny::icon("step-forward"), " Next"
      ),
      numericInput(ns("clipOverlay_step_ms"), label="Intervalo (ms)", value=50, min=1, step=1, width="120px") |>
        tagAppendAttributes(style=';margin-top: -25px;')
    )
  )

  # injeta o overlay
  insertUI(
    selector = parent_sel,
    where    = "beforeEnd",
    ui = div(
      id = overlay_id,
      style = paste(
        "position: fixed; inset: 0; background: rgba(0,0,0,.5); z-index: 1060;",
        "display: flex; align-items: center; justify-content: center;"
      ),
      div(
        style = paste(
          "background:#fff; border-radius:10px; width:min(1100px,96%);",
          "height:92vh; min-height:420px; box-shadow:0 12px 30px rgba(0,0,0,.25);",
          "display:flex; flex-direction:column;"
        ),
        div(
          style = "padding:16px 18px; border-bottom:1px solid #eee; flex:0 0 auto;",
          tags$h4("Clip selecionado", style = "margin:0;")
        ),
        div(
          style = "padding:10px; flex:1 1 auto; min-height:0; overflow-y:auto; overflow-x:hidden;",
          panelTitle(
            title = "Preview do Clip (multi-câmera)",
            background.color.title = "white",
            title.color  = "black",
            border.color = "lightgray",
            children = tagList(controls_block, br(), fluidRow(img_cards))
          ),
          br(),
          panelTitle(
            title = "Componentes",
            background.color.title = "white",
            title.color  = "black",
            border.color = "lightgray",
            children = div(style = "padding: 20px;", divLista)
          )
        ),
        div(
          style = "padding:12px 18px; border-top:1px solid #eee; text-align:right; flex:0 0 auto;",
          actionButton(ns("clipCloseVideo"), "Ok", class = "btn btn-primary btn-sm",width = "80px",height = "34px")
        )
      )
    )
  )
}

# ==================================================
# UI Builders (JS handlers, Header form, Lista de câmeras, Controles)
# ==================================================
uiClipsPanel <- function(ns){
  panelTitle(
    title = "Clips",
    background.color.title = "white",
    title.color  = "black",
    border.color = "lightgray",
    children = div(
      style = "padding: 10px;",
      tags$script(HTML("
      (function(){
        // Regex do formato dd/mm/yy HH:MM:SS
        const reDateTime = /^(0[1-9]|[12][0-9]|3[01])\\/(0[1-9]|1[0-2])\\/[0-9]{2}\\s([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$/;
      
        // Máscara + validação para inputs com classe .shiny-datetime-mask
        $(document).on('input', 'input.shiny-datetime-mask', function(e){
          let v = $(this).val();
      
          v = v.replace(/[^0-9\\/ :]/g, '');
          let digits = v.replace(/[^0-9]/g,'').substring(0,12);
          let out = '';
          if(digits.length > 0){ out += digits.substring(0,2); }
          if(digits.length >= 3){ out += '/' + digits.substring(2,4); }
          if(digits.length >= 5){ out += '/' + digits.substring(4,6); }
          if(digits.length >= 7){ out += ' ' + digits.substring(6,8); }
          if(digits.length >= 9){ out += ':' + digits.substring(8,10); }
          if(digits.length >= 11){ out += ':' + digits.substring(10,12); }
          $(this).val(out);

          if(reDateTime.test(out)){
            $(this).css('border-color', '');
          } else {
            $(this).css('border-color', 'red');
          }
          Shiny.setInputValue($(this).attr('id'), out, {priority:'event'});
        });
      
        $(document).on('blur', 'input.shiny-datetime-mask', function(e){
          const v = $(this).val();
          if(!reDateTime.test(v)){
            $(this).css('border-color','red');
          } else {
            $(this).css('border-color','');
          }
        });
      })();
    ")),
      DT::DTOutput(ns("clipsTable"))
    )
  )
}

ui_js_handlers <- function() {
  tags$script(HTML("
    (function(){
      const overlays = {}; const boundsOf = {}; const maps = {};
      function fitWholeImage(map, w, h){
        const size = map.getSize();
        const scale = Math.min(size.x / w, size.y / h);
        let z = Math.log2(scale); if (!isFinite(z)) z = 0;
        const center = [h/2, w/2];
        map.setView(center, z, {animate:false});
        const b = [[0,0],[h,w]];
        map.setMaxBounds(b); map.options.maxBoundsViscosity = 1.0; map.setMinZoom(z);
        setTimeout(function(){ map.invalidateSize(); }, 0);
        return b;
      }
      Shiny.addCustomMessageHandler('set_frame_to', function(msg){
        const widget = HTMLWidgets.find('#' + msg.id); if (!widget) return;
        const map = widget.getMap(); if (!map) return;
        const w = msg.w || 512, h = msg.h || 512; const b = [[0,0],[h,w]];
        const needRecreate = (!overlays[msg.id]) || (!maps[msg.id]) || (maps[msg.id] !== map);
        if (needRecreate) {
          try { if (overlays[msg.id]) overlays[msg.id].remove(); } catch(e){}
          overlays[msg.id] = L.imageOverlay(msg.url, b, {opacity:1}).addTo(map);
          maps[msg.id]     = map;
          boundsOf[msg.id] = fitWholeImage(map, w, h);
          return;
        }
        const changed = !boundsOf[msg.id] || boundsOf[msg.id][1][0] !== h || boundsOf[msg.id][1][1] !== w;
        if (changed){
          overlays[msg.id].setBounds(b);
          boundsOf[msg.id] = b;
          if (msg.fit) boundsOf[msg.id] = fitWholeImage(map, w, h);
        }
        overlays[msg.id].setUrl(msg.url);
      });
      Shiny.addCustomMessageHandler('reset_overlays', function(ids){
        (ids || []).forEach(function(id){
          try { if (overlays[id]) overlays[id].remove(); } catch(e){}
          delete overlays[id]; delete boundsOf[id]; delete maps[id];
        });
      });

      // >>> NOVO: handler para atualizar o frame do clip dentro do modal
      Shiny.addCustomMessageHandler('set_clip_overlay_frame', function(msg){
        var imgEl = document.getElementById(msg.img_id);
        if(!imgEl) return;
        imgEl.src = msg.url || '';
      });

    })();
  "))
}

uiMain <- function(ns, setores) {
  div(
    ui_js_handlers(),
    inlineCSS(paste0("#", ns("textNameTreino"), " {text-transform: uppercase;}")),
    panelTitle(
      title = "Configuração",
      background.color.title = 'white',
      title.color  = 'black',
      border.color = 'lightgray',
      children = fluidRow(
        style = 'padding-top: 10px; padding-left: 15px; padding-right: 15px;',
        column(3, selectizeInput(ns('comboSetor'),  label = 'Setor',  choices = setores$NAME_SETOR)),
        column(3, selectizeInput(ns('comboObjeto'), label = 'Objeto', choices = NA)),
        column(6, splitLayout(
          cellWidths = c("45%", "45%", "10%"),
          airDatepickerInput(
            inputId = ns("datetimeBegin"), label = "Data e Hora De:",
            language = "pt-BR", timepicker = TRUE, dateFormat = "dd/MM/yyyy",
            timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 1),
            update_on  = "change",
            readonly   = TRUE,
            onkeydown  = "if(event.key==='Enter'){this.blur();}",
            width ="98%", placeholder = "Escolha uma data e hora"
          ),
          airDatepickerInput(
            inputId = ns("datetimeEnd"), label = "Data e Hora Até:",
            language = "pt-BR", timepicker = TRUE, dateFormat = "dd/MM/yyyy",
            timepickerOpts = timepickerOptions(hoursStep = 1, minutesStep = 1),
            update_on  = "change",
            readonly   = TRUE,
            onkeydown  = "if(event.key==='Enter'){this.blur();}",
            width ="98%", placeholder = "Escolha uma data e hora"
          ),
          actionButton(ns('btBuscar'), label = '', icon = icon("search"),
                       style = "margin-top: 25px; margin-left: -5px;")
        ))
      )
    ),
    br(),
    uiOutput(ns('uiCamerasFrames'))
  )
}

uiCamerasComponentes <- function(ns, input, output, componentes) {
  cameras  <- purrr::map_df(componentes$CAMERA, ~ .x) |>
              dplyr::distinct(CD_ID_CAMERA, NAME_CAMERA)

  id_by_cam <- list()
  divLista  <- fluidRow()
  n_cam     <- nrow(cameras)

  for (i in seq_len(n_cam)) {
    local({
      cam_id       <- cameras$CD_ID_CAMERA[i]
      comps_by_cam <- componentes[componentes$CD_ID_CAMERA == cam_id,]

      cam_name <- cameras$NAME_CAMERA[i]
      out_id   <- paste0("map_", cam_id)
      dom_id   <- ns(out_id)
      id_by_cam[[as.character(cam_id)]] <<- dom_id

      output[[out_id]] <- renderLeaflet({
        map_cam <- leaflet(options = leafletOptions(
          crs = leafletCRS(crsClass = "L.CRS.Simple"),
          zoomSnap  = 0, zoomDelta = 0.25
        ))
        for (k in seq_len(nrow(comps_by_cam))) {
          comp      <- comps_by_cam[k,]
          poligno   <- comp$POLIGNO_COMPONENTE[[1]]
          estrutura <- comp$ESTRUTURA[[1]]
          label <- HTML(paste0("<strong>COMPONENTE:</strong> ", comp$NAME_COMPONENTE,
                               "<br><strong>ESTRUTURA:</strong> ", estrutura$NAME_ESTRUTURA))
          map_cam <- map_cam |>
            addPolygons(
              group = "comp",
              lng = poligno$x, lat = poligno$y,
              layerId = comp$CD_ID_COMPONENTE, weight = 2, fillOpacity = 0.1, label = label
            )
        }
        map_cam
      })

      cameraElement <- panelTitle(
        title = paste0("Câmera: ", cam_name),
        background.color.title = "white",
        title.color  = "black",
        border.color = "lightgray",
        children = div(style = "padding: 10px;", leafletOutput(dom_id, height = "350px", width = "100%"))
      )
      col_w <- if (i == n_cam && (n_cam %% 2L == 1L)) 12L else 6L
      divLista <<- tagAppendChildren(divLista, column(col_w, cameraElement))
    })
  }
  list(ui = divLista, id_by_cam = id_by_cam)
}

uiComponenteVideo <- function(ns){
  splitLayout(
    cellWidths = c("auto","auto","auto","auto","auto","110px","auto","120px","auto"),
    actionButton(ns("backPlay"),  label = "Reverse", icon = icon("backward"),
                 class = "btn btn-outline-secondary", title = "Reproduzir para trás"),
    actionButton(ns("prevFrame"), label = "Prev",    icon = icon("step-backward"),
                 class = "btn btn-outline-secondary", title = "Frame anterior"),
    actionButton(ns("play"),      label = "Play",    icon = icon("play"),
                 class = "btn btn-outline-secondary", title = "Reproduzir"),
    actionButton(ns("pause"),     label = "Pause",   icon = icon("pause"),
                 class = "btn btn-outline-secondary", title = "Pausar"),
    actionButton(ns("nextFrame"), label = "Next",    icon = icon("step-forward"),
                 class = "btn btn-outline-secondary", title = "Próximo frame"),
    numericInput(ns("step_ms"),"Intervalo (ms)", value = 50, min = 1, step = 1, width = "110px") |>
      tagAppendAttributes(style = ';margin-top: -25px;'),
    actionButton(ns("clipToggle"), label = "Start Clip", icon = icon("scissors"),
                 class = "btn btn-outline-primary", title = "Iniciar/encerrar clip (máx. duração)"),
    numericInput(ns("clip_max_min"), "Máx (min)", value = 1, min = 1, max = 60, step = 1, width = "120px") |>
      tagAppendAttributes(style = ';margin-top: -25px;'),
    textOutput(ns("titleClock"))
  )
}

# ==================================================
# Helpers públicos/compartilhados
# ==================================================
rv_get_current_ts <- function(rv) {
  isolate({
    if (is.null(rv$seq) || !nrow(rv$seq)) return(NULL)
    i  <- max(1L, min(nrow(rv$seq), rv$i))
    rv$seq$DT_HR_LOCAL[i]  # UTC no pipeline
  })
}
get_current_frame_ts <- function(rv, tz = "UTC", fmt = NULL) {
  ts <- rv_get_current_ts(rv)
  if (is.null(fmt)) return(ts)
  format(ts, tz = tz, usetz = FALSE, format = fmt)
}

# ==================================================
# Módulo principal (server)
# ==================================================
#' @export
uiNewTreinar <- function(ns, input, output, session, callback){

  .register_auto_dispose(session)

  e <- .get_private(session)

  clockupdate <- reactiveVal()
  obs         <- newObserve()
  setores      <- selectAllSetors(dbp$get_pool())
  objetos      <- selectAllObjetos(dbp$get_pool())
  tiposPacotes <- selectAllTypesPacote(dbp$get_pool())
  objeto      <- NULL
  
  if(nrow(objetos) == 0){
    obs$destroy()
    showNotification("Nenhum registro de objeto foi encontrado!", type = "error")
    callback()
  }

  # ---------- Estado player principal ----------
  rv <- reactiveValues(
    seq = NULL, i = 1L, w = 512L, h = 512L,
    id_by_cam = NULL,
    clip_active = FALSE, clip_t0 = NULL, clip_i0 = NA_integer_
  )

  playing   <- reactiveVal(FALSE)
  loop_on   <- FALSE
  play_dir  <- reactiveVal(+1L)

  # ---------- Store de Clips ----------
  clips <- reactiveVal(
    data.frame(
      id = integer(0),
      title = character(0),
      t0 = as.POSIXct(character(0), tz = "UTC"),
      t1 = as.POSIXct(character(0), tz = "UTC"),
      i0 = integer(0),
      i1 = integer(0),
      stringsAsFactors = FALSE
    )
  )
  next_clip_id <- reactiveVal(1L)

  updateClipsTable <- function(df){
    n <- nrow(df)
    if(!n) return(df)
    for(i in 1:nrow(df)){
      id            <- df$id[i]
      df$title[i]   <- isolate(input[[paste0("clip_title_",id)]])
      df            <- clips_update_date_time_t0(df,id,isolate(input[[paste0("clip_t0_",id)]]))
      df            <- clips_update_date_time_t1(df,id,isolate(input[[paste0("clip_t1_",id)]]))
    }
    df
  }

  clips_add <- function(t0, t1, i0, i1, title = ""){
    df <- updateClipsTable(clips())
    id <- next_clip_id()
    row <- data.frame(
      id    = id,
      title = if (nzchar(title)) title else sprintf("Clip %d", id),
      t0 = as.POSIXct(t0, tz = "UTC"),
      t1 = as.POSIXct(t1, tz = "UTC"),
      i0 = as.integer(i0),
      i1 = as.integer(i1),
      stringsAsFactors = FALSE
    )
    clips(rbind(df, row))
    next_clip_id(id + 1L)
  }

  clips_remove <- function(id){
    df <- updateClipsTable(clips())
    if (!nrow(df)) return(invisible())
    clips(df[df$id != id, , drop = FALSE])
  }

  # ---------- Cache + PlayerContext ----------
  if (is.null(session$userData$lru_cache))   session$userData$lru_cache   <- new_lru_cache(MAX_CACHE_ITEMS)
  if (is.null(session$userData$player_ctx))  session$userData$player_ctx  <- new_player_ctx(session, dbp$get_pool(), session$userData$lru_cache)
  ctx <- session$userData$player_ctx
  
  # ---------- Player do CLIP no modal (preview independente) ----------
  clipOverlayPlayer <- reactiveValues(
    seq = NULL,           # subconjunto de rv$seq (frames do clip)
    i   = 1L,             # índice atual no clip
    dir = +1L,            # direção +1 / -1
    playing = FALSE       # se está rodando loop
  )
  
  # desenha frame atual do clipOverlayPlayer na <img> do modal
  render_clip_overlay_frame <- function() {
    isolate({
      if (is.null(clipOverlayPlayer$seq) || !nrow(clipOverlayPlayer$seq)) return(invisible())
      j  <- max(1L, min(nrow(clipOverlayPlayer$seq), as.integer(clipOverlayPlayer$i)))
      ts <- as.POSIXct(clipOverlayPlayer$seq$DT_HR_LOCAL[j], tz = "UTC")
      
      # todas as linhas com o mesmo timestamp no sub-seq (multi-câmera)
      same_ts <- clipOverlayPlayer$seq[clipOverlayPlayer$seq$DT_HR_LOCAL == ts, , drop = FALSE]
      if (!nrow(same_ts)) return(invisible())
      
      for (r in seq_len(nrow(same_ts))) {
        cam <- as.integer(same_ts$CD_ID_CAMERA[r])
        uri <- ctx$fetch_dataurl_single(cam, ts)
        if (!is.null(uri)) {
          session$sendCustomMessage("set_clip_overlay_frame", list(
            img_id = ns(paste0("clipOverlayImg_", cam)),
            url    = uri
          ))
        }
      }
    })
    invisible()
  }

  # loop later para tocar o clip no modal
  overlay_play_step <- function() {  # >>> NOVO
    withReactiveDomain(session, {
      isolate({
        if (!isTRUE(clipOverlayPlayer$playing)) return(invisible())
        if (is.null(clipOverlayPlayer$seq) || !nrow(clipOverlayPlayer$seq)) {
          clipOverlayPlayer$playing <- FALSE
          return(invisible())
        }

        n   <- nrow(clipOverlayPlayer$seq)
        dir <- clipOverlayPlayer$dir
        if ((dir > 0L && clipOverlayPlayer$i >= n) ||
            (dir < 0L && clipOverlayPlayer$i <= 1L)) {
          clipOverlayPlayer$playing <- FALSE
          return(invisible())
        }

        clipOverlayPlayer$i <- clipOverlayPlayer$i + dir
        render_clip_overlay_frame()

        delay <- suppressWarnings(as.numeric(input$clipOverlay_step_ms) / 1000)
        if (!is.finite(delay) || delay <= 0) delay <- 0.001
        later::later(function(){ overlay_play_step() }, delay)
      })
    })
  }

  # ---------- Modal/UI ----------
  id       <- ns('dialogTrain')
  cssStyle <- list()
  cssStyle[[paste0(' #parent',id,' .modal-dialog')]]  <- 'width: 95% !important; height: 90% !important;'
  cssStyle[[paste0(' #parent',id,' .modal-content')]] <- 'width: 100% !important; height: 100% !important;'
  cssStyle[[paste0(' #parent',id,' .modal-body')]]    <- 'width: 100% !important; height: calc(100% - 57px - 65px) !important; overflow-y: auto; overflow-x: hidden;'

  showModal(
    session = session,
    div(
      id = paste0('parent', id),
      style = "height: 80%; overflow: hidden;",
      inlineCSS(cssStyle),
      dialogModal(
        title = "Novo Pacote",
        size  = 'm',
        uiMain(ns, setores),
        footer = uiOutput(ns('uiFooter'))
      )
    )
  )
  output$uiFooter <- renderUI(tagList(
    actionButton(ns("btSair"),  label = "Sair",    icon = icon("arrow-left")),
    actionButton(ns("btSalvar"),class = "btn-primary", label = "Salvar", icon = icon("save"))
  ))

  # ---------- Reagir troca de setor -> objetos ----------
  obs$add(observeEvent(input$comboSetor, {
    setor <- setores |> dplyr::filter(NAME_SETOR == input$comboSetor)
    if (!nrow(setor)) return()
    objs_setor <- objetos |> dplyr::filter(CD_ID_SETOR == setor$CD_ID_SETOR)
    updateSelectizeInput(session, "comboObjeto", choices = objs_setor$NAME_OBJETO)

    output$uiCamerasFrames <- NULL
    playing(FALSE); loop_on <<- FALSE
    session$userData$lru_cache$clear()
    if (isTRUE(rv$clip_active)) {
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }
  }, ignoreNULL = TRUE))

  # ---------- Botão Buscar ----------
  obs$add(observeEvent(input$btBuscar, {
    actionWebUser({
      objeto <<- objetos |> dplyr::filter(NAME_OBJETO == isolate(input$comboObjeto))
      if (!nrow(objeto)) {
        showNotification("Selecione um objeto válido.", type = "warning")
        removeProgressLoader()
        return(invisible())
      }

      time_begin <- isolate(input$datetimeBegin)
      time_end   <- isolate(input$datetimeEnd)

      if(is.null(time_begin)){
        showNotification("'Data e Hora De' está com campo vazio!", type = "warning")
        removeProgressLoader()
        return(invisible())
      }else if(is.null(time_end)){
        showNotification("'Data e Hora Até' está com campo vazio!", type = "warning")
        removeProgressLoader()
        return(invisible())
      }

      time_begin  <- as.POSIXct(time_begin,tz = "UTC")
      time_end    <- as.POSIXct(time_end,tz = "UTC")
      componentes <- objeto$CONFIG[[1]]$COMPONENTES[[1]]
      cameras_ids <- unique(purrr::map_int(componentes$CAMERA, "CD_ID_CAMERA"))
    
      frames_idx <- fetch_frames(
        dbp$get_pool(),
        time_begin    = time_begin,
        time_end      = time_end,
        camera_id_vec = cameras_ids
      )
      if (!nrow(frames_idx)) {
        removeProgressLoader()
        showNotification("Nenhum frame no intervalo/câmeras selecionados.", type = "warning")
        return(invisible())
      }

      res_cam <- uiCamerasComponentes(ns, input, output, componentes)
      rv$id_by_cam <- res_cam$id_by_cam
      output$uiCamerasFrames <- renderUI({
        output$titleClock <- renderText({
          clockupdate()
          get_current_frame_ts(rv, tz = Sys.timezone(), fmt = "%d/%m/%y %H:%M:%S")
        })
        tagList(
          br(),
          uiComponenteVideo(ns),
          uiClipsPanel(ns),
          br(),
          res_cam$ui
        )
      })

      session$userData$lru_cache$clear()
      if (isTRUE(rv$clip_active)) {
        rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
        try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
      }
      rv$seq <- frames_idx; rv$i <- 1L; rv$w <- 512L; rv$h <- 512L
      playing(FALSE); loop_on <<- FALSE

      # Descobre w/h do primeiro frame
      first <- frames_idx[1, ]
      res1  <- db_fetch_frame_raw(dbp$get_pool(), first$CD_ID_CAMERA, first$DT_HR_LOCAL)
      if (nrow(res1) && !is.null(res1$DATA_FRAME[[1]])) {
        wh <- img_dims(res1$DATA_FRAME[[1]])
        rv$w <- if (is.finite(wh[1])) as.integer(wh[1]) else 512L
        rv$h <- if (is.finite(wh[2])) as.integer(wh[2]) else 350L
      }

      id_map <- rv$id_by_cam; seq_df <- rv$seq; i0 <- rv$i; w0 <- rv$w; h0 <- rv$h
      session$onFlushed(function(){
        session$sendCustomMessage("reset_overlays", unname(id_map))
        st <- ctx$render_current(seq_df = seq_df, i = i0, w = w0, h = h0, id_map = id_map, fit_bounds = TRUE)
        isolate({ if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h } })
        ctx$prefetch_ahead_batch(seq_df = seq_df, i = i0, N = PREFETCH_AHEAD)
        removeProgressLoader(callback = function(){step_frame(+1L)})
      }, once = TRUE)

    }, auto.remove = FALSE)
  }, ignoreInit = TRUE))

  # ---------- Navegação frame a frame (player principal) ----------
  step_frame <- function(delta) {
    req(rv$seq, nrow(rv$seq) > 0)
    new_i <- max(1L, min(nrow(rv$seq), rv$i + delta))
    if (new_i == rv$i) return(invisible())
    rv$i <- new_i
    st <- ctx$render_current(rv$seq, rv$i, rv$w, rv$h, rv$id_by_cam, fit_bounds = FALSE)
    if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }
    ctx$prefetch_ahead_batch(rv$seq, rv$i, PREFETCH_AHEAD)
    if (clip_limit_exceeded(rv, suppressWarnings(as.numeric(input$clip_max_min)))) {
      playing(FALSE); loop_on <<- FALSE
      ts_start <- rv$clip_t0; i0 <- rv$clip_i0
      ts_end   <- rv_get_current_ts(rv); i1 <- rv$i
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
      clips_add(ts_start, ts_end, i0, i1)
      #clip_summary_overlay(ns, session, input, objeto, ts_start, ts_end,n_frames = abs(i1 - i0) + 1L)
    }
  }
  obs$add(observeEvent(input$prevFrame, { step_frame(-1L) }, ignoreInit = TRUE))
  obs$add(observeEvent(input$nextFrame, { step_frame(+1L) }, ignoreInit = TRUE))

  # ---------- Loop Play principal ----------
  play_step <- function() {
    withReactiveDomain(session, {
      isolate({
        if (!isTRUE(playing())) { loop_on <<- FALSE; return(invisible()) }
        if (is.null(rv$seq) || nrow(rv$seq) == 0L) { playing(FALSE); loop_on <<- FALSE; return(invisible()) }

        n   <- nrow(rv$seq)
        dir <- play_dir()
        if ((dir > 0L && rv$i >= n) || (dir < 0L && rv$i <= 1L)) {
          playing(FALSE); loop_on <<- FALSE; return(invisible())
        }

        rv$i <- rv$i + dir
        st <- ctx$render_current(rv$seq, rv$i, rv$w, rv$h, rv$id_by_cam, fit_bounds = FALSE)
        if (isTRUE(st$ok)) { rv$w <- st$w; rv$h <- st$h }
        ctx$prefetch_ahead_batch(rv$seq, rv$i, PREFETCH_AHEAD)

        clockupdate(Sys.time())
        if (clip_limit_exceeded(rv, suppressWarnings(as.numeric(input$clip_max_min)))) {
          playing(FALSE); loop_on <<- FALSE
          ts_start <- rv$clip_t0; i0 <- rv$clip_i0
          ts_end   <- rv_get_current_ts(rv); i1 <- rv$i
          rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
          updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
          clips_add(ts_start, ts_end, i0, i1)
          #clip_summary_overlay(ns, session, input, objeto, ts_start, ts_end, n_frames = abs(i1 - i0) + 1L)
          return(invisible())
        }

        delay <- suppressWarnings(as.numeric(input$step_ms) / 1000)
        if (!is.finite(delay) || delay <= 0) delay <- 0.001
        later::later(function(){ play_step();}, delay)
      })
    })
  }

  # ---------- Botões play/pause/reverse PRINCIPAL ----------
  obs$add(observeEvent(input$play, {
    play_dir(+1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  obs$add(observeEvent(input$backPlay, {
    play_dir(-1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
  }, ignoreInit = TRUE))
  obs$add(observeEvent(input$pause, { playing(FALSE) }, ignoreInit = TRUE))

  # ---------- Clip toggle PRINCIPAL ----------
  obs$add(observeEvent(input$clipToggle, {
    if (!isTRUE(rv$clip_active)) {
      if (!clip_start(rv)) {
        showNotification("Nenhum frame carregado para iniciar o clip.", type = "warning")
        return(invisible())
      }
      updateActionButton(session, "clipToggle", label = "Stop Clip", icon = icon("square"))
      showNotification("Clip iniciado.", type = "message")
      play_dir(+1L); playing(TRUE); if (!loop_on) { loop_on <<- TRUE; play_step() }
    } else {
      playing(FALSE); loop_on <<- FALSE
      ts_start <- rv$clip_t0; i0 <- rv$clip_i0
      ts_end   <- rv_get_current_ts(rv); i1 <- rv$i
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      updateActionButton(session, "clipToggle", label = "Start Clip", icon = icon("scissors"))
      clips_add(ts_start, ts_end, i0, i1)
    }
  }, ignoreInit = TRUE))

  # ---------- Render da tabela de clips ----------
  output$clipsTable <- DT::renderDT({
    
    df <- clips()
    if (!nrow(df)) {
      return(DT::datatable(
        data.frame(
          Linha  = integer(0),
          Título = character(0),
          Tipo   = character(0),
          De = character(0),
          Ate = character(0),
          Visualizar = character(0),
          Excluir = character(0)
        ),
        escape = FALSE, selection = "none",
        options = list(
          dom = "t", paging = FALSE, ordering = FALSE,
          language = list(
            emptyTable   = "Nenhum clip disponível na tabela",
            zeroRecords  = "Nenhum registro encontrado",
            infoEmpty    = "",
            info         = ""
          )
        )
      ))
    }
    
    rownum      <- seq_len(nrow(df))
    tiposPacote <- tiposPacotes$NAME_TIPO_PACOTE
    
    titulo <- vapply(df$id, function(id){
      as.character(
        textInput(
          inputId = ns(paste0("clip_title_", id)),
          label = NULL, value = df$title[df$id == id],
          width = "100%", placeholder = "Nome do clip"
        ) |> tagAppendAttributes(style = ';margin-top: 10px;')
      )
    }, character(1))
    
    # ===== NOVO: Combo "Tipo" =====
    tipo_cb <- vapply(df$id, function(id){
      idx <- which(df$id == id)
      
      # tenta pegar valor atual do df; senão usa o primeiro do vetor
      cur <- if ("tipo" %in% names(df)) as.character(df$tipo[idx]) else NA_character_
      if (is.na(cur) || !nzchar(cur) || !(cur %in% tiposPacote)) cur <- tiposPacote[[1]]
      
      as.character(
        selectInput(
          inputId = ns(paste0("clip_tipo_", id)),
          label   = NULL,
          choices = tiposPacote,
          selected = cur,
          width   = "100%"
        ) |> tagAppendAttributes(style = ';margin-top: 10px;')
      )
    }, character(1))
    
    t0_txt <- vapply(df$id, function(id){
      idx <- which(df$id == id)
      val <- fmt_pt(df$t0[idx], Sys.timezone())
      as.character(
        textInput(
          inputId = ns(paste0("clip_t0_", id)),
          label = NULL, value = val, width = "100%",
          placeholder = "dd/mm/aa HH:MM:SS"
        ) |> tagAppendAttributes(style = ';margin-top: 10px;')
      )
    }, character(1))
    
    t1_txt <- vapply(df$id, function(id){
      idx <- which(df$id == id)
      val <- fmt_pt(df$t1[idx], Sys.timezone())
      as.character(
        textInput(
          inputId = ns(paste0("clip_t1_", id)),
          label = NULL, value = val, width = "100%",
          placeholder = "dd/mm/aa HH:MM:SS"
        ) |> tagAppendAttributes(style = ';margin-top: 10px;')
      )
    }, character(1))
    
    btn_view <- vapply(df$id, function(id){
      sprintf(
        "<button class='btn btn-sm btn-outline-primary' onclick=\"Shiny.setInputValue('%s', {action:'view', id:%d, nonce:Math.random()}, {priority:'event'})\">
         <i class='fa fa-eye'></i>
       </button>", ns("clip_action"), id
      )
    }, character(1))
    
    btn_del <- vapply(df$id, function(id){
      sprintf(
        "<button class='btn btn-sm btn-danger' onclick=\"Shiny.setInputValue('%s', {action:'del', id:%d, nonce:Math.random()}, {priority:'event'})\">
         <i class='fa fa-trash'></i>
       </button>", ns("clip_action"), id
      )
    }, character(1))
    
    out <- data.frame(
      Linha          = rownum,
      Título         = titulo,
      Tipo           = tipo_cb,     # << NOVA COLUNA
      De             = t0_txt,
      Ate            = t1_txt,
      Visualizar     = btn_view,
      Excluir        = btn_del,
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      out, escape = FALSE, selection = "none",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        preDrawCallback = DT::JS(
          "function(settings){ Shiny.unbindAll(this.api().table().node()); }"
        ),
        drawCallback = DT::JS(
          "function(settings){ Shiny.bindAll(this.api().table().node()); }"
        ),
        columnDefs = list(
          list(visible = FALSE, targets = 0),
          list(className = 'dt-center', targets = "_all"),
          # ajuste de widths: agora tem 7 colunas visíveis (1..7)
          list(width = '75px', targets = c(1, 2, 6, 7))
        )
      ),
      callback = DT::JS(
        "table.on('click keydown', 'input, textarea, select', function(e){ e.stopPropagation(); });"
      )
    ) |> DT::formatStyle(names(out), cursor = 'pointer')
  })


  # ---------- Visualizar / Excluir clip ----------
  obs$add(observeEvent(input$clip_action, {
    req(input$clip_action$id, input$clip_action$action)
    df <- isolate(clips());
    if (!nrow(df)) return(invisible())
    id_sel <- as.integer(input$clip_action$id)
    act    <- as.character(input$clip_action$action)
    row    <- df[df$id == id_sel, , drop = FALSE]; if (!nrow(row)) return(invisible())

    if (act == "view") {
      n_frames <- abs(row$i1 - row$i0) + 1L

      # monta sequência do clip (subsequência de rv$seq)
      if (!is.null(rv$seq) && nrow(rv$seq) > 0) {
        i0 <- max(1L, min(nrow(rv$seq), row$i0[1]))
        i1 <- max(1L, min(nrow(rv$seq), row$i1[1]))
        rng <- sort(c(i0, i1))
        sub_seq <- rv$seq[seq.int(rng[1], rng[2]), , drop = FALSE]
      } else {
        sub_seq <- NULL
      }

      # abre overlay (isso injeta a UI com img e botões)
      clip_summary_overlay(
        ns, session, input, objeto,id_clip = id_sel,
        ts_start = row$t0, ts_end = row$t1,
        n_frames = n_frames
      )

      # inicializa player do overlay nesse clip
      clipOverlayPlayer$seq     <- sub_seq
      clipOverlayPlayer$i       <- 1L
      clipOverlayPlayer$dir     <- +1L
      clipOverlayPlayer$playing <- FALSE
      render_clip_overlay_frame()

    } else if (act == "del") {
      clips_remove(id_sel)
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
      clipOverlayPlayer$playing <- FALSE
      clipOverlayPlayer$seq     <- NULL
    }
  }, ignoreInit = TRUE))

  # ---------- Controles do player dentro do modal ----------
  # >>> NOVO
  obs$add(observeEvent(input$clipOverlay_action, {
    req(input$clipOverlay_action$action)
    act <- as.character(input$clipOverlay_action$action)

    if (is.null(clipOverlayPlayer$seq) || !nrow(clipOverlayPlayer$seq)) return(invisible())

    if (act == "reverse") {
      clipOverlayPlayer$dir     <- -1L
      clipOverlayPlayer$playing <- TRUE
      overlay_play_step()

    } else if (act == "play") {
      clipOverlayPlayer$dir     <- +1L
      clipOverlayPlayer$playing <- TRUE
      overlay_play_step()

    } else if (act == "pause") {
      clipOverlayPlayer$playing <- FALSE

    } else if (act == "prev") {
      clipOverlayPlayer$playing <- FALSE
      clipOverlayPlayer$i <- max(1L, clipOverlayPlayer$i - 1L)
      render_clip_overlay_frame()

    } else if (act == "next") {
      clipOverlayPlayer$playing <- FALSE
      clipOverlayPlayer$i <- min(nrow(clipOverlayPlayer$seq), clipOverlayPlayer$i + 1L)
      render_clip_overlay_frame()
    }
  }, ignoreInit = TRUE))

  # ---------- Rodapé ----------
  obs$add(observeEvent(input$btSair, {
    obs$destroy()
    output$uiCamerasFrames <- NULL
    playing(FALSE); loop_on <<- FALSE
    session$userData$lru_cache$clear()
    if (isTRUE(rv$clip_active)) {
      rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
      try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
    }
    # garante parar loop do overlay também
    clipOverlayPlayer$playing <- FALSE
    clipOverlayPlayer$seq     <- NULL

    removeModal(session); callback()
  }, ignoreInit = TRUE, ignoreNULL = TRUE))

  obs$add(observeEvent(input$btSalvar, {

    df <- isolate(clips())

    if (!nrow(df)){
      showNotification("Não foi possivel salvar o pacote de treino, nenhum clip foi encontrado!", type = "error")
      return(invisible())
    }

    if(!db$tryTransaction(function(conn){

      info <- build_objeto_descricao(input,df,objeto,tiposPacotes)

      if(!info$status){
        showNotification(info$message, type = "error")
        return(invisible(NULL))
      }

      for(i in seq_along(info$datas)){
        descricao  <- info$datas[[i]]
        objPacote  <- list()
        objPacote$CD_ID_OBJETO       <- objeto$CD_ID_OBJETO
        objPacote$TITULO_IA          <- descricao$titulo
        objPacote$INPUT_IA           <- descricao$input
        objPacote$CD_ID_TIPO_PACOTE  <- descricao$tipoPacote
        objPacote$OUTPUT_IA          <- descricao$output
        objPacote$DT_HR_LOCAL_BEGIN  <- descricao$begin
        objPacote$DT_HR_LOCAL_END    <- descricao$end
        db$insertTable(conn,"PACOTE_IA",objPacote)
      }

      dialogConfirm(
        session = session,
        id    = ns('dialogConfirm'),
        title = 'Pacote criado com sucesso!',
        text  = 'Deseja criar novamente um novo Pacote?')

        observeEvent(input$dialogConfirm,{

          status <- input$dialogConfirm

          playing(FALSE); loop_on <<- FALSE
          session$userData$lru_cache$clear()
          if (isTRUE(rv$clip_active)) {
            rv$clip_active <- FALSE; rv$clip_t0 <- NULL; rv$clip_i0 <- NA_integer_
            try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
          }

          clips(data.frame(
            id = integer(0),
            title = character(0),
            t0 = as.POSIXct(character(0), tz = "UTC"),
            t1 = as.POSIXct(character(0), tz = "UTC"),
            i0 = integer(0),
            i1 = integer(0),
            stringsAsFactors = FALSE
          ))
          next_clip_id(1L)

          # para preview do modal tbm
          clipOverlayPlayer$playing <- FALSE
          clipOverlayPlayer$seq     <- NULL

          if(!status){
            obs$destroy()
            output$uiCamerasFrames <- NULL
            removeModal(session); callback()
          }
        },ignoreInit = TRUE,once = TRUE)
      })){
        showNotification("Não foi possivel salvar o pacote de treino, durante o processo houve falha!", type = "error")
      }

  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$clipCloseVideo, {
    # botão OK dentro do overlay: fecha overlay e para loop
    clipOverlayPlayer$playing <- FALSE
    clipOverlayPlayer$seq     <- NULL
    try(removeUI(selector = paste0("#", ns("clip_summary_overlay")), immediate = TRUE), silent = TRUE)
  }, ignoreInit = TRUE))

}

# ==================================================
# Coleta dos valores dos atributos no overlay de clip
# ==================================================
collect_clip_attributes <- function(input,objeto,id_clip,t0,t1) {
  stopifnot(!is.null(objeto), nrow(objeto) >= 1)
  componentes <- objeto$CONFIG[[1]]$COMPONENTES[[1]]
  if (is.null(componentes) || !nrow(componentes)) {
    return(data.frame(
      CD_ID_COMPONENTE = integer(0),
      NAME_COMPONENTE  = character(0),
      NAME_ATRIBUTO    = character(0),
      NAME_DATA        = character(0),
      VALUE            = character(0),
      stringsAsFactors = FALSE
    ))
  }

  normalize_id_piece <- function(x) {
    x <- as.character(x)
    x <- gsub("\\s+", "_", x)
    x <- gsub("[^A-Za-z0-9_\\-]", "_", x)
    x
  }

  codigo_date <- paste0(id_clip,"_")

  make_ids <- function(comp_name, attr_name, k) {
    raw_id   <- paste0(codigo_date,comp_name, "_", attr_name, "_", k)
    norm_id  <- paste0(codigo_date,normalize_id_piece(comp_name), "_",normalize_id_piece(attr_name), "_", k)
    list(
      raw  = raw_id,
      norm = norm_id
    )
  }

  read_input <- function(id_candidates) {
    cand <- unlist(id_candidates, use.names = FALSE)
    for (cid in cand) {
      if (cid %in% names(input)) {
        return(input[[cid]])
      }
    }
    NA
  }

  rows <- list()

  for (i in seq_len(nrow(componentes))) {
    comp          <- componentes[i, ]
    estrutura     <- comp$ESTRUTURA[[1]]
    atributos     <- estrutura$CONFIGS[[1]]$ATRIBUTOS[[1]]
    if (is.null(atributos) || !nrow(atributos)) next

    comp_id   <- as.integer(comp$CD_ID_COMPONENTE[[1]])
    comp_name <- as.character(comp$NAME_COMPONENTE[[1]])

    for (k in seq_len(nrow(atributos))) {
      atributo   <- atributos[k, ]
      attr_id    <- atributo$CD_ID_ATRIBUTO[[1]]
      attr_name  <- as.character(atributo$NAME_ATRIBUTO[[1]])
      attr_type  <- as.character(atributo$NAME_DATA[[1]])      # "QUALITATIVE" | outro

      ids   <- make_ids(comp_id,attr_id,k)
      value <- read_input(ids)

      if (isTRUE(is.numeric(value))) {
        value <- as.character(value)
      } else if (isTRUE(is.logical(value))) {
        value <- ifelse(is.na(value), NA_character_, ifelse(value, "TRUE", "FALSE"))
      } else {
        value <- if (is.null(value)) NA_character_ else as.character(value)
      }

      rows[[length(rows) + 1L]] <- data.frame(
        CD_ID_COMPONENTE = comp_id,
        CD_ID_ATRIBUTO   = attr_id,
        NAME_COMPONENTE  = comp_name,
        NAME_ATRIBUTO    = attr_name,
        NAME_DATA        = attr_type,
        VALUE            = value,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) {
    return(data.frame(
      CD_ID_COMPONENTE = integer(0),
      NAME_COMPONENTE  = character(0),
      NAME_ATRIBUTO    = character(0),
      NAME_DATA        = character(0),
      VALUE            = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

build_objeto_descricao <- function(input,df,objeto,tiposPacotes){
  
  listas  <- list()
  status  <- TRUE
  message <- ""
  for(i in 1:nrow(df)){
    df_tmp <- df[i,]
    id     <- df_tmp$id
    df_tmp$title <- input[[paste0("clip_title_",id)]]
    t0           <- input[[paste0("clip_t0_",id)]]
    t1           <- input[[paste0("clip_t1_",id)]]
    tipoPacote   <- tiposPacotes |> filter(NAME_TIPO_PACOTE == input[[paste0("clip_tipo_",id)]])

    if(!validaDateFormat(t0) || !validaDateFormat(t1)){
      status <- FALSE
      message <- "❌ formato inválido na tabela clips (use dd/mm/aa HH:MM:SS)!"
      break
    }

    df_tmp <- clips_update_date_time_t0(df_tmp,id,t0)
    df_tmp <- clips_update_date_time_t1(df_tmp,id,t1)
    
    atributos <- collect_clip_attributes(input,objeto,id,df_tmp$t0,df_tmp$t1)
    atributos <- atributos |> 
      group_by(NAME_COMPONENTE) |> 
      nest() |> 
      ungroup() |> 
      mutate(output = purrr::map2_chr(NAME_COMPONENTE,data,function(nome,dados){
        x     <- dados |> select(CD_ID_COMPONENTE,NAME_ATRIBUTO,VALUE,NAME_DATA)
        id    <- x$CD_ID_COMPONENTE
        texto <- ""
        for(k in 1:nrow(x)){
          if(k > 1){
            texto <- paste0(texto,",")
          }
          att <- x[k,]
          if(att$NAME_DATA == "QUALITATIVE"){
            texto <- paste0(texto,paste0("\"",att$NAME_ATRIBUTO,"\": \"",att$VALUE,"\""))
          }else{
            texto <- paste0(texto,paste0("\"",att$NAME_ATRIBUTO,"\": ",att$VALUE))
          }
        }
        paste0("{\"",nome,"\": {",texto,"}, \"ID\": ",id,"}")
      }))
    
    output_ <- paste0("[",paste0(atributos$output,collapse = ","),"]")
    input_  <- paste0("OBJETO: ",objeto$NAME_OBJETO," TIPO: ",objeto$NAME_OBJETO_TIPO)
    listas[[i]] <- list(
      titulo = df_tmp$title,
      input  = input_,
      output = output_,
      tipo   = tipoPacote$CD_ID_TIPO_PACOTE,
      begin  = df_tmp$t0,
      end    = df_tmp$t1
    )
  }
  return(list(datas = listas,status = status,message = message))
}
