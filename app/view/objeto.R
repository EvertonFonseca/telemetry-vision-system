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
  shinyWidgets[multiInput,updateMultiInput,prettyToggle],
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

#' @export
 uiNewObjeto <- function(ns,input,output,session,callback){
  
  .register_auto_dispose(session)
  
  e <- .get_private(session)
  
  
  obs       <- newObserve()
  obs2      <- newObserve()
  obs3      <- newObserve()
  setores   <- selectAllSetors(dbp$get_pool())
  cameras   <- selectAllCameras(dbp$get_pool())
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
      
      output$slider1 <- renderUI({
        
        uiMain(ns,setores,cameras,tiposObjeto)
      })
      
      output$slider2 <- renderUI({
        
        req(sliderPosition() == 2L)
        camerasTargets <- isolate(input$multiCameras) 
        tipoObjeto     <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
       
        obs2$clear()
        
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
          
          if (is.null(frame_data$componente)) {
            frame_data$componente[[1]] <<- tibble::tibble(
              cd_id_componente    = feat$properties$`_leaflet_id`,
              name_componente     = "",
              cd_id_camera        = cameraTarget$cd_id_camera,
              poligno_componente  = list(poly),
              estrutura           = list(NULL)
            )
          } else {
            frame_data$componente[[1]] <<- bind_rows(
              frame_data$componente[[1]],
              tibble::tibble(
                cd_id_componente    = feat$properties$`_leaflet_id`,
                name_componente     = "",
                cd_id_camera        = cameraTarget$cd_id_camera,
                poligno_componente  = list(poly),
                estrutura           = list(NULL)
              )
            )
          }
        
          #objetos dinamicos apenas 1 compomentes
          if(tipoObjeto$cd_id_objeto_tipo == 2L && nrow(frame_data$componente[[1]]) == 1L){
            camera          <- cameras |> filter(name_camera == input$comboCameras)
            componentes     <- frame_data$componente[[1]]   
            updateObjDynamic(TRUE)
            output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,is_dynamic = TRUE)})
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
          
          frame_data$componente[[1]] <<- df
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
          
          frame_data$componente[[1]] <<- df
          
          if(nrow(df) == 0){
            if(isolate(updateObjDynamic())){
              camera          <- cameras |> filter(name_camera == input$comboCameras)
              componentes     <- frame_data$componente[[1]]   
              updateObjDynamic(FALSE)
              output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,is_dynamic = FALSE)})
            }
          }

        }, ignoreInit = TRUE))
        
        # -------------------------------------------------------------------
        # CLICKS EM SHAPES (somente se NÃO estiver editando nem deletando)
        # -------------------------------------------------------------------
        obs2$add(observeEvent(input$mapFrame_shape_draw_click, {
          req(sliderPosition() == 2L, !isTRUE(deleting()), !isTRUE(editing()))
          ev <- input$mapFrame_shape_draw_click
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
          camera          <- cameras |> filter(name_camera == input$comboCameras)
          componentes     <- frame_data$componente[[1]]   
          is_dynamic      <- updateObjDynamic()
          output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,is_dynamic = is_dynamic)})
        },ignoreNULL = TRUE))

        estruturas_dinamicos <- NULL
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
        }
        
        tagList(
          selectizeInput(ns('comboCameras'),label = 'Câmera',choices = camerasTargets,options  = list(
            dropdownParent = 'body',
            openOnFocus = TRUE,
            closeAfterSelect = TRUE
          )),
          leafletOutput(ns("mapFrame"), height = "512px", width = "100%"),
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
            #update mapa
            #proxy_update_componentes(map_id = ns("mapFrame"),ns = ns, camera = camera,componentes = componentes)  
            #proxy_update_componentes(session,ns,ns("mapFrame"),camera, componentes)           
            output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes)})
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
      }, ignoreInit = TRUE))
      
      ## Salvar Objeto
      obs$add(observeEvent(input$btSalvar,{
        
        current <- isolate(sliderPosition())
        
        if(current == 1L){
          nomeObjeto     <- isolate(toupper(input$textNameObjeto))
          camerasTargets <- isolate(input$multiCameras) 
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
            tipoObjeto     <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
            setor          <- setores |> filter(name_setor == isolate(input$comboSetor))
            
            # try insert or roolback
            obj               <- list()
            obj$name_objeto   <- nomeObjeto
            obj$fg_ativo      <- as.integer(ativoObjeto)
            obj$cd_id_setor   <- setor$cd_id_setor
            obj$cd_id_objeto_tipo <- tipoObjeto$cd_id_objeto_tipo
            obj$timeline_context_sec <- isolate(input$sliderTimeContexto)
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
  cameras         <- selectAllCameras(dbp$get_pool())
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
          
          colunaNames <- c('LINHA','OBJETO','TIPO','VISUALIZAR / EDITAR','REMOVER')
          
          DT$datatable({
            
            dataset |> 
            mutate_if(is.POSIXct,function(x){ format(x,'%d/%m/%Y %H:%M:%S')})  |> 
            mutate_if(is.Date,function(x){ format(x,'%d/%m/%Y')}) |> 
            mutate_if(is.character,toupper) |> 
            mutate(
              !!colunaNames[1] := 1:nrow(dataset),
              !!colunaNames[2] :=  dataset$name_objeto,
              !!colunaNames[3] :=  dataset$name_objeto_tipo,
              !!colunaNames[4] :=  sapply(dataset$cd_id_objeto, function (x) {
                
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
              !!colunaNames[5] :=  sapply(dataset$cd_id_objeto,function (x) {
                
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
          options = list(
            language = list(url = 'js/table.json'),
            dom = 't',
            bSort=FALSE,
            columnDefs = list(list(visible=FALSE, targets=c(0)),list(className = 'dt-center', targets = "_all"),list(width = '75px',targets = c(1,3,4,5)),list(width = 'autos',targets = c(2,4))),
            deferRender = TRUE,
            scroller = FALSE,
            fixedHeader = TRUE,
            scrollX = TRUE,
            scrollY = '280px'
          ),
          escape = F,
          selection = 'none',
        ) |> DT$formatStyle(colunaNames, cursor = 'pointer')
        
      })
      
      div(
        style = 'border-style: solid; border-color: white; border-width: 1px; overflow-x: auto;',
        DT$dataTableOutput(outputId = ns('tableDinamicaObjeto'))
      )
      
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
        
        row_new <- tibble::tibble(
          cd_id_componente    = feat$properties$`_leaflet_id`,
          name_componente     = "",
          cd_id_camera        = cameraTarget$cd_id_camera,
          poligno_componente  = list(poly),
          estrutura           = list(NULL)
        )
        
        if (is.null(frame_data$componente)) {
          frame_data$componente[[1]] <<- row_new
        } else {
          frame_data$componente[[1]] <<- bind_rows(frame_data$componente[[1]], row_new)  # CHANGE: bind_rows()
        }
        #objetos dinamicos apenas 1 compomentes
        if(tipoObjeto$cd_id_objeto_tipo == 2L && nrow(frame_data$componente[[1]]) == 1L){
          camera          <- cameras |> filter(name_camera == input$comboCameras)
          componentes     <- frame_data$componente[[1]]   
          updateObjDynamic(TRUE)
          output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,is_dynamic = TRUE)})
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
        
        frame_data$componente[[1]] <<- df
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
        frame_data$componente[[1]] <<- df
        
        if(nrow(df) == 0){
          if(isolate(updateObjDynamic())){
            camera          <- cameras |> filter(name_camera == input$comboCameras)
            componentes     <- frame_data$componente[[1]]   
            updateObjDynamic(FALSE)
            output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,is_dynamic = FALSE)})
          }
        }
      }, ignoreInit = TRUE))
      
      # --- clique nos shapes (mantidos exatamente seus inputs) ---
      obs2$add(observeEvent(input$mapFrame_shape_draw_click, {
        req(!deleting())  # mantido
        ev <- input$mapFrame_shape_draw_click

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
        camera          <- cameras |> filter(name_camera == input$comboCameras)
        componentes     <- frame_data$componente[[1]]   
        is_dynamic      <- updateObjDynamic()
        output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes,is_dynamic = is_dynamic)})
      },ignoreNULL = TRUE))
      
      estruturas_dinamicos <- NULL
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
        }
        
        tagList(
          selectizeInput(ns('comboCameras'),label = 'Câmera',choices = camerasTargets,options  = list(
            dropdownParent = 'body',
            openOnFocus = TRUE,
            closeAfterSelect = TRUE
          )),
          leafletOutput(ns("mapFrame"), height = "512px", width = "100%"),
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
            #update mapa
            #proxy_update_componentes(map_id = ns("mapFrame"),ns = ns, camera = camera,componentes = componentes)  
            #proxy_update_componentes(session,ns,ns("mapFrame"),camera, componentes)           
            output$mapFrame <- renderLeaflet({uiMapa(ns,camera,cameras,frame_data,componentes = componentes)})
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
      },ignoreInit = TRUE))
      
      obs$add(observeEvent(input$btSalvar,{
        
        req(objeto())
        
        objetoSelect <- isolate(objeto())
        current      <- isolate(sliderPosition())
        
        if(current == 2L){
          
          nomeObjeto     <- isolate(toupper(input$textNameObjeto))
          camerasTargets <- isolate(input$multiCameras) 
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
          
          if(!db$tryTransaction(function(conn){

            #check if it has already data of Câmera
            nomeObjeto     <- isolate(toupper(input$textNameObjeto))
            ativoObjeto    <- isolate(input$checkboxAtivoObjeto)  
            tipoObjeto     <- tiposObjeto |> filter(name_objeto_tipo == isolate(input$comboTipoObjeto))
            setor          <- setores |> filter(name_setor == isolate(input$comboSetor))
       
            obj                      <- list()
            obj$name_objeto          <- nomeObjeto
            obj$fg_ativo             <- as.integer(ativoObjeto)
            obj$cd_id_setor          <- setor$cd_id_setor
            obj$timeline_context_sec <- isolate(input$sliderTimeContexto)
        
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
        }
      },ignoreInit = T))
}



uiMain <- function(ns,
                   setores,
                   cameras,
                   tiposObjeto,
                   valueComboSetor = NULL,
                   valueAtivo      = TRUE,
                   valueTextName   = NULL,
                   valueTipoObjeto = NULL,
                   valueMultiCamera = NULL,
                   valueTempoContexto = 5
                  ){

     changetextPlaceHolder()
      cameras
      div(
          inlineCSS(paste0("#",ns("textNameObjeto")," {text-transform: uppercase;}")),
          fluidRow(
           column(6,selectizeInput(ns('comboSetor'),label = 'Setor',choices = setores$name_setor,selected = valueComboSetor)),
           column(6,sliderInput(ns('sliderTimeContexto'),label = 'Tempo Contexto Segundo',min = 1,step = 1,max = 10,round = TRUE,value = valueTempoContexto))
          ),
          splitLayout(
            cellWidths = c("10%", "60%","30%"),
            tagList(          
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
          textInput(paste0(ns('textNameObjeto')),label = 'Nome',placeholder = 'Digite o nome para o Objeto',width = "100%",value = valueTextName),
          selectizeInput(ns('comboTipoObjeto'),label = 'Tipo',choices = tiposObjeto$name_objeto_tipo,width = "100px",selected = valueTipoObjeto),
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
            choiceNames  = apply(cameras,1, function(x)  tagList(x["name_camera"])),
            choiceValues = apply(cameras,1, function(x)  x["name_camera"])
          ) |> tagAppendAttributes(style = ';height: auto; width: 100%;')
        )
}


uiMapa <-function(ns,camera,cameras,frame_data,componentes = NULL,is_dynamic = FALSE){

  data     <- frame_data |> filter(id == camera$cd_id_camera)
  data_uri <- base64enc$dataURI(file = data$img_path, mime = "image/png")
  
  mapa <- leaflet(options = leafletOptions(
          crs = leafletCRS(crsClass = "L.CRS.Simple"),
          zoomSnap  = 0,        # permite zoom fracionário
          zoomDelta = 0.25      # passo de zoom ao usar scroll
        )) |>
        addDrawToolbar(
          targetGroup = "draw",
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

    for(i in seq_len(nrow(componentes))){

      comp    <- componentes[i,]
      
      if(comp$cd_id_camera != camera$cd_id_camera) next

      poligno   <- comp$poligno_componente[[1]]
      estrutura <- comp$estrutura[[1]]
      label     <- NULL

      if(!stringi$stri_isempty(comp$name_componente)){
         label <-  HTML(paste0("<strong>COMPONENTE:</strong> ", comp$name_componente, "<br><strong>estrutura:</strong> ", estrutura$name_estrutura))
      }

      mapa <- mapa |> addPolygons(
                           group   = "draw",
                           lng = poligno$x,
                           lat = poligno$y,
                           layerId = comp$cd_id_componente,
                           weight  = 2,
                           fillOpacity = 0.2,
                           label = label
                          )
    }
  }
   mapa
}

# 2) Atualiza os componentes via PROXY (chame sempre que 'componentes' mudar)
proxy_update_componentes <- function(session,ns,map_id,camera, componentes){
  # Filtra só os componentes da câmera atual
  comps <- componentes |> filter(.data$cd_id_camera == camera$cd_id_camera)
  if (nrow(comps) == 0) return(invisible())

  # IDs (como string) para ficarem estáveis
  ids <- as.character(comps$cd_id_componente)

  # Remove shapes antigos com os mesmos IDs (evita duplicar)
  prx <- leafletProxy(mapId = map_id,session = session)

  # # Adiciona cada polígono
  for(i in seq_len(nrow(comps))){
    comp <- comps[i,]
    poly <- jsonlite::fromJSON(comp$poligno_componente)  # data.frame com x,y

    prx <- prx |>
      addPolygons(
        lng      = poly$x,
        lat      = poly$y,
        group    = "draw",                             # permite editar com Draw
        layerId  = as.character(comp$cd_id_componente),# id estável
        weight   = 2,
        fillOpacity = 0.2,
        label    = comp$name_componente
      )
  }

  invisible(TRUE)
}

searchFramesByCamerasSelected <- function(conn,camerasTargets,cameras,objeto = NULL){

  df <- map_df(camerasTargets,function(camera){

       cam_id <- cameras |> filter(name_camera == camera)
       frame  <- selectLastFrameById(conn,cam_id$cd_id_camera)

       if(nrow(frame) == 0) return(NULL)

       img       <- image_read(frame$data_frame[[1]])
       info      <- image_info(img)
       w         <- info$width
       h         <- info$height
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
               componente = list(componentes)
               )
    })
  df$componente[[1]] <- map_df(df$componente,~ .x)
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