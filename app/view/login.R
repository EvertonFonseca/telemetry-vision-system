# app/view/login.R
# ===============================================================
box::use(
  shiny[
    NS, showModal, removeModal, modalDialog, div, tags, tagList,
    textInput, passwordInput, actionButton,
    observeEvent, isolate, showNotification
  ],
  stringr,
  stringi,
  db = .. / infra / database,
  . / global[newObserve, play_sound]
)

#' @export
uiLogin <- function(ns,session,input,output,callback,btCancel = FALSE) {
  
  obs <- newObserve()
  
  # ============================================================
  # MODAL LOGIN (profissional + ícones + senha com olhinho)
  # Inputs QUADRADOS (border-radius 0)
  # Requer: shinyjs::useShinyjs() no UI principal (1x)
  # ============================================================
  btExist <- NULL
  if (isTRUE(btCancel)) {
    btExist <- shiny::actionButton(
      ns("loginBtSair"),
      label = "Cancelar",
      icon  = shiny::icon("xmark"),
      class = "btn btn-outline-secondary"
    )
  }
  
  shiny::showModal(
    shiny::modalDialog(
      title = NULL,
      size  = "s",
      easyClose = FALSE,
      footer = NULL,
      
      shiny::tags$div(
        class = "tvs-login-modal",
        
        # ===================== CSS =====================
        shiny::tags$style(shiny::HTML(paste0("
        .tvs-login-modal .modal-content{
          border: 0;
          border-radius: 16px; /* mantenha arredondado no modal (opcional) */
          box-shadow: 0 18px 60px rgba(0,0,0,.35);
          overflow: hidden;
        }
        .tvs-login-modal .modal-header{
          border: 0;
          padding: 16px 18px;
          background: linear-gradient(135deg, rgba(0,123,255,.18), rgba(0,0,0,0));
        }
        .tvs-login-title{
          display:flex; align-items:center; gap:10px;
          font-weight: 700; font-size: 18px;
        }
        .tvs-login-sub{
          margin: 4px 0 0 0; opacity:.75; font-size: 12.5px;
        }
        .tvs-login-body{ padding: 14px 18px 6px 18px; }
        .tvs-login-footer{
          border:0; padding: 12px 18px 16px 18px;
          display:flex; gap:10px; justify-content:flex-end;
        }
      
        /* ===== Inputs QUADRADOS (radius 0) ===== */
        .tvs-login-modal .form-control,
        .tvs-login-modal .input-group-text,
        .tvs-login-modal .btn{
          border-radius: 0 !important;
        }
      
        /* Input-group alinhado */
        .tvs-login-modal .input-group-text{ border-right: 0; background: rgba(255,255,255,.06); }
        .tvs-login-modal .tvs-pwd .btn{ border-left: 0; }
      
        .tvs-login-modal .form-control:focus{
          box-shadow: 0 0 0 .2rem rgba(0,123,255,.18);
        }
      
        /* Nome sempre em maiúsculo */
        #", ns("loginEditNome"), " { text-transform: uppercase; }
      "))),
        
        
        # ===================== HEADER =====================
        shiny::tags$div(
          class = "modal-header",
          shiny::tags$div(
            shiny::tags$div(
              class = "tvs-login-title",
              shiny::icon("shield-halved"),
              shiny::tags$span("Acesso ao Sistema")
            ),
            shiny::tags$p(class = "tvs-login-sub", "Informe usuário e senha para continuar.")
          )
        ),
        
        # ===================== BODY =====================
        shiny::tags$div(
          class = "tvs-login-body",
          
          # Usuário
          shiny::tags$div(
            class = "mb-3",
            shiny::tags$label(`for` = ns("loginEditNome"), class = "form-label", "Usuário"),
            shiny::tags$div(
              class = "input-group",
              shiny::tags$span(class = "input-group-text", shiny::icon("user")),
              shiny::tags$input(
                id = ns("loginEditNome"),
                type = "text",
                class = "form-control",
                value = "MASTER",
                placeholder = "DIGITE USUÁRIO",
                autocomplete = "username"
              )
            )
          ),
          
          # Senha + olhinho
          shiny::tags$div(
            class = "mb-2",
            shiny::tags$label(`for` = ns("loginEditPassword"), class = "form-label", "Senha"),
            shiny::tags$div(
              class = "input-group tvs-pwd",
              shiny::tags$span(class = "input-group-text", shiny::icon("lock")),
              shiny::tags$input(
                id = ns("loginEditPassword"),
                type = "password",
                class = "form-control",
                value = "master",
                placeholder = "DIGITE A SENHA",
                autocomplete = "current-password"
              ),
              shiny::actionButton(
                ns("loginBtTogglePwd"),
                label = NULL,
                icon  = shiny::icon("eye"),
                class = "btn btn-outline-secondary",
                `aria-label` = "Mostrar/ocultar senha"
              )
            )
          )
        ),
        
        # ===================== FOOTER =====================
        shiny::tags$div(
          class = "tvs-login-footer",
          btExist,
          shiny::actionButton(
            ns("loginBtLogar"),
            label = "Entrar",
            icon  = shiny::icon("right-to-bracket"),
            class = "btn btn-primary"
          )
        ),
        
        # Enter no campo senha -> clicar em Entrar
        shiny::tags$script(shiny::HTML(paste0("
        (function(){
          var pwd = document.getElementById('", ns("loginEditPassword"), "');
          if(!pwd) return;
          pwd.addEventListener('keydown', function(e){
            if(e.key === 'Enter'){
              var bt = document.getElementById('", ns("loginBtLogar"), "');
              if(bt) bt.click();
            }
          });
        })();
      ")))
      )
    )
  )

  # ============================================================
  # SERVER: toggle senha (olhinho) + troca ícone eye / eye-slash
  # ============================================================
  obs$add(observeEvent(input$loginBtTogglePwd, {
    shinyjs::runjs(sprintf("
      (function(){
        var pwd = document.getElementById('%s');
        var bt  = document.getElementById('%s');
        if(!pwd || !bt) return;
      
        var icon = bt.querySelector('i');
        var isPwd = (pwd.type === 'password');
      
        pwd.type = isPwd ? 'text' : 'password';
        pwd.focus();
      
        if(icon){
          icon.classList.remove('fa-eye','fa-eye-slash');
          icon.classList.add(isPwd ? 'fa-eye-slash' : 'fa-eye');
        }
      })();
    ", ns("loginEditPassword"), ns("loginBtTogglePwd")))
  },ignoreInit = T))

  obs$add(observeEvent(input$loginBtLogar,{
    
    user <- enterLogin(input)
    if(!is.null(user)){
      play_sound(session,"soundSessionDialog")
      obs$destroy()
      callback(user)
      #updateUserOnline(con,user,TRUE)
      showNotification("Login foi sucedido com sucesso!", type = "warning")
      removeModal()
    }
    
  },ignoreInit = T))
  
  obs$add(observeEvent(input$loginBtSair,{
    
    obs$destroy()
    removeModal()
    
  },ignoreInit = T))
  
}

enterLogin <- function(input){
  
  nome  <- toupper(input$loginEditNome)
  senha <- input$loginEditPassword
  
  if (stringi::stri_isempty(stringr::str_trim(nome))) {
    showNotification("O campo do nome esta vázio!",
    action = NULL,
    type = 'warning')
    return(NULL)
  }
  else if (stringi::stri_isempty(stringr::str_trim(senha))) {
    showNotification("O campo da senha esta vázio!", type = "warning")
    return(NULL)
  }
  
  #user <- loginUser(con,nome,senha)
  user  <- list(nome = "admin",tipo = "root",FG_ONLINE_USER = 0)
  # if(is.null(user)){
  #   showNotification("Permissão de autenticação negada!", type = "warning")
  #   return(NULL)
  # }
  # else if (nrow(user) == 0) {
  #   showNotification("Permissão de autenticação negada!", type = "warning")
  #   return(NULL)
  # }
  # else if (user$FG_ONLINE_USER == 1) {
  #   showNotification("Usuário já esta logado!", type = "warning")
  #   return(NULL)
  # }
  user 
}
