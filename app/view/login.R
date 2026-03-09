# app/view/login.R
# ===============================================================
box::use(
  shiny[
    NS, showModal, removeModal, modalDialog,
    tags, textInput, numericInput,
    actionButton, observeEvent, showNotification
  ],
  stringr,
  stringi,
  dbp = .. / infra / db_pool,
  . / global[newObserve, play_sound]
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

.trim <- function(x) stringr::str_trim(as.character(x %||% ""))

.parse_port <- function(x) {
  v <- suppressWarnings(as.integer(x))
  if (is.na(v) || v < 1L || v > 65535L) return(NA_integer_)
  v
}

.env_port <- function(name, default) {
  v <- suppressWarnings(as.integer(Sys.getenv(name, unset = "")))
  if (is.na(v)) as.integer(default) else v
}

.toggle_password_js <- function(input_id, button_id) {
  shinyjs::runjs(sprintf(
    "(function(){
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
    })();",
    input_id,
    button_id
  ))
}

.collect_connection_config <- function(input) {
  db <- list(
    dbname = .trim(input$loginDbName),
    host = .trim(input$loginDbHost),
    port = .parse_port(input$loginDbPort),
    user = .trim(input$loginDbUser),
    password = as.character(input$loginDbPassword %||% "")
  )

  if (stringi::stri_isempty(db$dbname)) return(list(ok = FALSE, msg = "Informe o nome do banco."))
  if (stringi::stri_isempty(db$host)) return(list(ok = FALSE, msg = "Informe o host do banco."))
  if (is.na(db$port)) return(list(ok = FALSE, msg = "Informe uma porta valida para o banco."))
  if (stringi::stri_isempty(db$user)) return(list(ok = FALSE, msg = "Informe o usuario do banco."))
  if (stringi::stri_isempty(stringr::str_trim(db$password))) return(list(ok = FALSE, msg = "Informe a senha do banco."))

  list(ok = TRUE, db = db)
}

enterLogin <- function(input, session) {
  cfg <- .collect_connection_config(input)
  if (!isTRUE(cfg$ok)) return(cfg)

  dbp$apply_db_env(
    dbname = cfg$db$dbname,
    host = cfg$db$host,
    port = cfg$db$port,
    user = cfg$db$user,
    password = cfg$db$password
  )

  db_err <- NULL
  db_ok <- tryCatch({
    dbp$close_pool()
    pool <- dbp$init(force = TRUE)
    DBI::dbGetQuery(pool, "select 1 as ok")
    TRUE
  }, error = function(e) {
    db_err <<- conditionMessage(e)
    FALSE
  })

  if (!isTRUE(db_ok)) {
    return(list(
      ok = FALSE,
      msg = paste0("Falha na conexao com PostgreSQL: ", db_err %||% "erro desconhecido")
    ))
  }

  list(
    ok = TRUE,
    user = list(
      nome = toupper(cfg$db$user),
      tipo = "db",
      FG_ONLINE_USER = 0
    )
  )
}

#' @export
uiLogin <- function(ns, session, input, output, callback, btCancel = FALSE) {
  obs <- newObserve()

  db_port_default <- .env_port("DB_PORT", 5434L)

  btExist <- NULL
  if (isTRUE(btCancel)) {
    btExist <- shiny::actionButton(
      ns("loginBtSair"),
      label = "Cancelar",
      icon = shiny::icon("xmark"),
      class = "btn btn-outline-secondary"
    )
  }

  showModal(
    modalDialog(
      title = NULL,
      size = "m",
      easyClose = FALSE,
      footer = NULL,
      tags$div(
        id = ns("loginRoot"),
        class = "tvs-login-modal",

        tags$style(shiny::HTML(paste0(
          ".tvs-login-modal .modal-content{border:0;border-radius:16px;box-shadow:0 18px 60px rgba(0,0,0,.35);overflow:hidden;}\n",
          ".tvs-login-modal .modal-header{border:0;padding:16px 18px;background:linear-gradient(135deg,rgba(0,123,255,.18),rgba(0,0,0,0));}\n",
          ".tvs-login-title{display:flex;align-items:center;gap:10px;font-weight:700;font-size:18px;}\n",
          ".tvs-login-sub{margin:4px 0 0 0;opacity:.75;font-size:12.5px;}\n",
          ".tvs-login-body{padding:14px 18px 6px 18px;max-height:70vh;overflow:auto;}\n",
          ".tvs-login-footer{border:0;padding:12px 18px 16px 18px;display:flex;gap:10px;justify-content:flex-end;}\n",
          ".tvs-login-modal .form-control,.tvs-login-modal .input-group-text,.tvs-login-modal .btn{border-radius:0 !important;}\n",
          ".tvs-login-modal .input-group-text{border-right:0;background:rgba(255,255,255,.06);}\n",
          ".tvs-login-modal .tvs-pwd .btn{border-left:0;}\n",
          ".tvs-login-modal .form-control:focus{box-shadow:0 0 0 .2rem rgba(0,123,255,.18);}\n",
          ".tvs-login-modal .section-title{font-size:12px;font-weight:700;color:#6c757d;text-transform:uppercase;letter-spacing:.04em;margin:8px 0;}\n"
        ))),

        tags$div(
          class = "modal-header",
          tags$div(
            tags$div(
              class = "tvs-login-title",
              shiny::icon("database"),
              tags$span("Conexao com PostgreSQL")
            ),
            tags$p(
              class = "tvs-login-sub",
              "Preencha os dados do banco. O sistema so libera acesso apos validar a conexao."
            )
          )
        ),

        tags$div(
          class = "tvs-login-body",

          tags$div(class = "section-title", "Banco de dados"),

          textInput(
            ns("loginDbName"),
            label = "Nome do banco",
            value = Sys.getenv("DB_NAME", "analytia_db"),
            placeholder = "analytia_db"
          ),

          textInput(
            ns("loginDbHost"),
            label = "Host",
            value = Sys.getenv("DB_HOST", "127.0.0.1"),
            placeholder = "127.0.0.1"
          ),

          numericInput(
            ns("loginDbPort"),
            label = "Porta",
            value = db_port_default,
            min = 1,
            max = 65535,
            step = 1
          ),

          textInput(
            ns("loginDbUser"),
            label = "Usuario",
            value = Sys.getenv("DB_USER", "analytia"),
            placeholder = "analytia"
          ),

          tags$div(
            class = "mb-3",
            tags$label(`for` = ns("loginDbPassword"), class = "form-label", "Senha"),
            tags$div(
              class = "input-group tvs-pwd",
              tags$span(class = "input-group-text", shiny::icon("lock")),
              tags$input(
                id = ns("loginDbPassword"),
                type = "password",
                class = "form-control",
                value = Sys.getenv("DB_PASS", "lytIA#2026!Elite@"),
                placeholder = "Senha do banco",
                autocomplete = "new-password"
              ),
              actionButton(
                ns("loginBtToggleDbPwd"),
                label = NULL,
                icon = shiny::icon("eye"),
                class = "btn btn-outline-secondary",
                `aria-label` = "Mostrar/ocultar senha do banco"
              )
            )
          )
        ),

        tags$div(
          class = "tvs-login-footer",
          btExist,
          actionButton(
            ns("loginBtLogar"),
            label = "Conectar",
            icon = shiny::icon("plug"),
            class = "btn btn-primary"
          )
        ),

        tags$script(shiny::HTML(paste0(
          "(function(){",
          "  var root = document.getElementById('", ns("loginRoot"), "');",
          "  if(!root) return;",
          "  root.addEventListener('keydown', function(e){",
          "    if(e.key === 'Enter'){",
          "      var bt = document.getElementById('", ns("loginBtLogar"), "');",
          "      if(bt) bt.click();",
          "    }",
          "  });",
          "})();"
        )))
      )
    )
  )

  obs$add(observeEvent(input$loginBtToggleDbPwd, {
    .toggle_password_js(ns("loginDbPassword"), ns("loginBtToggleDbPwd"))
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$loginBtLogar, {
    result <- enterLogin(input, session)

    if (!isTRUE(result$ok)) {
      showNotification(result$msg %||% "Falha ao validar conexao.", type = "error")
      return(invisible(NULL))
    }

    play_sound(session, "soundSessionDialog")
    obs$destroy()
    callback(result$user)
    showNotification("Conexao validada com sucesso.", type = "message")
    removeModal()
  }, ignoreInit = TRUE))

  obs$add(observeEvent(input$loginBtSair, {
    obs$destroy()
    removeModal()
  }, ignoreInit = TRUE))
}
