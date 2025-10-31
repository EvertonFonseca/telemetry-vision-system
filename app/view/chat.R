# app/views/assistant_ia.R
box::use(
  shiny[...],
  shinyjs,
  ../logic/chat_dao[
    handle_user_message
  ],
  ./global[
    actionWebUser
  ],
  plotly
)

`%||%` <- function(x, y) if (is.null(x)) y else x

ui <- function(ns) {

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        body {
          background: #E5EAF0;
        }
        .ai-chat-wrapper {
          height: calc(100vh - 40px);
          padding: 20px 24px 20px 24px;
        }
        .ai-chat-shell {
          height: 90%;
          display: flex;
          flex-direction: column;
          background: #fff;
          border: 1px solid rgba(0,0,0,0.04);
          border-radius: 10px;
          box-shadow: 0 6px 12px rgba(0,0,0,0.03);
        }
        .ai-chat-header {
          flex: 0 0 auto;
          padding: 12px 16px;
          background: #2F83AC;
          color: #fff;
          border-top-left-radius: 10px;
          border-top-right-radius: 10px;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        .ai-chat-title { font-size: 1rem; font-weight: 600; }
        .ai-chat-body {
          flex: 1 1 auto;
          overflow-y: auto;
          padding: 16px;
          gap: 10px;
          display: flex;
          flex-direction: column;
          background: #F6F8FC;
        }
        .ai-msg-user, .ai-msg-bot {
          max-width: 65%;
          padding: 10px 14px;
          border-radius: 12px;
          line-height: 1.35;
          font-size: 16px;
        }
        .ai-msg-user {
          align-self: flex-end;
          background:rgb(94, 154, 184);
          color: #fff;
        }
        .ai-msg-bot {
          align-self: flex-start;
          background: #fff;
          border: 1px solid rgba(0,0,0,0.03);
        }
        .ai-chat-input-bar {
          flex: 0 0 auto;
          border-top: 1px solid rgba(0,0,0,.04);
          padding: 10px 14px 12px;
          display: flex;
          gap: 8px;
          align-items: center;
          background: #fff;
          border-bottom-left-radius: 10px;
          border-bottom-right-radius: 10px;
        }
        .ai-chat-textarea {
          flex: 1 1 auto;
        }
        .ai-chat-input .form-control {
          background: #fff;
          border: 1px solid rgba(0,0,0,0.08);
          border-radius: 8px;
          color: #1f2e36;
        }
        .ai-round-btn {
          width: 38px;
          height: 38px;
          border-radius: 999px !important;
          display: flex;
          align-items: center;
          justify-content: center;
          border: none;
        }
        .ai-send-btn {
          background: #2F83AC;
          color: #fff;
        }
        .ai-msg-plot {
          align-self: flex-start;
          background: #fff;
          border: 1px solid rgba(0,0,0,0.03);
          border-radius: 12px;
          padding: 6px 6px 2px;
          max-width: 90%;
        }
        .ai-msg-plot-title {
          font-weight: 600;
          margin-bottom: 4px;
        }
      ")),
      shiny::tags$script(shiny::HTML(sprintf("
        Shiny.addCustomMessageHandler('%s-scroll', function(_) {
          const el = document.querySelector('.ai-chat-body');
          if (el) el.scrollTop = el.scrollHeight;
        });

        document.addEventListener('DOMContentLoaded', function() {
          document.addEventListener('keydown', function(e) {
            const ta  = document.getElementById('%s');
            const btn = document.getElementById('%s');
            if (!ta || !btn) return;

            if (document.activeElement === ta) {
              if (e.key === 'Enter' && !e.shiftKey) {
                e.preventDefault();
                Shiny.setInputValue('%s', ta.value, {priority: 'event'});
                setTimeout(function(){ btn.click(); }, 0);
              }
            }
          });
        });
      ", ns("chat"), ns("user_text"), ns("send_btn"), ns("user_text"))))
    ),

    shiny::div(
      class = "ai-chat-wrapper",
      shiny::div(
        class = "ai-chat-shell",

        shiny::div(
          class = "ai-chat-header",
          shiny::div(class = "ai-chat-title", "Assistente IA"),
          shiny::div("v1.0")
        ),

        shiny::div(
          class = "ai-chat-body",
          shiny::uiOutput(ns("chat_history"))
        ),

        shiny::div(
          class = "ai-chat-input-bar",
          shiny::div(
            class = "ai-chat-textarea ai-chat-input",
            shiny::textAreaInput(
              inputId = ns("user_text"),
              label   = NULL,
              width   = "100%",
              height  = "110px",
              placeholder = "Digite sua mensagem e aperte Enter (Shift+Enter quebra linha)",
              resize  = "none"
            )
          ),
          shiny::actionButton(
            inputId = ns("send_btn"),
            label   = "",
            icon    = shiny::icon("paper-plane"),
            class   = "ai-round-btn ai-send-btn"
          )
        )
      )
    )
  )
}

# 👉 assinatura certa de módulo
server <- function(ns,input, output, session) {

  rv <- shiny::reactiveValues(
    msgs = list(
      list(role = "bot", type = "text", text = "Olá 👋"),
      list(role = "bot", type = "text", text = "Posso te ajudar com consultas, IA e os dados da Elite aço.")
    ),
    chat_ctx = NULL   # 👉 AQUI: contexto vivo da conversa
  )

  append_msg <- function(msg) {
    rv$msgs <- append(rv$msgs, list(msg))
    session$sendCustomMessage(paste0(session$ns("chat"), "-scroll"), NULL)
  }

  send_message <- function(txt) {
    txt <- trimws(txt)
    if (!nzchar(txt)) return()

    append_msg(list(role = "user", type = "text", text = txt))
    shiny::updateTextAreaInput(session, "user_text", value = "")

    # 👉 passa o contexto atual pro DAO
    resp <- handle_user_message(txt, ctx = rv$chat_ctx)

    # 👉 atualiza o contexto que voltou do DAO
    rv$chat_ctx <- resp$ctx %||% rv$chat_ctx

    if (!is.null(resp) && !is.null(resp$items)) {
      for (it in resp$items) {
        if (identical(it$kind, "text")) {
          append_msg(list(
            role = "bot",
            type = "text",
            text = it$content
          ))
        } else if (identical(it$kind, "plot")) {
          append_msg(list(
            role  = "bot",
            type  = "plot",
            title = it$title %||% "Gráfico",
            plot  = it$plot
          ))
        }
      }
    }
  }

  shiny::observeEvent(input$send_btn, {
    actionWebUser({
      tryCatch({
        send_message(input$user_text)
      }, error = function(e) {
        showNotification("Não foi possível processar agora 😢. Tente de novo em instantes.", type = "warning")
      })
    })

  })

  output$chat_history <- shiny::renderUI({
    msgs <- rv$msgs
    lapply(seq_along(msgs), function(i) {
      m <- msgs[[i]]
      if (m$type == "text") {
        if (m$role == "user") {
          shiny::div(class = "ai-msg-user", m$text)
        } else {
          shiny::div(class = "ai-msg-bot", m$text)
        }
      } else if (m$type == "plot") {
        pid <- session$ns(paste0("plot_", i))
        shiny::div(
          class = "ai-msg-plot",
          shiny::div(class = "ai-msg-plot-title", m$title),
          shiny::plotOutput(pid, height = 280)
        )
      }
    })
  })

  shiny::observe({
    msgs <- rv$msgs
    for (i in seq_along(msgs)) {
      m <- msgs[[i]]
      if (!is.null(m$type) && m$type == "plot") {
        local({
          my_i   <- i
          my_plot <- msgs[[my_i]]$plot
          out_id <- paste0("plot_", my_i)
          output[[out_id]] <- shiny::renderPlot({
            my_plot
          })
        })
      }
    }
  })
}
