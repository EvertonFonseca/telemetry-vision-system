
box::use(
  shiny[...],
  shinyjs,
  ../logic/chat_dao[ handle_user_message,gpt_build_report_html],
  ./global[actionWebUser,debugLocal],
  plotly
)

`%||%` <- function(x, y) if (is.null(x)) y else x

#' @export
ui <- function(ns) {
  
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        body { background: #E5EAF0; }
        .ai-chat-wrapper { height: calc(100vh - 40px); padding: 20px 24px 20px 24px; }
        .ai-chat-shell {
          height: 90%; display: flex; flex-direction: column; background: #fff;
          border: 1px solid rgba(0,0,0,0.04); border-radius: 10px; box-shadow: 0 6px 12px rgba(0,0,0,0.03);
        }
        .ai-chat-header {
          flex: 0 0 auto; padding: 12px 16px; background: #2F83AC; color: #fff;
          border-top-left-radius: 10px; border-top-right-radius: 10px;
          display: flex; justify-content: space-between; align-items: center;
        }
        .ai-chat-title { font-size: 18px; font-weight: 600; }
        .ai-chat-body {
          flex: 1 1 auto; overflow-y: auto; padding: 16px; gap: 10px;
          display: flex; flex-direction: column; background: #F6F8FC;
        }
        .ai-msg-user,.ai-msg-bot {
          max-width: 65%; padding: 10px 14px; border-radius: 12px; line-height: 1.35; font-size: 16px;
        }
        .ai-msg-user { align-self: flex-end; background: rgb(94,154,184); color: #fff; }
        .ai-msg-bot  { align-self: flex-start; background: #fff; border: 1px solid rgba(0,0,0,0.03); }
        .ai-chat-input-bar {
          flex: 0 0 auto; border-top: 1px solid rgba(0,0,0,.04); padding: 10px 14px 12px;
          display: flex; gap: 8px; align-items: center; background: #fff;
          border-bottom-left-radius: 10px; border-bottom-right-radius: 10px;
        }
        .ai-chat-textarea { flex: 1 1 auto; }
        .ai-chat-input .form-control {
          background: #fff; border: 1px solid rgba(0,0,0,0.08); border-radius: 8px; color:#1f2e36;
        }
        .ai-round-btn { width: 38px; height: 38px; border-radius: 999px !important; display:flex; align-items:center; justify-content:center; border:none; }
        .ai-send-btn  { background:#2F83AC; color:#fff; }
        .ai-msg-plot { align-self:flex-start; background:#fff; border:1px solid rgba(0,0,0,0.03); border-radius:12px; padding:6px 6px 2px; max-width:90%; }
        .ai-msg-plot-title { font-weight:600; margin-bottom:4px; }
        .ai-link { text-decoration:none; color:#1f2e36; border:1px solid rgba(0,0,0,0.08); padding:8px 12px; border-radius:10px; display:inline-flex; gap:8px; align-items:center; }
        .ai-link:hover { background:#f2f6fa; }
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
            tags$style(HTML("#app-user_text { font-size:16px !important; }")),
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

#' @export
server <- function(ns, input, output, session) {
  
  rv <- shiny::reactiveValues(
    msgs = list(
      list(role = "bot", type = "text", text = "Olá 👋"),
      list(role = "bot", type = "text", text = "Posso te ajudar com consultas, IA e os dados da Elite aço.")
    ),
    chat_ctx     = NULL,
    last_df_ctx  = NULL,
    last_plots   = list(),
    last_texts   = character(0),
    busy         = FALSE,         # <- trava reentrância
    last_report  = NULL           # <- guarda último href gerado
  )
  
  append_msg <- function(msg) {
    rv$msgs <- append(rv$msgs, list(msg))
    session$sendCustomMessage(paste0(session$ns("chat"), "-scroll"), NULL)
  }
  
  # ------------ PDF sem Rmd (grid/gridExtra) ------------
  wrap_text <- function(txt, width = 95L) {
    paste(strwrap(txt, width = width), collapse = "\n")
  }
  
  sanitize_df_for_table <- function(df, max_rows = 20L, max_cols = 8L) {
    if (is.null(df) || !nrow(df)) return(data.frame())
    df <- df[seq_len(min(nrow(df), max_rows)), , drop = FALSE]
    # colunas compostas -> texto
    df[] <- lapply(df, function(col) {
      if (is.list(col)) vapply(col, toString, character(1)) else col
    })
    if (ncol(df) > max_cols) df <- df[, seq_len(max_cols), drop = FALSE]
    # tudo como character p/ evitar problemas de grob
    df[] <- lapply(df, function(x) {
      if (inherits(x, "POSIXct")) format(x, "%Y-%m-%d %H:%M:%S") else as.character(x)
    })
    df
  }
  
  generate_report_via_gpt <- function(df_ctx, user_prompt, ctx, title_fallback = "Relatório Operacional") {
    reports_dir <- getOption(".reports_dir")
    if (is.null(reports_dir) || !dir.exists(reports_dir)) {
      stop("Pasta de relatórios não está configurada (chame .init_reports_path() no início).")
    }
    
    # 1) Pede HTML ao GPT
    rep       <- handle_user_message # só para fix R CMD check
    build_fun <- gpt_build_report_html#get("gpt_build_report_html", envir = asNamespace("app.logic.chat_dao"), inherits = TRUE)
    ans <- build_fun(user_msg_for_report = user_prompt, df_ctx = df_ctx, ctx = rv$chat_ctx)
    if (!is.null(ans$error)) stop(ans$error)
    
    html <- ans$html %||% ""
    if (!nzchar(html)) stop("Modelo não devolveu HTML.")
    
    # 2) Salva HTML
    stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
    base  <- sprintf("relatorio-%s-%06d", stamp, sample.int(1e6, 1))
    f_html <- file.path(reports_dir, paste0(base, ".html"))
    writeLines(html, f_html, useBytes = TRUE)
    
    # 3) (Opcional) Converte para PDF com pagedown::chrome_print
    f_pdf  <- file.path(reports_dir, paste0(base, ".pdf"))
    pdf_ok <- FALSE
    if (requireNamespace("pagedown", quietly = TRUE)) {
      # chrome/edge precisa estar instalado no host
      try({
        pagedown::chrome_print(input = f_html, output = f_pdf, timeout = 120)
        pdf_ok <- file.exists(f_pdf)
      }, silent = TRUE)
    }
    
    list(
      href_html = sprintf("reports/%s.html", base),
      href_pdf  = if (pdf_ok) sprintf("reports/%s.pdf",  base) else NULL,
      title     = title_fallback
    )
  }
  
  send_message <- function(txt) {
    # evita reentrância
    if (isTRUE(rv$busy)) return(invisible(NULL))
    rv$busy <- TRUE
    shinyjs::disable("send_btn")
    on.exit({
      rv$busy <- FALSE
      shinyjs::enable("send_btn")
    }, add = TRUE)
    
    txt <- trimws(txt); if (!nzchar(txt)) return()
    append_msg(list(role = "user", type = "text", text = txt))
    shiny::updateTextAreaInput(session, "user_text", value = "")
    
    # 1) DAO
    resp <- handle_user_message(txt, ctx = rv$chat_ctx)
    rv$chat_ctx <- resp$ctx %||% rv$chat_ctx
    if (!is.null(resp$df_ctx)) rv$last_df_ctx <- resp$df_ctx
    
    # 2) Publica respostas e acumula conteúdo para relatório
    if (!is.null(resp$items)) {
      for (it in resp$items) {
        if (identical(it$kind, "text")) {
          rv$last_texts <- c(rv$last_texts, it$content)
          append_msg(list(role = "bot", type = "text", text = it$content))
        } else if (identical(it$kind, "plot")) {
          rv$last_plots <- append(rv$last_plots, list(it$plot))
          append_msg(list(role = "bot", type = "plot", title = it$title %||% "Gráfico", plot = it$plot))
        }
      }
    }
    
    # 3) Gatilho de relatório (pede para o GPT criar HTML e opcionalmente PDF)
    if (grepl("\\b(relat[óo]rio|report|baixar|pdf|documento|docx|html)\\b", txt, ignore.case = TRUE)) {
      if (is.null(rv$last_df_ctx) || !nrow(rv$last_df_ctx)) {
        append_msg(list(role = "bot", type = "text", text = "Faça uma consulta primeiro para que eu gere o relatório."))
      } else {
        ok <- TRUE; err <- NULL; rep <- NULL
        tryCatch({
          rep <- generate_report_via_gpt(
            df_ctx      = rv$last_df_ctx,
            user_prompt = txt,          # <- usa a própria frase do usuário como especificação do relatório
            ctx         = rv$chat_ctx
          )
        }, error = function(e) { ok <<- FALSE; err <<- e$message })
        
        if (ok && !is.null(rep)) {
          # Evita duplicar o mesmo href
          new_href <- rep$href_pdf %||% rep$href_html
          if (!identical(rv$last_report, new_href)) {
            rv$last_report <- new_href
            
            # Link(s) de download
            if (!is.null(rep$href_pdf)) {
              append_msg(list(
                role = "bot", type = "link",
                label = "Baixar relatório (PDF)",
                href  = rep$href_pdf
              ))
            }
            append_msg(list(
              role = "bot", type = "link",
              label = if (is.null(rep$href_pdf)) "Baixar relatório (HTML)" else "Visualizar versão HTML",
              href  = rep$href_html
            ))
          }
        } else {
          append_msg(list(role = "bot", type = "text", text = paste("Falha ao gerar o relatório:", err %||% "erro desconhecido")))
        }
      }
    }
  }
  
  shiny::observeEvent(input$send_btn, ignoreInit = TRUE, {
    if (isTRUE(rv$busy)) return()            # evita clique repetido durante processamento
    actionWebUser({
      tryCatch({ send_message(input$user_text) },
      error = function(e) showNotification("Não foi possível processar agora 😢. Tente de novo em instantes.", type = "warning"))
    })
  })
  
  # Histórico de mensagens
  output$chat_history <- shiny::renderUI({
    msgs <- rv$msgs
    lapply(seq_along(msgs), function(i) {
      m <- msgs[[i]]
      if (m$type == "text") {

        if (m$role == "user") div(class = "ai-msg-user", m$text) else div(class = "ai-msg-bot", m$text)
      } else if (m$type == "plot") {
        pid <- session$ns(paste0("plot_", i))
        div(class = "ai-msg-plot",
            div(class = "ai-msg-plot-title", m$title),
            plotOutput(pid, height = 280))
      } else if (m$type == "link") {
        div(class = "ai-msg-bot",
            tags$a(href = m$href, class = "ai-link", target = "_blank", download = NA,
                   icon("file-pdf"), span(m$label %||% "Baixar PDF")))
      }
    })
  })
  
  # Render dinâmico para cada plot
  shiny::observe({
    msgs <- rv$msgs
    for (i in seq_along(msgs)) {
      m <- msgs[[i]]
      if (!is.null(m$type) && m$type == "plot") {
        local({
          my_i   <- i
          my_plot <- msgs[[my_i]]$plot
          out_id <- paste0("plot_", my_i)
          output[[out_id]] <- shiny::renderPlot({ my_plot })
        })
      }
    }
  })
}
