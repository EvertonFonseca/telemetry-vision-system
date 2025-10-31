# app/dao/chat_dao.R
# Integração do chat com OpenAI + MariaDB + geração de gráfico dinâmico
# Everton Fonseca

box::use(
  DBI,
  RMariaDB,
  dplyr[...],
  jsonlite,
  httr2,
  lubridate,
  ggplot2
)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ------------------------------------------------------------
# CONFIGS
# ------------------------------------------------------------
OPENAI_API_KEY      <- Sys.getenv("OPENAI_API_KEY")
DEFAULT_N_REGISTROS <- 50L
DEFAULT_MODEL       <- "gpt-5-chat-latest"

# ------------------------------------------------------------
fetch_objeto_contexto <- function(con, n = DEFAULT_N_REGISTROS) {
  DBI::dbGetQuery(
    con,
    sprintf("
      SELECT DATA_OC, DT_HR_LOCAL
      FROM objeto_contexto
      ORDER BY DT_HR_LOCAL DESC
      LIMIT %d
    ", as.integer(n))
  )
}

build_context_text <- function(df) {
  if (!nrow(df)) return("Sem dados de objeto_contexto.")
  paste0(
    apply(df, 1, function(row) {
      dt <- row[["DT_HR_LOCAL"]]
      js <- row[["DATA_OC"]]
      js_limpo <- tryCatch({
        jsonlite::toJSON(jsonlite::fromJSON(js), auto_unbox = TRUE)
      }, error = function(e) js)
      sprintf("[%s] %s", dt, js_limpo)
    }),
    collapse = "\n"
  )
}

# ------------------------------------------------------------
# aqui pedimos pra OpenAI devolver também um bloco JSON
# com o código R do gráfico, quando fizer sentido
# ------------------------------------------------------------
call_openai_for_plot <- function(user_msg,
                                 contexto_txt,
                                 model = DEFAULT_MODEL,
                                 api_key = OPENAI_API_KEY) {

  if (is.null(api_key) || !nzchar(api_key)) {
    return(list(
      assistant_text = "⚠️ OPENAI_API_KEY não configurada.",
      plot_code = NULL
    ))
  }

  # prompt de instrução pro modelo:
  system_msg <- paste(
    "Você é um assistente de telemetria industrial dentro de um app Shiny.",
    "Quando o usuário pedir um gráfico, você deve devolver também UM BLOCO JSON no fim da resposta, no formato:",
    "",
    "{",
    '  "plot_code": "ggplot(dados, aes(...)) + ...",',
    '  "title": "Título do gráfico"          ',
    "}",
    "",
    "Regras:",
    "- o código deve ser válido em R",
    "- use sempre ggplot2",
    "- os dados estarão na variável df_ctx (data.frame) contendo ao menos as colunas:",
    "    DT_HR_LOCAL (timestamp) e DATA_OC (JSON com estados)",
    "- se o usuário pedir colunas específicas que não existem, crie um exemplo com mutate",
    "- se o usuário NÃO pedir gráfico, não devolva o JSON",
    "- não use markdown nem ``` no plot_code, devolva texto puro dentro do JSON"
  )

  user_full <- paste(
    "Usuário disse:\n", user_msg, "\n\n",
    "Contexto do banco (objeto_contexto):\n", contexto_txt
  )

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = list(
        list(role = "system", content = system_msg),
        list(role = "user",   content = user_full)
      )
    ))

  resp <- httr2::req_perform(req)

  if (httr2::resp_status(resp) >= 300) {
    return(list(
      assistant_text = paste("Erro da API:", httr2::resp_body_string(resp)),
      plot_code = NULL
    ))
  }

  body <- httr2::resp_body_json(resp)
  txt  <- body$choices[[1]]$message$content

  # tentar achar o JSON no final
  plot_code <- NULL
  plot_title <- NULL
  # estratégia simples: procurar última chave {
  open_idx <- regexpr("\\{[[:space:]]*\"plot_code\"", txt)
  if (open_idx > 0) {
    json_part <- substr(txt, open_idx, nchar(txt))
    # tentar parsear
    parsed <- tryCatch(jsonlite::fromJSON(json_part), error = function(e) NULL)
    if (!is.null(parsed$plot_code)) {
      plot_code  <- parsed$plot_code
      plot_title <- parsed$title %||% "Gráfico"
      # e o texto do assistente é só a parte anterior ao JSON
      assistant_text <- trimws(substr(txt, 1, open_idx - 1))
      return(list(
        assistant_text = assistant_text,
        plot_code      = plot_code,
        plot_title     = plot_title
      ))
    }
  }

  list(
    assistant_text = txt,
    plot_code      = NULL,
    plot_title     = NULL
  )
}

# ------------------------------------------------------------
# avaliar código de plot retornado pela IA
# ------------------------------------------------------------
safe_eval_plot <- function(plot_code, df_ctx) {
  # ambiente restrito
  env <- new.env(parent = emptyenv())
  env$df_ctx  <- df_ctx
  env$ggplot2 <- ggplot2
  # carrega ggplot2 no env
  env$`%>%` <- dplyr::`%>%`
  env$dplyr  <- dplyr
  env$lubridate <- lubridate

  # tenta avaliar
  plt <- tryCatch({
    eval(parse(text = plot_code), envir = env)
  }, error = function(e) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0,
                        label = paste("Erro ao gerar gráfico:", e$message)) +
      ggplot2::theme_void()
  })

  plt
}

# ------------------------------------------------------------
# função chamada pelo Shiny
# ------------------------------------------------------------
#' @export
handle_user_message <- function(user_msg) {

  con <- NULL
  df_ctx <- NULL
  contexto_txt <- ""
  try({
    df_ctx <- fetch_objeto_contexto(dbp$get_pool(), DEFAULT_N_REGISTROS)
    contexto_txt <- build_context_text(df_ctx)
  }, silent = TRUE)
  if (!is.null(con)) try(DBI::dbDisconnect(con), silent = TRUE)

  # chama o modelo (texto + possivelmente plot_code)
  ans <- call_openai_for_plot(
    user_msg     = user_msg,
    contexto_txt = contexto_txt
  )

  items <- list(
    list(
      kind    = "text",
      content = ans$assistant_text %||% "(sem resposta do modelo)"
    )
  )

  # se veio código de plot, avalia e adiciona
  if (!is.null(ans$plot_code)) {
    plt <- safe_eval_plot(ans$plot_code, df_ctx %||% data.frame())
    items <- c(
      items,
      list(
        list(
          kind  = "plot",
          title = ans$plot_title %||% "Gráfico",
          plot  = plt
        )
      )
    )
  }

  list(
    items = items
  )
}
