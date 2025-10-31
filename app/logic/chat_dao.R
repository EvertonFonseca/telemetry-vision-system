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
  ggplot2,
  dbp  = ../infra/db_pool
)

`%||%` <- function(x, y) if (is.null(x)) y else x

OPENAI_API_KEY      <- Sys.getenv("OPENAI_API_KEY")
DEFAULT_N_REGISTROS <- 50L
DEFAULT_MODEL       <- "gpt-5-chat-latest"

# ------------------------------------------------------------
# 1) GPT GERA A SQL (agora com contexto opcional)
# ------------------------------------------------------------
gpt_build_sql <- function(user_msg,
                          ctx    = NULL,
                          model  = DEFAULT_MODEL,
                          api_key = OPENAI_API_KEY) {

  if (!nzchar(api_key)) {
    return(list(sql = NULL, error = "OPENAI_API_KEY não configurada"))
  }

  # contexto anterior (objeto/setor) vira instrução
  ctx_txt <- ""
  if (!is.null(ctx)) {
    if (!is.null(ctx$last_object)) {
      ctx_txt <- paste0(ctx_txt,
        "- Se o usuário NÃO mencionar objeto agora, reutilize este: '", ctx$last_object, "'.\n"
      )
    }
    if (!is.null(ctx$last_setor)) {
      ctx_txt <- paste0(ctx_txt,
        "- Se o usuário NÃO mencionar setor agora, reutilize este: '", ctx$last_setor, "'.\n"
      )
    }
  }

  system_msg <- paste(
    "Você gera SQL para consultar telemetria industrial em MariaDB.",
    "",
    "BASE DA CONSULTA (sempre):",
    "SELECT",
    "  oc.DATA_OC,",
    "  oc.DT_HR_LOCAL,",
    "  o.NAME_OBJETO,",
    "  s.NAME_SETOR",
    "FROM objeto_contexto oc",
    "LEFT JOIN objeto o ON o.CD_ID_OBJETO = oc.CD_ID_OBJETO",
    "LEFT JOIN setor  s ON s.CD_ID_SETOR  = o.CD_ID_SETOR",
    "",
    "REGRAS MUITO IMPORTANTES:",
    "1. Responda APENAS em JSON, exatamente assim: {\"sql\": \"SELECT ...\"}",
    "2. NÃO use ```sql, não explique, não comente.",
    "3. SEMPRE termine com: ORDER BY oc.DT_HR_LOCAL DESC LIMIT 300",
    "4. APENAS SELECT. Proibido: UPDATE, DELETE, INSERT, CREATE, DROP.",
    "",
    "FILTROS QUE VOCÊ PODE USAR (APENAS ESTES):",
    "- Se o usuário falar de uma máquina ou objeto",
    "  filtre: o.NAME_OBJETO LIKE '%<NOME_EM_MAIUSCULO>%'",
    "",
    "- Se o usuário falar de um setor",
    "  filtre: s.NAME_SETOR LIKE '%<NOME_EM_MAIUSCULO>%'",
    "",
    "- Se o usuário falar de um intervalo de horário (ex.: 'entre 06:20 e 06:40'),",
    "  filtre: TIME(oc.DT_HR_LOCAL) BETWEEN '06:20:00' AND '06:40:59'",
    "",
    "- Se o usuário falar de uma data (ex.: '2025-10-31' ou 'hoje'),",
    "  use: DATE(oc.DT_HR_LOCAL) = '2025-10-31'",
    "",
    "IMPORTANTE:",
    "- NÃO crie filtros usando JSON_EXTRACT.",
    "- NÃO filtre pelo conteúdo de oc.DATA_OC.",
    "- Apenas traga oc.DATA_OC no SELECT.",
    "",
    "CONTEXTO ATUAL (se existir):",
    ctx_txt,
    "",
    "SAÍDA:",
    "- devolva somente: {\"sql\": \"SELECT ...\"}",
    sep = "\n"
  )

  user_full <- paste(
    "Usuário pediu:",
    user_msg,
    "\nMonte a SQL seguindo as regras."
  )

  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0,
      messages = list(
        list(role = "system", content = system_msg),
        list(role = "user",   content = user_full)
      )
    ))

  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 300) {
    return(list(sql = NULL, error = httr2::resp_body_string(resp)))
  }

  body    <- httr2::resp_body_json(resp)
  raw_txt <- body$choices[[1]]$message$content

  # 1) tenta JSON direto
  sql_txt <- NULL
  parsed  <- tryCatch(jsonlite::fromJSON(raw_txt), error = function(e) NULL)
  if (!is.null(parsed) && !is.null(parsed$sql)) {
    sql_txt <- parsed$sql
  }

  # 2) fallback: pegar primeira linha com SELECT
  if (is.null(sql_txt)) {
    lines <- strsplit(raw_txt, "\n", fixed = TRUE)[[1]]
    cand  <- lines[grepl("^\\s*SELECT\\s", lines, ignore.case = TRUE)]
    if (length(cand)) {
      sql_txt <- paste(cand, collapse = " ")
    }
  }

  # 3) fallback final
  if (is.null(sql_txt)) {
    sql_txt <- paste(
      "SELECT oc.DATA_OC, oc.DT_HR_LOCAL, o.NAME_OBJETO, s.NAME_SETOR",
      "FROM objeto_contexto oc",
      "LEFT JOIN objeto o ON o.CD_ID_OBJETO = oc.CD_ID_OBJETO",
      "LEFT JOIN setor s ON s.CD_ID_SETOR = o.CD_ID_SETOR",
      "ORDER BY oc.DT_HR_LOCAL DESC",
      "LIMIT 300;"
    )
    return(list(
      sql   = sql_txt,
      error = "Modelo não retornou SELECT; usei a query base."
    ))
  }

  # 4) saneamento mínimo
  if (!grepl("^\\s*SELECT\\s", sql_txt, ignore.case = TRUE)) {
    return(list(
      sql = NULL,
      error = "Modelo não retornou um SELECT mesmo após normalização."
    ))
  }
  if (grepl(";", sql_txt)) {
    parts   <- strsplit(sql_txt, ";", fixed = TRUE)[[1]]
    sql_txt <- paste0(trimws(parts[[1]]), ";")
  }

  list(
    sql = sql_txt,
    error = NULL
  )
}

# ------------------------------------------------------------
run_user_sql <- function(con, sql_txt) {
  DBI::dbGetQuery(con, sql_txt) |>
    dplyr::mutate(DT_HR_LOCAL = as.POSIXct(DT_HR_LOCAL, Sys.timezone()))
}

# ------------------------------------------------------------
# extrai contexto da resposta SQL
# ------------------------------------------------------------
infer_ctx_from_df <- function(df, old_ctx = NULL) {
  new_ctx <- old_ctx %||% list()
  if (!is.null(df) && nrow(df)) {
    # pega o objeto mais frequente retornado
    if ("NAME_OBJETO" %in% names(df)) {
      obj <- df$NAME_OBJETO
      obj <- obj[!is.na(obj) & nzchar(obj)]
      if (length(obj)) {
        tab <- sort(table(obj), decreasing = TRUE)
        new_ctx$last_object <- names(tab)[1]
      }
    }
    if ("NAME_SETOR" %in% names(df)) {
      st <- df$NAME_SETOR
      st <- st[!is.na(st) & nzchar(st)]
      if (length(st)) {
        tab2 <- sort(table(st), decreasing = TRUE)
        new_ctx$last_setor <- names(tab2)[1]
      }
    }
  }
  new_ctx
}

# ------------------------------------------------------------
gpt_answer_from_rows <- function(user_msg,
                                 df,
                                 ctx   = NULL,
                                 model = DEFAULT_MODEL,
                                 api_key = OPENAI_API_KEY) {

  if (!nzchar(api_key)) {
    return(list(
      assistant_text = "⚠️ IA indisponível no momento.",
      plot_code = NULL,
      plot_title = NULL
    ))
  }

  df_preview <- if (nrow(df)) {
    head_n <- min(40L, nrow(df))
    jsonlite::toJSON(df[seq_len(head_n), , drop = FALSE], auto_unbox = TRUE)
  } else {
    "[]"
  }

  ctx_txt <- ""
  if (!is.null(ctx)) {
    if (!is.null(ctx$last_object)) {
      ctx_txt <- paste0(ctx_txt, "- Último objeto em foco: ", ctx$last_object, "\n")
    }
    if (!is.null(ctx$last_setor)) {
      ctx_txt <- paste0(ctx_txt, "- Último setor em foco: ", ctx$last_setor, "\n")
    }
  }
  # system_msg <- paste(
  #   "Você é um ASSISTENTE DE OPERAÇÃO INDUSTRIAL.",
  #   "Você recebeu dados de operação já filtrados (cada linha tem DATA_OC, DT_HR_LOCAL, NAME_OBJETO, NAME_SETOR).",
  #   "DATA_OC é um JSON contendo 1 ou mais componentes do Objeto, cada componente possui nomes dos atributos e valores.",
  #   "NAME_OBJETO é uma string que contem o nome do objeto ou maquina.",
  #   "Seu trabalho é explicar para o usuário, em português claro as suas necessidades e faça um resumo em poucas palavras.",
  #   "IMPORTANTE:",
  #   "- NÃO fale de banco de dados, nem de SQL, nem de R.",
  #   "- NÃO repita o JSON bruto.",
  #   "- Se o usuário pedir gráfico ou escrever 'plota', aí SIM devolva, AO FINAL, um bloco JSON assim:",
  #   "{ \"plot_code\": \"ggplot(df_ctx, aes(...)) + ...\", \"title\": \"...\" }",
  #   "- caso contrário, NÃO devolva o JSON."
  # )
  system_msg <- paste(
    "Você é um ASSISTENTE DE OPERAÇÃO INDUSTRIAL dentro de um painel.",
    "Você recebe linhas já filtradas com: DATA_OC (JSON de componentes), DT_HR_LOCAL (timestamp), NAME_OBJETO (máquina/objeto) e NAME_SETOR.",
    "",
    "REGRAS DE INTERPRETAÇÃO:",
    "- Se o usuário pedir **nomes de máquinas / objetos / ativos / equipamentos**, responda usando APENAS os valores distintos de NAME_OBJETO que vieram nos dados.",
    "- NESSA SITUAÇÃO, NÃO use os componentes do JSON (DATA_OC) como se fossem máquinas.",
    "- Só use DATA_OC quando o usuário falar de 'componentes', 'partes', 'itens dentro do objeto', 'atributos do objeto' ou pedir estado/força/volume.",
    "- Se existir apenas 1 objeto nos dados, diga isso claramente: 'Nos registros há apenas o objeto X'.",
    "",
    "ESTILO DA RESPOSTA:",
    "- Responda de forma direta sobre o que o usuário perguntou (tempo, estado, máquina, setor).",
    "- Use no máximo 4 frases quando for só explicação.",
    "- NÃO repetir o nome do objeto ou do setor se eles não mudaram em relação à pergunta.",
    "- NÃO reexplicar o que é DATA_OC, nem falar de banco, SQL ou R.",
    "- Evite começar com 'Os registros mostram...' quando a pergunta for simples; vá direto: 'Nos registros há ...'.",
    "",
    "SE O USUÁRIO PEDIR GRÁFICO:",
    "- ao final, devolva um bloco JSON assim:",
    '{ \"plot_code\": \"ggplot(df_ctx, aes(...)) + ...\", \"title\": \"...\" }',
    "- caso contrário, NÃO devolva o JSON.",
    "",
    "Contexto atual:",
    ctx_txt,
    sep = "\n"
  )
  user_full <- paste(
    "Pergunta original do usuário:\n", user_msg, "\n\n",
    "Dados filtrados (JSON):\n", df_preview
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
      assistant_text = paste("Erro da IA:", httr2::resp_body_string(resp)),
      plot_code = NULL,
      plot_title = NULL
    ))
  }

  body <- httr2::resp_body_json(resp)
  txt  <- body$choices[[1]]$message$content

  plot_code  <- NULL
  plot_title <- NULL
  open_idx <- regexpr("\\{[[:space:]]*\"plot_code\"", txt)
  if (open_idx > 0) {
    json_part <- substr(txt, open_idx, nchar(txt))
    parsed <- tryCatch(jsonlite::fromJSON(json_part), error = function(e) NULL)
    if (!is.null(parsed$plot_code)) {
      plot_code  <- parsed$plot_code
      plot_title <- parsed$title %||% "Gráfico"
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
# 4) avaliar código de plot retornado
# ------------------------------------------------------------
safe_eval_plot <- function(plot_code, df_ctx) {
  env <- new.env(parent = emptyenv())
  env$df_ctx    <- df_ctx
  env$ggplot2   <- ggplot2
  env$dplyr     <- dplyr
  env$lubridate <- lubridate

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
# 5) função chamada pelo Shiny (agora recebe e devolve ctx)
# ------------------------------------------------------------
#' @export
handle_user_message <- function(user_msg, ctx = NULL) {

  # 1) monta SQL com base no contexto
  plan <- gpt_build_sql(user_msg, ctx = ctx)
  if (!is.null(plan$error) && is.null(plan$sql)) {
    return(list(
      items = list(
        list(kind = "text", content = paste("Não consegui montar a consulta:", plan$error))
      ),
      ctx = ctx
    ))
  }

  sql_txt <- plan$sql

  # 2) roda a SQL no banco
  con    <- dbp$get_pool()
  df     <- NULL
  ok_db  <- TRUE
  err_db <- NULL
  tryCatch({
    df  <- run_user_sql(con, sql_txt)
  }, error = function(e) {
    ok_db <<- FALSE
    err_db <<- e$message
  })

  if (!ok_db) {
    return(list(
      items = list(
        list(
          kind = "text",
          content = paste("Consulta gerada, mas não consegui executar no banco:", err_db)
        )
      ),
      ctx = ctx
    ))
  }

  # 2b) atualiza contexto com o que veio do banco
  new_ctx <- infer_ctx_from_df(df, old_ctx = ctx)

  # 3) pede pro GPT transformar os dados em resposta operacional
  ans <- gpt_answer_from_rows(user_msg, df, ctx = new_ctx)

  items <- list(
    list(
      kind    = "text",
      content = ans$assistant_text
    )
  )

  if (!is.null(ans$plot_code)) {
    plt <- safe_eval_plot(ans$plot_code, df %||% data.frame())
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
    items = items,
    ctx   = new_ctx
  )
}
