# ============================================================
# chat_from_db.R
# Alimentar a API do ChatGPT com dados do MariaDB em R
# Everton Fonseca :)
# ============================================================

# 1) Pacotes --------------------------------------------------
required_pkgs <- c("DBI", "RMariaDB", "dplyr", "jsonlite", "httr2")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) {
  install.packages(new_pkgs)
}

library(DBI)
library(RMariaDB)
library(dplyr)
library(jsonlite)
library(httr2)

# 2) CONFIGURAÇÕES -------------------------------------------
# Coloque sua chave numa variável de ambiente (mais seguro):
Sys.setenv(OPENAI_API_KEY = "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")

OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
if (OPENAI_API_KEY == "") {
  stop("Defina a chave primeiro: Sys.setenv(OPENAI_API_KEY = 'sk-...')")
}

# Dados do banco:
DB_HOST <- "127.0.0.1"
DB_USER <- "root"
DB_PASS <- "sua_senha"
DB_NAME <- "sua_base"
DB_PORT <- 3306

# Quantos registros pegar do banco para mandar ao modelo
N_REGISTROS <- 20L

# Qual modelo usar (ajuste se quiser outro)
OPENAI_MODEL <- "gpt-5-chat-latest"

# 3) Função para conectar no banco ---------------------------
conectar_mariadb <- function() {
  dbConnect(
    RMariaDB::MariaDB(),
    host = DB_HOST,
    user = DB_USER,
    password = DB_PASS,
    dbname = DB_NAME,
    port = DB_PORT
  )
}

# 4) Buscar dados da tabela objeto_contexto ------------------
buscar_objeto_contexto <- function(con, n = 20L) {
  dbGetQuery(
    con,
    sprintf("
      SELECT DATA_OC, DT_HR_LOCAL
      FROM objeto_contexto
      ORDER BY DT_HR_LOCAL DESC
      LIMIT %d
    ", as.integer(n))
  )
}

# 5) Montar texto de contexto --------------------------------
# Vai virar um blocão de texto que descreve cada linha
montar_contexto <- function(df) {
  if (!nrow(df)) return("Sem dados.")
  paste0(
    apply(df, 1, function(row) {
      # row é caractere, então:
      dt <- row[["DT_HR_LOCAL"]]
      js <- row[["DATA_OC"]]

      # tenta validar o JSON; se quebrar, deixa do jeito que veio
      js_limpo <- tryCatch({
        # pretty só pra ficar bonito
        jsonlite::toJSON(jsonlite::fromJSON(js), auto_unbox = TRUE)
      }, error = function(e) js)

      sprintf("[%s] %s", dt, js_limpo)
    }),
    collapse = "\n"
  )
}

# 6) Função que chama a API do OpenAI ------------------------
chamar_openai <- function(prompt_user,
                          system_msg = "Você é um analista de telemetria industrial. Resuma e detecte mudanças de estado.",
                          model = OPENAI_MODEL,
                          api_key = OPENAI_API_KEY) {

  req <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ) |>
    req_body_json(list(
      model = model,
      messages = list(
        list(role = "system", content = system_msg),
        list(role = "user",   content = prompt_user)
      )
    ))

  resp <- req_perform(req)

  if (resp_status(resp) >= 300) {
    # imprime o corpo de erro
    stop("Erro da API: ", resp_body_string(resp))
  }

  body <- resp_body_json(resp)
  # caminho padrão na API de chat
  out  <- body$choices[[1]]$message$content
  return(out)
}

# 7) ORQUESTRAÇÃO --------------------------------------------
main <- function() {

  message("-> Conectando ao MariaDB...")
  con <- conectar_mariadb()
  on.exit({ if (dbIsValid(con)) dbDisconnect(con) }, add = TRUE)

  message("-> Buscando dados da tabela objeto_contexto...")
  df <- buscar_objeto_contexto(con, N_REGISTROS)

  message("-> Montando contexto de texto...")
  contexto_txt <- montar_contexto(df)

  # aqui você controla o que quer perguntar pro modelo
  prompt_user <- paste(
    "Tenho abaixo os últimos registros de telemetria da fábrica.",
    "Cada linha é um JSON com máquinas como PRENSA, BOBINA, OPERADOR etc.",
    "Quero que você me diga:",
    "1) Qual o último estado da PRENSA;",
    "2) Se houve transição de OPERANDO -> TRABALHANDO ou vice-versa;",
    "3) Se a BOBINA teve mudança de VOLUME;",
    "4) Resuma em 3 linhas para mostrar no painel.",
    "\n--- DADOS ---\n",
    contexto_txt
  )

  message("-> Chamando o modelo na OpenAI...")
  resposta <- chamar_openai(prompt_user)

  message("-> Resposta do modelo:")
  cat("\n========================\n")
  cat(resposta)
  cat("\n========================\n")
}

# 8) Executar ------------------------------------------------
if (sys.nframe() == 0) {
  main()
}
