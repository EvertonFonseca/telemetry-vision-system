# app/logic/login_dao.R
# ===============================================================
box::use(
  DBI,
  digest,
  stringr
)

# tenta autenticar por ENV primeiro (admin), depois por tabela no DB
# retorna list(ok=TRUE, user=list(login=..., role=...)) ou ok=FALSE
#' @export
auth_user <- function(conn, login, password) {
  login <- stringr::str_trim(toupper(login %||% ""))
  password <- password %||% ""

  if (!nzchar(login) || !nzchar(password)) {
    return(list(ok = FALSE, msg = "Usuário/senha inválidos."))
  }

  # -----------------------------
  # 1) ENV (recomendado p/ bootstrap)
  # -----------------------------
  env_user <- Sys.getenv("TVS_ADMIN_USER", unset = "")
  env_pass <- Sys.getenv("TVS_ADMIN_PASS", unset = "")
  if (nzchar(env_user) && nzchar(env_pass)) {
    if (identical(login, toupper(env_user)) && identical(password, env_pass)) {
      return(list(ok = TRUE, user = list(login = login, role = "ADMIN", source = "ENV")))
    }
  }

  # -----------------------------
  # 2) DB (tabela de usuários)
  # -----------------------------
  salt <- Sys.getenv("TVS_AUTH_SALT", unset = "")
  pass_sha <- digest::digest(paste0(password, ":", salt), algo = "sha256")

  candidates <- c("TVS_USUARIO", "USUARIO", "TB_USUARIO", "USERS", "USER_APP")

  for (tbl in candidates) {
    if (!DBI::dbExistsTable(conn, tbl)) next

    fields <- toupper(DBI::dbListFields(conn, tbl))

    # tenta mapear colunas mais comuns
    login_col <- intersect(fields, c("LOGIN", "USERNAME", "USER", "USUARIO"))[1] %||% NA_character_
    pass_col  <- intersect(fields, c("SENHA_HASH", "PASSWORD_HASH", "PASS_HASH", "SENHA", "PASSWORD", "PASS"))[1] %||% NA_character_
    ativo_col <- intersect(fields, c("ATIVO", "ACTIVE", "IS_ACTIVE"))[1] %||% NA_character_
    role_col  <- intersect(fields, c("ROLE", "PERFIL", "CARGO"))[1] %||% NA_character_

    if (is.na(login_col) || is.na(pass_col)) next

    sql <- paste0("SELECT * FROM ", tbl, " WHERE UPPER(", login_col, ") = ? ",
                  if (!is.na(ativo_col)) paste0(" AND (", ativo_col, " = 1 OR ", ativo_col, " = TRUE) ") else "",
                  " LIMIT 1")

    row <- tryCatch(DBI::dbGetQuery(conn, sql, params = list(login)), error = function(e) NULL)
    if (is.null(row) || !nrow(row)) next

    stored <- as.character(row[[pass_col]][1] %||% "")
    if (!nzchar(stored)) next

    # se a coluna for HASH, assume sha256(password:salt); senão aceita plaintext
    is_hash <- grepl("HASH", pass_col, fixed = TRUE)

    ok_pass <- if (is_hash) identical(tolower(stored), tolower(pass_sha)) else identical(stored, password)
    if (!ok_pass) return(list(ok = FALSE, msg = "Senha incorreta."))

    role <- if (!is.na(role_col)) as.character(row[[role_col]][1] %||% "USER") else "USER"
    return(list(ok = TRUE, user = list(login = login, role = toupper(role), source = tbl)))
  }

  list(ok = FALSE, msg = "Usuário não encontrado (configure TVS_ADMIN_USER/TVS_ADMIN_PASS ou tabela de usuários).")
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
