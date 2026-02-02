box::use(
  dplyr[...],
  DBI,
  htmlwidgets
)

# ============================================================
# Postgres + lowercase:
# - SQL todo em lowercase (tabelas/colunas)
# - SEM json_valid / json_contains_path (isso era MySQL/MariaDB)
# - filtro JSON refeito com jsonb
# - IN seguro via placeholders ($1, $2, ...)
# ============================================================

# -------------------------------
# TZ helpers
# -------------------------------
#' @export
tz_local <- function() {
  Sys.timezone()
}

#' @export
to_utc <- function(x, tz = tz_local()) {
  if (is.null(x) || all(is.na(x))) return(x)
  as.POSIXct(format(as.POSIXct(x, tz = tz), tz = "UTC", usetz = TRUE), tz = "UTC")
}

#' @export
from_utc <- function(x, tz = tz_local()) {
  if (is.null(x) || all(is.na(x))) return(x)
  as.POSIXct(format(as.POSIXct(x, tz = "UTC"), tz = tz, usetz = TRUE), tz = tz)
}

#' @export
fmt_sql_dt_utc <- function(x_utc) {
  format(as.POSIXct(x_utc, tz = "UTC"), "%Y-%m-%d %H:%M:%S")
}

# -------------------------------
# SQL helpers (Postgres)
# -------------------------------
# monta "$k,$(k+1),..." para IN (...)
.pg_in <- function(vec, start_index = 1L) {
  vec <- vec[!is.na(vec) & nzchar(as.character(vec))]
  if (!length(vec)) return(list(sql = NULL, params = list(), nextt = start_index))
  n <- length(vec)
  idx <- start_index:(start_index + n - 1L)
  list(
    sql    = paste0("($", paste(idx, collapse = ",$"), ")"),
    params = as.list(as.character(vec)),
    nextt   = start_index + n
  )
}

# filtro JSON (Postgres):
# mantém linhas quando:
# - data_oc is null ou vazio
# - OU (é json válido) e NÃO contém os paths alvo
# Observação: se data_oc for TEXT, cast para jsonb pode falhar.
# Por isso usamos um "guard" simples: só tenta cast se parecer json.
.pg_json_filter <- function(col = "oc.data_oc") {
  paste0(
    "(",
    col, " is null or ", col, " = ''",
    " or (",
    "   (left(ltrim(", col, "),1) in ('{','['))",
    "   and (",
    "     not ((", col, ")::jsonb #> '{area_de_trabalho,estado}' is not null)",
    "     and not ((", col, ")::jsonb #> '{area_de_trabalho_a,estado}' is not null)",
    "     and not ((", col, ")::jsonb #> '{area_de_trabalho_b,estado}' is not null)",
    "   )",
    " )",
    ")"
  )
}

# ===============================
# build_sql (agora retorna {sql, params})
# ===============================
#' @export
build_sql <- function(dt_de_utc, dt_ate_utc, setor = NULL, objeto = NULL) {

  dt_de_utc  <- as.POSIXct(dt_de_utc,  tz = "UTC")
  dt_ate_utc <- as.POSIXct(dt_ate_utc, tz = "UTC")

  base <- "
    select
      oc.data_oc     as atributos,
      oc.dt_hr_local as date_time,
      o.name_objeto  as objeto,
      s.name_setor   as setor
    from objeto_contexto oc
    left join objeto o on o.cd_id_objeto = oc.cd_id_objeto
    left join setor  s on s.cd_id_setor  = o.cd_id_setor
    where o.fg_ativo = true
  "

  where <- c()
  params <- list()
  p <- 1L

  # intervalo de tempo (parametrizado)
  where <- c(where, sprintf("oc.dt_hr_local between $%d and $%d", p, p + 1L))
  params <- c(params, list(dt_de_utc, dt_ate_utc))
  p <- p + 2L

  # filtro JSON (Postgres jsonb)
  where <- c(where, .pg_json_filter("oc.data_oc"))

  # setor IN (...)
  if (!is.null(setor) && length(setor)) {
    in_s <- .pg_in(as.character(setor), start_index = p)
    if (!is.null(in_s$sql)) {
      where <- c(where, paste0("s.name_setor in ", in_s$sql))
      params <- c(params, in_s$params)
      p <- in_s$nextt
    }
  }

  # objeto IN (...)
  if (!is.null(objeto) && length(objeto)) {
    in_o <- .pg_in(as.character(objeto), start_index = p)
    if (!is.null(in_o$sql)) {
      where <- c(where, paste0("o.name_objeto in ", in_o$sql))
      params <- c(params, in_o$params)
      p <- in_o$nextt
    }
  }

  sql <- paste0(base, "\n  and ", paste(where, collapse = "\n  and "), "\n")

  list(sql = sql, params = params)
}

# ===============================
# run_query (Postgres safe)
# ===============================
#' @export
run_query <- function(pool, dt_de_utc, dt_ate_utc, setor = NULL, objeto = NULL) {
  q <- build_sql(dt_de_utc, dt_ate_utc, setor = setor, objeto = objeto)
  DBI::dbGetQuery(pool, q$sql, params = q$params)
}

# ===============================
# fetch_choices (lowercase list)
# ===============================
#' @export
fetch_choices <- function(pool) {
  sql <- "
    select distinct
      s.name_setor  as setor,
      o.name_objeto as objeto
    from objeto o
    left join setor s on s.cd_id_setor = o.cd_id_setor
    where o.name_objeto is not null and o.name_objeto <> ''
  "
  x <- DBI::dbGetQuery(pool, sql)

  # garante lower nos nomes do data.frame
  names(x) <- tolower(names(x))

  list(
    setores  = sort(unique(x$setor[!is.na(x$setor) & nzchar(x$setor)])),
    maquinas = sort(unique(x$objeto[!is.na(x$objeto) & nzchar(x$objeto)]))
  )
}

# ============================================================
# merge_setrema_bobina_prensa (não depende do banco)
# ============================================================
#' @export
merge_setrema_bobina_prensa <- function(ep_m,
                                        objeto       = "SETREMA",
                                        comp_bobina  = "BOBINA.ESTADO",
                                        comp_prensa  = "PRENSA.ESTADO",
                                        new_comp     = "SETREMA.ESTADO") {

  stopifnot(is.data.frame(ep_m))

  bp <- ep_m[ep_m$objeto == objeto & ep_m$componente %in% c(comp_bobina, comp_prensa), , drop = FALSE]
  if (!nrow(bp)) return(ep_m)

  bob <- bp[bp$componente == comp_bobina, , drop = FALSE]
  pre <- bp[bp$componente == comp_prensa, , drop = FALSE]
  if (!nrow(bob) || !nrow(pre)) return(ep_m)

  bob <- bob[order(bob$start_time), , drop = FALSE]
  pre <- pre[order(pre$start_time), , drop = FALSE]

  tz_use <- attr(bob$start_time, "tzone")
  to_num <- function(x) as.numeric(as.POSIXct(x, tz = tz_use))

  bnd <- sort(unique(c(to_num(bob$start_time), to_num(bob$end_time),
                       to_num(pre$start_time), to_num(pre$end_time))))
  if (length(bnd) < 2) return(ep_m)

  t0 <- bnd[-length(bnd)]
  t1 <- bnd[-1]
  keep <- (t1 > t0)
  t0 <- t0[keep]; t1 <- t1[keep]
  mid <- (t0 + t1) / 2

  get_state_at <- function(mid_num, seg) {
    s <- to_num(seg$start_time)
    e <- to_num(seg$end_time)
    idx <- findInterval(mid_num, s)
    ok  <- idx >= 1 & mid_num < e[pmax(idx,1)]
    out <- rep(NA_character_, length(mid_num))
    out[ok] <- as.character(seg$ESTADO)[idx[ok]]
    out
  }

  sb <- toupper(trimws(get_state_at(mid, bob)))
  sp <- toupper(trimws(get_state_at(mid, pre)))

  merged <- ifelse(sb == "OPERANDO" | sp == "OPERANDO", "OPERANDO",
            ifelse(sb == "SETUP"    | sp == "SETUP",    "SETUP",
              ifelse(!is.na(sb), sb,
                ifelse(!is.na(sp), sp, NA_character_))))

  seg <- data.frame(
    SETOR      = bob$SETOR[1],
    objeto     = objeto,
    componente = new_comp,
    ESTADO     = merged,
    start_time = as.POSIXct(t0, origin = "1970-01-01", tz = tz_use),
    end_time   = as.POSIXct(t1, origin = "1970-01-01", tz = tz_use),
    stringsAsFactors = FALSE
  )
  seg <- seg[!is.na(seg$ESTADO), , drop = FALSE]
  if (!nrow(seg)) return(ep_m)

  chg <- c(TRUE, seg$ESTADO[-1] != seg$ESTADO[-nrow(seg)])
  idx <- which(chg)
  end_idx <- c(idx[-1] - 1L, nrow(seg))

  out_seg <- data.frame(
    SETOR      = seg$SETOR[1],
    objeto     = objeto,
    componente = new_comp,
    ESTADO     = seg$ESTADO[idx],
    start_time = seg$start_time[idx],
    end_time   = seg$end_time[end_idx],
    stringsAsFactors = FALSE
  )
  out_seg$dur_secs <- as.numeric(difftime(out_seg$end_time, out_seg$start_time, units = "secs"))
  out_seg <- out_seg[out_seg$dur_secs > 0, , drop = FALSE]

  keep_rows <- !(ep_m$objeto == objeto & ep_m$componente %in% c(comp_bobina, comp_prensa))
  ep_new <- ep_m[keep_rows, , drop = FALSE]
  ep_new <- rbind(ep_new, out_seg)

  ep_new[order(ep_new$objeto, ep_new$componente, ep_new$start_time), , drop = FALSE]
}

# ============================================================
# (removidos) .sql_escape / .sql_in antigos
# - não use string IN com escape manual em Postgres
# - agora usamos placeholders com .pg_in
# ============================================================
