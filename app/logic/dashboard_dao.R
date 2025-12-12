box::use(
  dplyr[...],
  DBI,
  htmlwidgets
)

# ===============================
# Query consulta contexto
# ===============================
#' @export
build_sql <- function(dt_de_utc, dt_ate_utc, setor = NULL, objeto = NULL) {

  base <- paste(
    "SELECT 
        oc.DATA_OC     AS ATRIBUTOS,
        oc.DT_HR_LOCAL AS DATE_TIME,
        o.NAME_OBJETO  AS OBJETO,
        s.NAME_SETOR   AS SETOR
     FROM objeto_contexto oc
     LEFT JOIN objeto o       ON o.CD_ID_OBJETO = oc.CD_ID_OBJETO
     LEFT JOIN setor s        ON s.CD_ID_SETOR  = o.CD_ID_SETOR
     WHERE o.FG_ATIVO = 1 AND VISIBLE >= 90"
  )

  where <- c()

  # intervalo de tempo (já em UTC formatado)
  where <- c(where, sprintf(
    "oc.DT_HR_LOCAL BETWEEN '%s' AND '%s'",
    fmt_sql_dt_utc(dt_de_utc), fmt_sql_dt_utc(dt_ate_utc)
  ))

  # remove registros com "erro" (AREA_DE_TRABALHO / _A / _B contendo ESTADO)
  # e NÃO elimina linhas com DATA_OC NULL/vazio
  where <- c(where, "
  (
    oc.DATA_OC IS NULL OR oc.DATA_OC = ''
    OR (
      JSON_VALID(oc.DATA_OC) = 1
      AND JSON_CONTAINS_PATH(oc.DATA_OC, 'one', '$.AREA_DE_TRABALHO.ESTADO') = 0
      AND JSON_CONTAINS_PATH(oc.DATA_OC, 'one', '$.AREA_DE_TRABALHO_A.ESTADO') = 0
      AND JSON_CONTAINS_PATH(oc.DATA_OC, 'one', '$.AREA_DE_TRABALHO_B.ESTADO') = 0
    )
  )
  ")

  if (!is.null(setor) && length(setor)) {
    in_setor <- .sql_in(setor)
    if (!is.null(in_setor)) where <- c(where, paste0("s.NAME_SETOR IN ", in_setor))
  }

  if (!is.null(objeto) && length(objeto)) {
    in_obj <- .sql_in(objeto)
    if (!is.null(in_obj)) where <- c(where, paste0("o.NAME_OBJETO IN ", in_obj))
  }

  paste0(base, "\n AND ", paste(where, collapse = " AND "), "\n")
}

#' @export
run_query <- function(pool, dt_de_utc, dt_ate_utc, setor = NULL, objeto = NULL) {
  sql <- build_sql(dt_de_utc, dt_ate_utc, setor = setor, objeto = objeto)
  DBI::dbGetQuery(pool, sql)
}

#' @export
fetch_choices <- function(pool) {
  sql <- "
    SELECT DISTINCT s.NAME_SETOR AS SETOR, o.NAME_OBJETO AS OBJETO
    FROM objeto o
    LEFT JOIN setor s ON s.CD_ID_SETOR = o.CD_ID_SETOR
    WHERE o.NAME_OBJETO IS NOT NULL AND o.NAME_OBJETO <> ''
  "
  x <- DBI::dbGetQuery(pool, sql)
  list(
    setores  = sort(unique(x$SETOR[!is.na(x$SETOR) & nzchar(x$SETOR)])),
    maquinas = sort(unique(x$OBJETO[!is.na(x$OBJETO) & nzchar(x$OBJETO)]))
  )
}
#' @export
tz_local <- function() {
  # se quiser fixo: "America/Sao_Paulo"
  Sys.timezone()
}
#' @export
to_utc <- function(x, tz = tz_local()) {
  # x: POSIXct "no tz certo" (local). Converte para UTC mantendo o instante.
  if (is.null(x) || all(is.na(x))) return(x)
  as.POSIXct(format(as.POSIXct(x, tz = tz), tz = "UTC", usetz = TRUE), tz = "UTC")
}
#' @export
from_utc <- function(x, tz = tz_local()) {
  # x: POSIXct em UTC. Converte para tz local mantendo o instante.
  if (is.null(x) || all(is.na(x))) return(x)
  as.POSIXct(format(as.POSIXct(x, tz = "UTC"), tz = tz, usetz = TRUE), tz = tz)
}

#' @export
fmt_sql_dt_utc <- function(x_utc) {
  # x_utc deve estar em tz="UTC"
  format(as.POSIXct(x_utc, tz = "UTC"), "%Y-%m-%d %H:%M:%S")
}

#' @export
merge_setrema_bobina_prensa <- function(ep_m,
                                        objeto     = "SETREMA",
                                        comp_bobina = "BOBINA.ESTADO",
                                        comp_prensa = "PRENSA.ESTADO",
                                        new_comp    = "SETREMA.ESTADO") {

  stopifnot(is.data.frame(ep_m))
  need <- c("SETOR","OBJETO","COMPONENTE","ESTADO","start_time","end_time","dur_secs")
  if (!all(need %in% names(ep_m))) return(ep_m)

  # recorta só o SETREMA + componentes alvo
  bp <- ep_m[ep_m$OBJETO == objeto & ep_m$COMPONENTE %in% c(comp_bobina, comp_prensa), , drop = FALSE]
  if (!nrow(bp)) return(ep_m)

  bob <- bp[bp$COMPONENTE == comp_bobina, , drop = FALSE]
  pre <- bp[bp$COMPONENTE == comp_prensa, , drop = FALSE]
  if (!nrow(bob) || !nrow(pre)) return(ep_m)

  # garante ordenação
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

  # estado vigente num tempo (via amostragem no meio do intervalo)
  get_state_at <- function(mid_num, seg) {
    s <- to_num(seg$start_time)
    e <- to_num(seg$end_time)
    idx <- findInterval(mid_num, s)  # maior start <= mid
    ok  <- idx >= 1 & mid_num < e[pmax(idx,1)]
    out <- rep(NA_character_, length(mid_num))
    out[ok] <- as.character(seg$ESTADO)[idx[ok]]
    out
  }

  sb <- toupper(trimws(get_state_at(mid, bob)))
  sp <- toupper(trimws(get_state_at(mid, pre)))

  # regra de fusão: OPERANDO domina; depois SETUP; senão PARADO
  merged <- ifelse(sb == "OPERANDO" | sp == "OPERANDO", "OPERANDO",
            ifelse(sb == "SETUP"    | sp == "SETUP",    "SETUP",
              ifelse(!is.na(sb), sb,
                ifelse(!is.na(sp), sp, NA_character_))))

  # monta segmentos e comprime (run-length)
  seg <- data.frame(
    SETOR      = bob$SETOR[1],
    OBJETO     = objeto,
    COMPONENTE = new_comp,
    ESTADO     = merged,
    start_time = as.POSIXct(t0, origin = "1970-01-01", tz = tz_use),
    end_time   = as.POSIXct(t1, origin = "1970-01-01", tz = tz_use),
    stringsAsFactors = FALSE
  )
  seg <- seg[!is.na(seg$ESTADO), , drop = FALSE]
  if (!nrow(seg)) return(ep_m)

  # comprime estados iguais consecutivos
  chg <- c(TRUE, seg$ESTADO[-1] != seg$ESTADO[-nrow(seg)])
  idx <- which(chg)
  end_idx <- c(idx[-1] - 1L, nrow(seg))

  out_seg <- data.frame(
    SETOR      = seg$SETOR[1],
    OBJETO     = objeto,
    COMPONENTE = new_comp,
    ESTADO     = seg$ESTADO[idx],
    start_time = seg$start_time[idx],
    end_time   = seg$end_time[end_idx],
    stringsAsFactors = FALSE
  )
  out_seg$dur_secs <- as.numeric(difftime(out_seg$end_time, out_seg$start_time, units = "secs"))
  out_seg <- out_seg[out_seg$dur_secs > 0, , drop = FALSE]

  # remove bobina/prensa do SETREMA e adiciona o novo
  keep_rows <- !(ep_m$OBJETO == objeto & ep_m$COMPONENTE %in% c(comp_bobina, comp_prensa))
  ep_new <- ep_m[keep_rows, , drop = FALSE]
  ep_new <- rbind(ep_new, out_seg)

  ep_new[order(ep_new$OBJETO, ep_new$COMPONENTE, ep_new$start_time), , drop = FALSE]
}

#' @export
.sql_escape <- function(x) gsub("'", "''", as.character(x), fixed = TRUE)
#' @export
.sql_in <- function(vec) {
  vec <- vec[!is.na(vec) & nzchar(vec)]
  if (!length(vec)) return(NULL)
  paste0("('", paste(.sql_escape(vec), collapse = "','"), "')")
}