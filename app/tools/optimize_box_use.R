# app/tools/optimize_box_use.R

`%||%` <- function(x, y) if (is.null(x)) y else x

.read_lines_utf8 <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}
.write_lines_utf8 <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
}

.find_box_use_block <- function(lines) {
  start <- grep("box\\s*::\\s*use\\s*\\(", lines, perl = TRUE)
  if (!length(start)) stop("Não encontrei box::use( no arquivo.")
  start <- start[1]

  in_s <- FALSE; in_d <- FALSE; esc <- FALSE
  depth <- 0L; seen_open <- FALSE

  for (i in start:length(lines)) {
    s <- lines[i]
    for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
      if (esc) { esc <- FALSE; next }
      if (ch == "\\") { esc <- TRUE; next }
      if (!in_d && ch == "'") { in_s <- !in_s; next }
      if (!in_s && ch == "\"") { in_d <- !in_d; next }
      if (in_s || in_d) next

      if (ch == "(") { depth <- depth + 1L; seen_open <- TRUE }
      if (ch == ")") { depth <- depth - 1L }
      if (seen_open && depth == 0L) return(list(start = start, end = i))
    }
  }
  stop("Encontrei box::use( mas não consegui achar o fechamento ')'.")
}

.is_box_use_call <- function(expr) {
  if (!is.call(expr)) return(FALSE)
  f <- expr[[1]]
  if (!is.call(f)) return(FALSE)
  if (!identical(f[[1]], as.name("::")) && !identical(f[[1]], as.name(":::"))) return(FALSE)
  pkg <- f[[2]]; fn <- f[[3]]
  is.symbol(pkg) && as.character(pkg) == "box" && is.symbol(fn) && as.character(fn) == "use"
}

.safe_namespace_exports <- function(pkg) {
  tryCatch({
    if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
    unique(getNamespaceExports(pkg))
  }, error = function(e) NULL)
}

# ------------------------------------------------------------
# ✅ Helper: tenta extrair trecho por BYTE ou por CHAR
# ------------------------------------------------------------
.extract_span <- function(s, c1, c2, mode = c("raw","chr")) {
  mode <- match.arg(mode)
  if (!nzchar(s)) return("")
  if (mode == "raw") {
    # bytes (UTF-8 ou nativo; a gente valida depois)
    raw <- charToRaw(s)
    nB  <- length(raw)
    if (c1 < 1 || c2 < c1 || c1 > nB) return("")
    if (c2 > nB) c2 <- nB
    return(rawToChar(raw[c1:c2]))
  } else {
    # chars
    nC <- nchar(s, type = "chars", allowNA = FALSE)
    if (c1 < 1 || c2 < c1 || c1 > nC) return("")
    if (c2 > nC) c2 <- nC
    return(substr(s, c1, c2))
  }
}

.apply_one_repl_validated <- function(s, c1, c2, expected_old, replacement) {
  # tenta RAW primeiro (bytes)
  seg_raw <- .extract_span(s, c1, c2, "raw")
  if (identical(seg_raw, expected_old)) {
    raw <- charToRaw(s)
    pre  <- if (c1 > 1) raw[1:(c1 - 1)] else raw(0)
    post <- if (c2 < length(raw)) raw[(c2 + 1):length(raw)] else raw(0)
    out  <- rawToChar(c(pre, charToRaw(replacement), post))
    return(list(ok = TRUE, mode = "raw", out = out, got = seg_raw))
  }

  # se RAW não bate, tenta CHAR
  seg_chr <- .extract_span(s, c1, c2, "chr")
  if (identical(seg_chr, expected_old)) {
    pre  <- if (c1 > 1) substr(s, 1, c1 - 1) else ""
    post <- if (c2 < nchar(s)) substr(s, c2 + 1, nchar(s)) else ""
    out  <- paste0(pre, replacement, post)
    return(list(ok = TRUE, mode = "chr", out = out, got = seg_chr))
  }

  list(ok = FALSE, mode = NA_character_, out = s, got = seg_raw %||% seg_chr)
}

.apply_repls <- function(lines, repls) {
  if (!nrow(repls)) {
    attr(lines, "apply_stats") <- list(applied_raw = 0L, applied_chr = 0L, skipped = 0L, skipped_rows = NULL)
    return(lines)
  }

  o <- order(repls$line, repls$col1, decreasing = TRUE)
  repls <- repls[o, , drop = FALSE]

  applied_raw <- 0L
  applied_chr <- 0L
  skipped     <- 0L
  skipped_rows <- list()

  for (k in seq_len(nrow(repls))) {
    ln <- repls$line[k]
    c1 <- repls$col1[k]
    c2 <- repls$col2[k]
    expected_old <- repls$expected_old[k]
    replacement  <- repls$replacement[k]

    s <- lines[ln]
    if (is.na(s)) next

    r <- .apply_one_repl_validated(s, c1, c2, expected_old, replacement)

    if (isTRUE(r$ok)) {
      lines[ln] <- r$out
      if (identical(r$mode, "raw")) applied_raw <- applied_raw + 1L else applied_chr <- applied_chr + 1L
    } else {
      skipped <- skipped + 1L
      if (length(skipped_rows) < 12L) {
        skipped_rows[[length(skipped_rows) + 1L]] <- list(
          line = ln, col1 = c1, col2 = c2, expected = expected_old, got = r$got, text = s
        )
      }
    }
  }

  attr(lines, "apply_stats") <- list(
    applied_raw = applied_raw,
    applied_chr = applied_chr,
    skipped     = skipped,
    skipped_rows = skipped_rows
  )
  lines
}

.extract_local_fun_defs <- function(exprs) {
  defs <- character(0)
  walk <- function(x) {
    if (is.call(x)) {
      head <- x[[1]]
      if (identical(head, as.name("<-")) || identical(head, as.name("=")) || identical(head, as.name("<<-"))) {
        lhs <- x[[2]]; rhs <- x[[3]]
        if (is.symbol(lhs) && is.call(rhs) && identical(rhs[[1]], as.name("function"))) {
          defs <<- c(defs, as.character(lhs))
        }
        walk(rhs); return(invisible(NULL))
      }
      for (i in seq_along(x)) walk(x[[i]])
    }
    invisible(NULL)
  }
  for (e in exprs) walk(e)
  unique(defs)
}

.get_box_context <- function(box_expr) {
  args <- as.list(box_expr)[-1]

  is_local_path_expr <- function(e) {
    txt <- paste(deparse(e), collapse = "")
    grepl("\\./|\\.\\./", txt)
  }

  alias_names   <- character(0)
  local_args    <- list()
  pkg_args      <- list()
  imported_pkgs <- character(0)

  for (a in args) {
    if (is.call(a) && identical(a[[1]], as.name("=")) && length(a) >= 3) {
      lhs <- a[[2]]
      if (is.symbol(lhs)) alias_names <- c(alias_names, as.character(lhs))
      local_args <- c(local_args, list(a))
      next
    }

    if (is_local_path_expr(a)) {
      local_args <- c(local_args, list(a))
      next
    }

    if (is.symbol(a)) {
      imported_pkgs <- c(imported_pkgs, as.character(a))
      pkg_args <- c(pkg_args, list(a))
      next
    }

    if (is.call(a) && identical(a[[1]], as.name("[")) && is.symbol(a[[2]])) {
      imported_pkgs <- c(imported_pkgs, as.character(a[[2]]))
      pkg_args <- c(pkg_args, list(a))
      next
    }

    pkg_args <- c(pkg_args, list(a))
  }

  list(
    alias_names   = unique(alias_names),
    local_args    = local_args,
    pkg_args      = pkg_args,
    imported_pkgs = unique(imported_pkgs)
  )
}

.rebuild_box_pruned <- function(box_expr, used_names, inject_map) {
  ctx <- .get_box_context(box_expr)

  pkg_brackets <- list()
  pkg_plain <- character(0)
  kept_other <- list()

  for (a in ctx$pkg_args) {
    if (is.symbol(a)) {
      pkg_plain <- c(pkg_plain, as.character(a)); next
    }

    if (is.call(a) && identical(a[[1]], as.name("[")) && is.symbol(a[[2]])) {
      pkg <- as.character(a[[2]])
      items <- as.list(a)[-c(1,2)]
      has_ellipsis <- any(vapply(items, function(z) is.symbol(z) && as.character(z) == "...", logical(1)))

      if (has_ellipsis) {
        ex <- .safe_namespace_exports(pkg)
        if (!is.null(ex)) {
          funs <- sort(intersect(used_names, ex))
        } else {
          funs <- unique(vapply(items, function(z) if (is.symbol(z)) as.character(z) else NA_character_, character(1)))
          funs <- funs[!is.na(funs) & funs != "..."]
          funs <- intersect(funs, used_names)
        }
      } else {
        funs <- unique(vapply(items, function(z) if (is.symbol(z)) as.character(z) else NA_character_, character(1)))
        funs <- funs[!is.na(funs)]
        funs <- intersect(funs, used_names)
      }

      if (length(funs)) pkg_brackets[[pkg]] <- sort(unique(c(pkg_brackets[[pkg]] %||% character(0), funs)))
      next
    }

    kept_other <- c(kept_other, list(a))
  }

  for (pkg in names(inject_map)) {
    funs <- sort(unique(inject_map[[pkg]]))
    if (!length(funs)) next
    pkg_brackets[[pkg]] <- sort(unique(c(pkg_brackets[[pkg]] %||% character(0), funs)))
  }

  for (pkg in names(pkg_brackets)) {
    pkg_brackets[[pkg]] <- intersect(pkg_brackets[[pkg]], used_names)
    if (!length(pkg_brackets[[pkg]])) pkg_brackets[[pkg]] <- NULL
  }

  pkg_plain <- intersect(unique(pkg_plain), used_names)

  new_args <- list()
  new_args <- c(new_args, ctx$local_args)

  for (pkg in sort(names(pkg_brackets))) {
    funs <- pkg_brackets[[pkg]]
    new_args <- c(new_args, list(as.call(c(list(as.name("["), as.name(pkg)), lapply(funs, as.name)))))
  }

  for (pkg in sort(pkg_plain)) {
    new_args <- c(new_args, list(as.name(pkg)))
  }

  new_args <- c(new_args, kept_other)

  txt <- vapply(new_args, function(x) paste(deparse(x), collapse=""), character(1))
  new_args <- new_args[!duplicated(txt)]

  as.call(c(list(box_expr[[1]]), new_args))
}

.collect_ns_uses <- function(pd, local_defs) {
  uses <- data.frame(
    pkg=character(), fun=character(), op=character(),
    line=integer(), col1=integer(), col2=integer(),
    stringsAsFactors = FALSE
  )

  ns_idx <- which(pd$token %in% c("NS_GET", "NS_GET_INT"))
  if (!length(ns_idx)) return(uses)

  for (i in ns_idx) {
    ln   <- pd$line1[i]
    c1ns <- pd$col1[i]
    c2ns <- pd$col2[i]
    op   <- if (pd$token[i] == "NS_GET_INT") ":::" else "::"

    sy <- pd[pd$line1 == ln & pd$token == "SYMBOL", , drop = FALSE]
    if (!nrow(sy)) next

    left  <- sy[sy$col2 < c1ns, , drop = FALSE]
    right <- sy[sy$col1 > c2ns, , drop = FALSE]
    if (!nrow(left) || !nrow(right)) next

    left  <- left[which.max(left$col2), , drop = FALSE]
    right <- right[which.min(right$col1), , drop = FALSE]

    pkg <- left$text
    fun <- right$text
    if (fun %in% local_defs) next

    uses <- rbind(uses, data.frame(pkg=pkg, fun=fun, op=op, line=ln, col1=left$col1, col2=right$col2))
  }

  uses
}

.collect_dollar_uses <- function(pd, local_defs, imported_pkgs, alias_names) {
  uses <- data.frame(
    pkg=character(), fun=character(), op=character(),
    line=integer(), col1=integer(), col2=integer(),
    stringsAsFactors = FALSE
  )

  dollar_idx <- which(pd$token == "$")
  if (!length(dollar_idx)) return(uses)

  for (i in dollar_idx) {
    ln  <- pd$line1[i]
    c1d <- pd$col1[i]
    c2d <- pd$col2[i]

    sy <- pd[pd$line1 == ln & pd$token == "SYMBOL", , drop = FALSE]
    if (!nrow(sy)) next

    left  <- sy[sy$col2 < c1d, , drop = FALSE]
    right <- sy[sy$col1 > c2d, , drop = FALSE]
    if (!nrow(left) || !nrow(right)) next

    left  <- left[which.max(left$col2), , drop = FALSE]
    right <- right[which.min(right$col1), , drop = FALSE]

    pkg <- left$text
    fun <- right$text

    # só mexe em pkg$fun se pkg estiver importado no box::use e não for alias
    if (!(pkg %in% imported_pkgs)) next
    if (pkg %in% alias_names) next
    if (fun %in% local_defs) next

    uses <- rbind(uses, data.frame(pkg=pkg, fun=fun, op="$", line=ln, col1=left$col1, col2=right$col2))
  }

  uses
}

.print_parse_context <- function(lines, err, pad = 10L) {
  msg <- conditionMessage(err)
  m <- regexec("<text>:(\\d+):(\\d+):", msg)
  r <- regmatches(msg, m)[[1]]
  if (length(r) >= 3) {
    ln <- as.integer(r[2])
    c1 <- as.integer(r[3])
    a <- max(1L, ln - pad)
    b <- min(length(lines), ln + pad)
    cat("\n❌ Parse quebrou perto de <text>:", ln, "col", c1, "\n")
    for (i in a:b) cat(sprintf("%5d | %s\n", i, lines[i]))
    cat("\nMensagem do parse:\n", msg, "\n\n")
  } else {
    cat("\n❌ Parse falhou. Mensagem:\n", msg, "\n\n")
  }
}

pull_and_prune_box_use <- function(file_in, file_out = "tmp.R", verbose = TRUE, debug = FALSE) {
  stopifnot(file.exists(file_in))
  lines <- .read_lines_utf8(file_in)

  blk <- .find_box_use_block(lines)
  box_text <- paste(lines[blk$start:blk$end], collapse = "\n")
  box_exprs <- parse(text = box_text, keep.source = FALSE)
  if (!length(box_exprs) || !.is_box_use_call(box_exprs[[1]])) stop("Não consegui parsear o box::use.")
  box_expr <- box_exprs[[1]]

  ctx <- .get_box_context(box_expr)
  alias_names   <- ctx$alias_names
  imported_pkgs <- ctx$imported_pkgs

  op <- options(keep.parse.data = TRUE)
  on.exit(options(op), add = TRUE)

  p <- parse(text = lines, keep.source = TRUE)
  pd <- getParseData(p, includeText = TRUE)

  exprs_all <- parse(text = lines, keep.source = FALSE)
  local_defs <- .extract_local_fun_defs(exprs_all)

  uses_ns <- .collect_ns_uses(pd, local_defs)
  uses_d  <- .collect_dollar_uses(pd, local_defs, imported_pkgs, alias_names)
  uses    <- rbind(uses_ns, uses_d)

  if (isTRUE(debug)) {
    cat("DEBUG: imported_pkgs =", paste(imported_pkgs, collapse = ", "), "\n")
    cat("DEBUG: ns uses =", nrow(uses_ns), " | $ uses =", nrow(uses_d), "\n")
  }

  conflicted <- character(0)
  if (nrow(uses)) {
    fun_pkgs <- split(uses$pkg, uses$fun)
    conflicted <- names(fun_pkgs)[vapply(fun_pkgs, function(v) length(unique(v)) > 1, logical(1))]
    if (length(conflicted)) uses <- uses[!(uses$fun %in% conflicted), , drop = FALSE]
  }

  # monta expected_old e replacement
  repls <- unique(uses[, c("line","col1","col2","pkg","fun","op")])
  repls$expected_old <- ifelse(repls$op == "$",
                               paste0(repls$pkg, "$", repls$fun),
                               paste0(repls$pkg, repls$op, repls$fun))
  repls$replacement  <- repls$fun

  lines2 <- .apply_repls(lines, repls)
  st <- attr(lines2, "apply_stats")

  inject_map <- split(uses$fun, uses$pkg)
  inject_map <- lapply(inject_map, function(v) sort(unique(v)))

  p2 <- try(parse(text = lines2, keep.source = TRUE), silent = TRUE)
  if (inherits(p2, "try-error")) {
    .print_parse_context(lines2, attr(p2, "condition") %||% simpleError(as.character(p2)))
    if (!is.null(st$skipped_rows) && length(st$skipped_rows)) {
      cat("\n⚠️ Exemplos SKIPPED (não substituídos porque o recorte não bateu 100%):\n")
      for (x in st$skipped_rows) {
        cat(sprintf(" - linha %d [%d:%d] expected='%s' got='%s'\n", x$line, x$col1, x$col2, x$expected, x$got))
      }
      cat("\n")
    }
    stop("O arquivo ficou inválido após as substituições. Veja o contexto acima.")
  }

  pd2 <- getParseData(p2, includeText = TRUE)
  used2 <- unique(pd2$text[pd2$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")])
  used2 <- setdiff(used2, local_defs)

  new_box <- .rebuild_box_pruned(box_expr, used2, inject_map)
  new_box_lines <- deparse(new_box, width.cutoff = 100)

  lines2 <- c(
    lines2[seq_len(blk$start - 1)],
    new_box_lines,
    lines2[(blk$end + 1):length(lines2)]
  )

  .write_lines_utf8(lines2, file_out)

  if (isTRUE(verbose)) {
    cat("✅ pull + prune concluído\n")
    cat(" - Entrada:", normalizePath(file_in), "\n")
    cat(" - Saída  :", normalizePath(file_out), "\n")
    cat(" - Total encontrados:", nrow(repls), "\n")
    cat(" - Aplicados RAW:", st$applied_raw, " | Aplicados CHR:", st$applied_chr, "\n")
    cat(" - Skipped (protegido por validação):", st$skipped, "\n")
    if (length(conflicted)) cat(" - ⚠️ Mantive qualificado por conflito:", paste(conflicted, collapse=", "), "\n")
  }

  invisible(list(
    file_in=file_in,
    file_out=file_out,
    found=nrow(repls),
    applied_raw=st$applied_raw,
    applied_chr=st$applied_chr,
    skipped=st$skipped,
    conflicted=conflicted
  ))
}
