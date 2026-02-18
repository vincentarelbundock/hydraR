# Work with path join.
.path_join <- function(parts, sep, empty = "<root>") {

    if (length(parts) == 0L) {
        return(empty)
    }
    paste(parts, collapse = sep)
}

# Work with path label.
.path_label <- function(parts) {
    .path_join(parts, sep = ".", empty = "<root>")
}

# Work with path key.
.path_key <- function(parts) {
    .path_join(parts, sep = "\r", empty = "<root>")
}

ESCAPED_INTERP_OPEN_TOKEN <- "<<hydraR_ESCAPED_INTERP_OPEN>>"

# In plain text fragments, `\${...}` represents literal `${...}`.
.unescape_escaped_interpolations <- function(text) {
    if (!nzchar(text)) {
        return(text)
    }

    chars <- strsplit(text, "", fixed = TRUE)[[1]]
    n <- length(chars)
    i <- 1L
    out <- character()

    while (i <= n) {
        if (identical(chars[[i]], "\\") &&
            i + 2L <= n &&
            identical(chars[[i + 1L]], "$") &&
            identical(chars[[i + 2L]], "{")) {
            out <- c(out, ESCAPED_INTERP_OPEN_TOKEN)
            i <- i + 3L
            next
        }
        out <- c(out, chars[[i]])
        i <- i + 1L
    }

    paste0(out, collapse = "")
}

# Restore escaped `${` markers in resolved string output.
.restore_escaped_interpolations <- function(text) {
    if (!is.character(text) || length(text) != 1L || !nzchar(text)) {
        return(text)
    }
    gsub(ESCAPED_INTERP_OPEN_TOKEN, "${", text, fixed = TRUE)
}

# Check whether mapping list.
.is_mapping_list <- function(x) {
    is.list(x) && !is.null(names(x)) && any(nzchar(names(x)))
}

# Check whether sequence list.
.is_sequence_list <- function(x) {
    is.list(x) && !.is_mapping_list(x)
}

# Check whether indexable atomic.
.is_indexable_atomic <- function(x) {
    is.atomic(x) && is.null(dim(x))
}

# Convert to index token.
.as_index_token <- function(token) {

    if (!grepl("^[0-9]+$", token)) {
        .error_entry(token)
    }
    as.integer(token) + 1L
}

# Internal helper for node step.
.node_step <- function(node, part) {

    if (.is_mapping_list(node)) {
        if (!(part %in% names(node))) {
            return(list(ok = FALSE, value = NULL))
        }
        return(list(ok = TRUE, value = node[[part]]))
    }

    if (.is_sequence_list(node) || .is_indexable_atomic(node)) {
        idx <- suppressWarnings(.as_index_token(part))
        if (is.na(idx) || idx < 1L || idx > length(node)) {
            return(list(ok = FALSE, value = NULL))
        }
        return(list(ok = TRUE, value = node[[idx]]))
    }

    list(ok = FALSE, value = NULL)
}

# Get node by parts.
.get_node_by_parts <- function(cfg, parts, raw_expr = NULL, source_path = character()) {

    probe <- .try_get_node_by_parts(cfg, parts)
    if (isTRUE(probe$found)) {
        return(probe$value)
    }

    expr_label <- if (is.null(raw_expr)) {
        .path_label(parts)
    } else {
        paste0("${", raw_expr, "}")
    }

    .error_entry(expr_label)
}

# Try to resolve get node by parts.
.try_get_node_by_parts <- function(cfg, parts) {

    if (length(parts) == 0L) {
        return(list(found = TRUE, value = cfg))
    }

    node <- cfg
    for (part in parts) {
        step <- .node_step(node, part)
        if (!isTRUE(step$ok)) {
            return(list(found = FALSE, value = NULL))
        }
        node <- step$value
    }

    list(found = TRUE, value = node)
}

# Internal helper for extract interpolations.
.extract_interpolations <- function(text, current_path = character()) {
    .scanner_extract_interpolations(text, current_path = current_path)
}


# Parse path tokens.
.parse_path_tokens <- function(path_expr, raw_expr) {

    expr <- trimws(path_expr)
    if (!nzchar(expr)) {
        return(character())
    }
    fail <- function() .error_entry(raw_expr)

    chars <- strsplit(expr, "", fixed = TRUE)[[1]]
    n <- length(chars)
    i <- 1L
    tokens <- character()
    expect_token <- TRUE

    while (i <= n) {
        ch <- chars[[i]]

        if (!expect_token && !identical(ch, ".") && !identical(ch, "[")) {
            fail()
        }

        if (identical(ch, ".")) {
            if (expect_token) {
                fail()
            }
            expect_token <- TRUE
            i <- i + 1L
            next
        }

        if (identical(ch, "[")) {
            i <- i + 1L
            if (i > n) {
                fail()
            }

            buf <- character()
            quote <- ""
            escaped <- FALSE
            closed <- FALSE

            while (i <= n) {
                ch2 <- chars[[i]]
                if (escaped) {
                    buf <- c(buf, ch2)
                    escaped <- FALSE
                    i <- i + 1L
                    next
                }
                if (nzchar(quote)) {
                    buf <- c(buf, ch2)
                    if (identical(ch2, "\\")) {
                        escaped <- TRUE
                        i <- i + 1L
                        next
                    }
                    if (identical(ch2, quote)) {
                        quote <- ""
                    }
                    i <- i + 1L
                    next
                }
                if (identical(ch2, "\"") || identical(ch2, "'")) {
                    quote <- ch2
                    buf <- c(buf, ch2)
                    i <- i + 1L
                    next
                }
                if (identical(ch2, "]")) {
                    closed <- TRUE
                    i <- i + 1L
                    break
                }
                buf <- c(buf, ch2)
                i <- i + 1L
            }

            if (!closed) {
                fail()
            }

            token <- trimws(paste(buf, collapse = ""))
            token <- .strip_outer_quotes(token)
            if (!nzchar(token)) {
                fail()
            }
            tokens <- c(tokens, token)
            expect_token <- FALSE
            next
        }

        start <- i
        while (i <= n && !identical(chars[[i]], ".") && !identical(chars[[i]], "[")) {
            i <- i + 1L
        }
        token <- trimws(substr(expr, start, i - 1L))
        if (!nzchar(token)) {
            fail()
        }
        tokens <- c(tokens, token)
        expect_token <- FALSE
    }

    if (expect_token) {
        fail()
    }

    tokens
}

# Resolve interpolation target parts.
.resolve_interpolation_target_parts <- function(path_expr, current_path, raw_expr) {

    expr <- trimws(path_expr)
    if (!nzchar(expr)) {
        .error_entry(raw_expr)
    }

    dot_match <- regexpr("^\\.+", expr, perl = TRUE)
    lead <- if (dot_match[1] == 1L) attr(dot_match, "match.length") else 0L
    container_path <- if (length(current_path) > 0L) current_path[-length(current_path)] else character()

    if (lead > 0L) {
        up <- lead - 1L
        if (up > length(container_path)) {
            .error_entry(raw_expr)
        }
        if (up == 0L) {
            base <- container_path
        } else if (up == length(container_path)) {
            base <- character()
        } else {
            base <- container_path[seq_len(length(container_path) - up)]
        }
        remainder <- trimws(sub("^\\.+", "", expr, perl = TRUE))
        tokens <- .parse_path_tokens(remainder, raw_expr)
        return(c(base, tokens))
    }

    .parse_path_tokens(expr, raw_expr)
}

# Coerce fragment to string.
.coerce_fragment_to_string <- function(value, raw_expr, current_path) {

    if (is.null(value)) {
        return("null")
    }
    if (is.atomic(value) && length(value) == 1L) {
        return(as.character(value))
    }

    .error_entry(raw_expr)
}

# Resolve interpolation path.
.resolve_interpolation_path <- function(parts, state, raw_expr, current_path) {

    key <- .path_key(parts)

    if (exists(key, envir = state$resolved, inherits = FALSE)) {
        return(get(key, envir = state$resolved, inherits = FALSE))
    }

    if (key %in% state$resolving) {
        .error_entry(raw_expr)
    }

    state$resolving <- c(state$resolving, key)
    on.exit({
        state$resolving <- state$resolving[state$resolving != key]
    }, add = TRUE)

    raw_value <- .get_node_by_parts(
        cfg = state$root,
        parts = parts,
        raw_expr = raw_expr,
        source_path = current_path
    )
    resolved <- .resolve_value(raw_value, path = parts, state = state)
    assign(key, resolved, envir = state$resolved)
    resolved
}

# Resolve path expr.
.resolve_path_expr <- function(path_expr, current_path, state, raw_expr = NULL) {

    expr_label <- if (is.null(raw_expr)) path_expr else raw_expr
    target_parts <- .resolve_interpolation_target_parts(
        path_expr = path_expr,
        current_path = current_path,
        raw_expr = expr_label
    )
    .resolve_interpolation_path(
        parts = target_parts,
        state = state,
        raw_expr = expr_label,
        current_path = current_path
    )
}

# Try to resolve resolve path expr.
.try_resolve_path_expr <- function(path_expr, current_path, state, raw_expr = NULL) {

    expr_label <- if (is.null(raw_expr)) path_expr else raw_expr
    target_parts <- .resolve_interpolation_target_parts(
        path_expr = path_expr,
        current_path = current_path,
        raw_expr = expr_label
    )

    probe <- .try_get_node_by_parts(state$root, target_parts)
    if (!isTRUE(probe$found)) {
        return(list(found = FALSE, value = NULL))
    }

    value <- .resolve_interpolation_path(
        parts = target_parts,
        state = state,
        raw_expr = expr_label,
        current_path = current_path
    )
    list(found = TRUE, value = value)
}


# Evaluate interpolation expr.
.eval_interpolation_expr <- function(expr, current_path, state) {

    expr_trim <- trimws(expr)
    if (!nzchar(expr_trim)) {
        .error_entry(.path_label(current_path))
    }

    resolver_call <- .parse_resolver_call(expr_trim, current_path = current_path, state = state)
    if (!is.null(resolver_call)) {
        return(.eval_resolver_call(resolver_call, current_path = current_path, state = state))
    }

    expr_expanded <- .resolve_text_interpolations(
        text = expr_trim,
        current_path = current_path,
        state = state,
        force_string = TRUE
    )
    .resolve_path_expr(
        path_expr = expr_expanded,
        current_path = current_path,
        state = state,
        raw_expr = expr_trim
    )
}

# Resolve text interpolations.
.resolve_text_interpolations <- function(text, current_path, state, force_string = FALSE) {

    spans <- .extract_interpolations(text, current_path = current_path)
    if (length(spans) == 0L) {
        return(.unescape_escaped_interpolations(text))
    }

    if (!force_string &&
        length(spans) == 1L &&
        spans[[1L]]$start == 1L &&
        spans[[1L]]$end == nchar(text)) {
        return(.eval_interpolation_expr(spans[[1L]]$expr, current_path = current_path, state = state))
    }

    pieces <- character()
    cursor <- 1L
    for (span in spans) {
        if (span$start > cursor) {
            pieces <- c(
                pieces,
                .unescape_escaped_interpolations(substr(text, cursor, span$start - 1L))
            )
        }
        value <- .eval_interpolation_expr(span$expr, current_path = current_path, state = state)
        pieces <- c(pieces, .coerce_fragment_to_string(value, span$expr, current_path))
        cursor <- span$end + 1L
    }
    if (cursor <= nchar(text)) {
        pieces <- c(
            pieces,
            .unescape_escaped_interpolations(substr(text, cursor, nchar(text)))
        )
    }
    paste0(pieces, collapse = "")
}

# Resolve value.
.resolve_value <- function(node, path, state) {

    if (is.list(node)) {
        out <- node
        n <- length(node)
        nms <- names(node)
        for (i in seq_len(n)) {
            token <- if (!is.null(nms) && nzchar(nms[[i]])) {
                nms[[i]]
            } else {
                as.character(i - 1L)
            }
            child <- .resolve_value(node[[i]], path = c(path, token), state = state)
            if (is.null(child)) {
                out[i] <- list(NULL)
            } else {
                out[[i]] <- child
            }
        }
        return(out)
    }

    if (is.character(node) && length(node) == 1L && grepl("${", node, fixed = TRUE)) {
        resolved <- .resolve_text_interpolations(
            text = node,
            current_path = path,
            state = state
        )
        if (is.list(resolved)) {
            return(.resolve_value(resolved, path = path, state = state))
        }
        if (is.character(resolved) &&
            length(resolved) == 1L &&
            grepl("${", resolved, fixed = TRUE) &&
            !identical(resolved, node)) {
            original_spans <- .extract_interpolations(node, current_path = path)
            if (length(original_spans) > 0L) {
                return(.resolve_value(resolved, path = path, state = state))
            }
        }
        if (is.character(resolved) && length(resolved) == 1L) {
            resolved <- .restore_escaped_interpolations(resolved)
        }
        return(resolved)
    }

    node
}

# Resolve interpolations.
.resolve_interpolations <- function(cfg) {

    state <- new.env(parent = emptyenv())
    state$root <- cfg
    state$resolved <- new.env(parent = emptyenv())
    state$resolving <- character()

    .resolve_interpolation_path(
        parts = character(),
        state = state,
        raw_expr = "<root>",
        current_path = character()
    )
}
