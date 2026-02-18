# Find the first top level delim.
.first_top_level_delim <- function(text, target) {
    .scanner_find_top_level_delim(text, target)
}

# Internal helper for unescape resolver arg.
.unescape_resolver_arg <- function(text) {

    chars <- strsplit(text, "", fixed = TRUE)[[1]]
    n <- length(chars)
    if (n == 0L) {
        return("")
    }

    out <- character()
    i <- 1L
    escapable <- c("\\", ",", " ", "$", "{", "}", "[", "]", "(", ")", ":", "=")
    while (i <= n) {
        ch <- chars[[i]]
        if (identical(ch, "\\") && i < n) {
            next_ch <- chars[[i + 1L]]
            if (next_ch %in% escapable) {
                out <- c(out, next_ch)
                i <- i + 2L
                next
            }
        }
        out <- c(out, ch)
        i <- i + 1L
    }

    paste(out, collapse = "")
}

# Split resolver args.
.split_resolver_args <- function(args_text) {
    if (!nzchar(args_text)) {
        return(character())
    }

    parts <- .scanner_split_by_delim(args_text, ",")
    vapply(parts, .unescape_resolver_arg, character(1), USE.NAMES = FALSE)
}
# Resolve resolver arg.
.resolve_resolver_arg <- function(arg, current_path, state, force_string = FALSE) {

    token <- trimws(arg)
    token <- .strip_outer_quotes(token)

    value <- token
    if (grepl("${", token, fixed = TRUE)) {
        value <- .resolve_text_interpolations(
            text = token,
            current_path = current_path,
            state = state,
            force_string = force_string
        )
    } else if (!force_string) {
        value <- .parse_yaml_value(token)
    }

    if (is.character(value) && length(value) == 1L) {
        value <- .restore_escaped_interpolations(value)
    }

    if (force_string) {
        if (!is.atomic(value) || length(value) != 1L) {
            .error_entry(arg)
        }
        return(as.character(value))
    }

    value
}

# Parse resolver call.
.parse_resolver_call <- function(expr, current_path, state) {

    colon <- .first_top_level_delim(expr, ":")
    if (colon == 0L) {
        return(NULL)
    }

    name_raw <- trimws(substr(expr, 1L, colon - 1L))
    if (!nzchar(name_raw)) {
        return(NULL)
    }

    name <- name_raw
    if (grepl("${", name_raw, fixed = TRUE)) {
        name <- .resolve_text_interpolations(
            text = name_raw,
            current_path = current_path,
            state = state,
            force_string = TRUE
        )
    }
    if (!is.atomic(name) || length(name) != 1L) {
        .error_entry(name_raw)
    }

    args_text <- if (colon < nchar(expr)) substr(expr, colon + 1L, nchar(expr)) else ""
    list(
        name = trimws(as.character(name)),
        args = .split_resolver_args(args_text),
        raw = expr
    )
}
