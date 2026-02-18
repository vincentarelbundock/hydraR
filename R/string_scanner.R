# Unified string scanner for character-by-character parsing with quote/escape tracking.
# Use case: split overrides and resolver args by delimiters while ignoring delimiters
# nested in quotes, `${...}` interpolations, or bracket/brace/paren groups.

# Build scanner state for a single left-to-right pass over text.
.scanner_new <- function(text) {
    chars <- strsplit(text, "", fixed = TRUE)[[1]]
    structure(
        list(
            text = text,
            chars = chars,
            n = length(chars),
            pos = 1L,
            quote = "",
            escaped = FALSE,
            interp_depth = 0L,
            bracket_depth = 0L,
            brace_depth = 0L,
            paren_depth = 0L
        ),
        class = "string_scanner"
    )
}

# Check whether the scanner reached the end of input.
.scanner_done <- function(scanner) {
    scanner$pos > scanner$n
}

# Peek a character at the current position plus an offset.
.scanner_peek <- function(scanner, offset = 0L) {
    pos <- scanner$pos + offset
    if (pos < 1L || pos > scanner$n) {
        return(NULL)
    }
    scanner$chars[[pos]]
}

# Count consecutive backslashes before a character index.
.scanner_count_preceding_backslashes <- function(chars, index) {
    if (index <= 1L) {
        return(0L)
    }
    count <- 0L
    j <- index - 1L
    while (j >= 1L && identical(chars[[j]], "\\")) {
        count <- count + 1L
        j <- j - 1L
    }
    count
}

# Check whether `${` is escaped (for example `\${...}`).
.scanner_is_escaped_interpolation_start <- function(chars, dollar_index) {
    .scanner_count_preceding_backslashes(chars, dollar_index) %% 2L == 1L
}

# Advance the scanner position by `n` characters.
.scanner_advance <- function(scanner, n = 1L) {
    scanner$pos <- scanner$pos + n
    scanner
}

# Read the character under the current scanner position.
.scanner_current_char <- function(scanner) {
    .scanner_peek(scanner, 0L)
}

# Extract top-level interpolation spans from text.
.scanner_extract_interpolations <- function(text, current_path = character()) {
    chars <- strsplit(text, "", fixed = TRUE)[[1]]
    n <- length(chars)
    i <- 1L
    out <- list()

    is_interp_start <- function(idx) {
        idx < n &&
            identical(chars[[idx]], "$") &&
            identical(chars[[idx + 1L]], "{") &&
            !.scanner_is_escaped_interpolation_start(chars, idx)
    }

    while (i <= n) {
        if (!is_interp_start(i)) {
            i <- i + 1L
            next
        }

        start <- i
        i <- i + 2L

        depth <- 1L
        quote <- ""
        escaped <- FALSE
        brace_depth <- 0L

        expr_chars <- character(max(0L, n - start + 1L))
        k <- 0L

        while (i <= n && depth > 0L) {
            ch <- chars[[i]]

            if (escaped) {
                k <- k + 1L
                expr_chars[[k]] <- ch
                escaped <- FALSE
                i <- i + 1L
                next
            }

            if (nzchar(quote)) {
                k <- k + 1L
                expr_chars[[k]] <- ch
                if (identical(ch, "\\")) {
                    escaped <- TRUE
                } else if (identical(ch, quote)) {
                    quote <- ""
                }
                i <- i + 1L
                next
            }

            if (identical(ch, "\"") || identical(ch, "'")) {
                quote <- ch
                k <- k + 1L
                expr_chars[[k]] <- ch
                i <- i + 1L
                next
            }

            if (identical(ch, "\\")) {
                escaped <- TRUE
                k <- k + 1L
                expr_chars[[k]] <- ch
                i <- i + 1L
                next
            }

            if (is_interp_start(i)) {
                depth <- depth + 1L
                k <- k + 1L
                expr_chars[[k]] <- "$"
                k <- k + 1L
                expr_chars[[k]] <- "{"
                i <- i + 2L
                next
            }

            if (identical(ch, "{")) {
                brace_depth <- brace_depth + 1L
                k <- k + 1L
                expr_chars[[k]] <- ch
                i <- i + 1L
                next
            }

            if (identical(ch, "}")) {
                if (brace_depth > 0L) {
                    brace_depth <- brace_depth - 1L
                    k <- k + 1L
                    expr_chars[[k]] <- ch
                    i <- i + 1L
                    next
                }

                depth <- depth - 1L
                if (depth == 0L) {
                    i <- i + 1L
                    break
                }

                k <- k + 1L
                expr_chars[[k]] <- ch
                i <- i + 1L
                next
            }

            k <- k + 1L
            expr_chars[[k]] <- ch
            i <- i + 1L
        }

        if (depth != 0L) {
            .error_entry(text)
        }

        out[[length(out) + 1L]] <- list(
            start = start,
            end = i - 1L,
            expr = paste(expr_chars[seq_len(k)], collapse = "")
        )
    }

    out
}

# Check whether we are outside any nested container (`${}`, `[]`, `{}`, `()`).
.scanner_at_top_level <- function(scanner) {
    scanner$interp_depth == 0L &&
        scanner$bracket_depth == 0L &&
        scanner$brace_depth == 0L &&
        scanner$paren_depth == 0L
}

# Check whether we are currently inside a quoted string.
.scanner_in_quote <- function(scanner) {
    nzchar(scanner$quote)
}

# Update scanner nesting/escape/quote state based on the current character.
# This is the core state machine shared by all delimiter-search helpers.
.scanner_update_state <- function(scanner) {
    ch <- .scanner_current_char(scanner)
    if (is.null(ch)) {
        return(scanner)
    }

    # Escape always applies to the next character and then resets.
    if (scanner$escaped) {
        scanner$escaped <- FALSE
        return(scanner)
    }

    # Inside quotes, only escapes and matching quote terminators affect state.
    if (.scanner_in_quote(scanner)) {
        if (identical(ch, "\\")) {
            scanner$escaped <- TRUE
        } else if (identical(ch, scanner$quote)) {
            scanner$quote <- ""
        }
        return(scanner)
    }

    # Enter a quoted region when we see an opening quote at top parsing level.
    if (identical(ch, "\"") || identical(ch, "'")) {
        scanner$quote <- ch
        return(scanner)
    }

    # Allow escaping outside quotes as well for consistency with existing inputs.
    if (identical(ch, "\\")) {
        scanner$escaped <- TRUE
        return(scanner)
    }

    # `${` opens interpolation context; nested `${...}` is supported via depth.
    next_ch <- .scanner_peek(scanner, 1L)
    if (identical(ch, "$") && identical(next_ch, "{")) {
        scanner$interp_depth <- scanner$interp_depth + 1L
        return(scanner)
    }

    # `}` closes interpolation first, otherwise closes regular `{...}` groups.
    if (identical(ch, "}")) {
        if (scanner$interp_depth > 0L) {
            scanner$interp_depth <- scanner$interp_depth - 1L
        } else if (scanner$brace_depth > 0L) {
            scanner$brace_depth <- scanner$brace_depth - 1L
        }
        return(scanner)
    }

    # Track `[]`, `{}`, `()` only outside interpolation bodies.
    # This lets callers ignore commas/dots inside nested literal structures.
    if (scanner$interp_depth == 0L) {
        if (identical(ch, "[")) {
            scanner$bracket_depth <- scanner$bracket_depth + 1L
        } else if (identical(ch, "]") && scanner$bracket_depth > 0L) {
            scanner$bracket_depth <- scanner$bracket_depth - 1L
        } else if (identical(ch, "{")) {
            scanner$brace_depth <- scanner$brace_depth + 1L
        } else if (identical(ch, "(")) {
            scanner$paren_depth <- scanner$paren_depth + 1L
        } else if (identical(ch, ")") && scanner$paren_depth > 0L) {
            scanner$paren_depth <- scanner$paren_depth - 1L
        }
    }

    scanner
}

# Find the next target character outside quotes and escapes.
# Use case: locate separators like `=` or `,` that should not match inside quotes.
.scanner_find_unquoted <- function(text, target) {
    scanner <- .scanner_new(text)

    while (!.scanner_done(scanner)) {
        ch <- .scanner_current_char(scanner)

        if (!.scanner_in_quote(scanner) &&
            !scanner$escaped &&
            identical(ch, target)) {
            return(scanner$pos)
        }

        scanner <- .scanner_update_state(scanner)
        scanner <- .scanner_advance(scanner)
    }

    0L
}

# Find the next delimiter that is truly top-level.
# Use case: resolver argument parsing where commas inside `${...}` or lists/maps
# must not split the outer argument list.
.scanner_find_top_level_delim <- function(text, target) {
    scanner <- .scanner_new(text)

    while (!.scanner_done(scanner)) {
        scanner <- .scanner_update_state(scanner)

        ch <- .scanner_current_char(scanner)
        if (!is.null(ch) &&
            !.scanner_in_quote(scanner) &&
            !scanner$escaped &&
            identical(ch, target) &&
            .scanner_at_top_level(scanner)) {
            return(scanner$pos)
        }

        scanner <- .scanner_advance(scanner)
    }

    0L
}

# Split text by delimiter at top level only.
# Use case: parse resolver args like `a,\"b,c\",${x}` into logical outer parts.
.scanner_split_by_delim <- function(text, delim = ",") {
    if (!nzchar(text)) {
        return(character())
    }

    scanner <- .scanner_new(text)
    parts <- character()
    current_start <- 1L

    while (!.scanner_done(scanner)) {
        scanner <- .scanner_update_state(scanner)

        ch <- .scanner_current_char(scanner)
        if (!is.null(ch) &&
            !.scanner_in_quote(scanner) &&
            !scanner$escaped &&
            identical(ch, delim) &&
            .scanner_at_top_level(scanner)) {

            # Extract the segment before this top-level delimiter.
            part <- substr(text, current_start, scanner$pos - 1L)
            parts <- c(parts, trimws(part))
            current_start <- scanner$pos + 1L
        }

        scanner <- .scanner_advance(scanner)
    }

    # Add the final segment after the last delimiter.
    if (current_start <= scanner$n) {
        part <- substr(text, current_start, scanner$n)
        parts <- c(parts, trimws(part))
    } else if (current_start == scanner$n + 1L) {
        # Keep trailing-empty behavior for inputs like `"a,"`.
        parts <- c(parts, "")
    }

    # Normalize empty input to `character()` rather than `\"\"`.
    if (length(parts) == 1L && !nzchar(parts[[1L]])) {
        return(character())
    }

    parts
}

# Tokenize dotted key paths at top level.
# Use case: `db.pool.size` -> `c(\"db\", \"pool\", \"size\")`, while preserving
# quoted segments that include dots until outer quotes are stripped.
.scanner_tokenize_path <- function(key) {
    parts <- .scanner_split_by_delim(key, ".")

    # Strip optional outer quotes around each token and validate tokens are non-empty.
    for (i in seq_along(parts)) {
        parts[[i]] <- .strip_outer_quotes(parts[[i]])
        if (!nzchar(parts[[i]])) {
            .error_entry(key)
        }
    }

    parts
}
