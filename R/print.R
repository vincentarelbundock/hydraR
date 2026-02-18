# Normalize max depth.
.normalize_print_max_depth <- function(max_depth) {
    if (!is.numeric(max_depth) || length(max_depth) != 1L || is.na(max_depth) || max_depth < 1L) {
        .error_entry(max_depth)
    }
    as.integer(max_depth)
}

# Normalize print values.
.normalize_print_values <- function(values) {
    if (!is.logical(values) || length(values) != 1L || is.na(values)) {
        .error_entry(values)
    }
    isTRUE(values)
}

# Internal helper for tree lines.
.inline_value <- function(value) {
    txt <- paste(deparse(value), collapse = " ")
    if (nchar(txt, type = "width") > 80L) {
        txt <- paste0(substr(txt, 1L, 77L), "...")
    }
    txt
}

# Internal helper for tree lines.
.tree_lines <- function(node, prefix = "", depth = 1L, max_depth = 2L, values = FALSE) {
    if (!is.list(node) || length(node) == 0L) {
        return(character())
    }

    n <- length(node)
    nms <- names(node)
    if (is.null(nms)) {
        nms <- paste0("[[", seq_len(n), "]]")
    } else {
        missing <- is.na(nms) | nms == ""
        if (any(missing)) {
            nms[missing] <- paste0("[[", which(missing), "]]")
        }
    }

    lines <- character()
    for (i in seq_len(n)) {
        name <- nms[[i]]
        value <- node[[i]]
        is_last <- i == n
        branch <- if (is_last) "`- " else "|- "
        next_prefix <- paste0(prefix, if (is_last) "   " else "|  ")

        if (is.list(value) && length(value) > 0L) {
            if (depth < max_depth) {
                lines <- c(lines, paste0(prefix, branch, name))
                lines <- c(
                    lines,
                    .tree_lines(
                        node = value,
                        prefix = next_prefix,
                        depth = depth + 1L,
                        max_depth = max_depth,
                        values = values
                    )
                )
            } else {
                value_n <- length(value)
                line <- paste0(prefix, branch, name, " (", as.character(value_n), ")")
                if (isTRUE(values)) {
                    line <- paste0(line, ": ", .inline_value(value))
                }
                lines <- c(lines, line)
            }
        } else if (is.list(value) && length(value) == 0L) {
            line <- paste0(prefix, branch, name, ": ", as.character(0L))
            if (isTRUE(values)) {
                line <- paste0(prefix, branch, name, ": ", .inline_value(value))
            }
            lines <- c(lines, line)
        } else {
            line <- paste0(prefix, branch, name, ": ", as.character(length(value)))
            if (isTRUE(values)) {
                line <- paste0(prefix, branch, name, ": ", .inline_value(value))
            }
            lines <- c(lines, line)
        }
    }

    lines
}

#' Print method for hydraRig
#'
#' Prints the config as an ASCII key tree.
#'
#' The number of printed levels can be controlled with `max_depth`.
#' Defaults come from `options(hydraR.print.max_depth = <n>)`
#' (or `options(hydraR.print.levels = <n>)`) with default `4`.
#' Value printing defaults come from
#' `options(hydraR.print.values = <TRUE|FALSE>)` with default `FALSE`.
#'
#' @param x A hydraRig object.
#' @param max_depth Maximum tree depth to print.
#' @param values Logical; print node values instead of lengths, including
#' truncated node values when `max_depth` is reached.
#' @param ... Additional arguments passed to print.
#' @return Invisibly returns `x`.
#' @export
print.hydraRig <- function(
  x,
  max_depth = getOption("hydraR.print.max_depth", getOption("hydraR.print.levels", 4L)),
  values = getOption("hydraR.print.values", TRUE),
  ...
) {
    n_top <- length(x)
    cat(sprintf("<hydraRig> %d top-level key%s\n", n_top, if (n_top == 1L) "" else "s"))

    max_depth <- .normalize_print_max_depth(max_depth)
    values <- .normalize_print_values(values)
    lines <- .tree_lines(unclass(x), max_depth = max_depth, values = values)
    if (length(lines) > 0L) {
        cat(paste0(lines, collapse = "\n"), "\n", sep = "")
    }
    invisible(x)
}
