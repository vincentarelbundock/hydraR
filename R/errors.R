# Generic error helpers.

# Format value text.
.format_error_value <- function(value) {
    paste(deparse(value), collapse = "")
}

# Stop with generic unsupported-entry message.
.error_entry <- function(value) {
    stop(
        sprintf("Unsupported entry: %s", .format_error_value(value)),
        call. = FALSE
    )
}

# Argument validation helper for resolvers.
.check_resolver_args <- function(args, expected, raw_expr) {
    n <- length(args)

    if (is.numeric(expected) && length(expected) == 1L) {
        if (n != expected) {
            .error_entry(raw_expr)
        }
    } else if (is.vector(expected) && length(expected) == 2L) {
        if (!(n %in% expected)) {
            .error_entry(raw_expr)
        }
    }

    invisible(NULL)
}
