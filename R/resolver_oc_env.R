# Evaluate resolver oc env.
.eval_resolver_oc_env <- function(args, current_path, state, raw_expr) {
    .check_resolver_args(args, c(1L, 2L), raw_expr)

    var <- trimws(.resolve_resolver_arg(args[[1L]], current_path = current_path, state = state, force_string = TRUE))
    if (!.is_nonempty_string(var)) {
        .error_entry(raw_expr)
    }

    value <- Sys.getenv(var, unset = NA_character_)
    if (!is.na(value)) {
        return(value)
    }

    if (length(args) == 1L) {
        .error_entry(raw_expr)
    }

    fallback <- .resolve_resolver_arg(args[[2L]], current_path = current_path, state = state, force_string = FALSE)
    if (is.null(fallback) || .is_null_token(fallback)) {
        return(NULL)
    }

    if (!.is_scalar(fallback)) {
        .error_entry(raw_expr)
    }

    as.character(fallback)
}
