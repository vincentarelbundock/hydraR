# Evaluate resolver oc select.
.eval_resolver_oc_select <- function(args, current_path, state, raw_expr) {
    .check_resolver_args(args, c(1L, 2L), raw_expr)

    key <- .resolve_resolver_arg(args[[1L]], current_path = current_path, state = state, force_string = TRUE)
    key <- trimws(key)
    if (!.is_nonempty_string(key)) {
        .error_entry(raw_expr)
    }

    selected <- .try_resolve_path_expr(
        path_expr = key,
        current_path = current_path,
        state = state,
        raw_expr = raw_expr
    )
    if (isTRUE(selected$found)) {
        return(selected$value)
    }
    if (length(args) != 2L) {
        return(NULL)
    }
    .resolve_resolver_arg(args[[2L]], current_path = current_path, state = state, force_string = FALSE)
}
