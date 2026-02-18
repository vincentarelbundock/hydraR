# Evaluate resolver oc dict.
.eval_resolver_oc_dict <- function(args, current_path, state, raw_expr, mode = c(RESOLVER_MODE_KEYS, RESOLVER_MODE_VALUES)) {
    mode <- match.arg(mode)
    .check_resolver_args(args, 1L, raw_expr)

    key <- .resolve_resolver_arg(args[[1L]], current_path = current_path, state = state, force_string = TRUE)
    key <- trimws(key)

    value <- .resolve_path_expr(
        path_expr = key,
        current_path = current_path,
        state = state,
        raw_expr = raw_expr
    )

    if (!.is_mapping_list(value)) {
        .error_entry(raw_expr)
    }

    if (identical(mode, RESOLVER_MODE_KEYS)) {
        return(unname(names(value)))
    }
    unname(value)
}
