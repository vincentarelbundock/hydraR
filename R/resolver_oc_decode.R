# Evaluate resolver oc decode.
.eval_resolver_oc_decode <- function(args, current_path, state, raw_expr) {
    .check_resolver_args(args, 1L, raw_expr)

    value <- .resolve_resolver_arg(args[[1L]], current_path = current_path, state = state, force_string = TRUE)

    if (.is_null_token(value)) {
        return(NULL)
    }

    .parse_yaml_value(value)
}
