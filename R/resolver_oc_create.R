# Evaluate resolver oc create.
.eval_resolver_oc_create <- function(args, current_path, state, raw_expr) {
    .check_resolver_args(args, 1L, raw_expr)

    value <- .resolve_resolver_arg(args[[1L]], current_path = current_path, state = state, force_string = FALSE)

    # Already resolved to list or null
    if (.is_list_or_null(value)) {
        return(value)
    }

    # Not a string - return as-is
    if (!.is_scalar_string(value)) {
        return(value)
    }

    # Try direct YAML parse
    created <- tryCatch(
        yaml::yaml.load(value, eval.expr = FALSE),
        error = function(e) NULL
    )
    if (!is.null(created)) {
        return(created)
    }

    # Try value wrapper parse
    .parse_yaml_value(value)
}
