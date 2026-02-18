# Evaluate resolver oc deprecated.
.eval_resolver_oc_deprecated <- function(args, current_path, state, raw_expr) {
    .check_resolver_args(args, c(1L, 2L), raw_expr)

    new_key <- .resolve_resolver_arg(args[[1L]], current_path = current_path, state = state, force_string = TRUE)
    new_key <- trimws(new_key)
    old_key <- .path_label(current_path)

    message <- "'$OLD_KEY' is deprecated. Change your code and config to use '$NEW_KEY'."
    if (length(args) == 2L) {
        message <- .resolve_resolver_arg(args[[2L]], current_path = current_path, state = state, force_string = TRUE)
    }
    message <- gsub("$OLD_KEY", old_key, message, fixed = TRUE)
    message <- gsub("$NEW_KEY", new_key, message, fixed = TRUE)
    warning(message, call. = FALSE)

    .resolve_path_expr(
        path_expr = new_key,
        current_path = current_path,
        state = state,
        raw_expr = raw_expr
    )
}
