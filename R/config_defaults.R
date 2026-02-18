# Parse defaults entry.
.parse_defaults_entry <- function(entry) {
    if (is.null(entry)) {
        .error_entry(entry)
    }

    if (is.character(entry)) {
        if (!.is_scalar_string(entry)) {
            .error_entry(entry)
        }
        token <- entry[[1]]
        if (identical(token, TOKEN_SELF)) {
            return(list(type = DEFAULT_TYPE_SELF))
        }
        if (grepl("^(optional|override)\\b", token)) {
            .error_entry(entry)
        }
        return(list(type = DEFAULT_TYPE_CONFIG, path = token))
    }

    if (!is.list(entry) || length(entry) != 1L || is.null(names(entry)) || !nzchar(names(entry)[1])) {
        .error_entry(entry)
    }

    group <- names(entry)[1]
    option <- entry[[1]]

    if (grepl("@", group, fixed = TRUE) ||
        grepl("^(optional|override)\\b", group) ||
        is.null(option) ||
        !.is_scalar(option)) {
        .error_entry(entry)
    }

    list(type = DEFAULT_TYPE_GROUP, group = group, option = as.character(option[[1]]))
}

# Parse defaults list.
.parse_defaults_list <- function(defaults) {
    if (is.null(defaults)) {
        return(list(list(type = DEFAULT_TYPE_SELF)))
    }
    if (is.character(defaults)) {
        defaults <- as.list(defaults)
    }
    if (!is.list(defaults)) {
        .error_entry(defaults)
    }
    lapply(defaults, .parse_defaults_entry)
}

# Normalize defaults list.
.normalize_defaults_list <- function(defaults) {
    parsed <- .parse_defaults_list(defaults)
    has_self <- any(vapply(parsed, function(x) identical(x$type, DEFAULT_TYPE_SELF), logical(1)))
    if (!has_self) {
        warning("`defaults` list is missing `_self_`; appending `_self_` at the end.", call. = FALSE)
        parsed <- c(parsed, list(list(type = DEFAULT_TYPE_SELF)))
    }
    parsed
}
