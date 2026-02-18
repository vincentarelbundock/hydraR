# Type predicates
.is_scalar <- function(x) {
    is.atomic(x) && length(x) == 1L
}

# Check whether scalar string.
.is_scalar_string <- function(x) {
    is.character(x) && length(x) == 1L
}

# Check whether nonempty string.
.is_nonempty_string <- function(x) {
    .is_scalar_string(x) && nzchar(x)
}

# Check whether list or null.
.is_list_or_null <- function(x) {
    is.list(x) || is.null(x)
}

# Check whether null token.
.is_null_token <- function(x) {
    .is_scalar_string(x) && tolower(trimws(x)) %in% NULL_TOKENS
}

# YAML parsing utility
.parse_yaml_value <- function(text) {
    if (!nzchar(trimws(text))) {
        return("")
    }

    # Try parsing as YAML with "value:" wrapper
    parsed <- tryCatch(
        yaml::yaml.load(paste0("value: ", text), eval.expr = FALSE),
        error = function(e) NULL
    )

    # If successful and has expected structure, extract value
    if (is.list(parsed) && "value" %in% names(parsed)) {
        return(parsed$value)
    }

    # Otherwise return original text
    text
}

# Find the first unquoted index.
.first_unquoted_index <- function(text, target) {
    .scanner_find_unquoted(text, target)
}

# Internal helper for strip outer quotes.
.strip_outer_quotes <- function(x) {
    if (!is.character(x) || length(x) != 1L || nchar(x) < 2L) {
        return(x)
    }
    first <- substr(x, 1L, 1L)
    last <- substr(x, nchar(x), nchar(x))
    if ((first == "\"" && last == "\"") || (first == "'" && last == "'")) {
        return(substr(x, 2L, nchar(x) - 1L))
    }
    x
}

# Tokenize key path.
.tokenize_key_path <- function(key) {
    .scanner_tokenize_path(key)
}

# Check whether a mapping path exists.
.path_exists <- function(cfg, parts) {
    if (length(parts) == 0L) {
        return(TRUE)
    }

    node <- cfg
    for (part in parts) {
        if (!is_named_list(node) || !(part %in% names(node))) {
            return(FALSE)
        }
        node <- node[[part]]
    }

    TRUE
}

# Set cfg value.
.set_cfg_value <- function(cfg, key, value, create = TRUE) {
    parts <- .tokenize_key_path(key)

    if (length(parts) == 0L) {
        return(if (is.null(value)) list(NULL) else value)
    }

    # Use recursive helper for setting
    .set_path_recursive_new(cfg, parts, value, create)
}

# Cleaner recursive setter
.set_path_recursive_new <- function(node, parts, value, create) {
    ensure_create <- function() {
        if (!create) {
            .error_entry(parts)
        }
    }

    if (!is_named_list(node)) {
        ensure_create()
        node <- list()
    }

    key <- parts[[1L]]

    # Base case: last part
    if (length(parts) == 1L) {
        if (is.null(value)) {
            node[key] <- list(NULL)
        } else {
            node[[key]] <- value
        }
        return(node)
    }

    # Recursive case: descend
    if (!(key %in% names(node))) {
        ensure_create()
        node[[key]] <- list()
    }

    child <- node[[key]]
    if (!is_named_list(child)) {
        ensure_create()
        child <- list()
    }

    node[[key]] <- .set_path_recursive_new(child, parts[-1L], value, create)
    node
}

# Remove cfg value.
.unset_cfg_value <- function(cfg, key, has_expected = FALSE, expected = NULL, raw_override = NULL) {
    parts <- .tokenize_key_path(key)
    if (length(parts) == 0L) {
        .error_entry(if (is.null(raw_override)) key else raw_override)
    }

    .unset_path_recursive(
        node = cfg,
        parts = parts,
        key = key,
        has_expected = has_expected,
        expected = expected,
        raw_override = raw_override
    )
}

# Recursively remove a key from a nested mapping path.
.unset_path_recursive <- function(node, parts, key, has_expected, expected, raw_override) {
    fail <- function() .error_entry(if (is.null(raw_override)) key else raw_override)

    if (!is_named_list(node)) {
        fail()
    }

    part <- parts[[1L]]
    if (!(part %in% names(node))) {
        fail()
    }

    if (length(parts) == 1L) {
        current <- node[[part]]
        if (isTRUE(has_expected) && !identical(current, expected)) {
            fail()
        }
        node[[part]] <- NULL
        return(node)
    }

    child <- node[[part]]
    node[[part]] <- .unset_path_recursive(
        node = child,
        parts = parts[-1L],
        key = key,
        has_expected = has_expected,
        expected = expected,
        raw_override = raw_override
    )
    node
}
