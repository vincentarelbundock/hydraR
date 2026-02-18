# Tokenize override.
.tokenize_override <- function(override) {
    body <- override
    op <- OVERRIDE_OP_SET

    if (startsWith(override, "++")) {
        body <- substring(override, 3L)
        op <- OVERRIDE_OP_ADD_OR_SET
    } else if (startsWith(override, "~")) {
        body <- substring(override, 2L)
        op <- OVERRIDE_OP_REMOVE
    } else if (startsWith(override, "+")) {
        body <- substring(override, 2L)
        op <- OVERRIDE_OP_ADD
    }

    if (!nzchar(body)) {
        .error_entry(override)
    }

    eq <- .first_unquoted_index(body, "=")
    if (eq == 0L && !identical(op, OVERRIDE_OP_REMOVE)) {
        .error_entry(override)
    }

    if (eq == 0L) {
        key <- trimws(body)
        value_raw <- NULL
    } else {
        key <- trimws(substr(body, 1L, eq - 1L))
        value_raw <- substr(body, eq + 1L, nchar(body))
    }

    if (!nzchar(key)) {
        .error_entry(override)
    }

    list(op = op, key = key, value_raw = value_raw)
}

# Parse override.
.parse_override <- function(override) {
    tok <- .tokenize_override(override)

    list(
        op = tok$op,
        key = tok$key,
        key_parts = .tokenize_key_path(tok$key),
        value_raw = tok$value_raw,
        value = if (is.null(tok$value_raw)) NULL else .parse_override_value(tok$value_raw)
    )
}

# Parse override value.
.parse_override_value <- function(value_text) {
    if (!nzchar(trimws(value_text))) {
        return("")
    }

    trimmed <- trimws(value_text)
    lower <- tolower(trimmed)

    # Handle boolean literals
    if (lower %in% c("true", "false")) {
        return(identical(lower, "true"))
    }

    # Handle null literals
    if (.is_null_token(trimmed)) {
        return(NULL)
    }

    # Handle numeric literals
    if (grepl("^[+-]?((\\d+\\.?\\d*)|(\\.\\d+))([eE][+-]?\\d+)?$", trimmed)) {
        return(as.numeric(trimmed))
    }

    # Try YAML parsing for complex values
    .parse_yaml_value(value_text)
}

# Apply overrides.
.apply_overrides <- function(cfg, overrides) {
    if (is.null(overrides) || length(overrides) == 0L) {
        return(cfg)
    }

    out <- cfg
    for (raw in overrides) {
        fail <- function() .error_entry(raw)
        ov <- .parse_override(raw)

        if (identical(ov$op, OVERRIDE_OP_REMOVE)) {
            out <- .unset_cfg_value(
                cfg = out,
                key = ov$key,
                has_expected = !is.null(ov$value_raw),
                expected = ov$value,
                raw_override = raw
            )
            next
        }

        if (identical(ov$op, OVERRIDE_OP_ADD) && .path_exists(out, ov$key_parts)) {
            fail()
        }

        out <- .set_cfg_value(out, ov$key, ov$value, create = TRUE)
    }
    out
}
