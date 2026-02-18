#' Compose configuration from a config directory
#'
#' @md
#' @param config_path Relative or absolute configuration directory path.
#' @param config_name Primary config filename without extension.
#' @param overrides Character vector of CLI-style overrides. Supports both
#'   value overrides (e.g. `db.pool.size=20`) and config group selection
#'   overrides (e.g. `db=postgres`, `+db=postgres`, `~db`).
#' @template compose_resolvers
#' @return A composed `hydraRig` object.
#' @examples
#' cfg <- compose(config_path = "conf", config_name = "config", overrides = c("db=mysql"))
#' @export
compose <- function(config_path = "conf", config_name = "config", overrides = character()) {
    checkmate::assert_string(config_path, min.chars = 1)
    checkmate::assert_string(config_name, min.chars = 1)
    checkmate::assert_character(overrides, any.missing = FALSE, null.ok = TRUE)

    path <- .resolve_config_path(config_path)
    checkmate::assert_directory_exists(path, access = "r")

    .compose(
        config_path = path,
        config_name = config_name,
        overrides = .null_coalesce(overrides, character())
    )
}

# Resolve config path.
.resolve_config_path <- function(path) {
    if (!grepl("^(/|[A-Za-z]:[\\\\/]|\\\\\\\\)", path)) {
        path <- file.path(getwd(), path)
    }
    normalizePath(path, winslash = "/", mustWork = FALSE)
}

# Handle null coalesce.
.null_coalesce <- function(x, default) {
    if (is.null(x)) {
        return(default)
    }
    x
}

# Build compose context.
.new_compose_context <- function() {
    context <- new.env(parent = emptyenv())
    context$yaml_cache <- new.env(parent = emptyenv())
    context$group_options_cache <- new.env(parent = emptyenv())
    context$compose_stack <- character()
    context
}

# Handle config roots.
.config_roots <- function(config_path, search_path = NULL) {

    roots <- c(config_path, search_path)
    roots <- roots[!is.na(roots) & nzchar(roots)]
    unique(roots)
}

# Find yaml file.
.find_yaml_file <- function(roots, rel_no_ext) {

    for (root in roots) {
        for (ext in c(".yaml", ".yml")) {
            candidate <- file.path(root, paste0(rel_no_ext, ext))
            if (file.exists(candidate)) {
                return(candidate)
            }
        }
    }
    NULL
}

# Load yaml file.
.load_yaml_file <- function(path) {

    out <- tryCatch(
        yaml::yaml.load_file(path, eval.expr = FALSE),
        error = function(...) {
            .error_entry(path)
        }
    )

    if (is.null(out)) {
        return(list())
    }
    if (!is.list(out)) {
        .error_entry(path)
    }
    out
}

# Work with group options.
.group_options <- function(group, roots, context = NULL) {

    cache_key <- paste0(group, "\r", paste(roots, collapse = "\r"))
    if (!is.null(context) &&
        exists(cache_key, envir = context$group_options_cache, inherits = FALSE)) {
        return(get(cache_key, envir = context$group_options_cache, inherits = FALSE))
    }

    options <- character()
    for (root in roots) {
        grp <- file.path(root, group)
        if (!dir.exists(grp)) {
            next
        }
        files <- list.files(grp, pattern = "\\.(ya?ml)$", full.names = FALSE)
        if (length(files) == 0L) {
            next
        }
        opts <- sub("\\.(ya?ml)$", "", files)
        options <- c(options, opts)
    }
    resolved <- sort(unique(options))
    if (!is.null(context)) {
        assign(cache_key, resolved, envir = context$group_options_cache)
    }
    resolved
}

# Find default index.
.find_default_index <- function(defaults, predicate) {

    which(vapply(defaults, predicate, logical(1)))
}

# Find group default index.
.find_group_default_index <- function(defaults, group) {
    .find_default_index(
        defaults = defaults,
        function(entry) identical(entry$type, DEFAULT_TYPE_GROUP) && identical(entry$group, group)
    )
}

# Find config default index.
.find_config_default_index <- function(defaults, path) {
    .find_default_index(
        defaults = defaults,
        function(entry) identical(entry$type, DEFAULT_TYPE_CONFIG) && identical(entry$path, path)
    )
}

# Parse oc env args.
.parse_oc_env_args <- function(arg_text) {

    comma <- .first_unquoted_index(arg_text, ",")
    if (comma == 0L) {
        var <- trimws(arg_text)
        fallback <- NULL
    } else {
        var <- trimws(substr(arg_text, 1L, comma - 1L))
        fallback <- trimws(substr(arg_text, comma + 1L, nchar(arg_text)))
        fallback <- .strip_outer_quotes(fallback)
    }

    if (!nzchar(var)) {
        .error_entry(arg_text)
    }

    list(var = var, fallback = fallback)
}

# Resolve defaults interpolation expr.
.resolve_defaults_interpolation_expr <- function(expr) {

    expr_trim <- trimws(expr)
    prefix <- "oc.env:"
    if (!startsWith(expr_trim, prefix)) {
        .error_entry(expr_trim)
    }

    args <- .parse_oc_env_args(
        arg_text = substr(expr_trim, nchar(prefix) + 1L, nchar(expr_trim))
    )

    value <- Sys.getenv(args$var, unset = NA_character_)
    if (!is.na(value)) {
        return(value)
    }

    if (is.null(args$fallback)) {
        .error_entry(expr_trim)
    }

    if (identical(args$fallback, "???")) {
        .error_entry(expr_trim)
    }

    args$fallback
}

# Resolve defaults token.
.resolve_defaults_token <- function(token) {

    spans <- .extract_interpolations(token, current_path = "defaults")
    if (length(spans) == 0L) {
        return(token)
    }

    if (length(spans) == 1L &&
        spans[[1L]]$start == 1L &&
        spans[[1L]]$end == nchar(token)) {
        resolved <- .resolve_defaults_interpolation_expr(spans[[1L]]$expr)
        return(as.character(resolved))
    }

    parts <- character()
    cursor <- 1L
    for (span in spans) {
        if (span$start > cursor) {
            parts <- c(parts, substr(token, cursor, span$start - 1L))
        }
        resolved <- .resolve_defaults_interpolation_expr(span$expr)
        parts <- c(parts, as.character(resolved))
        cursor <- span$end + 1L
    }
    if (cursor <= nchar(token)) {
        parts <- c(parts, substr(token, cursor, nchar(token)))
    }
    paste0(parts, collapse = "")
}

# Resolve defaults interpolations.
.resolve_defaults_interpolations <- function(defaults) {

    out <- defaults
    for (i in seq_along(out)) {
        entry <- out[[i]]
        if (identical(entry$type, DEFAULT_TYPE_GROUP)) {
            entry$option <- .resolve_defaults_token(as.character(entry$option))
            out[[i]] <- entry
            next
        }
        if (identical(entry$type, DEFAULT_TYPE_CONFIG)) {
            entry$path <- .resolve_defaults_token(as.character(entry$path))
            out[[i]] <- entry
        }
    }

    out
}

# Parse config ref.
.parse_config_ref <- function(path_token) {

    at <- .first_unquoted_index(path_token, "@")
    if (at == 0L) {
        return(list(path = path_token, package = NULL))
    }

    path <- trimws(substr(path_token, 1L, at - 1L))
    pkg <- trimws(substr(path_token, at + 1L, nchar(path_token)))
    if (!nzchar(path) || !nzchar(pkg)) {
        .error_entry(path_token)
    }

    list(path = path, package = pkg)
}

# Resolve config ref.
.resolve_config_ref <- function(path_token, parent_group = "", in_primary = FALSE) {

    parsed <- .parse_config_ref(path_token)
    path <- parsed$path
    pkg <- parsed$package
    is_absolute <- startsWith(path, "/")

    if (!is.null(pkg) && !identical(pkg, TOKEN_HERE)) {
        .error_entry(path_token)
    }
    if (!isTRUE(in_primary) && is_absolute && is.null(pkg)) {
        .error_entry(path_token)
    }

    path_clean <- if (is_absolute) substring(path, 2L) else path
    if (!nzchar(path_clean)) {
        .error_entry(path_token)
    }

    if (!is_absolute && nzchar(parent_group)) {
        rel_no_ext <- file.path(parent_group, path_clean)
    } else {
        rel_no_ext <- path_clean
    }

    list(rel_no_ext = rel_no_ext)
}

# Read yaml config.
.read_yaml_config <- function(rel_no_ext, roots, compose_context = NULL) {

    cache_key <- paste0(rel_no_ext, "\r", paste(roots, collapse = "\r"))
    if (!is.null(compose_context) &&
        exists(cache_key, envir = compose_context$yaml_cache, inherits = FALSE)) {
        return(get(cache_key, envir = compose_context$yaml_cache, inherits = FALSE))
    }

    path <- .find_yaml_file(roots, rel_no_ext)
    if (is.null(path)) {
        .error_entry(rel_no_ext)
    }

    raw <- .load_yaml_file(path)
    defaults <- raw$defaults
    body <- raw
    body$defaults <- NULL
    group <- dirname(rel_no_ext)
    if (identical(group, ".")) {
        group <- ""
    }

    source <- list(
        rel_no_ext = rel_no_ext,
        group = group,
        path = path,
        defaults = defaults,
        body = body
    )
    if (!is.null(compose_context)) {
        assign(cache_key, source, envir = compose_context$yaml_cache)
    }
    source
}

# Check whether group override candidate.
.is_group_override_candidate <- function(override, roots, compose_context = NULL) {

    if (!("key_parts" %in% names(override)) || length(override$key_parts) != 1L) {
        return(FALSE)
    }

    group <- override$key_parts[[1]]
    if (!is.character(group) || length(group) != 1L || !nzchar(group)) {
        return(FALSE)
    }

    options <- .group_options(group, roots, context = compose_context)
    length(options) > 0L
}

# Check whether config default override candidate.
.is_config_default_override_candidate <- function(override, defaults) {

    if (!(override$op %in% c(OVERRIDE_OP_SET, OVERRIDE_OP_ADD, OVERRIDE_OP_ADD_OR_SET))) {
        return(FALSE)
    }
    if (length(override$key_parts) != 1L) {
        return(FALSE)
    }
    length(.find_config_default_index(defaults, override$key)) > 0L
}

# Assert group option exists.
.assert_group_option_exists <- function(group, option, roots, raw_override, compose_context = NULL) {

    options <- .group_options(group, roots, context = compose_context)
    if (option %in% options) {
        return(invisible(NULL))
    }

    .error_entry(raw_override)
}

# Replace group default.
.replace_group_default <- function(defaults, group, option, raw_override) {

    idx <- .find_group_default_index(defaults, group)

    if (length(idx) == 0L) {
        .error_entry(raw_override)
    }

    defaults[[idx[[1]]]]$option <- option
    defaults
}

# Insert group default.
.insert_group_default <- function(defaults, group, option, raw_override) {

    idx <- .find_group_default_index(defaults, group)
    if (length(idx) > 0L) {
        .error_entry(raw_override)
    }

    entry <- list(type = DEFAULT_TYPE_GROUP, group = group, option = option)
    self_idx <- which(vapply(defaults, function(x) identical(x$type, DEFAULT_TYPE_SELF), logical(1)))
    if (length(self_idx) > 0L) {
        pos <- self_idx[[1]]
        return(append(defaults, list(entry), after = pos - 1L))
    }
    c(defaults, list(entry))
}

# Remove default entry.
.remove_default_entry <- function(defaults, idx) {

    defaults[-idx]
}

# Remove group default.
.remove_group_default <- function(defaults, group, raw_override, expected_option = NULL) {

    idx <- .find_group_default_index(defaults, group)
    if (length(idx) == 0L) {
        .error_entry(raw_override)
    }

    selected <- defaults[[idx[[1]]]]$option
    if (!is.null(expected_option) && !identical(selected, expected_option)) {
        .error_entry(raw_override)
    }

    .remove_default_entry(defaults, idx[[1]])
}

# Remove config default.
.remove_config_default <- function(defaults, path, raw_override) {

    idx <- .find_config_default_index(defaults, path)
    if (length(idx) == 0L) {
        .error_entry(raw_override)
    }

    .remove_default_entry(defaults, idx[[1]])
}

# Apply defaults overrides.
.apply_defaults_overrides <- function(defaults, overrides, roots, compose_context = NULL) {

    if (length(overrides) == 0L) {
        return(list(defaults = defaults, value_overrides = overrides))
    }

    value_overrides <- character()
    resolved_defaults <- defaults

    for (raw in overrides) {
        fail <- function() .error_entry(raw)
        ov <- .parse_override(raw)

        if (identical(ov$op, OVERRIDE_OP_REMOVE)) {
            # Route `~key` to defaults removal only when `key` is actually a defaults entry.
            # Otherwise this is a regular config-key deletion and should be processed later.
            if (length(ov$key_parts) != 1L) {
                value_overrides <- c(value_overrides, raw)
                next
            }

            key <- ov$key_parts[[1]]
            group_idx <- .find_group_default_index(resolved_defaults, key)
            config_idx <- .find_config_default_index(resolved_defaults, key)
            has_expected <- !is.null(ov$value_raw)
            expected <- NULL

            if (length(group_idx) > 0L) {
                if (isTRUE(has_expected)) {
                    if (is.null(ov$value) || !is.atomic(ov$value) || length(ov$value) != 1L) {
                        fail()
                    }
                    expected <- as.character(ov$value[[1]])
                }
                resolved_defaults <- .remove_group_default(
                    defaults = resolved_defaults,
                    group = key,
                    raw_override = raw,
                    expected_option = expected
                )
                next
            }
            if (length(config_idx) > 0L) {
                if (has_expected) {
                    fail()
                }
                resolved_defaults <- .remove_config_default(
                    defaults = resolved_defaults,
                    path = key,
                    raw_override = raw
                )
                next
            }

            value_overrides <- c(value_overrides, raw)
            next
        }

        if (.is_config_default_override_candidate(ov, resolved_defaults)) {
            fail()
        }

        if (!.is_group_override_candidate(ov, roots, compose_context = compose_context)) {
            value_overrides <- c(value_overrides, raw)
            next
        }

        if (!is.atomic(ov$value) || length(ov$value) != 1L || is.null(ov$value)) {
            fail()
        }

        group <- ov$key_parts[[1]]
        option <- as.character(ov$value[[1]])
        .assert_group_option_exists(
            group = group,
            option = option,
            roots = roots,
            raw_override = raw,
            compose_context = compose_context
        )

        if (identical(ov$op, OVERRIDE_OP_ADD)) {
            resolved_defaults <- .insert_group_default(
                defaults = resolved_defaults,
                group = group,
                option = option,
                raw_override = raw
            )
        } else if (identical(ov$op, OVERRIDE_OP_ADD_OR_SET)) {
            idx <- .find_group_default_index(resolved_defaults, group)
            if (length(idx) > 0L) {
                resolved_defaults <- .replace_group_default(
                    defaults = resolved_defaults,
                    group = group,
                    option = option,
                    raw_override = raw
                )
            } else {
                resolved_defaults <- .insert_group_default(
                    defaults = resolved_defaults,
                    group = group,
                    option = option,
                    raw_override = raw
                )
            }
        } else {
            resolved_defaults <- .replace_group_default(
                defaults = resolved_defaults,
                group = group,
                option = option,
                raw_override = raw
            )
        }
    }

    list(defaults = resolved_defaults, value_overrides = value_overrides)
}

# Evaluate defaults.
.eval_defaults <- function(defaults, self_body, search_path, parent_group = "", in_primary = FALSE, compose_context = NULL) {

    cfg <- list()
    for (entry in defaults) {
        type <- entry$type
        cfg <- if (identical(type, DEFAULT_TYPE_SELF)) {
            .deep_merge(cfg, self_body)
        } else if (identical(type, DEFAULT_TYPE_GROUP)) {
            group_cfg <- .read_group_config(
                group = entry$group,
                option = entry$option,
                search_path = search_path,
                compose_context = compose_context
            )
            target_key <- gsub("/", ".", entry$group, fixed = TRUE)
            scoped_cfg <- .set_cfg_value(list(), target_key, group_cfg, create = TRUE)
            .deep_merge(cfg, scoped_cfg)
        } else if (identical(type, DEFAULT_TYPE_CONFIG)) {
            resolved <- .resolve_config_ref(
                path_token = entry$path,
                parent_group = parent_group,
                in_primary = in_primary
            )
            cfg_default <- .compose_non_primary(
                rel_no_ext = resolved$rel_no_ext,
                search_path = search_path,
                compose_context = compose_context
            )
            .deep_merge(cfg, cfg_default)
        } else {
            .error_entry(entry)
        }
    }

    cfg
}

# Internal helper for compose non primary.
.compose_non_primary <- function(rel_no_ext, search_path, compose_context) {

    if (rel_no_ext %in% compose_context$compose_stack) {
        chain <- c(compose_context$compose_stack, rel_no_ext)
        .error_entry(chain)
    }
    compose_context$compose_stack <- c(compose_context$compose_stack, rel_no_ext)
    on.exit({
        n <- length(compose_context$compose_stack)
        if (n > 0L) {
            compose_context$compose_stack <- compose_context$compose_stack[-n]
        }
    }, add = TRUE)

    source <- .read_yaml_config(
        rel_no_ext = rel_no_ext,
        roots = search_path,
        compose_context = compose_context
    )
    defaults <- .normalize_defaults_list(source$defaults)
    defaults <- .resolve_defaults_interpolations(defaults = defaults)

    .eval_defaults(
        defaults = defaults,
        self_body = source$body,
        search_path = search_path,
        parent_group = source$group,
        in_primary = FALSE,
        compose_context = compose_context
    )
}

# Read group config.
.read_group_config <- function(group, option, search_path, compose_context) {

    rel <- file.path(group, option)
    path <- .find_yaml_file(search_path, rel)
    if (is.null(path)) {
        .error_entry(paste0(group, "=", option))
    }

    .compose_non_primary(rel_no_ext = rel, search_path = search_path, compose_context = compose_context)
}

# Internal helper for compose.
.compose <- function(config_path, config_name = "config", overrides = character(), search_path = NULL) {

    overrides <- .null_coalesce(overrides, character())
    compose_context <- .new_compose_context()

    roots <- .config_roots(config_path, search_path)
    if (length(roots) == 0L) {
        .error_entry(roots)
    }
    primary <- .read_yaml_config(
        rel_no_ext = config_name,
        roots = roots,
        compose_context = compose_context
    )
    primary$roots <- roots

    defaults <- .normalize_defaults_list(primary$defaults)
    resolved <- .apply_defaults_overrides(
        defaults = defaults,
        overrides = overrides,
        roots = primary$roots,
        compose_context = compose_context
    )
    defaults <- .resolve_defaults_interpolations(defaults = resolved$defaults)

    cfg <- .eval_defaults(
        defaults = defaults,
        self_body = primary$body,
        search_path = primary$roots,
        parent_group = "",
        in_primary = TRUE,
        compose_context = compose_context
    )

    if (length(resolved$value_overrides)) {
        cfg <- .apply_overrides(cfg, resolved$value_overrides)
    }
    cfg <- .resolve_interpolations(cfg)
    structure(cfg, class = c("hydraRig", "list"))
}
