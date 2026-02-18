#' Compose configuration from a config directory
#'
#' @md
#' @param config_path Relative or absolute configuration directory path.
#' @param config_name Primary config filename without extension.
#' @param overrides Character vector of CLI-style overrides for Python Hydra.
#' @param resolve Logical; resolve OmegaConf interpolations before conversion.
#' @return A composed `HydraConfig` object.
#' @examples
#' cfg <- compose(config_path = "conf", config_name = "config", overrides = c("db=mysql"))
#' @export
compose <- function(
    config_path = "conf",
    config_name = "config",
    overrides = character(),
    resolve = TRUE
) {
    .validate_compose_inputs(
        config_path = config_path,
        config_name = config_name,
        overrides = overrides,
        resolve = resolve
    )
    path <- .resolve_config_path(config_path)
    .validate_config_dir(path)
    .require_reticulate()
    .ensure_python_compose_helpers()
    .sync_hydrar_env()

    overrides <- .normalize_overrides(overrides)
    prepared_config <- .prepare_config_dir_for_hydra(
        config_path = path,
        config_name = config_name
    )
    on.exit({
        if (!is.null(prepared_config$tmp_dir) && dir.exists(prepared_config$tmp_dir)) {
            unlink(prepared_config$tmp_dir, recursive = TRUE, force = TRUE)
        }
    }, add = TRUE)

    cfg <- .compose_with_hydra(
        prepared_config = prepared_config,
        overrides = overrides,
        resolve = resolve
    )

    structure(cfg, class = c("hydraRig", "list"))
}

# Resolve config path.
.resolve_config_path <- function(path) {
    if (!grepl("^(/|[A-Za-z]:[\\\\/]|\\\\\\\\)", path)) {
        path <- file.path(getwd(), path)
    }
    normalizePath(path, winslash = "/", mustWork = FALSE)
}

.validate_compose_inputs <- function(config_path, config_name, overrides, resolve) {
    .assert_non_empty_character_scalar(config_path, "config_path")
    .assert_non_empty_character_scalar(config_name, "config_name")
    if (!is.null(overrides) && (!is.character(overrides) || anyNA(overrides))) {
        stop("`overrides` must be a character vector (or NULL) with no missing values.", call. = FALSE)
    }
    if (!is.logical(resolve) || length(resolve) != 1L || is.na(resolve)) {
        stop("`resolve` must be TRUE or FALSE.", call. = FALSE)
    }
    invisible(NULL)
}

.assert_non_empty_character_scalar <- function(x, name) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
        stop(sprintf("`%s` must be a non-empty character scalar.", name), call. = FALSE)
    }
    invisible(NULL)
}

.validate_config_dir <- function(path) {
    if (!dir.exists(path)) {
        stop(sprintf("Configuration directory does not exist: %s", path), call. = FALSE)
    }
    if (file.access(path, mode = 4) != 0L) {
        stop(sprintf("Configuration directory is not readable: %s", path), call. = FALSE)
    }
    invisible(NULL)
}

.require_reticulate <- function() {
    if (!requireNamespace("reticulate", quietly = TRUE)) {
        stop("`compose()` requires the `reticulate` package.", call. = FALSE)
    }
    invisible(NULL)
}

.normalize_overrides <- function(overrides) {
    if (is.null(overrides)) {
        return(character())
    }
    overrides
}

.compose_state <- new.env(parent = emptyenv())
.compose_state$python_helpers_loaded <- FALSE

.python_helper_available <- function(name) {
    out <- tryCatch(reticulate::py[[name]], error = function(...) NULL)
    !is.null(out)
}

.ensure_python_compose_helpers <- function() {
    if (isTRUE(.compose_state$python_helpers_loaded) &&
        .python_helper_available("_hydrar_compose") &&
        .python_helper_available("_hydrar_sync_env")) {
        return(invisible(NULL))
    }

    initialized <- tryCatch({
        reticulate::py_run_string(paste(
            "from hydra import compose, initialize_config_dir",
            "from omegaconf import OmegaConf",
            "import os",
            "",
            "def _hydrar_sync_env(env_vars):",
            "    for key in list(os.environ.keys()):",
            "        if key.startswith('hydraR_'):",
            "            os.environ.pop(key, None)",
            "    for key, value in env_vars.items():",
            "        os.environ[str(key)] = str(value)",
            "",
            "def _hydrar_compose(config_dir, config_name, overrides, resolve=True):",
            "    if isinstance(overrides, str):",
            "        overrides_list = [overrides]",
            "    elif overrides is None:",
            "        overrides_list = []",
            "    else:",
            "        overrides_list = list(overrides)",
            "    with initialize_config_dir(config_dir=config_dir, version_base=None):",
            "        cfg = compose(",
            "            config_name=config_name,",
            "            overrides=overrides_list,",
            "        )",
            "    return OmegaConf.to_container(cfg, resolve=resolve)",
            sep = "\n"
        ))
        TRUE
    }, error = identity)

    if (inherits(initialized, "error")) {
        msg <- conditionMessage(initialized)
        if (grepl("No module named ['\\\"]hydra['\\\"]", msg, perl = TRUE)) {
            py <- tryCatch(reticulate::py_config()$python, error = function(...) "<unknown>")
            stop(
                paste0(
                    "Python module `hydra` is not installed in the selected Python environment: ",
                    py,
                    ". To let reticulate resolve an ephemeral managed environment, set ",
                    "`RETICULATE_PYTHON=managed` before loading hydraR. ",
                    "Alternatively, install dependencies with ",
                    "`reticulate::py_install(c('hydra-core', 'omegaconf'), envname = 'r-reticulate')` ",
                    "or set `RETICULATE_PYTHON` to an environment that already has them."
                ),
                call. = FALSE
            )
        }
        stop(sprintf("Python Hydra initialization failed: %s", msg), call. = FALSE)
    }

    .compose_state$python_helpers_loaded <- TRUE
    invisible(NULL)
}

.compose_with_hydra <- function(prepared_config, overrides, resolve) {
    composed <- tryCatch(
        reticulate::py$`_hydrar_compose`(
            config_dir = prepared_config$config_path,
            config_name = prepared_config$config_name,
            overrides = overrides,
            resolve = resolve
        ),
        error = identity
    )

    if (inherits(composed, "error")) {
        stop(sprintf("Python Hydra compose failed: %s", conditionMessage(composed)), call. = FALSE)
    }

    reticulate::py_to_r(composed)
}

# Prepare config dir for Python Hydra primary-config loading.
.prepare_config_dir_for_hydra <- function(config_path, config_name) {
    yaml_path <- file.path(config_path, paste0(config_name, ".yaml"))
    yml_path <- file.path(config_path, paste0(config_name, ".yml"))
    if (file.exists(yaml_path) || !file.exists(yml_path)) {
        return(list(config_path = config_path, config_name = config_name, tmp_dir = NULL))
    }

    tmp_dir <- tempfile("hydraR-conf-")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    .copy_tree(from = config_path, to = tmp_dir)

    yml_files <- list.files(tmp_dir, pattern = "\\.yml$", recursive = TRUE, full.names = TRUE)
    for (file in yml_files) {
        yaml_file <- sub("\\.yml$", ".yaml", file)
        if (!file.exists(yaml_file)) {
            file.copy(file, yaml_file, overwrite = FALSE)
        }
    }

    list(config_path = tmp_dir, config_name = config_name, tmp_dir = tmp_dir)
}

# Copy a directory tree.
.copy_tree <- function(from, to) {
    dirs <- list.dirs(from, recursive = TRUE, full.names = TRUE)
    for (dir in dirs) {
        rel <- substr(dir, nchar(from) + 1L, nchar(dir))
        rel <- sub("^/", "", rel)
        target_dir <- if (!nzchar(rel)) to else file.path(to, rel)
        dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    }

    files <- list.files(from, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    files <- files[!file.info(files)$isdir]
    for (file in files) {
        rel <- substr(file, nchar(from) + 1L, nchar(file))
        rel <- sub("^/", "", rel)
        target_file <- file.path(to, rel)
        dir.create(dirname(target_file), recursive = TRUE, showWarnings = FALSE)
        file.copy(file, target_file, overwrite = TRUE)
    }
}

# Sync hydraR_* env vars from R to python os.environ.
.sync_hydrar_env <- function() {
    env <- Sys.getenv()
    env <- env[grepl("^hydraR_", names(env))]
    py_env <- as.list(unname(env))
    names(py_env) <- names(env)

    reticulate::py$`_hydrar_sync_env`(py_env)
    invisible(NULL)
}
