#' Hydra-style script entrypoint
#'
#' Compose configuration from command-line style arguments and run a task
#' function, similar to Python Hydra's `@hydra.main`.
#'
#' @param fn Function called with the composed config as first argument.
#'   If `fn` accepts a second argument (or `...`), app arguments provided
#'   after `--` are passed as that second argument.
#' @param config_path Relative or absolute configuration directory path.
#' @param config_name Primary config filename without extension.
#' @param argv Character vector of command-line arguments. Defaults to
#'   `commandArgs(trailingOnly = TRUE)`.
#' @param resolve Logical; resolve OmegaConf interpolations before conversion.
#' @return Invisibly returns the value produced by `fn`. Returns `NULL`
#'   (invisibly) when `--help` or `-h` is requested. When `-m`/`--multirun`
#'   is enabled, returns a list with one task result per sweep combination.
#' @examples
#' \dontrun{
#' cp <- system.file("examples", package = "hydraR")
#' main(
#'   function(cfg) print(cfg),
#'   config_path = cp,
#'   config_name = "minimal",
#'   argv = character()
#' )
#' }
#' @export
main <- function(
    fn,
    config_path = getOption("hydraR.main.config_path", "conf"),
    config_name = getOption("hydraR.main.config_name", "config"),
    resolve = getOption("hydraR.main.resolve", TRUE),
    argv = commandArgs(trailingOnly = TRUE)
) {
    .validate_main_inputs(
        fn = fn,
        config_path = config_path,
        config_name = config_name,
        argv = argv,
        resolve = resolve
    )

    parsed <- .parse_main_argv(argv)

    if (isTRUE(parsed$help)) {
        .print_main_usage(
            config_path = config_path,
            config_name = config_name,
            script_name = .detect_main_script_name()
        )
        return(invisible(NULL))
    }

    if (!is.null(parsed$config_path)) {
        config_path <- parsed$config_path
    }
    if (!is.null(parsed$config_name)) {
        config_name <- parsed$config_name
    }
    if (!is.null(parsed$resolve)) {
        resolve <- parsed$resolve
    }

    override_sets <- .expand_main_overrides(
        overrides = parsed$overrides,
        multirun = parsed$multirun
    )

    if (!isTRUE(parsed$multirun)) {
        cfg <- compose(
            config_path = config_path,
            config_name = config_name,
            overrides = override_sets[[1L]],
            resolve = resolve
        )

        result <- .invoke_main_task(
            fn = fn,
            cfg = cfg,
            app_args = parsed$app_args
        )

        return(invisible(result))
    }

    results <- vector("list", length(override_sets))
    for (i in seq_along(override_sets)) {
        cfg <- compose(
            config_path = config_path,
            config_name = config_name,
            overrides = override_sets[[i]],
            resolve = resolve
        )

        results[[i]] <- .invoke_main_task(
            fn = fn,
            cfg = cfg,
            app_args = parsed$app_args
        )
    }

    invisible(results)
}

.validate_main_inputs <- function(fn, config_path, config_name, argv, resolve) {
    if (!is.function(fn)) {
        stop("`fn` must be a function.", call. = FALSE)
    }

    fn_formals <- names(formals(fn))
    if (!("..." %in% fn_formals) && length(fn_formals) < 1L) {
        stop("`fn` must accept at least one argument for the composed config.", call. = FALSE)
    }

    .assert_non_empty_character_scalar(config_path, "config_path")
    .assert_non_empty_character_scalar(config_name, "config_name")

    if (!is.character(argv) || anyNA(argv)) {
        stop("`argv` must be a character vector with no missing values.", call. = FALSE)
    }

    if (!is.logical(resolve) || length(resolve) != 1L || is.na(resolve)) {
        stop("`resolve` must be TRUE or FALSE.", call. = FALSE)
    }

    invisible(NULL)
}

.parse_main_argv <- function(argv) {
    parsed <- list(
        help = FALSE,
        multirun = FALSE,
        config_path = NULL,
        config_name = NULL,
        resolve = NULL,
        overrides = character(),
        app_args = character()
    )

    n <- length(argv)
    i <- 1L
    while (i <= n) {
        arg <- argv[[i]]

        if (identical(arg, "--")) {
            if (i < n) {
                parsed$app_args <- argv[(i + 1L):n]
            }
            break
        }

        if (arg %in% c("-h", "--help")) {
            parsed$help <- TRUE
            i <- i + 1L
            next
        }

        if (arg %in% c("-m", "--multirun")) {
            parsed$multirun <- TRUE
            i <- i + 1L
            next
        }

        if (startsWith(arg, "--multirun=")) {
            parsed$multirun <- .parse_main_logical(
                x = sub("^--multirun=", "", arg),
                flag = "--multirun"
            )
            i <- i + 1L
            next
        }

        if (identical(arg, "--config-path")) {
            if (i == n) {
                stop("`--config-path` requires a value.", call. = FALSE)
            }
            parsed$config_path <- argv[[i + 1L]]
            i <- i + 2L
            next
        }

        if (startsWith(arg, "--config-path=")) {
            parsed$config_path <- sub("^--config-path=", "", arg)
            i <- i + 1L
            next
        }

        if (identical(arg, "--config-name")) {
            if (i == n) {
                stop("`--config-name` requires a value.", call. = FALSE)
            }
            parsed$config_name <- argv[[i + 1L]]
            i <- i + 2L
            next
        }

        if (startsWith(arg, "--config-name=")) {
            parsed$config_name <- sub("^--config-name=", "", arg)
            i <- i + 1L
            next
        }

        if (identical(arg, "--resolve")) {
            parsed$resolve <- TRUE
            i <- i + 1L
            next
        }

        if (identical(arg, "--no-resolve")) {
            parsed$resolve <- FALSE
            i <- i + 1L
            next
        }

        if (startsWith(arg, "--resolve=")) {
            parsed$resolve <- .parse_main_logical(
                x = sub("^--resolve=", "", arg),
                flag = "--resolve"
            )
            i <- i + 1L
            next
        }

        if (startsWith(arg, "-")) {
            stop(sprintf("Unsupported CLI option: %s", arg), call. = FALSE)
        }

        parsed$overrides <- c(parsed$overrides, arg)
        i <- i + 1L
    }

    parsed
}

.expand_main_overrides <- function(overrides, multirun) {
    if (!isTRUE(multirun)) {
        return(list(overrides))
    }

    variants <- lapply(overrides, .main_override_variants)
    combinations <- .main_cartesian_product(variants)

    max_jobs <- getOption("hydraR.main.max_jobs", 1000L)
    if (length(combinations) > max_jobs) {
        stop(
            sprintf(
                "Sweep expands to %d jobs, above hydraR.main.max_jobs=%d.",
                length(combinations),
                max_jobs
            ),
            call. = FALSE
        )
    }

    combinations
}

.main_override_variants <- function(override) {
    eq_idx <- .find_main_top_level(override, "=")
    if (is.na(eq_idx)) {
        return(override)
    }

    lhs <- substr(override, 1L, eq_idx)
    rhs <- substr(override, eq_idx + 1L, nchar(override))
    rhs_values <- .split_main_top_level(rhs, ",")
    if (length(rhs_values) <= 1L) {
        return(override)
    }

    rhs_values <- trimws(rhs_values)
    if (any(!nzchar(rhs_values))) {
        stop(sprintf("Invalid sweep override: %s", override), call. = FALSE)
    }

    paste0(lhs, rhs_values)
}

.main_cartesian_product <- function(variants) {
    combos <- list(character())
    for (choices in variants) {
        next_combos <- vector("list", length(combos) * length(choices))
        idx <- 1L
        for (base in combos) {
            for (choice in choices) {
                next_combos[[idx]] <- c(base, choice)
                idx <- idx + 1L
            }
        }
        combos <- next_combos
    }
    combos
}

.find_main_top_level <- function(x, target) {
    chars <- strsplit(x, "", fixed = TRUE)[[1L]]
    if (length(chars) == 0L) {
        return(NA_integer_)
    }

    in_single <- FALSE
    in_double <- FALSE
    escaped <- FALSE
    depth_round <- 0L
    depth_square <- 0L
    depth_curly <- 0L

    for (i in seq_along(chars)) {
        ch <- chars[[i]]

        if (escaped) {
            escaped <- FALSE
            next
        }

        if ((in_single || in_double) && identical(ch, "\\")) {
            escaped <- TRUE
            next
        }

        if (!in_double && identical(ch, "'")) {
            in_single <- !in_single
            next
        }

        if (!in_single && identical(ch, "\"")) {
            in_double <- !in_double
            next
        }

        if (in_single || in_double) {
            next
        }

        if (identical(ch, "(")) {
            depth_round <- depth_round + 1L
            next
        }
        if (identical(ch, ")")) {
            depth_round <- max(0L, depth_round - 1L)
            next
        }
        if (identical(ch, "[")) {
            depth_square <- depth_square + 1L
            next
        }
        if (identical(ch, "]")) {
            depth_square <- max(0L, depth_square - 1L)
            next
        }
        if (identical(ch, "{")) {
            depth_curly <- depth_curly + 1L
            next
        }
        if (identical(ch, "}")) {
            depth_curly <- max(0L, depth_curly - 1L)
            next
        }

        if (depth_round == 0L &&
            depth_square == 0L &&
            depth_curly == 0L &&
            identical(ch, target)) {
            return(i)
        }
    }

    NA_integer_
}

.split_main_top_level <- function(x, sep) {
    chars <- strsplit(x, "", fixed = TRUE)[[1L]]
    if (length(chars) == 0L) {
        return("")
    }

    parts <- character()
    current <- ""
    in_single <- FALSE
    in_double <- FALSE
    escaped <- FALSE
    depth_round <- 0L
    depth_square <- 0L
    depth_curly <- 0L

    for (ch in chars) {
        if (escaped) {
            current <- paste0(current, ch)
            escaped <- FALSE
            next
        }

        if ((in_single || in_double) && identical(ch, "\\")) {
            current <- paste0(current, ch)
            escaped <- TRUE
            next
        }

        if (!in_double && identical(ch, "'")) {
            in_single <- !in_single
            current <- paste0(current, ch)
            next
        }

        if (!in_single && identical(ch, "\"")) {
            in_double <- !in_double
            current <- paste0(current, ch)
            next
        }

        if (!in_single && !in_double) {
            if (identical(ch, "(")) {
                depth_round <- depth_round + 1L
            } else if (identical(ch, ")")) {
                depth_round <- max(0L, depth_round - 1L)
            } else if (identical(ch, "[")) {
                depth_square <- depth_square + 1L
            } else if (identical(ch, "]")) {
                depth_square <- max(0L, depth_square - 1L)
            } else if (identical(ch, "{")) {
                depth_curly <- depth_curly + 1L
            } else if (identical(ch, "}")) {
                depth_curly <- max(0L, depth_curly - 1L)
            }
        }

        if (!in_single &&
            !in_double &&
            depth_round == 0L &&
            depth_square == 0L &&
            depth_curly == 0L &&
            identical(ch, sep)) {
            parts <- c(parts, current)
            current <- ""
            next
        }

        current <- paste0(current, ch)
    }

    c(parts, current)
}

.parse_main_logical <- function(x, flag) {
    x <- tolower(trimws(x))
    if (x %in% c("true", "1", "yes")) {
        return(TRUE)
    }
    if (x %in% c("false", "0", "no")) {
        return(FALSE)
    }
    stop(sprintf("`%s` expects a boolean value.", flag), call. = FALSE)
}

.invoke_main_task <- function(fn, cfg, app_args) {
    fn_formals <- names(formals(fn))
    if ("..." %in% fn_formals || length(fn_formals) >= 2L) {
        return(fn(cfg, app_args))
    }
    fn(cfg)
}

.detect_main_script_name <- function() {
    full_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- full_args[grepl("^--file=", full_args)]
    if (length(file_arg) == 0L) {
        return("app.R")
    }
    basename(sub("^--file=", "", file_arg[[1]]))
}

.print_main_usage <- function(config_path, config_name, script_name = "app.R") {
    cmd <- sprintf("Rscript %s", script_name)
    cat(
        paste(
            "Usage:",
            sprintf("  %s [OVERRIDE...] [-- APP_ARGS...]", cmd),
            "",
            "Hydra-like options:",
            sprintf("  --config-path PATH   (default: %s)", config_path),
            sprintf("  --config-name NAME   (default: %s)", config_name),
            "  -m, --multirun",
            "  --resolve | --no-resolve",
            "  -h, --help",
            "",
            "Examples:",
            sprintf("  %s 'run.lr=0.01'", cmd),
            sprintf("  %s --config-name minimal 'run.lr=0.2'", cmd),
            sprintf("  %s -m 'run.lr=0.01,0.02' 'selected_plan=A,B'", cmd),
            sprintf("  %s 'db=mysql' -- --input data.csv", cmd),
            sep = "\n"
        ),
        "\n"
    )
}
