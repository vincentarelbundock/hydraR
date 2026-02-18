helper_path <- system.file("tinytest", "helpers.R", package = "hydraR")
if (!nzchar(helper_path)) {
    helper_path <- c(file.path("inst", "tinytest", "helpers.R"), "helpers.R")
    helper_path <- helper_path[file.exists(helper_path)][1]
}
source(helper_path, local = TRUE)

basic_conf_dir <- hydra_test_paths()$basic_conf_dir

# main() composes config and calls fn -------------------------------------
value_one_arg <- main(
    function(cfg) cfg$run$lr,
    config_path = basic_conf_dir,
    config_name = "main",
    argv = character()
)
expect_equal(value_one_arg, 0.1)

value_two_args <- main(
    function(cfg, app_args) {
        list(
            given = cfg$author$given,
            numeric = cfg$numeric,
            app_args = app_args
        )
    },
    config_path = basic_conf_dir,
    config_name = "main",
    argv = c("--config-name=minimal", "numeric=3", "--", "--input", "data.csv")
)
expect_equal(value_two_args$given, "Vincent")
expect_equal(value_two_args$numeric, 3)
expect_equal(value_two_args$app_args, c("--input", "data.csv"))

# main() defaults can come from options ----------------------------------------
old_opts <- options(
    hydraR.main.config_path = basic_conf_dir,
    hydraR.main.config_name = "minimal",
    hydraR.main.resolve = TRUE
)

value_from_options <- main(
    function(cfg) cfg$numeric,
    argv = character()
)
expect_equal(value_from_options, 2)
options(old_opts)

# multirun sweeping -------------------------------------------------------------
sweep_results <- main(
    function(cfg) sprintf("%s|%s", cfg$run$lr, cfg$plan),
    config_path = basic_conf_dir,
    config_name = "main",
    argv = c("-m", "run.lr=0.2,0.3", "selected_plan=A,B")
)
expect_equal(
    sweep_results,
    list("0.2|plan A", "0.2|plan B", "0.3|plan A", "0.3|plan B")
)

sweep_with_app_args <- main(
    function(cfg, app_args) paste(cfg$numeric, paste(app_args, collapse = " ")),
    config_path = basic_conf_dir,
    config_name = "minimal",
    argv = c("--multirun", "numeric=2,3", "--", "--input", "data.csv")
)
expect_equal(
    sweep_with_app_args,
    list("2 --input data.csv", "3 --input data.csv")
)

quoted_comma_not_swept <- main(
    function(cfg) cfg$author$given,
    config_path = basic_conf_dir,
    config_name = "minimal",
    argv = c("--multirun", "author.given='A,B'")
)
expect_equal(quoted_comma_not_swept, list("A,B"))

# --help prints usage and skips task execution ---------------------------------
state <- new.env(parent = emptyenv())
state$called <- FALSE
help_output <- capture.output(
    help_result <- main(
        function(cfg) {
            state$called <- TRUE
            cfg
        },
        config_path = basic_conf_dir,
        config_name = "main",
        argv = c("--help")
    )
)
expect_false(state$called)
expect_null(help_result)
expect_true(any(grepl("^Usage:", help_output)))

# parser errors ----------------------------------------------------------------
expect_error(
    main(
        function(cfg) cfg,
        config_path = basic_conf_dir,
        config_name = "main",
        argv = c("--unknown")
    ),
    "Unsupported CLI option"
)

expect_error(
    main(
        function(cfg) cfg,
        config_path = basic_conf_dir,
        config_name = "main",
        argv = c("--config-path")
    ),
    "--config-path.*requires a value"
)
