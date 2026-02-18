helper_path <- system.file("tinytest", "helpers", "test_helpers.R", package = "hydraR")
if (!nzchar(helper_path)) {
    helper_path <- normalizePath(file.path("inst", "tinytest", "helpers", "test_helpers.R"), winslash = "/", mustWork = FALSE)
}
source(helper_path, local = TRUE)

basic_conf_dir <- hydra_test_paths()$basic_conf_dir

cfg <- compose(config_path = basic_conf_dir, config_name = "main", overrides = c("run.lr=0.2"))
print_lines <- capture.output(print(cfg))
expect_true(any(grepl("^<hydraRig> [0-9]+ top-level key", print_lines)))
expect_true(any(grepl("^\\|- db$", print_lines)))
expect_true(any(grepl("^\\|  \\|- host: 1$", print_lines)))

print_lines_shallow <- capture.output(print(cfg, max_depth = 1L))
expect_true(any(grepl("^\\|- db \\([0-9]+\\)$", print_lines_shallow)))

print_lines_shallow_values <- capture.output(print(cfg, max_depth = 1L, values = TRUE))
expect_true(any(grepl("^\\|- db \\([0-9]+\\): list\\(", print_lines_shallow_values)))

print_lines_values <- capture.output(print(cfg, values = TRUE))
expect_true(any(grepl('^\\|  \\|- host: "localhost"$', print_lines_values)))

old_max_depth <- getOption("hydraR.print.max_depth")
old_values <- getOption("hydraR.print.values")
on.exit(options(hydraR.print.max_depth = old_max_depth, hydraR.print.values = old_values), add = TRUE)

options(hydraR.print.max_depth = 1L, hydraR.print.values = TRUE)
print_lines_defaults <- capture.output(print(cfg))
expect_true(any(grepl("^\\|- db \\([0-9]+\\): list\\(", print_lines_defaults)))
