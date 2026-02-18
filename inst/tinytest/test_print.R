helper_path <- system.file("tinytest", "helpers.R", package = "hydraR")
if (!nzchar(helper_path)) {
    helper_path <- c(file.path("inst", "tinytest", "helpers.R"), "helpers.R")
    helper_path <- helper_path[file.exists(helper_path)][1]
}
source(helper_path, local = TRUE)

basic_conf_dir <- hydra_test_paths()$basic_conf_dir

cfg <- compose(config_path = basic_conf_dir, config_name = "main", overrides = c("run.lr=0.2"))
print_lines <- capture.output(print(cfg))

expect_true(any(grepl("^db:$", print_lines)))
expect_true(any(grepl("^  host: localhost$", print_lines)))
expect_true(any(grepl("^run:$", print_lines)))
expect_true(any(grepl("^  lr: 0\\.2$", print_lines)))

print_lines_with_args <- capture.output(print(cfg, max_depth = 1L, values = TRUE))
expect_equal(print_lines_with_args, print_lines)
