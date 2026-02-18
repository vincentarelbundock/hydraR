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

expect_error(print(cfg, max_depth = 1L))
expect_error(print(cfg, values = TRUE))

out_file <- tempfile("hydraR-print-", fileext = ".yaml")
print_out <- capture.output(print(cfg, filename = out_file))
expect_equal(length(print_out), 0L)
expect_true(file.exists(out_file))
written <- yaml::read_yaml(out_file)
expect_equal(written$run$lr, 0.2)

expect_error(print(cfg, filename = tempfile("hydraR-print-", fileext = ".txt")))
expect_error(print(cfg, filename = file.path(tempdir(), "does-not-exist", "out.yaml")))
