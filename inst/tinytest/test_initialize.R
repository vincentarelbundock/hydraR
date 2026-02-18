helper_path <- system.file("tinytest", "helpers.R", package = "hydraR")
if (!nzchar(helper_path)) {
    helper_path <- c(file.path("inst", "tinytest", "helpers.R"), "helpers.R")
    helper_path <- helper_path[file.exists(helper_path)][1]
}
source(helper_path, local = TRUE)

paths <- hydra_test_paths()
examples_dir <- paths$examples_dir
basic_conf_dir <- paths$basic_conf_dir

expect_true(dir.exists(examples_dir))
expect_true(file.exists(file.path(basic_conf_dir, "main.yml")))
expect_true(file.exists(file.path(basic_conf_dir, "minimal.yml")))
expect_true(file.exists(file.path(basic_conf_dir, "combine.yml")))
expect_true(file.exists(file.path(basic_conf_dir, "db", "mysql.yml")))
expect_true(file.exists(file.path(basic_conf_dir, "db", "postgres.yml")))
yaml_files <- list.files(
    basic_conf_dir,
    pattern = "\\.(ya?ml)$",
    recursive = TRUE,
    full.names = TRUE
)
expect_equal(length(yaml_files), 5L)

# Public API surface -----------------------------------------------------------
expected_exports <- c("compose", "main")
actual_exports <- getNamespaceExports("hydraR")
expect_equal(sort(actual_exports), sort(expected_exports))
expect_equal(length(actual_exports), length(expected_exports))

# Check S3 method is registered
expect_true("print.HydraConfig" %in% names(getNamespace("hydraR")))

# compose requires a valid config_path -----------------------------------------
expect_error(
    compose(config_path = tempfile("missing-conf-"), config_name = "main")
)

expect_error(
    compose(config_name = "main")
)
