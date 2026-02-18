helper_path <- system.file("tinytest", "helpers.R", package = "hydraR")
if (!nzchar(helper_path)) {
    helper_path <- c(file.path("inst", "tinytest", "helpers.R"), "helpers.R")
    helper_path <- helper_path[file.exists(helper_path)][1]
}
source(helper_path, local = TRUE)

basic_conf_dir <- hydra_test_paths()$basic_conf_dir

# compose behavior -------------------------------------------------------------
cfg <- compose(config_path = basic_conf_dir, config_name = "main", overrides = c("run.lr=0.2"))
expect_true(inherits(cfg, "hydraRig"))
expect_true(inherits(cfg, "list"))
expect_equal(cfg$db$host, "localhost")
expect_equal(cfg$db$port, 3306)
expect_equal(cfg$db$credentials$user, "root")
expect_equal(cfg$db$pool$size, 10)
expect_equal(cfg$run$lr, 0.2)
expect_equal(cfg$service$retries, 3)
expect_equal(cfg$client$url, "http://localhost:80/")
expect_equal(cfg$paths$by_brackets, "localhost")
expect_equal(cfg$first_user, "user1")
expect_equal(cfg$second_user, "user2")
expect_equal(cfg$tree$nested$from_parent, 9)

cfg_switch <- compose(config_path = basic_conf_dir, config_name = "switch")
expect_equal(cfg_switch$mode, "switched")
expect_equal(cfg_switch$run$lr, 0.05)

cfg_main_override <- compose(
    config_path = basic_conf_dir,
    config_name = "main",
    overrides = c("selected_plan=B")
)
expect_equal(cfg_main_override$plan, "plan B")

cfg_null_overrides <- compose(config_path = basic_conf_dir, config_name = "main", overrides = NULL)
expect_equal(cfg_null_overrides$run$lr, 0.1)
