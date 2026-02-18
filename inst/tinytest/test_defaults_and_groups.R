helper_path <- system.file("tinytest", "helpers", "test_helpers.R", package = "hydraR")
if (!nzchar(helper_path)) {
    helper_path <- normalizePath(file.path("inst", "tinytest", "helpers", "test_helpers.R"), winslash = "/", mustWork = FALSE)
}
source(helper_path, local = TRUE)

# Defaults behavior on generated fixture --------------------------------------
tmp_conf_dir <- hydra_make_tmp_conf_dir()

# Config group override behavior ----------------------------------------------
cfg_group_set <- compose(config_path = tmp_conf_dir, config_name = "config", overrides = c("db=postgres"))
expect_equal(cfg_group_set$db$host, "backup-host")
expect_equal(cfg_group_set$db$driver, "postgres")
expect_equal(cfg_group_set$db$port, 5432)
expect_equal(cfg_group_set$db$credentials$user, "pg")

cfg_group_and_value <- compose(
    config_path = tmp_conf_dir,
    config_name = "config",
    overrides = c("db=postgres", "db.pool.size=7")
)
expect_equal(cfg_group_and_value$db$host, "backup-host")
expect_equal(cfg_group_and_value$db$driver, "postgres")
expect_equal(cfg_group_and_value$db$port, 5432)
expect_equal(cfg_group_and_value$db$pool$size, 7)

cfg_group_removed_defaults_only <- compose(config_path = tmp_conf_dir, config_name = "config", overrides = c("~db"))
expect_equal(cfg_group_removed_defaults_only$db$host, "backup-host")
expect_true(is.null(cfg_group_removed_defaults_only$db$driver))

expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config", overrides = c("+db=postgres"))
)

cfg_group_add <- compose(config_path = tmp_conf_dir, config_name = "config_plus", overrides = c("+db=postgres"))
expect_equal(cfg_group_add$db$host, "postgres-host")
expect_equal(cfg_group_add$db$driver, "postgres")
expect_equal(cfg_group_add$run$lr, 0.25)

cfg_nested_group_set <- compose(
    config_path = tmp_conf_dir,
    config_name = "config_nested",
    overrides = c("trainer/optimizer=sgd")
)
expect_equal(cfg_nested_group_set$trainer$optimizer$name, "sgd")
expect_equal(cfg_nested_group_set$run$lr, 0.4)

expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_plus", overrides = c("db=postgres"))
)

cfg_group_removed <- compose(config_path = tmp_conf_dir, config_name = "config_remove_db", overrides = c("~db"))
expect_false("db" %in% names(cfg_group_removed))
expect_equal(cfg_group_removed$run$lr, 0.88)

cfg_group_removed_with_option <- compose(
    config_path = tmp_conf_dir,
    config_name = "config_remove_db",
    overrides = c("~db=mysql")
)
expect_false("db" %in% names(cfg_group_removed_with_option))

expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_remove_db", overrides = c("~db=postgres"))
)

cfg_group_upsert_add <- compose(config_path = tmp_conf_dir, config_name = "config_plus", overrides = c("++db=postgres"))
expect_equal(cfg_group_upsert_add$db$driver, "postgres")
expect_equal(cfg_group_upsert_add$run$lr, 0.25)

cfg_group_upsert_replace <- compose(config_path = tmp_conf_dir, config_name = "config_remove_db", overrides = c("++db=postgres"))
expect_equal(cfg_group_upsert_replace$db$driver, "postgres")
expect_equal(cfg_group_upsert_replace$run$lr, 0.88)

cfg_non_group_default <- compose(config_path = tmp_conf_dir, config_name = "config_non_group")
expect_equal(cfg_non_group_default$value_from_file, 42)
expect_equal(cfg_non_group_default$service$mode, "primary")

expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_non_group", overrides = c("some_file=alt"))
)

cfg_non_group_removed <- compose(config_path = tmp_conf_dir, config_name = "config_non_group", overrides = c("~some_file"))
expect_false("value_from_file" %in% names(cfg_non_group_removed))
expect_equal(cfg_non_group_removed$service$mode, "primary")

cfg_extend_same <- compose(config_path = tmp_conf_dir, config_name = "config_extend_same")
expect_equal(cfg_extend_same$db$host, "localhost")
expect_equal(cfg_extend_same$db$port, 3307)
expect_equal(cfg_extend_same$db$user, "omry")
expect_equal(cfg_extend_same$db$password, "secret")
expect_equal(cfg_extend_same$db$encoding, "utf8")

cfg_extend_cross <- compose(config_path = tmp_conf_dir, config_name = "config_extend_cross")
expect_equal(cfg_extend_cross$db$host, "schema-host")
expect_equal(cfg_extend_cross$db$port, 3307)
expect_equal(cfg_extend_cross$db$user, "omry")
expect_equal(cfg_extend_cross$db$password, "secret")
expect_equal(cfg_extend_cross$db$encoding, "utf8")

expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_extend_cross_bad")
)

expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_defaults_cycle_a")
)
