helper_path <- system.file("tinytest", "helpers", "test_helpers.R", package = "hydraR")
if (!nzchar(helper_path)) {
    helper_path <- normalizePath(file.path("inst", "tinytest", "helpers", "test_helpers.R"), winslash = "/", mustWork = FALSE)
}
source(helper_path, local = TRUE)

tmp_conf_dir <- hydra_make_tmp_conf_dir()
cfg_non_group_add <- compose(config_path = tmp_conf_dir, config_name = "config", overrides = c("+new_key=abc"))
expect_equal(cfg_non_group_add$new_key, "abc")

cfg_upsert_new_key <- compose(config_path = tmp_conf_dir, config_name = "config_delete_keys", overrides = c("++new.branch=7"))
expect_equal(cfg_upsert_new_key$new$branch, 7)

cfg_upsert_existing_key <- compose(config_path = tmp_conf_dir, config_name = "config_delete_keys", overrides = c("++run.lr=0.9"))
expect_equal(cfg_upsert_existing_key$run$lr, 0.9)

cfg_delete_top <- compose(config_path = tmp_conf_dir, config_name = "config_delete_keys", overrides = c("~db"))
expect_false("db" %in% names(cfg_delete_top))

cfg_delete_nested <- compose(config_path = tmp_conf_dir, config_name = "config_delete_keys", overrides = c("~db.nested.drop"))
expect_false("drop" %in% names(cfg_delete_nested$db$nested))
expect_equal(cfg_delete_nested$db$nested$keep, "keep")

cfg_delete_expected <- compose(config_path = tmp_conf_dir, config_name = "config_delete_keys", overrides = c("~run.lr=0.5"))
expect_false("lr" %in% names(cfg_delete_expected$run))

cfg_delete_expected_null <- compose(config_path = tmp_conf_dir, config_name = "config_delete_keys", overrides = c("~null_value=null"))
expect_false("null_value" %in% names(cfg_delete_expected_null))

expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_delete_keys", overrides = c("~run.lr=1"))
)

expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_delete_keys", overrides = c("~db.nested.missing"))
)
