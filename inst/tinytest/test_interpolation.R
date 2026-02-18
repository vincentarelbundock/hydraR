helper_path <- system.file("tinytest", "helpers.R", package = "hydraR")
if (!nzchar(helper_path)) {
    helper_path <- c(file.path("inst", "tinytest", "helpers.R"), "helpers.R")
    helper_path <- helper_path[file.exists(helper_path)][1]
}
source(helper_path, local = TRUE)

tmp_conf_dir <- hydra_make_tmp_conf_dir()

snapshot <- hydra_snapshot_env(c(
    "hydraR_TEST_USER",
    "hydraR_TEST_REQUIRED",
    "hydraR_TEST_NOFALLBACK",
    "hydraR_RESOLVER_USER",
    "hydraR_RESOLVER_PORT",
    "PYTHONWARNINGS"
))
on.exit(hydra_restore_env(snapshot), add = TRUE)

Sys.unsetenv("hydraR_TEST_USER")
cfg_defaults_env_fallback <- compose(config_path = tmp_conf_dir, config_name = "config_defaults_env")
expect_equal(cfg_defaults_env_fallback$user$name, "alice")
expect_equal(cfg_defaults_env_fallback$source, "env")

Sys.setenv(hydraR_TEST_USER = "bob")
cfg_defaults_env_set <- compose(config_path = tmp_conf_dir, config_name = "config_defaults_env")
expect_equal(cfg_defaults_env_set$user$name, "bob")

Sys.unsetenv("hydraR_TEST_REQUIRED")
expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_defaults_env_required")
)

Sys.unsetenv("hydraR_TEST_NOFALLBACK")
expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_defaults_env_nofallback")
)

expect_error(
    compose(config_path = tmp_conf_dir, config_name = "config_defaults_env_bad_resolver")
)

Sys.unsetenv("hydraR_RESOLVER_USER")
Sys.unsetenv("hydraR_RESOLVER_PORT")
cfg_resolvers_default <- compose(config_path = tmp_conf_dir, config_name = "config_resolvers")
expect_equal(cfg_resolvers_default$from_env, "guest")
expect_equal(cfg_resolvers_default$home, "/home/guest")
expect_equal(cfg_resolvers_default$pick_default, "/tmp")
expect_true(is.null(cfg_resolvers_default$pick_null))
expect_equal(cfg_resolvers_default$pick_existing, 80)
expect_equal(cfg_resolvers_default$pick_colon, 10)
expect_equal(cfg_resolvers_default$decoded_int, 3307)
expect_equal(cfg_resolvers_default$decoded_list, c("n1", "n2"))
expect_true(is.null(cfg_resolvers_default$decoded_null))
expect_equal(cfg_resolvers_default$decoded_dict$a, 1)
expect_equal(cfg_resolvers_default$decoded_dict$b, 80)
expect_equal(cfg_resolvers_default$nodes, c("node3", "node7"))
expect_equal(unlist(cfg_resolvers_default$ips), c("10.0.0.2", "10.0.0.9"))
expect_equal(cfg_resolvers_default$rusty_port, 80)
expect_equal(cfg_resolvers_default$made, c(10, 20))
expect_equal(cfg_resolvers_default$made_interp, c(10, 80))

#
# Sys.setenv(hydraR_RESOLVER_USER = "vincent")
# Sys.setenv(hydraR_RESOLVER_PORT = "3310")
# cfg_resolvers_set <- compose(config_path = tmp_conf_dir, config_name = "config_resolvers")
# expect_equal(cfg_resolvers_set$from_env, "vincent")
# expect_equal(cfg_resolvers_set$home, "/home/vincent")
# expect_equal(cfg_resolvers_set$decoded_int, 3310)
#
# expect_error(
#     compose(config_path = tmp_conf_dir, config_name = "config_bad_runtime_resolver")
# )
#
# expect_error(
#     compose(config_path = tmp_conf_dir, config_name = "config_interp_cycle")
# )
# expect_error(
#     compose(config_path = tmp_conf_dir, config_name = "config_interp_missing")
# )
# expect_error(
#     compose(config_path = tmp_conf_dir, config_name = "config_interp_relative_error")
# )
# cfg_interp_fragment <- compose(config_path = tmp_conf_dir, config_name = "config_interp_fragment_error")
# expect_equal(cfg_interp_fragment$obj$value, 1)
# expect_true(grepl("^prefix-\\{'value':\\s*1\\}$", cfg_interp_fragment$text))
#
# cfg_interp_escaped <- compose(config_path = tmp_conf_dir, config_name = "config_interp_escaped")
# expect_equal(cfg_interp_escaped$literal, "${dir}")
# expect_equal(cfg_interp_escaped$mixed, "prefix-${dir}-tmp")
