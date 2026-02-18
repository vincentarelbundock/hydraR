hydra_test_paths <- function() {
    testdata_dir <- system.file("testdata", package = "hydraR")
    if (!nzchar(testdata_dir)) {
        testdata_dir <- normalizePath(file.path("inst", "testdata"), winslash = "/", mustWork = FALSE)
    }

    list(
        testdata_dir = testdata_dir,
        basic_conf_dir = file.path(testdata_dir, "conf")
    )
}

hydra_make_tmp_conf_dir <- function() {
    tmp_root <- tempfile("hydraR-conf-")
    dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)
    tmp_conf_dir <- file.path(tmp_root, "conf")
    dir.create(tmp_conf_dir, recursive = TRUE, showWarnings = FALSE)

    write_cfg <- function(path, lines) {
        dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
        writeLines(lines, con = path, useBytes = TRUE)
    }

    # Baseline fixture for defaults/group override tests.
    write_cfg(
        file.path(tmp_conf_dir, "db", "mysql.yaml"),
        c(
            "driver: mysql",
            "host: mysql-host",
            "port: 3306",
            "credentials:",
            "  user: root",
            "  password: root",
            "pool:",
            "  size: 10",
            "  timeout: 30"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "db", "postgres.yaml"),
        c(
            "driver: postgres",
            "host: postgres-host",
            "port: 5432",
            "credentials:",
            "  user: pg",
            "  password: pgpass",
            "pool:",
            "  size: 20"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config.yaml"),
        c(
            "defaults:",
            "  - db: mysql",
            "  - _self_",
            "db:",
            "  host: backup-host",
            "run:",
            "  lr: 0.1",
            "service:",
            "  retries: 3"
        )
    )

    write_cfg(
        file.path(tmp_conf_dir, "config_plus.yaml"),
        c(
            "defaults:",
            "  - _self_",
            "run:",
            "  lr: 0.25"
        )
    )

    write_cfg(
        file.path(tmp_conf_dir, "trainer", "optimizer", "adam.yaml"),
        c(
            "name: adam",
            "lr: 0.001"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "trainer", "optimizer", "sgd.yaml"),
        c(
            "name: sgd",
            "lr: 0.01"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_nested.yaml"),
        c(
            "defaults:",
            "  - trainer/optimizer: adam",
            "  - _self_",
            "run:",
            "  lr: 0.4"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_remove_db.yaml"),
        c(
            "defaults:",
            "  - db: mysql",
            "  - _self_",
            "run:",
            "  lr: 0.88"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "some_file.yaml"),
        c(
            "value_from_file: 42",
            "service:",
            "  mode: from_file"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_non_group.yaml"),
        c(
            "defaults:",
            "  - some_file",
            "  - _self_",
            "service:",
            "  mode: primary"
        )
    )

    write_cfg(
        file.path(tmp_conf_dir, "db", "base_mysql_ext.yaml"),
        c(
            "host: localhost",
            "port: 3306",
            "user: ???",
            "password: ???"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "db", "mysql_ext.yaml"),
        c(
            "defaults:",
            "  - base_mysql_ext",
            "  - _self_",
            "user: omry",
            "password: secret",
            "port: 3307",
            "encoding: utf8"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "db_schema", "base_mysql.yaml"),
        c(
            "host: schema-host",
            "port: 3306",
            "user: schema_user",
            "password: schema_pass"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "db", "mysql_cross.yaml"),
        c(
            "defaults:",
            "  - /db_schema/base_mysql@_here_",
            "  - _self_",
            "user: omry",
            "password: secret",
            "port: 3307",
            "encoding: utf8"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "db", "mysql_cross_bad.yaml"),
        c(
            "defaults:",
            "  - /db_schema/base_mysql",
            "  - _self_",
            "user: omry"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_extend_same.yaml"),
        c(
            "defaults:",
            "  - db: mysql_ext",
            "  - _self_"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_extend_cross.yaml"),
        c(
            "defaults:",
            "  - db: mysql_cross",
            "  - _self_"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_extend_cross_bad.yaml"),
        c(
            "defaults:",
            "  - db: mysql_cross_bad",
            "  - _self_"
        )
    )

    write_cfg(file.path(tmp_conf_dir, "user", "alice.yaml"), c("name: alice"))
    write_cfg(file.path(tmp_conf_dir, "user", "bob.yaml"), c("name: bob"))
    write_cfg(
        file.path(tmp_conf_dir, "config_defaults_env.yaml"),
        c(
            "defaults:",
            "  - user: ${oc.env:hydraR_TEST_USER,alice}",
            "  - _self_",
            "source: env"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_defaults_env_required.yaml"),
        c(
            "defaults:",
            "  - user: ${oc.env:hydraR_TEST_REQUIRED,???}",
            "  - _self_"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_defaults_env_nofallback.yaml"),
        c(
            "defaults:",
            "  - user: ${oc.env:hydraR_TEST_NOFALLBACK}",
            "  - _self_"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_defaults_env_bad_resolver.yaml"),
        c(
            "defaults:",
            "  - user: ${foo:BAR}",
            "  - _self_"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_resolvers.yaml"),
        c(
            "defaults:",
            "  - _self_",
            "  - db: mysql",
            "server:",
            "  port: 80",
            "\"a:b\": 10",
            "from_env: '${oc.env:hydraR_RESOLVER_USER,guest}'",
            "home: '/home/${oc.env:hydraR_RESOLVER_USER,guest}'",
            "pick_default: '${oc.select:missing,/tmp}'",
            "pick_null: '${oc.select:missing}'",
            "pick_existing: '${oc.select:server.port,0}'",
            "pick_colon: '${oc.select:\"a:b\"}'",
            "decoded_int: '${oc.decode:${oc.env:hydraR_RESOLVER_PORT,3307}}'",
            "decoded_list: '${oc.decode:\"[n1, n2]\"}'",
            "decoded_null: '${oc.decode:null}'",
            "decoded_dict: '${oc.decode:\"{a: 1, b: ${server.port}}\"}'",
            "workers:",
            "  node3: 10.0.0.2",
            "  node7: 10.0.0.9",
            "nodes: '${oc.dict.keys:workers}'",
            "ips: '${oc.dict.values:workers}'",
            "rusty_port: '${oc.deprecated:server.port}'",
            "made: '${oc.create:[10, 20]}'",
            "made_interp: '${oc.create:[10, ${server.port}]}'"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_bad_runtime_resolver.yaml"),
        c(
            "defaults:",
            "  - _self_",
            "x: ${foo.bar:1}"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_interp_cycle.yaml"),
        c(
            "defaults:",
            "  - _self_",
            "a: ${b}",
            "b: ${a}"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_interp_missing.yaml"),
        c(
            "defaults:",
            "  - _self_",
            "x: ${nope.value}"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_interp_relative_error.yaml"),
        c(
            "defaults:",
            "  - _self_",
            "x: ${..foo}"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_interp_fragment_error.yaml"),
        c(
            "defaults:",
            "  - _self_",
            "obj:",
            "  value: 1",
            "text: \"prefix-${obj}\""
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_interp_escaped.yaml"),
        c(
            "defaults:",
            "  - _self_",
            "dir: tmp",
            r"(literal: '\${dir}')",
            r"(mixed: 'prefix-\${dir}-${dir}')"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_delete_keys.yaml"),
        c(
            "defaults:",
            "  - _self_",
            "run:",
            "  lr: 0.5",
            "db:",
            "  host: localhost",
            "  nested:",
            "    keep: keep",
            "    drop: 123",
            "null_value: null"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_defaults_cycle_a.yaml"),
        c(
            "defaults:",
            "  - config_defaults_cycle_b",
            "  - _self_"
        )
    )
    write_cfg(
        file.path(tmp_conf_dir, "config_defaults_cycle_b.yaml"),
        c(
            "defaults:",
            "  - config_defaults_cycle_a",
            "  - _self_"
        )
    )

    tmp_conf_dir
}

hydra_snapshot_env <- function(vars) {
    values <- Sys.getenv(vars, unset = NA_character_)
    names(values) <- vars
    values
}

hydra_restore_env <- function(snapshot) {
    for (name in names(snapshot)) {
        value <- snapshot[[name]]
        if (is.na(value)) {
            Sys.unsetenv(name)
        } else {
            do.call(Sys.setenv, stats::setNames(list(value), name))
        }
    }
    invisible(NULL)
}
