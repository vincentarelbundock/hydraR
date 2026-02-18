# hydraR


Modern data analysis projects need to store and retrieve many configuration settings, such as secret keys and tokens, paths, tuning parameters, database ports, and authentication values. The `hydraR` package for `R` provides a powerful and flexible composition workflow to manage such configuration settings. This package is written entirely in `R`, but is designed to be (mostly) API-compatible two very popular Python libraries: OmegaConf and Hydra.

OmegaConf is a hierarchical configuration system with interpolation and resolvers ([website](https://omegaconf.readthedocs.io/)). Hydra is a composition framework built on OmegaConf for defaults-driven configuration and runtime overrides ([website](https://hydra.cc/)). hydraR is implements a focused Hydra/OmegaConf-style composition workflow for YAML configs in `R`.

## Public API

The user-facing API is intentionally minimal:

``` r
compose(config_path, config_name, overrides = ...)
```

## Install

``` bash
remotes::install_github("vincentarelbundock/hydraR")
```

## Quick Start

Fixture layout used in these examples:

``` text
inst/testdata/conf/
  main.yml
  switch.yml
```

Example:

``` r
pkgload::load_all()
```

    ℹ Loading hydraR

``` r
conf_dir <- system.file("testdata", "conf", package = "hydraR")

cfg <- compose(
  config_path = conf_dir,
  config_name = "main",
  overrides = c("run.lr=0.01", "selected_plan=B")
)

print(cfg, max_depth = 4)
```

    <hydraRig> 15 top-level keys
    |- db
    |  |- host: "localhost"
    |  |- port: 3306L
    |  |- credentials
    |  |  |- user: "root"
    |  |  `- password: "root"
    |  `- pool
    |     `- size: 10L
    |- run
    |  `- lr: 0.01
    |- service
    |  `- retries: 3L
    |- server
    |  |- host: "localhost"
    |  `- port: 80L
    |- client
    |  |- url: "http://localhost:80/"
    |  |- server_port: 80L
    |  `- description: "Client of http://localhost:80/"
    |- paths
    |  `- by_brackets: "localhost"
    |- plans
    |  |- A: "plan A"
    |  `- B: "plan B"
    |- selected_plan: "B"
    |- plan: "plan B"
    |- owner
    |  |- height: 180L
    |  `- weight: 75L
    |- player
    |  |- height: 180L
    |  `- weight: 75L
    |- tree
    |  |- value: 9L
    |  `- nested
    |     `- from_parent: 9L
    |- users: c("user1", "user2")
    |- first_user: "user1"
    `- second_user: "user2"

## Alternative Configs

``` r
compose(config_path = conf_dir, config_name = "main")$run$lr
```

    [1] 0.1

``` r
compose(config_path = conf_dir, config_name = "switch")$run$lr
```

    [1] 0.05

## Value Overrides

Set or replace values with `key=value`:

``` r
cfg <- compose(
  config_path = conf_dir,
  config_name = "main",
  overrides = c("service.retries=7", "selected_plan=B")
)
cfg$service$retries
```

    [1] 7

``` r
cfg$plan
```

    [1] "plan B"

Use `+key=value` to add a key that must not already exist:

``` r
cfg <- compose(config_path = conf_dir, config_name = "main", overrides = c("+new_key=abc"))
cfg$new_key
```

    [1] "abc"

Use `++key=value` to upsert (insert or replace):

``` r
compose(config_path = conf_dir, config_name = "main", overrides = c("++new.branch=7"))$new$branch
```

    [1] 7

Use `~key` (or `~key=expected_value`) to delete keys:

``` r
cfg <- compose(config_path = conf_dir, config_name = "main", overrides = c("~service.retries"))
is.null(cfg$service$retries)
```

    [1] TRUE

`~` can also target nested paths (for example `~service.retries`).

## Config Group Overrides

Config-group selection overrides are supported when your config directory contains group folders (for example `db/mysql.yaml`, `db/postgres.yaml`) and defaults entries such as:

``` yaml
defaults:
  - db: mysql
  - _self_
```

Then:

- `db=postgres` replaces the selected group option.
- `+db=postgres` inserts a new group selection if absent.
- `++db=postgres` upserts group selection.
- `~db` removes the group defaults entry.

## Environment Variables

Use `${oc.env:VAR_NAME,fallback}` to read from the process environment.

Set variables before composing:

``` bash
export APP_USER=alice
export APP_PORT=3310
```

Example config values:

``` yaml
db:
  user: ${oc.env:APP_USER,guest}
  password: ${oc.env:APP_PASSWORD,???}
  port: ${oc.decode:${oc.env:APP_PORT,3306}}
```

- `guest` is used if `APP_USER` is unset.
- `???` makes `APP_PASSWORD` required (compose errors if missing).
- `oc.decode` turns `"3306"`/`"3310"` into numeric values.

## Interpolation

`compose()` resolves `${...}` interpolations after composition and overrides.

``` r
cfg_interp <- compose(
  config_path = conf_dir,
  config_name = "main",
  overrides = c("selected_plan=B")
)

cfg_interp$client$url
```

    [1] "http://localhost:80/"

``` r
cfg_interp$client$description
```

    [1] "Client of http://localhost:80/"

``` r
cfg_interp$plan
```

    [1] "plan B"

## Extending Configs

Defaults inside non-primary configs are supported, so a config can extend another config and override selected fields.

Same-group extension example (`db/mysql_ext.yaml`):

``` yaml
defaults:
  - base_mysql_ext
user: omry
password: secret
port: 3307
encoding: utf8
```

Cross-group extension into the current package:

``` yaml
defaults:
  - /db_schema/base_mysql@_here_
```

## Development

``` bash
make document
make test
```

## Contributing

See `AGENTS.md` for repository contribution guidelines.

Before opening a PR, run:

``` bash
make document
make test
make check
```
