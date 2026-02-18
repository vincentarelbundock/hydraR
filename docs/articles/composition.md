# Interpolation and Composition

Composition builds one final config from multiple YAML files. In
`hydraR`, this is controlled by `compose(config_name = ...)`,
`defaults`, and `_self_`.

## Directory and files

To illustrate how to use these features, we will use YAML files
distributed alongside the `hydraR` package: `minimal.yml`, `main.yml`,
`combine.yml`, `db/mysql.yml`, and `db/postgres.yml`. These files are
stored in the `examples/` directory of the package. We can find this
directory on the file system using:

``` r
system.file("examples", package = "hydraR")

[1] "/path/to/R/library/hydraR/examples"
```

To avoid specifying the config directory path and config name every time
we call [`compose()`](../reference/compose.md), we set global options:

``` r
library(hydraR)

options(
  hydraR.compose.config_path = system.file("examples", package = "hydraR"),
  hydraR.compose.config_name = "main"
)
```

The first config file we will use is: `minimal.yml`

``` yaml
author:
  given: Vincent
  family: Arel-Bundock
numeric: 2
```

The second config file is: `main.yml`

``` yaml
defaults:
  - _self_
db:
  host: localhost
  port: 3306
  credentials:
    user: root
    password: root
  pool:
    size: 10
run:
  lr: 0.1
service:
  retries: 3
server:
  host: localhost
  port: 80
client:
  url: "http://${server.host}:${server.port}/"
  server_port: ${server.port}
  description: "Client of ${.url}"
paths:
  by_brackets: ${[server][host]}
plans:
  A: plan A
  B: plan B
selected_plan: A
plan: ${plans[${selected_plan}]}
owner:
  height: 180
  weight: 75
player: ${owner}
tree:
  value: 9
  nested:
    from_parent: ${..value}
users:
  - user1
  - user2
first_user: ${users[0]}
second_user: ${users[1]}
```

## Interpolation

As you can see above, `main.yml` uses many placeholders and nested
values. These placeholders are interpolated and resolved when calling
[`compose()`](../reference/compose.md) or
[`main()`](../reference/main.md).

``` r
cfg <- compose()
```

For example, the `url` field is defined as
`"http://${server.host}:${server.port}/"`. The placeholders are resolved
at compose time by extracting the relevant information from other fields
in the YAML file, and return:

``` r
cfg$client$url
#> [1] "http://localhost:80/"
```

We can also use complex, nested, indexing as in the `plan` field:
`${plans[${selected_plan}]}`

``` r
cfg$plan
#> [1] "plan A"
```

## Switch root config

Switch complete entrypoints by changing `config_name`.

``` r
cfg <- compose(config_name = "minimal")
print(cfg)
#> author:
#>   given: Vincent
#>   family: Arel-Bundock
#> numeric: 2

cfg <- compose()
print(cfg)
#> db:
#>   host: localhost
#>   port: 3306
#>   credentials:
#>     user: root
#>     password: root
#>   pool:
#>     size: 10
#> run:
#>   lr: 0.1
#> service:
#>   retries: 3
#> server:
#>   host: localhost
#>   port: 80
#> client:
#>   url: http://localhost:80/
#>   server_port: 80
#>   description: Client of http://localhost:80/
#> paths:
#>   by_brackets: localhost
#> plans:
#>   A: plan A
#>   B: plan B
#> selected_plan: A
#> plan: plan A
#> owner:
#>   height: 180
#>   weight: 75
#> player:
#>   height: 180
#>   weight: 75
#> tree:
#>   value: 9
#>   nested:
#>     from_parent: 9
#> users:
#> - user1
#> - user2
#> first_user: user1
#> second_user: user2
```

## Combining config files

To illustrate how to combine multiple config files, we use the
`combine.yml` config distributed with `hydraR`:

``` yaml
defaults:
  - minimal
  - db: mysql
  - _self_
db:
  pool:
    size: 15
```

Note that there is one `db` group member (`mysql` by default) and a
`_self_` token.

The `_self_` token means “merge the current file here in the order.”
Because `_self_` is listed last in `combine.yml`, values in
`combine.yml` override values from included files when they overlap.

There are two more `.yml` files in `examples/db/`: `mysql.yml` and
`postgres.yml`. We can switch between them by changing the selected `db`
option.

Default `db/mysql.yml`:

``` yaml
driver: mysql
host: mysql-host
port: 3306
credentials:
  user: root
  password: root
pool:
  size: 10
```

Alternative `db/postgres.yml`:

``` yaml
driver: postgres
host: postgres-host
port: 5432
credentials:
  user: pg
  password: pgpass
pool:
  size: 20
```

``` r
compose(config_name = "combine")
#> author:
#>   given: Vincent
#>   family: Arel-Bundock
#> numeric: 2
#> db:
#>   driver: mysql
#>   host: mysql-host
#>   port: 3306
#>   credentials:
#>     user: root
#>     password: root
#>   pool:
#>     size: 15

compose(
  config_name = "combine",
  overrides = c("db=postgres")
)
#> author:
#>   given: Vincent
#>   family: Arel-Bundock
#> numeric: 2
#> db:
#>   driver: postgres
#>   host: postgres-host
#>   port: 5432
#>   credentials:
#>     user: pg
#>     password: pgpass
#>   pool:
#>     size: 15
```
