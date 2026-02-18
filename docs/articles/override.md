# Override

Overrides let you change config values on the fly at compose time
without editing YAML files. In this vignette, we use one base config
file for every example:

``` yaml
author:
  given: Vincent
  family: Arel-Bundock
numeric: 2
```

A copy of this config file is distributed with the `hydraR` package. It
is stored in a local directory that we can identify with
[`system.file()`](https://rdrr.io/r/base/system.file.html).

``` r
system.file("examples", package = "hydraR")

[1] "/path/to/R/library/hydraR/examples"
```

To avoid repeating the full config directory path and config name every
time we call [`compose()`](../reference/compose.md), we set global
options once at the start.

``` r
library(hydraR)
options(
  hydraR.compose.config_path = system.file("examples", package = "hydraR"),
  hydraR.compose.config_name = "minimal"
)
```

## No Overrides

``` r
cfg <- compose()
print(cfg)
```

    ## author:
    ##   given: Vincent
    ##   family: Arel-Bundock
    ## numeric: 2

## Replace: `key=value`

``` r
cfg <- compose(
  overrides = c("numeric=3", "author.given=Alice")
)
print(cfg)
```

    ## author:
    ##   given: Alice
    ##   family: Arel-Bundock
    ## numeric: 3

## Add: `+key=value`

``` r
cfg <- compose(
  overrides = c("+experiment.name=baseline", "+experiment.seed=42")
)
print(cfg)
```

    ## author:
    ##   given: Vincent
    ##   family: Arel-Bundock
    ## numeric: 2
    ## experiment:
    ##   name: baseline
    ##   seed: 42

## Upsert: `++key=value`

Upsert means update a value if the key exists, or insert it if the key
does not exist.

``` r
cfg <- compose(
  overrides = c("++numeric=5", "++experiment.tag=trial")
)
print(cfg)
```

    ## author:
    ##   given: Vincent
    ##   family: Arel-Bundock
    ## numeric: 5
    ## experiment:
    ##   tag: trial

## Remove: `~key`

``` r
cfg <- compose(
  overrides = c("~author.family")
)
print(cfg)
```

    ## author:
    ##   given: Vincent
    ## numeric: 2

## Combine Multiple Overrides

``` r
cfg <- compose(
  overrides = c(
    "numeric=4",
    "+experiment.name=stress",
    "++author.middle=A.",
    "~author.family"
  )
)
print(cfg)
```

    ## author:
    ##   given: Vincent
    ##   middle: A.
    ## numeric: 4
    ## experiment:
    ##   name: stress
