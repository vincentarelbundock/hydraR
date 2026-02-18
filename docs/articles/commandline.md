# Command line

Sometimes, the analyst wants to run an `R` script from the shell and
override a few configuration values at runtime, without editing YAML
files. They may also want a multi-run call that executes the same script
over a grid of configuration parameters.

This workflow can be achieved using `Rscript` and the
[`hydraR::main()`](../reference/main.md) function.

## Minimal example

When you only need one run with default parameters, a minimal config and
a single `Rscript` call keep things simple and reproducible.

The full `minimal.yml` used in this example is:

``` yaml
author:
  given: Vincent
  family: Arel-Bundock
numeric: 2
```

The `example.R` script executed by `Rscript` is:

``` r
#!/usr/bin/env Rscript

square <- function(cfg) {
    x <- cfg$numeric
    out <- sprintf("Input=%s -> Square=%s", x, x^2)
    print(out)
}

hydraR::main(
    square,
    config_path = "/path/to/config/",
    config_name = "minimal"
)
```

Run the script once with no command-line overrides:

``` bash
Rscript example.R
```

The printed output is:

``` bash
[1] "Input=2 -> Square=4"
```

## Config override

When one script needs to run against different config folders or file
names, you can switch both from the command line without editing the
script itself.

``` bash
Rscript example.R --config-path config_alt/ --config-name minimal
```

## Parameter override

A common workflow is to keep a stable base config and override just one
value from the shell for quick experiments.

Using the same `minimal` config as before, we can do a single parameter
override:

``` bash
Rscript example.R 'numeric=3'
```

The printed output is:

``` bash
[1] "Input=3 -> Square=9"
```

## Multi-parameter sweep

When you want to evaluate multiple values in one command, multi-run
sweeps avoid manual repetition and keep runs consistent.

Run a sweep with `-m` across three values, separated by commas:

``` bash
Rscript example.R -m 'numeric=2,3,4'
```

The printed output is:

``` bash
[1] "Input=2 -> Square=4"
[1] "Input=3 -> Square=9"
[1] "Input=4 -> Square=16"
```

## Global options

If many functions share the same config, setting `hydraR.main.*` options
lets you avoid repeating `config_path` and `config_name` on every call.

The `example.R` script executed by `Rscript` is:

``` r
#!/usr/bin/env Rscript

options(
    hydraR.main.config_path = "/path/to/config/",
    hydraR.main.config_name = "minimal"
)

square <- function(cfg) {
    x <- cfg$numeric
    out <- sprintf("Input=%s -> Square=%s", x, x^2)
    print(out)
}

cube <- function(cfg) {
    x <- cfg$numeric
    out <- sprintf("Input=%s -> Square=%s", x, x^3)
    print(out)
}

hydraR::main(square)
hydraR::main(cube)
```
