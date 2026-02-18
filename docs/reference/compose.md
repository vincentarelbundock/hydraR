# Compose configuration from a config directory

Compose configuration from a config directory

## Usage

``` r
compose(
  config_path = getOption("hydraR.compose.config_path", "conf"),
  config_name = getOption("hydraR.compose.config_name", "config"),
  overrides = character(),
  resolve = TRUE
)
```

## Arguments

- config_path:

  Relative or absolute configuration directory path.

- config_name:

  Primary config filename without extension.

- overrides:

  Character vector of CLI-style overrides for Python Hydra.

- resolve:

  Logical; resolve OmegaConf interpolations before conversion.

## Value

A nested named list of config values with the `HydraConfig` class.

## Examples

``` r
cp <- system.file("examples", package = "hydraR")
cfg <- compose(config_path = cp, config_name = "minimal")
```
