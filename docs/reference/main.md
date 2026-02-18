# Hydra-style script entrypoint

Compose configuration from command-line style arguments and run a task
function, similar to Python Hydra's \`@hydra.main\`.

## Usage

``` r
main(
  fn,
  config_path = getOption("hydraR.main.config_path", "conf"),
  config_name = getOption("hydraR.main.config_name", "config"),
  resolve = getOption("hydraR.main.resolve", TRUE),
  argv = commandArgs(trailingOnly = TRUE)
)
```

## Arguments

- fn:

  Function called with the composed config as first argument. If \`fn\`
  accepts a second argument (or \`...\`), app arguments provided after
  \`–\` are passed as that second argument.

- config_path:

  Relative or absolute configuration directory path.

- config_name:

  Primary config filename without extension.

- resolve:

  Logical; resolve OmegaConf interpolations before conversion.

- argv:

  Character vector of command-line arguments. Defaults to
  \`commandArgs(trailingOnly = TRUE)\`.

## Value

Invisibly returns the value produced by \`fn\`. Returns \`NULL\`
(invisibly) when \`–help\` or \`-h\` is requested. When
\`-m\`/\`–multirun\` is enabled, returns a list with one task result per
sweep combination.

## Examples

``` r
if (FALSE) { # \dontrun{
cp <- system.file("examples", package = "hydraR")
main(
  function(cfg) print(cfg),
  config_path = cp,
  config_name = "minimal",
  argv = character()
)
} # }
```
