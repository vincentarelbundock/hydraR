# Print method for HydraConfig

Prints the config as YAML by writing it to a temporary file and reading
it back before printing.

## Usage

``` r
# S3 method for class 'HydraConfig'
print(x, filename = NULL)
```

## Arguments

- x:

  A HydraConfig object.

- filename:

  Optional output path ending in \`.yml\` or \`.yaml\`. When \`NULL\`
  (default), prints to console. Otherwise writes YAML to \`filename\`.

## Value

Invisibly returns \`x\`.
