# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

hydraR is an R package that ports Facebook/Meta's Hydra configuration composition framework to R. It enables hierarchical YAML config composition with defaults lists, CLI-style overrides, and OmegaConf-style interpolation resolvers.

**API:** The public API is minimal: `initialize()` and `compose()`. Composed configs are returned as `HydraConfig` S3 objects (which behave like lists but have enhanced printing).

## Common Commands

```bash
make install        # Install the package locally (no deps)
make testall        # Build, install, and run all tinytest tests
make testone TESTFILE=inst/tinytest/test_initialize.R  # Run a single test file
make document       # Regenerate roxygen2 docs + render README.qmd → README.md
make check          # Full R CMD check
make clean          # Remove build artifacts
```

## Architecture

The package follows a two-phase pattern: **initialize** (point at config root) → **compose** (resolve defaults + apply overrides).

### Source modules (`R/`)

#### Core composition flow
- **initialize.R** — Initialization API and state management. Exports `initialize()` and manages package-level state in the `.initialize_state` environment. Handles config path resolution and initialization assertions.
- **compose.R** — Core composition engine and exported `compose()` function. Orchestrates: primary config loading → defaults resolution → group config merging → overrides application → interpolation resolution. Returns a `HydraConfig` S3 object.
- **config_defaults.R** — Defaults list parsing and normalization. Handles `_self_` positioning, group→option entries, and config-path entries from the `defaults:` key in YAML configs.
- **config_overrides.R** — CLI-style override parsing and application. Supports `key=value`, `+key=value`, and `~key` syntax with dot-path navigation, booleans, null, numbers, and inline YAML.
- **config_merge.R** — Deep merge implementation for named lists (right overwrites left, recursively).

#### Interpolation system
- **config_interpolation.R** — Interpolation resolution engine. Handles `${...}` expressions with cycle detection, relative paths (`.`, `..`), and bracket notation (`[0]`, `['key']`). Dispatches to resolver functions for `oc.*` calls.
- **resolver_dispatch.R** — Resolver dispatch logic. Routes `oc.env`, `oc.select`, `oc.decode`, `oc.create`, `oc.deprecated`, `oc.dict.keys`, and `oc.dict.values` to their implementations.
- **resolver_oc_env.R** — `${oc.env:VAR,default}` resolver for environment variables.
- **resolver_oc_select.R** — `${oc.select:path,default}` resolver for safe config lookups with fallback.
- **resolver_oc_decode.R** — `${oc.decode:string}` resolver for base64 decoding.
- **resolver_oc_create.R** — `${oc.create:{...}}` resolver for inline YAML/list construction.
- **resolver_oc_deprecated.R** — `${oc.deprecated:...}` resolver for deprecation warnings.
- **resolver_oc_dict.R** — `${oc.dict.keys:path}` and `${oc.dict.values:path}` resolvers for dictionary introspection.
- **resolver_utils.R** — Shared resolver utilities (argument parsing, quote stripping, etc.).

#### Utilities and output
- **config_utils.R** — Low-level configuration utilities: dot-path tokenization with quote support, nested list get/set operations (`.get_cfg_value`, `.set_cfg_value`), unquoted character search (`.first_unquoted_index`).
- **print.R** — S3 print method for `HydraConfig`. Renders configs as ASCII trees with configurable depth (controlled by `levels` argument or `options(hydraR.print.max_depth = n)`).

### Conventions

- Internal functions use `.name` naming (dot prefix for internals).
- Parameter validation uses the `checkmate` package.
- Documentation is roxygen2-generated (`man/` files should not be edited by hand).
- Tests live in `inst/tinytest/` split by functionality, with YAML fixtures in `inst/examples/`.
- Test fixtures include group structure: `db/mysql.yaml`, `db/postgres.yaml`, `config.yaml`, `config_alt.yaml`, `config_interp.yaml`.

### Dependencies

Minimal: `yaml` (YAML parsing), `checkmate` (parameter validation), `tinytest` (testing, suggested only).
