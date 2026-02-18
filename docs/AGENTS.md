# Repository Guidelines

## Project Structure & Module Organization

- `R/`: package source organized by concern (`initialization.R`,
  `composition.R`, `config_overrides.R`, etc.).
- `inst/tinytest/`: tinytest suites split by functionality
  (`test_initialize.R`, `test_compose.R`, etc.).
- `inst/examples/`: YAML fixtures used in tests.
- `man/`: generated Rd documentation (from roxygen2).
- Root metadata and tooling: `DESCRIPTION`, `NAMESPACE`, `README.qmd`,
  `README.md`, `Makefile`, `CHANGES.md`.

## Build, Test, and Development Commands

- `make document`: regenerate `NAMESPACE`/`man/` via roxygen2 and render
  `README.qmd` to `README.md`.
- `make test`: build, install, and run all tinytest tests.
- `make testone TESTFILE=inst/tinytest/test_initialize.R`: run a single
  test file during development.
- `make check`: run full package checks with `devtools::check('.')`.
- `make install`: document and install locally (without dependency
  installation).
- `make clean`: remove `*.Rcheck` directories and source tarballs.

## Coding Style & Naming Conventions

- Use 4-space indentation and `snake_case` identifiers.
- Internal helpers follow `.name` (dot prefix for internals); exported
  API uses plain names (for example, `initialize`, `compose`).
- Validate function inputs with `checkmate` at entry points.
- Add/maintain roxygen comments for exported functions; do not edit
  generated files in `man/` by hand.

## Testing Guidelines

- Framework: `tinytest` (`tests/tinytest.R` is the package test
  entrypoint).
- Name new tests `test_<feature>.R` and keep reusable fixtures in
  `inst/examples/`.
- For behavior changes, add assertions for defaults resolution, override
  parsing, initialization state transitions, and tree-print output.
- Run `make test` before opening a PR; use `make testone` for focused
  debugging loops.

## Commit & Pull Request Guidelines

- `.git` history is not present in this workspace snapshot; use
  consistent imperative commit subjects with a scope when helpful
  (example: `fix: parse quoted override paths`).
- Keep commits focused and include tests/docs for user-facing changes.
- PRs should contain: summary of changes, rationale, test evidence
  (`make test` and/or `make check`), and explicit notes for breaking API
  changes (update `CHANGES.md` when relevant).
