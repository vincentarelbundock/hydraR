#' @section Interpolation resolvers:
#' `compose()` resolves `${...}` expressions after defaults expansion and
#' overrides are applied.
#'
#' Supported resolvers:
#' - `${oc.env:VAR}`: Read environment variable `VAR`. Errors when unset.
#' - `${oc.env:VAR,fallback}`: Read environment variable `VAR`, or return
#'   `fallback` (scalar or `null`) when unset.
#' - `${oc.select:path}`: Return config value at `path`. Returns `NULL` when
#'   missing.
#' - `${oc.select:path,fallback}`: Return config value at `path`, or `fallback`
#'   when missing.
#' - `${oc.decode:text}`: Decode YAML text into typed R values (for example
#'   numbers, logicals, vectors, lists, or `NULL`).
#' - `${oc.create:value}`: Create values from YAML-like literals; if the
#'   argument is already a list or `NULL`, it is returned unchanged.
#' - `${oc.deprecated:new_key}`: Emit a deprecation warning and return the value
#'   at `new_key`.
#' - `${oc.deprecated:new_key,message}`: Same as above, with a custom warning
#'   message. Use `$OLD_KEY` and `$NEW_KEY` placeholders in `message`.
#' - `${oc.dict.keys:path}`: Return key names from a mapping at `path`.
#' - `${oc.dict.values:path}`: Return values from a mapping at `path`.
#'
#' In defaults-list interpolation (inside `defaults:`), only
#' `${oc.env:...}` is supported.
