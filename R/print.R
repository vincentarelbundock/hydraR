#' Print method for HydraConfig
#'
#' Prints the config as YAML by writing it to a temporary file and reading it
#' back before printing.
#'
#' @param x A HydraConfig object.
#' @param filename Optional output path ending in `.yml` or `.yaml`. When
#'   `NULL` (default), prints to console. Otherwise writes YAML to `filename`.
#' @return Invisibly returns `x`.
#' @export
print.HydraConfig <- function(x, filename = NULL) {
    if (!is.null(filename)) {
        filename <- .validate_print_filename(filename)
        yaml::write_yaml(unclass(x), file = filename)
        return(invisible(x))
    }

    tmp <- tempfile("hydraR-", fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)

    yaml::write_yaml(unclass(x), file = tmp)
    lines <- readLines(tmp, warn = FALSE)
    if (length(lines)) {
        cat(paste(lines, collapse = "\n"), "\n", sep = "")
    }

    invisible(x)
}

.validate_print_filename <- function(filename) {
    if (!is.character(filename) || length(filename) != 1L || is.na(filename) || !nzchar(filename)) {
        stop("`filename` must be a non-empty character scalar.", call. = FALSE)
    }

    if (!grepl("\\.ya?ml$", filename, ignore.case = TRUE)) {
        stop("`filename` must end with `.yml` or `.yaml`.", call. = FALSE)
    }

    filename <- path.expand(filename)
    if (dir.exists(filename)) {
        stop("`filename` must be a file path, not a directory.", call. = FALSE)
    }

    dir_path <- dirname(filename)
    if (!dir.exists(dir_path)) {
        stop(sprintf("Directory does not exist: %s", dir_path), call. = FALSE)
    }

    if (file.access(dir_path, mode = 2) != 0L) {
        stop(sprintf("Directory is not writable: %s", dir_path), call. = FALSE)
    }

    if (file.exists(filename) && file.access(filename, mode = 2) != 0L) {
        stop(sprintf("File is not writable: %s", filename), call. = FALSE)
    }

    filename
}
