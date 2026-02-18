#' Print method for hydraRig
#'
#' Prints the config as YAML by writing it to a temporary file and reading it
#' back before printing.
#'
#' @param x A hydraRig object.
#' @param max_depth Unused, kept for backward compatibility.
#' @param values Unused, kept for backward compatibility.
#' @param ... Additional arguments passed to print.
#' @return Invisibly returns `x`.
#' @export
print.hydraRig <- function(
  x,
  max_depth = getOption("hydraR.print.max_depth", getOption("hydraR.print.levels", 4L)),
  values = getOption("hydraR.print.values", FALSE),
  ...
) {
    tmp <- tempfile("hydraR-", fileext = ".yaml")
    on.exit(unlink(tmp), add = TRUE)

    yaml::write_yaml(unclass(x), file = tmp)
    lines <- readLines(tmp, warn = FALSE)
    if (length(lines)) {
        cat(paste(lines, collapse = "\n"), "\n", sep = "")
    }

    invisible(x)
}
