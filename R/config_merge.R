is_named_list <- function(x) {
    is.list(x) && !is.null(names(x)) && all(nzchar(names(x)))
}

# Internal helper for deep merge.
.deep_merge <- function(x, y) {
    if (is_named_list(x) && is_named_list(y)) {
        out <- x
        for (nm in names(y)) {
            out[[nm]] <- if (nm %in% names(out)) .deep_merge(out[[nm]], y[[nm]]) else y[[nm]]
        }
        return(out)
    }
    y
}
