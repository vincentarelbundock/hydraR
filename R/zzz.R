.onLoad <- function(lib, pkg) {
    if (isNamespaceLoaded("reticulate")) {
        py_require_hydra()
    } else {
        setHook(packageEvent("reticulate", "onLoad"), py_require_hydra)
    }
}

py_require_hydra <- function(...) {
    reticulate::py_require("hydra-core")
    reticulate::py_require("omegaconf")
}
