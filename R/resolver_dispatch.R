# Resolver dispatch table.
.resolver_dispatch_table <- list(
    "oc.env" = list(fn = ".eval_resolver_oc_env"),
    "oc.select" = list(fn = ".eval_resolver_oc_select"),
    "oc.decode" = list(fn = ".eval_resolver_oc_decode"),
    "oc.create" = list(fn = ".eval_resolver_oc_create"),
    "oc.deprecated" = list(fn = ".eval_resolver_oc_deprecated"),
    "oc.dict.keys" = list(fn = ".eval_resolver_oc_dict", mode = RESOLVER_MODE_KEYS),
    "oc.dict.values" = list(fn = ".eval_resolver_oc_dict", mode = RESOLVER_MODE_VALUES)
)

# Evaluate resolver call.
.eval_resolver_call <- function(call, current_path, state) {

    name <- call$name
    args <- call$args
    raw <- call$raw

    handler <- .resolver_dispatch_table[[name]]
    if (!is.null(handler)) {
        fn <- get(handler$fn, mode = "function")
        if (!("mode" %in% names(handler))) {
            return(fn(args, current_path = current_path, state = state, raw_expr = raw))
        }
        return(fn(args, current_path = current_path, state = state, raw_expr = raw, mode = handler$mode))
    }

    .error_entry(name)
}
