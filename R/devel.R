## a simple mechanism for tracing
##
## Formerly this used variables 'em_global.res' and 'em_globalAll.res' in the global
## environment.

devel_envir <- local({
    em_global.res    <- list()
    em_globalAll.res <- list()

    environment()
})

## TODO: better still, avoid the need for these, if possible.
globalVariables(c("get_nu", "get_non_fixed")) # em_est_dist()
