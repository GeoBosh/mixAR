mixAR_BIC <- function(y, model, fix = NULL, comp_loglik = TRUE, index){
              # TODO: argument 'fix' needs to be inferred from the model but currently
              #       that doesn't contain that information. 

                                        # ifelse(identical(fix, "shift"), 0, 1)
    one_or_zero <- as.numeric(!identical(fix, "shift"))

    if(is.list(model)){
        loglik <- model$vallogf
        model <- model$model
    }
    stopifnot(is(model, "MixAR"))

    pk <- model@order
    p <- max(pk)
    g <- length(pk)

    ## 2020-06-25 take into account distribution parameters.
    ##
    ## TODO: add an argument to control this, since there may be more appropriate way of
    ##       doing it.
    ndist_par <- length(unlist(noise_params(model)))
    
    n <- length(y)
    if(missing(index))
        index <- (p + 1):n

    if(comp_loglik)
        loglik <- sum(log(mix_pdf(model, y, index)))
    
    -2 * loglik + ((2 + one_or_zero) * g - 1 + sum(pk) + ndist_par) * log(length(index)) 
}

BIC_comp <- function(x, y){ ### Returns best selected model (as text only) and 
                            ### list of BIC in order of input
  xx <- sapply(x, mixAR_BIC, y = y)
  best <- which.min(xx)
  if(is.list(x[[best]])) cat("Best model: ", x[[best]]$model@order, "\n")
  else cat("Best model: ", x[[best]]@order, "\n")
  xx
}

mixAR_diag <- function(model, y, ...){
    ## .Deprecated("tsdiag")
    if(is.list(model))
        ## the loglik was not used, so not kept here.
        model <- model$model
    
    tsdiag(model, y = y, ...)
}

