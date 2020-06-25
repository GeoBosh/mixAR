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



## 2020-06-25: renamed from tsdiag.MixARGaussian to tsdiag_MixARGaussian
##             since the mixAR method cover this case as well.
##
## TODO: remove eventually
tsdiag_MixARGaussian <- function(object, gof.lag = NULL, y, ask = interactive(), ...){
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))     # restore graphics parameters before exiting.

    ## changing 'f' to 'model'. 'model' is used below only in the call to
    ## mixAR_BIC but the loglik is calculated by the latter anyway. Also, 'f' is
    ## definitely not suitable name for this variable.
    ##
    ## changing 'model' to 'object' for consistency with 'tsdiag'
    if(is.list(object))
        object <- object$model
    
    n   <- length(y)
    p   <- max(object@order)
    g   <- length(object@prob)
    mu  <- mix_hatk(object, y, index = (p+1):n)@m
   
    prob  <- object@prob
    sigma <- object@scale
    ar    <- object@arcoef@a
    shift <- object@shift

    err <- c(y[-c(1:p)] - (mu %*% prob)) #rowSums(t(t(mu) * prob))

    lb <-  Box.test(err, lag = if(is.null(gof.lag)) 20 else gof.lag,
                    type = "Ljung-Box") 

    cdf <- mix_cdf(object, y, index = (p+1):n)

    tau <- mu 
    for(k in 1:g){
        tau[,k] <-  prob[k] * dnorm(y[-c(1:p)], mu[,k], sigma[k])
    }

    ## ## the following are equivalent - if uncommented the stopifnot() conditions pass 'R CMD
    ## ## check'
    ## tau_new <- (prob * (noise_dist(object, "pdf") %of%
    ##                    ((y[-c(1:p)] - mix_hatk(object, y, index = (p+1):n)) / sigma) /sigma))@m
    ## ## tau_new split on several lines for clarity
    ## ##
    ## ## can be replaced by
    ## ##        ek_new <- mix_ek(object, y, index = (p+1):n, scale = TRUE)
    ## ## but check
    ## ek_new <- (y[-c(1:p)] - mix_hatk(object, y, index = (p+1):n)) / sigma
    ## pdf_k_new <- noise_dist(object, "pdf") %of% ek_new
    ## tau_new2 <- (prob * (pdf_k_new /sigma))@m
    ## 
    ## stopifnot(max(abs(tau      - tau_new)) < 1e-20)
    ## stopifnot(max(abs(tau_new2 - tau_new)) < 1e-20)
  
    ks <- ks.test(cdf, punif)

    v <- qnorm(cdf)
    sw <- shapiro.test(v)

    par(mfrow = c(2,1))
    acf(err, main = "ACF of residuals from model")
    pacf(err, main = "PACF of residuals from model")
    
    par(ask = ask)
    acf(cdf, main = "", lag.max = gof.lag)
    title("ACF of" ~U[t])
    pacf(cdf, main = "", lag.max = gof.lag)
    title("PACF of" ~U[t])
  
  
    index <- apply(tau, 1, function(x) which.max(x))
    err2 <- numeric(n-p)

    for(t in seq_along(index)){
        err2[t] <- (y[t+p] - mu[t, index[t]])/sigma[index[t]]
    }
  
    sw2 <- shapiro.test(err2)

    ## no need to repeat this, 'mfrow' and 'ask' were set above
    ##   par(mfrow = c(2,1), ask = TRUE)

    acf(err2, main = "ACF of tau_residuals", lag.max = gof.lag)
    hist(err2, freq = FALSE, main = "Histogram of tau_residuals", xlab  =  "",
         ylim = c(0, 0.5))
    lines(seq(-5, 5, .01), dnorm(seq(-5, 5, .01)), col = "red")
    
    BIC <- mixAR_BIC(y, object)

    ## @Davide, see the beginning of the function for a better way to restore parameters.
    ##          Note that the command below does not really restore parameters,
    ##          since they may have had other values when the function was called.
    ##
    ## par(mfrow = c(1,1), ask = FALSE)

    res <- list("residuals" = err, "U" = cdf, "V" = v, "tau_residuals" = err2,
                "tests" = list("Ljung-Box"  =  lb, "K-S" = ks, "Shapiro-Wilk" = sw,
                               "Shapiro-Wilk_tau" = sw2),
                BIC = BIC)
    
    invisible(res)
}

mixAR_diag <- function(model, y, ask = interactive()){
    ## .Deprecated("tsdiag")
    if(is.list(model))
        ## the loglik was not used, so not kept here.
        model <- model$model
    
    tsdiag(model, y = y, ask = ask)
}


tsdiag.MixAR <- function(object, gof.lag = NULL, y, ask = interactive(), ...){
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))     # restore graphics parameters before exiting.

    ## changing 'f' to 'model'. 'model' is used below only in the call to
    ## mixAR_BIC but the loglik is calculated by the latter anyway. Also, 'f' is
    ## definitely not suitable name for this variable.
    ##
    ## changing 'model' to 'object' for consistency with 'tsdiag'
    if(is.list(object))
        object <- object$model
    
    n   <- length(y)
    p   <- max(object@order)
    g   <- length(object@prob)
    mu  <- mix_hatk(object, y, index = (p+1):n)@m
  
    prob  <- object@prob
    sigma <- object@scale
    ar    <- object@arcoef@a
    shift <- object@shift

    err <- c(y[-c(1:p)] - (mu %*% prob)) #rowSums(t(t(mu) * prob))    
    
    lb <-  Box.test(err, lag = if(is.null(gof.lag)) 20 else gof.lag,
                    type = "Ljung-Box") 

    cdf <- mix_cdf(object, y, index = (p + 1):n)

                # or: ek_new <- mix_ek(object, y, index = (p+1):n, scale = TRUE)
    ek_new <- (y[-c(1:p)] - mix_hatk(object, y, index = (p+1):n)) / sigma
    pdf_k_new <- noise_dist(object, "pdf") %of% ek_new
    tau <- (prob * (pdf_k_new /sigma))@m

    ## tau <- mu 
    ## for(k in 1:g){
    ##     tau[,k] <-  prob[k] * dnorm(y[-c(1:p)], mu[,k], sigma[k])
    ## }
    ##
    ## The calculation for tau can be replaced by any of the following. At the same time it
    ## becomes valid for any noise distributions.
    ## 
    ## ## the following are equivalent - if uncommented the stopifnot() conditions pass 'R CMD
    ## ## check'
    ## tau_new <- (prob * (noise_dist(object, "pdf") %of%
    ##                    ((y[-c(1:p)] - mix_hatk(object, y, index = (p+1):n)) / sigma) /sigma))@m
    ## ## tau_new split on several lines for clarity
    ## ##
    ## ## can be replaced by
    ## ##        ek_new <- mix_ek(object, y, index = (p+1):n, scale = TRUE)
    ## ## but check
    ## ek_new <- (y[-c(1:p)] - mix_hatk(object, y, index = (p+1):n)) / sigma
    ## pdf_k_new <- noise_dist(object, "pdf") %of% ek_new
    ## tau_new2 <- (prob * (pdf_k_new /sigma))@m
    ## 
    ## stopifnot(max(abs(tau      - tau_new)) < 1e-20)
    ## stopifnot(max(abs(tau_new2 - tau_new)) < 1e-20)
    
    ks <- ks.test(cdf, punif)

    v <- qnorm(cdf)
    sw <- shapiro.test(v)

    par(mfrow = c(2,1))
    acf(err, main = "ACF of residuals from model")
    pacf(err, main = "PACF of residuals from model")
    
    par(ask = ask)
    acf(cdf, main = "", lag.max = gof.lag)
    title("ACF of" ~U[t])
    pacf(cdf, main = "", lag.max = gof.lag)
    title("PACF of" ~U[t])
  
    index <- apply(tau, 1, function(x) which.max(x))
    err2 <- numeric(n-p)

    for(t in seq_along(index)){
        err2[t] <- (y[t+p] - mu[t, index[t]])/sigma[index[t]]
    }
  
    sw2 <- shapiro.test(err2)

    ## no need to repeat this, 'mfrow' and 'ask' were set above
    ##   par(mfrow = c(2,1), ask = TRUE)

    acf(err2, main = "ACF of tau_residuals", lag.max = gof.lag)
    hist(err2, freq = FALSE, main = "Histogram of tau_residuals", xlab  =  "",
         ylim = c(0, 0.5))
    lines(seq(-5, 5, .01), dnorm(seq(-5, 5, .01)), col = "red")
    
    BIC <- mixAR_BIC(y, object)

    res <- list("residuals" = err, "U" = cdf, "V" = v, "tau_residuals" = err2,
                "tests" = list("Ljung-Box"  =  lb, "K-S" = ks, "Shapiro-Wilk" = sw,
                               "Shapiro-Wilk_tau" = sw2),
                BIC = BIC)
    
    invisible(res)
}
