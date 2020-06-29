tsdiag.MixAR <- function(object, gof.lag = NULL, y, ask = interactive(), ...,
                         plot = TRUE, std.resid = FALSE){
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))     # restore graphics parameters before exiting.

    ## changing 'f' to 'model'. 'model' is used below only in the call to
    ## mixAR_BIC but the loglik is calculated by the latter anyway. Also, 'f' is
    ## definitely not suitable name for this variable.
    ##
    ## changing 'model' to 'object' for consistency with 'tsdiag'
    if(is.list(object))
        object <- object$model

    if(is.null(gof.lag))
        gof.lag <- 20  # NOTE: arbitrary value
    else if(!is.numeric(gof.lag))
        stop("'gof.lag' must be numeric and contain positive integers")

    lag.max <- max(gof.lag)
    
    n   <- length(y)
    p   <- max(object@order)
    g   <- length(object@prob)
    mu  <- mix_hatk(object, y, index = (p+1):n)@m
  
    prob  <- object@prob
    sigma <- object@scale
    ar    <- object@arcoef@a
    shift <- object@shift

    err <- c(y[-c(1:p)] - (mu %*% prob)) #rowSums(t(t(mu) * prob))
    if(std.resid)
        err <- err / sqrt(mix_variance(object, y, index = (p+1):n))

    LBtest <-
        function(x, lags){
            m <- sapply(lags, function(lag)
                                 c(lag, Box.test(x, lag = lag, type = "Ljung-Box")$p.value))
            res <- t(m)
            colnames(res) <- c("df", "p.value")
            rownames(res) <- paste0("Lag_", lags)
            res
        }
    
        # lb <-  Box.test(err, lag = gof.lag, type = "Ljung-Box")
        #or:
        # lb <- sapply(gof.lag,
        #              function(lag)  c(lag, Box.test(err, lag = lag, type = "Ljung-Box")$p.value))
        # lb <- t(lb)
        # colnames(lb) <- c("df", "p.value")
        # rownames(lb) <- paste0("Lag_", gof.lag)
    lb <- LBtest(err, gof.lag)

    
    cdf <- mix_cdf(object, y, index = (p + 1):n)
    lb_cdf <- LBtest(cdf, gof.lag)

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
    lb_v <- LBtest(v, gof.lag)
    
    index <- apply(tau, 1, which.max) # function(x) which.max(x)
    err2 <- numeric(n-p)

    for(t in seq_along(index)){
        err2[t] <- (y[t+p] - mu[t, index[t]])/sigma[index[t]]
    }
  
    sw2 <- shapiro.test(err2)
    lb_err2 <- LBtest(err2, gof.lag)

    
    choices <- c(
        "ACF/PACF of residuals",
        "ACF/PACF of U_residuals",
        "ACF/PACF of tau_residuals",
        "ACF/Histogram of tau_residuals"
    )

    if(!isTRUE(plot)){                  # plot is typically numeric index;
        nchoice <- length(choices)      # FALSE or NULL give zero length result, so no plots
        choices <- choices[plot]
        if(anyNA(choices)){
            warning("'plot' should be TRUE/FALSE or vector of positive integers <= ",
                    nchoice, ",\n", "ignoring non-existent values")
            choices <- choices[!is.na(choices)]
        }
    }

    if(length(choices) > 0){
        par(mfrow = c(2,1))
        choice_title <- "Select a plot number or 0 to exit"
        choice <- if(length(choices) == 1)
                      choices
                  else if(interactive())
                      menu(choices, title = choice_title)
                  else 0 
        
        while(choice != 0){
            switch(choice,
            { # 1:  "ACF/PACF of residuals",
                acf(err, main = "ACF of residuals from model", lag.max = lag.max)
                pacf(err, main = "PACF of residuals from model", lag.max = lag.max)
            },
            { # 2:  "ACF/PACF of U_residuals"
                acf(cdf, main = "", lag.max = lag.max)
                title("ACF of" ~U[t])
                pacf(cdf, main = "", lag.max = lag.max)
                title("PACF of" ~U[t])
            },
            { # 3: "ACF/PACF of tau_residuals"
                acf(err2, main = "ACF of tau_residuals", lag.max = lag.max)
                pacf(err2, main = "PACF of tau_residuals", lag.max = lag.max)
            },
            { # 4: "ACF/Histogram of tau_residuals"
                acf(err2, main = "ACF of tau_residuals", lag.max = lag.max)
                hist(err2, freq = FALSE, main = "Histogram of tau_residuals", xlab  =  "",
                     ylim = c(0, 0.5))
                lines(seq(-5, 5, .01), dnorm(seq(-5, 5, .01)), col = "red")
            }
            )
            if(length(choices) == 1)
                break
            choice <- menu(choices, title = choice_title)
        }
    }
    
    BIC <- mixAR_BIC(y, object)

        # res <- list("residuals" = err, "U" = cdf, "V" = v, "tau_residuals" = err2,
        #             "tests" = list("Ljung-Box"  =  lb, "K-S" = ks, "Shapiro-Wilk" = sw,
        #                            "Shapiro-Wilk_tau" = sw2),
        #             lb_cdf = lb_cdf, lb_err2 = lb_err2,
        #             BIC = BIC)

        # res <- list(residuals = list(values = err, "Ljung-Box" = lb),
        #             U_residuals = list(values = cdf, , "Ljung-Box" = lb_cdf, "KS" = ks),
        #             V_residuals = list(values = v, "Shapiro-Wilk" = sw),
        #             tau_residuals = list(values = err2, "Ljung-Box" = lb_err2,
        #                                  "Shapiro-Wilk" = sw2),
        #             BIC = BIC)
        # class(res) <- "diagMixAR"

    res <- structure(
        list(
            residuals = list(values = err, "Ljung-Box" = lb),
            U_residuals = list(values = cdf, "Ljung-Box" = lb_cdf, "KS" = ks),
            V_residuals = list(values = v, "Ljung-Box" = lb_v, "Shapiro-Wilk" = sw),
            tau_residuals = list(values = err2, "Ljung-Box" = lb_err2, "Shapiro-Wilk" = sw2),
            BIC = BIC),
        class = "tsdiagMixAR")
    
    invisible(res)
}

print.tsdiagMixAR <- function(x, ...){
    ## for now just drop the values of the residuals
    dashes <- "--------------------------------------------------"
    cat("\n", dashes, "\nTests on the ordinary residuals\n", dashes, "\n")
    print(x$residuals[-1])
    
    cat("\n", dashes, "\nTests on the U_residuals\n", dashes, "\n")
    print(x$U_residuals[-1])

    cat("\n", dashes, "\nTests on the V_residuals\n", dashes, "\n")
    print(x$V_residuals[-1])

    cat("\n", dashes, "\nTests on the tau_residuals\n", dashes, "\n")
    print(x$tau_residuals[-1])
}
