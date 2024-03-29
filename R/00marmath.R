## Do not edit this file manually.
## It has been automatically generated from mixAR.org.

# translations of some of my Mathematica programs.

# params e list, vseki komponent na koyto e list ot 4 elementa, opisvasti edna komponenta na
# MAR model v sledniya red: (prob_k, shift_k, arcoeff_k, sigma2_k
# (todo: proveri dali v Mathematica programite poslednata komp. e sigma ili sigma^2)
tomarparambyType <- function(params){
    list( prob   = sapply(params, function(x) x[[1]]),
          shift  = sapply(params, function(x) x[[2]]),
          arcoef = lapply(params, function(x) x[[3]]),  # note: lapply here!
          s2     = sapply(params, function(x) x[[4]])
         )

}

# this is for completeness, my Mathematica programs do not have this currently.
tomarparambyComp <- function(params){
    g <- length(params[[1]])               # 2019-03-23 was:  g <- length(params)
    res <- vector("list", length = g)
    for(i in 1:g){
        res[[i]] <- list( params[[c(1,i)]], params[[c(2,i)]],
                          params[[c(3,i)]], params[[c(4,i)]] )
    }
    res
}

adjustLengths <- function(x,y){   # todo: check that the function produces the expected
                                  # results.
    m <- length(x) - length(y)
    if(m>0)       c(y, rep(0,m))    # pad y, if it is short
    else if(m<0)  head(y, m)        # drop excess elements
    else          y                 # do not change (presumably because m==0,
                                    #    or when arguments are wrong...
}

# izglezhda, che tova vsastnost pravi "permuteMarComponents", t.e. permute'va shift, scale,
# etc., no ostavyam orig. ime
permuteArpar <- function(params){   # params are by component here.
    models <- permn(params)
                                        # p <- sapply(params, function(x) length(x[[3]]))
    f <- function(x){
        for(i in 1:length(x))           # todo: more advanced method for this? (needs theory!)
            x[[c(i,3)]] <- adjustLengths( params[[c(i,3)]], x[[c(i,3)]])
        x
    }
    lapply(models, f)
}

randomArCoefficients <- function(ts, wv, pk, pmax, partempl,
                                 sub_size = 10, condthr = 10, nattempt = 10,
                                 startfrom = pmax + 1){
    wflag <- length(wv) > 0                    # todo: length(wv) must be length(ts)-startfrom
    n <- length(ts)
    ind <- startfrom : n

    cond <- 1
                            # todo: is it a good idea to check, besides the condition number,
                            # also the (numerical) rank and require that it is 1 or something?
    while(cond < condthr && nattempt > 0){
            # subind <- sample(ind, sub_size, prob = wv)
        subind <- sample(ind, sub_size)
           # the Mathematica program returns a list containing, besides the matrix,
           # a parameter template and names of variables.
        wrk <- tsDesignMatrixExtended(ts, pk, subind, partempl)
        m <- wrk$m

        cond <- if(wflag){
                    wm <- wv[subind - pmax] * m     # weight the rows of m
                                        # kappa(t(wm) %*% wm, exact = TRUE) # 2-norm
                    kappa(t(wm) %*% wm) # 2-norm
                }else{
                      # kappa(t(m) %*% m, exact = TRUE) # 2-norm;  no need of such precision?
                    kappa(t(m) %*% m) # 2-norm; todo: no need of such precision?
                }
        if(FALSE) # 2020-06-13 TODO: needs new arrangements
            cat("cond: ", cond, "\n")
    }


    ## 2021-08-09 handle case when there are no parameters to estimate
    newpar <- c(partempl[[1]], partempl[[2]])
    if(ncol(m) > 1){
        rhs <- m[ ,  ncol(m)]  # last is the righ-hand side of ax = rhs
        a   <- m[ , -ncol(m)]
        res <- pseudoInverse(a) %*% rhs
        newpar[is.na(newpar)] <- res # assumes 'res' has the appropriate length; todo:  check!
    }

    list(newpar[1], newpar[-1])
}

            # za (malka) razlika ot moite Mathematica prorami, tuk partempl e list(shift, ar)
randomMarParametersKernel <- function(ts, ww, pk, pmax, partempl, ...){
    co <- randomArCoefficients(ts, ww, pk, pmax, partempl, ...)
                                                     # ek <- arResiduals(ts, co[[2]], co[[1]])
    ek <- raghat1(co[[2]], ts, (pmax+1):length(ts), shift = co[[1]], residual = TRUE)

    list(shift = co[[1]], ar = co[[2]], ek = ek)    # shift, arparam, residuals
}

randomMarResiduals <- function(ts, p, partempl){
    ssiz <- 10  # todo: this is arbitrary, make argument?

    pmax <- max(p)
    nmp <- length(ts) - pmax

    ww <- rep(1.0, nmp)
                                        # ee <- matrix(0, nrow = nmp, ncol = length(p))
    ee <- matrix(0, nrow = nmp, ncol = 0)
    for(i in 1:length(p)){
        wrk <- randomMarParametersKernel(ts, ww, p[i], pmax, partempl[[i]], sub_size = ssiz)
                                        # ee[ , i] <- wrk$ek
        ee <- cbind(ee, wrk$ek)
        ww <- apply(ee, 1, function(x) min(abs(x)))
        ww <- ww / sum(ww)
    }
    ee
}

                                          # note: this deviates from the Mathematica original.
tsDesignMatrixExtended <- function(ts, p, ind, partempl){
                                                            # note: x[t] is in the last column
    m <- t( sapply(ind, function(i) c(1, ts[i - c(seq_len(p),0)])) )

    x <- unlist(partempl)
    indest <- which(is.na(x))      # parameters to be estimated are NA, the rest  are numbers
    xext <- c(x, -1)
                                        # flag <- all(is.na(x)) # for the matrix mult. below
    rhs <- - m[ , -indest, drop = FALSE] %*% xext[-indest, drop = FALSE]
    lhs <-   m[ , indest]
    list(m = cbind(lhs, rhs), vpar = x, vars = indest)
}
