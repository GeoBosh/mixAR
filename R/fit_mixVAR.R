mixVARfit <- function(y, model, fix = FALSE, tol = 10^-6, verbose = FALSE){
    verbose <- verbose && interactive()
    
  prob <- model@prob
  Scale <- model@vcov
  arcoef <- model@arcoef@a
  shift <- model@shift
  one_or_zero <- ifelse(identical(fix, "shift"), FALSE, TRUE)
  if(ncol(y) > nrow(y)) y <- t(y)
  mult <- ncol(y)
  n <- nrow(y)
  g <- length(prob)
  pk <- sapply(arcoef, function(x) dim(x)[3])
  p <- max(pk) 
  e <- array(matrix(0, ncol=ncol(y), nrow=(n-p)), dim=c(n-p, ncol(y), g))
  diff <- 1
  count <- 0
  while(diff>tol){
    count <- count +1; if(count%%25 == 0) {
        loglik <- cond_loglikV(new("MixVARGaussian", prob=prob, vcov=Scale, arcoef=arcoef,
                                   shift=shift), y)
        if(verbose)
            cat("niter:", count, "\tvallogf:", loglik, "\n")
    }
    ##Calculate residuals. Each residual is a mult x 1 vector
    
    for(k in 1:g){
      armat <- matrix(arcoef[[k]], nrow=ncol(y), ncol=(ncol(y) * pk[k]))
      
      for(t in 1:(n-p)){
        e[t, ,k] <- y[t+p, ] - shift[,k] - 
          rowSums(t(t(armat) * c( t(y[(t+p-1):(t+p-pk[k]),]) )))
      }
      
    }
    dens <- matrix(ncol=g, nrow=(n-p))
    for(k in 1:g){
      d <- sqrt(det(Scale[,,k]))
      inv <- solve(Scale[,,k])
      for(t in 1:(n-p)){
        dens[t, k] <- prob[k] / d * 
          exp(-0.5*e[t,,k] %*% inv %*% e[t,,k]) 
      }
    }
    tau <- dens/rowSums(dens)
    prob_new <- colMeans(tau)
    
    Scale_new <- Scale 
    for(k in 1:g){
      sig_sum <-matrix(0, nrow=mult, ncol=mult)
      for(t in 1:(n-p)){
        sig_sum <- sig_sum + tau[t, k] * e[t,,k] %*% t(e[t,,k])
      }
      Scale_new[,,k] <- sig_sum / sum(tau[,k])
    }
    AR_new <- arcoef
    if(one_or_zero) shift_new <- shift
    for(k in 1:g){
      sum1 <- matrix(0,ncol=(mult * pk[k]+one_or_zero), nrow=(mult*pk[k]+one_or_zero))
      sum2 <- matrix(0,nrow=(mult * pk[k]+one_or_zero), ncol=mult)
      for(t in 1:(n-p)){
        x <- c(if(one_or_zero) 1, c(t(y[(t+p-1):(t+p-pk[k]),])))
        sum1 <- sum1 + tau[t,k] * x %*% t(x)
        sum2 <- sum2 + tau[t,k] * x %*% t(y[t+p,])
      }
      theta <- t(solve(sum1) %*% sum2)
      if(one_or_zero) {shift_new[,k] <- theta[,1] ; theta <- theta[,-1]}
      for(i in 1:pk[k]){
        AR_new[[k]][,,i] <- theta[ ,1:mult] ; theta <- theta[ ,-c(1:mult)]  
      }  
    }
    diff <- sum(abs(prob_new-prob)) + 
      sum(abs(unlist(Scale) - unlist(Scale_new))) +
      sum(abs(unlist(arcoef) - unlist(AR_new)))
    Scale  <- Scale_new
    arcoef <- AR_new
    if(one_or_zero) shift  <- shift_new
    prob   <- prob_new
    if(count == 200) break
  }
  mod <- new("MixVARGaussian", prob=prob, shift=shift, vcov=Scale, arcoef=arcoef)
  list(model = mod, vallogf = cond_loglikV(mod, y))
}


fit_mixVAR <- function(x, model, fix, ...){
  stop("There is currently no default for this function")
}

setGeneric("fit_mixVAR")

setMethod("fit_mixVAR", signature(x = "ANY", model = "MixVAR"), 
          function(x, model, fix, ...){
              if(missing(fix)) fix = FALSE
            mixVARfit(x, model, fix)
          })



#setMethod("fit_mixAR", signature(x = "ANY", model = "numeric", init = "numeric"), 
#          function(x, model, init, fix, ...){
#            model <- new("MixARGaussian", order = model)
#            fit_mixAR(x, model, init = init, fix = fix, ...)
#          })












