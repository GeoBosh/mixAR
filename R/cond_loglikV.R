cond_loglikV <- function(model, x, index){
  if(ncol(x) > nrow(x)) x <- t(x)
  mult <- ncol(x)
  n <- nrow(x)
  g <- length(model@prob)
  pk <- sapply(model@arcoef@a, function(x) dim(x)[3])
  p <- max(pk) 
  
  if(missing(index))
    index <- (p+1):nrow(x)
  
  yhat <- array(matrix(0, ncol=ncol(x), nrow=length(index)), 
               dim=c(length(index), ncol(x), g))
  dens <- matrix(0, ncol=g, nrow=length(index))
  
  for(k in 1:g){
    armat <- model@arcoef[k]
    
    for(i in index){
      yhat[(i + 1 - min(index)), ,k] <- model@shift[,k] + 
        rowSums(t(t(armat) * c( t(x[(i-1):(i-pk[k]),]) )))
      dens[i + 1 - min(index), ] <- model@prob[k] * 
          dmvnorm(x[i, ], yhat[(i + 1 - min(index)), , k], model@vcov[,,k], log = TRUE)
    }
    
  }
  sum(dens)
}


