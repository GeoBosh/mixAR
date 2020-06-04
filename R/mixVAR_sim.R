## todo: tova e nay-dobre da stane generic.
## todo: change the default for nskip to 0?
##       set default for init using the unconditional mean?
mixVAR_sim <- function(model, n, init, nskip = 100, flag = FALSE){

  sigma <- model@vcov
  mult  <- nrow(sigma)   
  shift <- model@shift
  zdist <- model@prob
  K <- length(zdist) # number of components

  order <- model@order
  if (missing(init))
    init <- matrix(rep(0, mult * max(order)), ncol=mult)
  
  if(nrow(init) < max(order))
    stop("The rows of argument 'init' must be at least equal to the maximal order.")
  resinit <- tail(init, max(order))   # lastn(init, max(order))
  # use only the last max(order) values  in init.
  ntot     <- nskip + n
  nresinit <- nrow(resinit)
  
  z <- sample.int(K, size = ntot, replace = TRUE, prob = zdist)
  
  #noisedist <- noise_rand(model)
  #res <- mixARnoise_sim(noisedist, z)
  
  res <- resinit                # now prepend the initial values
  z   <- c(numeric(nresinit), z)
  
  ar <- model@arcoef
  ## needs efficient implementation, this is extremely inefficient (but straightforward)
  for(i in nresinit + (1:ntot)){
    k <- z[i]
    res <- rbind(res,rmvnorm(1, rep(0, mult), sigma[ , ,k]))
    wrk <- res[i, ] + shift[ ,k]

    ## 2020-04-21 @Davide: Why do you prefer ar[[1]] to ar@a, eg filt <- ar@a[[k]].
    ##   ar[[1]]  and ar[[2]] have nothing in common and code useing them is completely opaque
    ##   and error prone. I don't understand why you have defined a method for '[[' doing this.
    ##   If anything, a '[[' method could pick ar@a[[k]] which would make sense. 
    filt <- ar@a[[k]]
    for(j in seq_len(order[k])){       # 2011-07-12: was 1:order[k]
      wrk <- wrk + filt[ , ,j] %*% res[i - j, ]
    }
    res[i, ] <- wrk
  }
  if(flag) structure(tail(res, n), regimes = lastn(z, n)) # return the last n values (could
  else     tail(res, n)                                   # use tail()); if 'flag', attach
}                                                            # 'z' as attribute to the result.



