std_err_mixar <- function(y, model, fix_shift){
    
    y <- as.numeric(y)
    cl <- inherits(model@arcoef,"raggedCoefS")
    
    n <- length(y)
    g <- length(model@prob)    ## Set up useful quantities
    one_or_zero <- ifelse(fix_shift, 0, 1)
    
    pk  <- model@order
    pks <- rep(0, g)
    if(cl){s <- model@arcoef@s; pks <-   model@arcoef@ps}
    p <- if(cl) {max(pks)*s
    }else{max(pk)}
    
    sigma <- model@scale
    pi <- model@prob
    
    if(cl){
        e <- yhat <- tau <- matrix(0,nrow=n-p,ncol=g)
        for(t in 1:(n-p)){
            for(k in 1:g){
                
                yhat[t, k] <- sum(ui(y, t+p, c(0, 1:pk[k], (1:pks[k]) * s)) *
                                              c(model@shift[k], model@arcoef@a[[k]], 
                                                model@arcoef@as[[k]]))
            }
            e[t, ] <- y[t+p] - yhat[t, ]
            tau[t, ] <- pi * dnorm(y[t+p], yhat[t, ], sigma) / sum(pi * dnorm(y[t+p], yhat[t, ], sigma))
        }
    }else{
        e <- mix_ek(model, y, (p+1):n)@m
        yhat <- mix_hatk(model, y, (p+1):n)@m
        dens_tot <- mix_pdf(model, y, (p+1):n) 
        tau <- e   ### only doing this to create tau with same dimensions as e
        for(t in 1:(n-p)){
            tau[t, ] <- pi * dnorm(y[t+p], yhat[t, ], sigma) / dens_tot[t]
    }
    }
    
    
    ##### Complete information matrix
    
    Ic0 <- matrix( nrow=(g-1), ncol=(g-1))
    for(k in 1:(g-1)){
        for(l in k:(g-1)){
            Ic0[k,l] <- Ic0[l, k] <- ifelse( k==l, sum(tau[,k] / pi[k]^2 + tau[,g] / pi[g]^2), 
                                             sum(tau[,g] / pi[g] ^ 2))
        }
    }
    compinf <- matrix(0, nrow = (g - 1 + sum(pk + pks + 1 + one_or_zero)),  ## complete information
                      ncol = (g - 1 + sum(pk + pks + 1 + + one_or_zero)))      ## matrix
    compinf[1 : (g-1), 1 : (g-1)] <- Ic0
    
    for(k in 1:g){
        xx <- one_or_zero + pk[k] + pks[k] + 1 
        ak <- c(if(one_or_zero) 0, seq_len(pk[k]), if(cl) s*(seq_len(pks[k])))
        Ick <- matrix(0, nrow = xx, 
                         ncol = xx)
        
        for(i in seq_along(ak) - 1){
            for(j in i : (length(ak)-1)){
                for(t in 1:(n-p)){
            
                        Ick[i + 1, j + 1] <-
                        Ick[j + 1, i + 1] <-
                            Ick[i + 1, j + 1] + tau[t, k] *
                            ui(y, t + p, ak[i + 1]) *
                            ui(y, t + p, ak[j + 1])  /
                            sigma[k] ^ 2
                }
            }
        }
        Ick[xx, xx] <- sum(tau[,k] / sigma[k] ^ 2 * (
            3 * e[,k]^2 / sigma[k] ^ 2 - 1))
        
        for(i in seq_along(ak) - 1){
            for(t in 1 : (n-p)){
                Ick[i+1, xx] <- 
                Ick[xx, i+1] <- Ick[i+1, xx] +
                    2 * tau[t, k] * e[t, k] / sigma[k] ^ 3 *
                    ui(y, t + p, ak[i + 1])
                         
            }
        }
        compinf[(g + sum(pk[0 : (k-1)] + pks[0:(k-1)] + 1 + one_or_zero)) : 
                    (g + sum(pk[1:k] + pks[1:k] + 1 + one_or_zero) - 1),
                (g + sum(pk[0 : (k-1)] + pks[0:(k-1)] + 1 + one_or_zero)) : 
                    (g + sum(pk[1:k] + pks[1:k] + 1 + one_or_zero) - 1)] <- Ick
    }
    
    
    ##################### Missing information matrix
    Im00 <- matrix(nrow= g-1, ncol = g-1)
    
    for(k in 1:(g-1)){
        for(l in k:(g-1)){
           Im00[k, l] <- Im00[l, k] <- 
                ifelse(k == l, 
                       sum(tau[ , k] * (1 - tau[,k]) / pi[k]^2 + 
                           tau[ , g] * (1 - tau[,g]) / pi[g]^2 +
                            2 * tau[ , k] * tau[ , g] / (pi[k] * pi[g])),
                      sum(tau[ , k] * tau[ , g] / (pi[k] * pi[g]) +
                          tau[ , l] * tau[ , g] / (pi[l] * pi[g]) -
                          tau[ , k] * tau[ , l] / (pi[k] * pi[l]) +
                          tau[ , g] * (1 - tau[ , g]) / pi[g]^2)
                      )
        }
    }
    missinf <- matrix(0, nrow = (g - 1 + sum(pk + pks + 1 + one_or_zero)),
                      ncol = (g - 1 + sum(pk + pks + 1 + one_or_zero)))
    missinf[1: (g-1), 1: (g-1)] <- Im00
    
    for(k in 1:g){       
        xx <- one_or_zero + pk[k] + pks[k] + 1 
        ak <- c(if(one_or_zero) 0, seq_len(pk[k]), if(cl) s*(seq_len(pks[k])))
        Imkk <- matrix(0, nrow = xx, 
                       ncol = xx)
        for(i in seq_along(ak) - 1){
            for(j in i : (length(ak)-1)){
                for(t in 1:(n-p)){
                    
                    Imkk[i + 1, j + 1] <-
                        Imkk[j + 1, i + 1] <-
                        Imkk[i + 1, j + 1] + tau[t, k] * (1 - tau[t, k]) *
                        ui(y, t + p, ak[i+1]) *
                        ui(y, t + p, ak[j+1]) *
                        e[t, k] ^ 2 /
                        sigma[k] ^ 4
                }
            }
        }

        
        Imkk[xx, xx] <- sum(tau[,k] * (1 - tau[,k]) / sigma[k] ^ 2 * (e[ , k]^2/ sigma[k]^2 - 1)^2)
        
        for(i in seq_along(ak) - 1){
            for(t in 1 : (n-p)){
                Imkk[i+1, xx] <- 
                    Imkk[xx, i+1] <- Imkk[i+1, xx] +
                    tau[t, k] * (1 - tau[t,k]) * e[t, k] / sigma[k] ^ 3 *
                    (e[t, k] ^ 2 / sigma[k] ^ 2 - 1) * 
                    ui(y, t + p, ak[i+1])
                
            }
        }
        missinf[(g + sum(pk[0 : (k - 1)] + pks[0: (k - 1)] + 1 + one_or_zero) ) : 
                    (g + sum(pk[1:k] + pks[1:k] + 1 + one_or_zero) - 1),
                (g + sum(pk[0 : (k - 1)] + pks[0:(k-1)] + 1 + one_or_zero) ) : 
                    (g + sum(pk[1:k] + pks[1:k] + 1 + one_or_zero ) - 1)] <- Imkk
    }
    
    for(k in 1:(g-1)){
        xx <- one_or_zero + pk[k] + pks[k] + 1 
        ak <- c(if(one_or_zero) 0, seq_len(pk[k]), if(cl) s*(seq_len(pks[k])))
        Imk0 <- matrix(0, nrow = xx, ncol = g-1)
        for(i in seq_along(ak) - 1){
            for(t in 1:(n-p)){
                Imk0[i + 1, k] <- Imk0[i+1, k] +
                    ((1 - tau[t, k]) / pi[k] + tau[t, g] / pi[g]) *
                    tau[t, k] * ui(y, t + p, ak[i+1]) *
                    e[t, k] / sigma[k] ^ 2
                
                for(l in 1:(g-1)){
                    if(l != k) {Imk0[i + 1, l] <- Imk0[i+1, l] +
                            (tau[t, g] / pi[g] - tau[t, l]/ pi[l]) * 
                            tau[t,k] * ui(y, t+p, ak[i+1]) * e[t,k] / sigma[k] ^ 2}
                }
            }
        }
        Imk0[xx, k] <- sum( ( (1 - tau[,k]) / pi[k] + tau[ ,g] / pi[g]) *
                                tau[, k] / sigma[k] * 
                                (e[,k]^2 / sigma[k]^2 - 1))
        for(l in 1:(g-1)){
            if(l != k){
                Imk0[xx, l] <- sum((tau[ , g] / pi[g] - tau[ , l]/ pi[l]) *
                                       tau[, k] / sigma[k] * 
                                       (e[,k]^2 / sigma[k]^2 - 1))
            } 
        }
        missinf[1 : (g - 1),(g + sum(pk[0 : (k - 1)] + pks[0:(k-1)] + 1 + one_or_zero)) :
                    (g + sum(pk[1 : k] + pks[1:k] + 1 + one_or_zero) - 1)] <- t(Imk0)
        missinf[(g + sum(pk[0 : (k - 1)] + pks[0:(k-1)] + 1 + one_or_zero)) : 
                    (g + sum(pk[1 : k] + pks[1:k] + 1 + one_or_zero) - 1),
                1 : (g - 1)] <- Imk0
    }
    
    xx <- one_or_zero + pk[g] + pks[g] + 1 
    ak <- c(if(one_or_zero) 0, seq_len(pk[g]), if(cl) s*(seq_len(pks[g])))
    Img0 <- matrix(0, nrow = xx, ncol = (g - 1))
    for(k in 1:g-1){
        for(i in seq_along(ak) - 1){
            for(t in 1:(n-p)){
                Img0[i + 1, k] <-  Img0[i + 1, k] + 
                    (- tau[t, k] / pi[k] - (1 - tau[g]) / pi[g]) *
                    tau[t, g] * ui(y, t + p, ak[i+1]) *
                    e[t, g] / sigma[g] ^ 2
            }
        }
        Img0[xx, k] <- sum((-tau[ , k] / pi[k] - (1 - tau[ , g]) / pi[g]) *
                               tau[ , g] / sigma[g] * (e[ , g]^2 / sigma[g] ^ 2 - 1))
    }
    missinf[1 : (g - 1),
            (1 + nrow(missinf) - (pk[g] + pks[g] + 1 + one_or_zero) ) : nrow(missinf)] <- t(Img0)
    missinf[(1 + nrow(missinf) - (pk[g] + pks[g] + 1 + one_or_zero) ) : nrow(missinf),
            1 : (g - 1)] <- Img0
    
    
    
    for(k in 1:(g-1)){
        for(l in (k+1):g){
            xxk <- one_or_zero + pk[k] + pks[k] + 1 
            xxl <- one_or_zero + pk[l] + pks[l] + 1
            ak <- c(if(one_or_zero) 0, seq_len(pk[k]), if(cl) s*(seq_len(pks[k])))
            al <- c(if(one_or_zero) 0, seq_len(pk[l]), if(cl) s*(seq_len(pks[l])))
            Imkl <- matrix(0, nrow = xxk, ncol = xxl)
            for(i in seq_along(ak) - 1){
                for(j in seq_along(al) - 1){
                    for(t in 1:(n-p)){
                        Imkl[i+1, j+1] <- 
                            Imkl[i+1, j+1] -
                            tau[t, k] * tau[t, l] * 
                            ui(y, t+p, ak[i+1]) * ui(y, t+p, al[j+1]) *
                            e[t, k] * e[t, l] / (sigma[k] ^ 2 * sigma[l] ^ 2)
                    }
                    
                }
                
            }
            Imkl[xxk, xxl] <- sum(- tau[ , k] * tau[ , l] / (sigma[k] * sigma[l]) *
                                      (e[ , k] ^ 2 / sigma[k] ^ 2 - 1) *
                                      (e[ , l] ^ 2 / sigma[l] ^ 2 - 1))
            
            for(i in seq_along(ak) - 1){
                    for(t in 1:(n-p)){
                        
                        Imkl[i+1, xxl] <- 
                            Imkl[i+1, xxl] -
                            tau[t, k] * tau[t, l] * 
                            ui(y, t+p, ak[i+1]) * 
                                   e[t, k] / (sigma[k] ^ 2 * sigma[l]) *
                            (e[t, l]^2 / sigma[l] ^ 2 - 1)
                    }
            }
            for(i in seq_along(al) - 1){
                for(t in 1:(n-p)){
                    
                    Imkl[xxk, i+1] <- 
                        Imkl[xxk, i+1] -
                        tau[t, k] * tau[t, l] * 
                        ui(y, t+p, al[i+1]) * 
                        e[t, l] / (sigma[l] ^ 2 * sigma[k]) *
                        (e[t, k]^2 / sigma[k] ^ 2 - 1)
                }
            }
            missinf[(g + sum(pk[1 : (l - 1)] + pks[1:(l-1)] + 1 + one_or_zero)) : 
                        (g - 1 + sum(pk[1:l] + pks[1:l] + 1 + one_or_zero)),
                    (g + sum(pk[0 : (k - 1)] + pks[0:(k-1)] + 1 + one_or_zero)) :
                        (g + sum(pk[1:k] + pks[1:k] + 1 + one_or_zero) - 1)] <- t(Imkl)
            missinf[(g + sum(pk[0 : (k - 1)] + pks[0:(k-1)] + 1 + one_or_zero)) :
                        (g + sum(pk[1:k] + pks[1:k] + 1 + one_or_zero) - 1),
                    (g + sum(pk[0 : (l - 1)] + pks[0:(l-1)] + 1 + one_or_zero)) : 
                        (g + sum(pk[1:l] + pks[1:l] + 1 + one_or_zero) - 1)] <- Imkl
        }
    }
    
    I <- compinf - missinf
    cm <- pseudoInverse(I)
    se <- sqrt( diag( cm ) )
    se <- c( se[1 : (g-1)], sqrt( sum(cm[1 : (g - 1), 1 : (g - 1)]) ),
             se[ - c(1 : (g - 1))] )
    
    comp_se = list( )
    if(cl){
        for(i in 1:g) {
            if (i == 1) {
                comp_se[[i]] <- cbind( c( model@prob[i],
                                          if(one_or_zero) model@shift[i],
                                          if(pk[i] > 0) c(model@arcoef@a[[i]]),
                                          if(pks[i] > 0) c(model@arcoef@as[[i]]),
                                          model@scale[i] ),
                                       c( se[i], se[(g + 1) : (g + pk[i] + pks[i] + 1 +
                                                                   one_or_zero)] ) )
            } else {
                comp_se[[i]] <- cbind( c( model@prob[i],
                                          if(one_or_zero) model@shift[i],
                                          if(pk[i] > 0) c(model@arcoef@a[[i]]),
                                          if(pks[i] > 0) c(model@arcoef@as[[i]]),
                                          model@scale[i] ),
                                       c( se[i], se[(g + 1 + one_or_zero +
                                                         sum(pk[1 : (i - 1)] + pks[1:(i-1)] +1)) :
                                                        (g + sum(pk[1:i] + pks[1:i] + 1 +
                                                                     one_or_zero))] ) )
            }
            colnames(comp_se[[i]]) <- c("Estimate", "Standard Error")
            rownames(comp_se[[i]])[c(1, pk[i] + pks[i] + 2 + one_or_zero)] <-
                c("prob", "scale")
            if(one_or_zero) rownames(comp_se[[i]])[2] <-
                "shift"
            if(pk[i] > 0) rownames(comp_se[[i]])[(2 + one_or_zero) : (1 + one_or_zero + pk[i])] <- 
                paste("AR_", 1:pk[i], sep="")
            if(pks[i] > 0) rownames(comp_se[[i]])[(2 + one_or_zero + pk[i]) : 
                                                      (1 + one_or_zero + pk[i] + pks[i])] <-
                paste("AR_", c((1:pks[i])*s), sep="")
        }
        
    }else{
        for(i in 1:g) {
            if (i == 1) {
                comp_se[[i]] <- cbind( c( model@prob[i],
                                          if(one_or_zero) model@shift[i],
                                          if(pk[i] > 0) model@arcoef[i, 1:pk[i]],
                                          model@scale[i] ),
                                       c( se[i], se[(g + 1) : (g + pk[i] + 1 + one_or_zero)] ) )
            } else {
                comp_se[[i]] <- cbind( c( model@prob[i],
                                          if(one_or_zero) model@shift[i],
                                          if(pk[i] > 0) model@arcoef[i, 1:pk[i]],
                                          model@scale[i] ),
                                       c( se[i], se[(g + 1 +sum(pk[1 : (i - 1)] +1 + one_or_zero)) :
                                                        (g + sum(pk[1:i] + 1 + one_or_zero))] ) )
            }
            colnames(comp_se[[i]]) <- c("Estimate", "Standard Error")
            rownames(comp_se[[i]])[c(1,pk[i] + 2 + one_or_zero)] <-
                c("prob", "scale")
            if(one_or_zero) rownames(comp_se[[i]])[2] <-
                "shift"
            if(pk[i]>0) rownames(comp_se[[i]])[(2 + one_or_zero) : (1 + one_or_zero + pk[i])] <- 
                paste("AR_", 1:pk[i], sep="")
        }
    }
    names(comp_se) <- paste0("Component_", 1:g)
    
    list("standard_errors"      = comp_se,
         "covariance_matrix"    = cm,
         "Complete_Information" = compinf,
         "Missing_Information"  = missinf )
    
}



