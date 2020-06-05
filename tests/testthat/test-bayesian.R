
#### Testing Bayesian functions.

test_that("Functions for Bayesian mixAR work", {
    
    model <- new("MixARGaussian", prob = c(.7,.3), scale = c(1, 2), arcoef = list(-0.5, 1.1))
    model2 <- new("MixARGaussian", prob = c(.5,.3, .2), scale = c(1, 2, 3), 
                  arcoef = list(c(-0.5,0.1), 1.1,0.5))
    
    nsim = 20
    
    g <- length(model@prob)
    p <- max(model@order)
    
    set.seed(15042020)
    y <- mixAR_sim(model, 300, init = 0)
    
    ############## Test sampling function for mixing weights
    
    
    bk1 <- sapply(model@arcoef@a, function(x) 1 - sum(x))
    bk2 <- sapply(model2@arcoef@a, function(x) 1 - sum(x))
    mu1 <- model@shift / bk1 ; mu2 <- model2@shift / bk2
    set.seed(123)
    Zpi <- sampZpi(y, model@order, model@prob, mu1, 
                   model@arcoef@a, model@scale, nsim, d = 1)
    
    ## Now change the length of "d" and have identical result
    
    set.seed(123)
    Zpi2 <- sampZpi(y, model@order, model@prob,mu2, 
                    model@arcoef@a, model@scale, nsim, d = c(1, 1))
    
    expect_identical(Zpi, Zpi2)
    
    #expect_equal_to_reference(Zpi, "sampZpi.rds") 
    
    ## Expect error if length of "d" is different from 1 or g
    
    expect_error(sampZpi(y, model@order, model@prob, mu1, 
                         model@arcoef@a, model@scale, nsim, d = rep(1, 3)))
    
    expect_error(sampZpi(y, model2@order, model2@prob, mu2, 
                         model2@arcoef@a, model2@scale, nsim, d = rep(1, 2)))
    
    ## test dimensions of output
    
    expect_equal(length(Zpi), 3)   ## output should be a list of 3
    
    expect_equal(dim(Zpi$mix_weights), c(nsim, g)) 
    expect_equal(dim(Zpi$latentZ), c(length(y) - p, g))
    expect_equal(length(Zpi$nk), g)
    
    ############## Test sampling function for component mean/shift
    
    #set.seed(234)
    mu_shift <- sampMuShift(y, model@order, 1/model@scale^2, Zpi$nk, 
                                       model@shift, Zpi$latentZ, model@arcoef@a, nsim = 20)
    
    #expect_equal_to_reference(mu_shift, "sampMuShift.rds")
    
    
    ## test dimension of output
    
    expect_equal(length(mu_shift), 2)  ## a list of 2
    expect_equal(dim(mu_shift$shift), c(nsim, g))
    expect_identical(dim(mu_shift$shift), dim(mu_shift$mu))
    
    
    ############## Test sampling function for component scale/precision
    
    #set.seed(345)
    
    # a = 0.2 , c = 2 are hyperparameters
    
    sigma_tau <- sampSigmaTau(y, model@order, 1/model@scale^2, Zpi$nk, model@arcoef@a, 
                              mu_shift$mu[nsim, ], Zpi$latentZ, a = 0.2 , c = 2, nsim)
    
    #expect_equal_to_reference(sigma_tau, "sampSigmaTau.rds")
    
    ## test dimension of output
    
    expect_equal(length(sigma_tau), 3)  ## a list of 2
    expect_equal(dim(sigma_tau$scale), c(nsim, g))
    expect_identical(dim(sigma_tau$scale), dim(sigma_tau$precision))
    expect_equal(length(sigma_tau$lambda), nsim)
    
    
    ############## Test bayes_mixAR
    
    nsim = 50
    burnin = 20
    tau <- c(.1, .2)
    
    #set.seed(456)    
    
    bayes <- bayes_mixAR(y, model, fix_shift = FALSE, a = 0.2, c = 2, tau, nsim = 50, burnin = 20)
    
    #expect_equal_to_reference(bayes, "bayes_mixAR.rds")
    
    ## test dimension of output
    
    expect_equal(length(bayes), 11) ## a list of 11
    expect_equal(dim(bayes$mix_weights), c(nsim - burnin, g))
    expect_identical(dim(bayes$mix_weights), dim(bayes$scale))
    expect_identical(dim(bayes$mix_weights), dim(bayes$precision))
    expect_identical(dim(bayes$mix_weights), dim(bayes$mu))
    expect_identical(dim(bayes$mix_weights), dim(bayes$shift))
    
    expect_equal(dim(bayes$LatentZ), c(length(y) - p, g))
    
    expect_equal(length(bayes$ARcoeff), g)
    expect_equal(dim(bayes$ARcoeff$Component_1), c(nsim - burnin, model@order[1]))
    expect_equal(dim(bayes$ARcoeff$Component_2), c(nsim-burnin, model@order[2]))
    expect_equal(bayes$n_samp, nsim-burnin)
    expect_equal(bayes$ncomp, g)
    
    expect_error(bayes_mixAR(y, model, fix_shift = FALSE, a = 0.2, c = 2, 
                          tau=c(1, 2, 3), nsim = 11, burnin = 1))
    expect_error(bayes_mixAR(y, model2, fix_shift = FALSE, a = 0.2, c = 2, 
                          tau=c(1, 2), nsim = 11, burnin = 1))
    
    ############## Test Choose_pk
    
    ar_orders <- Choose_pk(y, model, fix_shift = FALSE, tau, pmax = 5, method = "NULL",
                            par = NULL, nsim = 10)  
    
    expect_equal(ncol(ar_orders$x), g + 1)
    expect_equal(sum(ar_orders$x[ , g + 1]), 1)
    
    expect_error(Choose_pk(y, model, fix_shift = FALSE, tau, pmax = 5, 
                           method = c("Poisson", "NULL"), par = NULL, nsim = 10))
    
    expect_error(Choose_pk(y, model, fix_shift = FALSE, tau = c(1,2,3), pmax = 5, 
                           method = "NULL", par = NULL, nsim = 10))
    
    expect_error(Choose_pk(y, model2, fix_shift = FALSE, tau = c(1,2), pmax = 5, 
                           method = "NULL", par = NULL, nsim = 10))
    
    expect_error(Choose_pk(y, model2, fix_shift = FALSE, tau = c(1,2), pmax = 1, 
                           method = "NULL", par = NULL, nsimv= 10))
    
    expect_error(Choose_pk(y, model2, fix_shift = FALSE, tau = c(1,2), pmax = -1, 
                           method = "NULL", par = NULL, nsim = 10))
    
    ############## Test marg_loglik
    
    marginal <- marg_loglik(y, model, tau, nsim = 10, prob_mod = ar_orders$x[1, g + 1])
    
    expect_equal(length(marginal), 5)
    expect_equal(length(marginal$phi_hd), g)
    expect_equal(length(marginal$phi_hd[[1]]), model@order[1])
    expect_equal(length(marginal$phi_hd[[2]]), model@order[2])
    expect_equal(length(marginal$prec_hd), g)
    expect_identical(length(marginal$prec_hd), length(marginal$mu_hd))
    expect_identical(length(marginal$prec_hd), length(marginal$weig_hd))
    
    expect_error(marg_loglik(y, model, tau, nsim = 10, prob_mod = -1))
    expect_error(marg_loglik(y, model, tau, nsim = 10, prob_mod =  2))
    expect_error(marg_loglik(y, model, tau = c(1, 2, 3), nsim = 10, 
                             prob_mod = ar_orders$x[1, g + 1]))
    expect_error(marg_loglik(y, model2, tau = c(1, 2), nsim = 10, 
                             prob_mod = ar_orders$x[1, g + 1]))
    
    marg_loglik(y, model2, tau=c(.1), nsim = 10, prob_mod = 0.3)
    
    
    ########### Test label_switch()
    
    label_switch(rbind(bayes$scale[1:25,], bayes$scale[26:30, 2:1]) , m=5)
    
})
