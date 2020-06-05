test_that("mixVAR and new() for mixVAR work", {
    
    ######## Model for PortfolioData1
    
    Sigma1 <- matrix(round(c(
        1.043263e-03, 0.0001088793, 0.0001531034, 7.727422e-05,
        1.088793e-04, 0.0002877020, 0.0002298401, 1.247476e-04,
        1.531034e-04, 0.0002298401, 0.0006942631, 1.094758e-04,
        7.727422e-05, 0.0001247476, 0.0001094758, 5.605651e-04),6), nrow=4, byrow=T)
    Sigma2 <- matrix(round(c(
        1.757501e-04, 4.393529e-05, 4.812120e-05, 3.855004e-05,
        4.393529e-05, 6.408360e-05, 4.664858e-05, 2.616022e-05,
        4.812120e-05, 4.664858e-05, 8.904955e-05, 3.578720e-05,
        3.855004e-05, 2.616022e-05, 3.578720e-05, 4.873475e-05),6), nrow=4, byrow=T)
    Sigma3 <- matrix(round(c(
        8.182502e-05, 7.342250e-05, 7.593666e-05, 3.899146e-05,
        7.342250e-05, 1.615862e-04, 1.478421e-04, 7.862971e-05,
        7.593666e-05, 1.478421e-04, 3.466054e-04, 9.317318e-05,
        3.899146e-05, 7.862971e-05, 9.317318e-05, 9.935612e-05),6), nrow=4, byrow=T)
    
    AR <- list()
    AR[[1]] <- array(c(
        matrix(c(0.6, 0.5, -0.1, 0.1,
                 0.2, 0.4, 0, 0.2,
                 0.2, 0.25, 0.1, -0.15,
                 0.2, 0.05, -0.05, 0.1), nrow=4, byrow=T),
        matrix(c(-0.25, 0.8, -0.05, 0.8,
                 -0.1, 0.2, -0.1, 0.35,
                 -0.1, -0.2, 0.3, -0.05,
                 -0.2, 0.2, 0, 0.2), nrow=4, byrow=T),
        matrix(c(0.2, -0.1, 1.2, -0.7,
                 0.1, 0.1, 0.35, -0.45,
                 -0.05, -0.4, 0.7, -0.05,
                 0.1, -0.4, 0.4, 0.2), nrow=4, byrow=T)), dim=c(4,4,3))
    
    AR[[2]] <- array(c(
        matrix(c(0, -0.1, -0.1, 0.1,
                 -0.1, 0.05, -0.05, 0,
                 -0.1, -0.1, 0.05, 0.1,
                 -0.1, 0, 0, 0.05), nrow=4, byrow=T),
        matrix(c(0, -0.05, -0.05, 0.1,
                 -0.03, -0.03, 0, 0.05,
                 0, -0.1, 0, 0.07,
                 -0.1, -0.03, -0.05, 0.1), nrow=4, byrow=T)), dim=c(4,4,2))
    AR[[3]] <- array(c(
        matrix(c(0.1, -0.2, 0, 0,
                 -0.03, -0.7, 0.1, -0.1,
                 -0.1, -0.2, -0.2, 0.2,
                 0, -0.05, -0.1, -0.15), nrow=4, byrow=T)), dim=c(4,4,1))
    
    prob <- c(0.1381, 0.5926, 0.2693)
    shift <- matrix(c(-0.0076, 0.0021, 0.0018,
                      -0.0062, 0.0020, 0.003,
                      -0.0017, 0.0018, 0.0016,
                      -0.0043, 0.0008, 0.0012), nrow=4, byrow=T)
    
    Scale <- array(c(Sigma1, Sigma2,Sigma3), dim=c(4,4,3))
    
    model <- new("MixVARGaussian", prob=prob, shift=shift, vcov=Scale, arcoef=AR)
    
    ### Test missing inputs
    
    expect_error(new("MixVARGaussian", prob = prob, shift = shift, vcov = Scale))
    new("MixVARGaussian", shift = shift, vcov = Scale, arcoef = AR)
    new("MixVARGaussian", prob = 1/3, shift = shift, vcov = Scale, arcoef = AR)
    new("MixVARGaussian", prob = 1/3, vcov = Scale, arcoef = AR)
    new("MixVARGaussian", prob = 1/3, shift = matrix(rep(0, 4), ncol = 1), vcov = Scale, arcoef = AR)
    new("MixVARGaussian", prob = 1/3, shift = shift, arcoef = AR)
    new("MixVARGaussian", prob = 1/3, shift = shift, vcov = diag(rep(1, 4)), arcoef = AR)
    
    new("MixVARGaussian", model = model)
    expect_error(new("MixVARGaussian", model = model, order=c(1, 2)))
    expect_error(new("MixVARGaussian", model = model, order=c(2, 1, 1)))
    new("MixVARGaussian", model = model, prob = 1/3)
    expect_error(new("MixVARGaussian", model = model, prob = c(1,2)))
    new("MixVARGaussian", model = model, shift = matrix(rep(0, 4), ncol=1))
    expect_error(new("MixVARGaussian", model = model, shift = matrix(rep(0, 4), ncol=2)))
    new("MixVARGaussian", model = model, arcoef = AR)
    expect_error(new("MixVARGaussian", model = model, arcoef = AR, p=c(1,2)))
    
    ###################################
    

    expect_output(show(model))
    
    expect_error(new("MixVARGaussian", prob=c(1,0), shift=shift, vcov=Scale, arcoef=AR))#,
    # "Arg. `prob' has wrong length (should be 1 or the length of `order')." ) 
    expect_error(new("MixVARGaussian", prob=prob, shift=matrix(0,ncol=2, nrow=4), 
                     vcov=Scale, arcoef=AR))#,
    # "Arg. `shift' has wrong length (should be a matrix with 
    #     colums equal to 1 or the length of `order')." ) 
    expect_error(new("MixVARGaussian", prob=prob, shift=shift, vcov=Scale[,,1:2], arcoef=AR))#,
    #   "Arg. `vcov' has wrong dimensions (should be an array of dim. 1 or
    #     the length of `order').")
    expect_message(new("MixVARGaussian", prob=prob, shift=shift, vcov=Scale, arcoef=AR, order=c(1,2)))
    
    
    companion_matrix(model@arcoef[1], 12)
    companion_matrix(model@arcoef[1], 20)
    companion_matrix(model@arcoef[2], 12)    
    
    expect_error(companion_matrix(model@arcoef[1], 18))
    
    isStable(model)
    
    set.seed(21042020)
    ts1 <- mixVAR_sim(model, n = 200)
    ts1_reg <- mixVAR_sim(model, n = 200, flag = TRUE)
    ## show that missing "init" sets "init" to matrix of 0s
    
    set.seed(21042020)
    ts2 <- mixVAR_sim(model, n = 200, init = diag(0, nrow = 3, ncol = 4))
    
    expect_identical(ts1, ts2)
    
    ## Throw error if "init" is shorter than the maximum order
    
    expect_error(mixVAR_sim(model, 200, init = rep(0, 4))) 
    
    ####### Test fit_mixVAR
    
    expect_error(fit_mixVAR())
    
    fit1 <- fit_mixVAR(ts1, model)
    expect_equal(fit1, fit_mixVAR(t(ts1), model))

    fixmod<- fit_mixVAR(ts1, model, fix="shift")
    
    expect_identical(model@shift, fixmod$model@shift)
    
    
})
