test_that("mixARreg and fit_mixARreg work", {
    
    xReg  <- data.frame(rnorm(200,7,1), rt(200,3),rnorm(200,3,2))
    xReg2 <- seq(-10,10,length.out=200)
    ## Build mixAR part
    
    probReg <- c(0.7, 0.3)
    sigmaReg <- c(1, 5)
    arReg <- list(c(-0.5, 0.5), 1.1)
    
    modelReg <- new("mixARGaussian", prob=probReg, scale=sigmaReg, arcoef=arReg)
    
    ##Simulate from mixAR part
    uReg <- mixAR_sim(modelReg, 200, c(0,0))
    
    ## Model yReg is:
    ## y = 10 + x1 + 3* x2 + 2 * x3 + e
    ## Model yReg2 is:
    ## y = 10 + x
    ## uReg is mixAR, same for both models
    
    yReg <- xReg[,1] + 3 * xReg[,2] + 2 * xReg[,3] + uReg
    yReg2 <- 10 + xReg2 + uReg
    
    fit1 <- mixARreg(yReg, xReg, modelReg)
    fit2 <- mixARreg(yReg2, xReg2, modelReg)
    
    expect_identical(class(fit1), "list")
    expect_length(fit1, 4)
    expect_s3_class(fit1$reg, "lm")
    expect_true(inherits(fit1$mixARmodel, "mixAR"))
    
    fit3 <- fit_mixARreg(yReg, xReg, modelReg)
    fit4 <- fit_mixARreg(yReg, as.matrix(xReg), modelReg)
    
    expect_equal(fit1, fit3)
    expect_equal(fit3, fit4)
    
    EMinit <- list(prob = probReg, scale = sigmaReg, arcoef = arReg)
    
    fit5 <- fit_mixARreg(yReg, xReg, EMinit = EMinit)
    
    expect_equal(fit1, fit5)
    
    expect_error(fit_mixARreg())
    expect_error(fit_mixARreg(yReg[1:100], xReg[1:150,], modelReg))
    expect_error(fit_mixARreg(yReg, xReg, EMinit = list(c(1,2), c(3,4), list(5, 6))))
    
    
})