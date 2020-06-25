test_that("diagnostics for mixAR work",{
    
    prob <- c(0.5,0.5)
    sigma <- c(1, 5)
    arco <- list(c(-0.5, 0.5), 1)
    
    model <- new("MixARGaussian", prob = prob, scale = sigma, arcoef = arco)
    model2 <- new("MixARGaussian", prob = prob, scale = sigma, arcoef = list(-0.5, 1))
    
    y <- mixAR_sim(model, 400, init = c(0, 0))
    
    fit1 <- fit_mixAR(y, model)
    
    bic1 <- mixAR_BIC(y, model, fix=NULL, comp_loglik=TRUE, index=3:400)
    
    expect_equal(bic1, mixAR_BIC(y, model))
    
    mixAR_BIC(y, model, fix="shift")
    mixAR_BIC(y, model, index = 100:200) 
    mixAR_BIC(y, fit1)
    
    expect_output( BIC_comp(list(model, model2), y) )
    expect_output( BIC_comp(list(model, model2, fit1), y) )
    
    d <- tsdiag(model, y = y, ask = FALSE)
    expect_equal(d, tsdiag_MixARGaussian(model, y = y, ask = FALSE) )

    mixAR_diag(model, y = y, ask = FALSE)
    mixAR_diag(fit1, y = y, ask = FALSE)
})
