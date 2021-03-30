test_that("functions in predict.R are ok", {
    predict_coef(exampleModels$WL_ibm, maxh = 3)

    ## exact method, without xcond
    dist <- multiStep_dist(exampleModels$WL_ibm, maxh = 3)
    tfpdf <- dist(3, "pdf", xcond = c(560, 600)) # xcond is argument to 'dist' here
    tfcdf <- dist(3, "cdf", xcond = c(560, 600))
    expect_message(dist(3, "cdf", xcond = c(300, 560, 600)),
                   "using the last ")
    expect_error(dist(3, "cdf", xcond = c(600)),
                 "length\\(xcond\\) must be >= maximal AR order")
    
    ## currently 2nd argument is anything besides "pdf" and "cdf"
    ## for the method above. (looks like *:TODO:* - see the method with "N = ..." below
    dist(3, "model", xcond = c(560, 600))
    
## use a simulation method with N = 1000
tf  <- multiStep_dist(exampleModels$WL_ibm, maxh = 3, N = 1000, xcond = c(560, 600))
tfpdf <- tf(3, "pdf")
tfcdf <- tf(3, "cdf")
tf(3, "location")
tf(3, "sd")
tf(3, "variance")
tf(3, "skewness")
tf(3, "kurtosis")
tf(3, "summary")
tf(3, function(x) sum(x)/length(x))
expect_error(tf(3, pi), "Argument 'what' is of incorrect type")
    
## get the raw data
tfs <- tf(1, "sampled")
apply(tfs, 1, mean) # location for lags from 1 to maxh (here 3)

## the exact method may also be used with fixed xcond:
tfe <- multiStep_dist(exampleModels$WL_ibm, maxh = 3, xcond = c(560, 600))

## get pdf and cdf for horizon 3
tfepdf <- tfe(3, "pdf")
tfecdf <- tfe(3, "cdf")
invisible(tfe(3, "model"))

    tfe_mes <- multiStep_dist(exampleModels$WL_ibm, maxh = 3, xcond = c(300, 560, 600))
    tfe_err <- multiStep_dist(exampleModels$WL_ibm, maxh = 3, xcond = c(600))
    
    expect_message(tfe_mes(3, "pdf"), "using the last ")
    expect_error(tfe_err(3, "pdf"), "length\\(xcond\\) must be >= maximal AR order")
    

    
})

test_that("functions for 'predict' work", {
    
    #extend_index(rbind(c(1,2),c(3,4)), g = 2)
    
    m <- new("MixARGaussian", prob = c(.5 ,.5), scale = c(1,2), arcoef = list(-0.5, 1.1))
    
    predict_coef(m, maxh = 2)
    
    

    })



