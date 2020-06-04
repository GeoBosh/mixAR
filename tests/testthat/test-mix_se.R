library(mixAR)
context("tests of mix_se()")

test_that("mix_se works", {
    ## Davide's example with IBM data

    ## 2020-03-09
    ##     In v2.4 of package "fma" this gives error, at least in R-devel.
    ##     Replacing everywhere 'ibmclose' with 'fma::ibmclose',
    ##     which works in fma v2.4 and earlier versions of fma.
    ##
    ## data(ibmclose, package = "fma") ## TODO: Suggests: fma

    ##Wong&Li estimates as starting values
    ## 
    ##
    moWLprob  <- c(0.5439, 0.4176, 0.0385)
    moWLsigma <- c(4.8227, 6.0082, 18.1716)

    moWLibm   <- new("mixARGaussian", prob = moWLprob, scale = moWLsigma, order = c(1, 1, 0))
    
    moWLibm@arcoef@a[[1]] <- -0.3208 ; moWLibm@arcoef@a[[2]] <- 0.6711
    
    IBM <- diff(fma::ibmclose)

    expect_equal_to_reference(mix_se(as.numeric(IBM), moWLibm, fix_shift=TRUE)$'standard_errors',
                              "mix_se_ibmclose.rds")
    

    ## TODO:
    ## use "as.numeric()" because function "mix_ek" has no method for this
    ## such combination of objects apparently
    
    ## @Georgi: when I put fix_shift=FALSE here, the resulting model is a degenerate case 
    ## (one of the components only gets assigned 2 observations, so that its variance
    ##  is practically 0)  
    ## I think that creates a problem computationally.
    

    ## Wong&Li estimates using mix_se, with Wong&Li estimates in brackets

    seWLibm <- list("Component_1" = cbind(c(0.5439, -0.3208, 4.8227),
                                          c(0.0898, 0.0725, 0.4850)), 
                    "Component_2" = cbind(c(0.4176, 0.6711, 6.0082),
                                          c(0.0865, 0.1227, 0.7203)),
                    "Component_3" = cbind(c(0.0385, 18.1716),
                                          c(0.0326, 5.6881)))
    
    colnames(seWLibm[[1]]) <- c("Estimate", "Standard Error")
    colnames(seWLibm[[2]]) <- c("Estimate", "Standard Error")
    colnames(seWLibm[[3]]) <- c("Estimate", "Standard Error")
    
    rownames(seWLibm[[1]]) <- c("prob", "AR_1", "scale")
    rownames(seWLibm[[2]]) <- c("prob", "AR_1", "scale")
    rownames(seWLibm[[3]]) <- c("prob", "scale")
    
    expect_equal(lapply(mix_se(as.numeric(IBM), moWLibm, fix_shift=TRUE)$'standard_errors', 
                        round, 4),
                 seWLibm, tolerance = 0.002)
    

## Example with fix_shift=FALSE

data(lynx)
loglynx <- as.numeric(log(lynx))

lynxprob  <- c(0.3163, 0.6837)
lynxscale <- c(0.2043, 0.4899)
lynxshift <- c(1.6364, 2.2529)
lynxar    <- list(c(1.1022, -0.2835), c(1.5279, -0.8871))

lynxmodel <- new("mixARGaussian", prob = lynxprob, scale = lynxscale, shift = lynxshift,
                 arcoef = lynxar)

expect_equal_to_reference(mix_se(loglynx, lynxmodel, fix_shift=FALSE)$'standard_errors',
                          "mix_se_lynx.rds")

mix_se(loglynx, fit_mixAR(loglynx, lynxmodel))
expect_error(mix_se(loglynx, list(lynxmodel)))

### Test that default value of fix_shift is FALSE

expect_identical(mix_se(loglynx, lynxmodel), mix_se(loglynx, lynxmodel, fix_shift=FALSE))


## We expect some "NaN"s when we use parameter values other than MLEs
## so test for warnings

lynxWLprob  <- c(0.3163, 0.6837)
lynxWLscale <- c(0.0887, 0.2020)
lynxWLshift <- c(0.7107, 0.9784)
lynxWLar    <- list(c(1.1022, -0.2835), c(1.5279, -0.8871))

lynxWLmodel <- new("mixARGaussian", prob = lynxWLprob, scale = lynxWLscale, 
                   shift = lynxWLshift, arcoef = lynxWLar)

expect_warning(mix_se(loglynx, lynxWLmodel))


### Test seasonal model

probS <- c(0.5, 0.5)
sigmaS <- c(5, 1)
ar1 <- list(c(0.5, -0.5), c(1.1, 0, -0.5))
ar12 <- list(0, c(-0.3, 0.1))
s <- 12

rag <- new("raggedCoefS", a=ar1, as=ar12, s=s)

modelS <- new("mixARGaussian", prob=probS, scale=sigmaS, arcoef=rag)
yS <- mixAR_sim(modelS, n=200, init=rep(0,24))

fitS <- fit_mixAR(yS, modelS)
mix_se(yS, fitS)

})


