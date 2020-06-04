library(mixAR)
context("tests of mixARemFixedPoint()")

test_that("mixARemFixedPoint works", {
    ## 2020-03-09
    ##     In v2.4 of package "fma" this gives error, at least in R-devel.
    ##     Replacing everywhere 'ibmclose' with 'fma::ibmclose',
    ##     which works in fma v2.4 and earlier versions of fma. 
    ## data(ibmclose, package = "fma") # ibm data from BJ

    m0 <- exampleModels$WL_ibm
    m1 <- mixARemFixedPoint(fma::ibmclose, m0)
    m1a <- mixARemFixedPoint(fma::ibmclose, m1$model)

    expect_equal_to_reference(m1, "m1.RDS")
    expect_equal_to_reference(m1a, "m1a.RDS")

    ms <- mixARemFixedPoint(fma::ibmclose, m0, est_shift = FALSE)
    expect_equal_to_reference(ms, "ms.RDS")

    set.seed(1234)
    ts1 <- mixAR_sim(m0, n = 1000, init = c(346, 352, 357), nskip = 0)
    m2a <- mixARemFixedPoint(ts1,       m0, est_shift = FALSE)$model
    m2b <- mixARemFixedPoint(diff(ts1), m0, est_shift = FALSE)$model

    expect_equal_to_reference(m2a, "m2a.RDS")
    expect_equal_to_reference(m2b, "m2b.RDS")

})

