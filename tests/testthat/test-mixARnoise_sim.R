test_that("mixARnoise_sim works", {
    ## from Examples

    set.seed = 1234
    z <- sample(2, size = 5, replace = TRUE)
    mixARnoise_sim(list(rnorm, function(n) rt(n, 5)), z)
})
