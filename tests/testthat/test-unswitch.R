test_that("unswitch works", {
    ## from Examples
    
    ## N is small for to make the examples run instantly.
    set.seed(1234)
    aII10data <- mixARExperiment(exampleModels$WL_II, N = 5, raw = TRUE,
                                 estargs = list(crit = 1e-4))
    aII10 <- test_unswitch(aII10data$Raw, exampleModels$WL_II)
    aII10
    
    aII10adata <- mixARExperiment(exampleModels$WL_II, N = 5, raw = TRUE,
                                  simargs = list(n = 100), estargs = list(crit = 1e-4))
    aII10a <- test_unswitch(aII10adata$Raw, exampleModels$WL_II)
    aII10a
})

