test_that("mixAR and new() for mixAR work", {

    fdist_stdnorm() ## not needed - there is only one N(0,1) and it is created
    ## dist_norm       ##              at build time (see dist_norm)
    lapply(dist_norm, function(f) f)
    names(dist_norm)
    
    ## from examples
    
    ## some familiar positive integer moments
    stdnormmoment(1:6)
    ## fractional moments of N(0,1) currently give NaN
    stdnormmoment(seq(1, 6, by = 0.5))
    ## abs moments don't need to be integer
    curve(stdnormabsmoment, from = 0, to = 6, type = "l", col = "blue")

    ## standardised-t
    stdtmoment(5, 1:6)
    stdtabsmoment(5, 1:3)
    stdtabsmoment(5, 4:6)

    param_score_stdt(2, 5)
    param_score_stdt(2, 10)

    ## Student-t
    tabsmoment(5, 1:3)
    tabsmoment(5, 4:6)

})
