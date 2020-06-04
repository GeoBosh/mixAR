context("Testing exampleModels")

test_that("exampleModels is consistent with the old moWL objects", {

    ## these are the all names, before putting them in 'exampleModels'
    moWLprob <- c(0.5439, 0.4176, 0.0385)              # model coefficients from Wong&Li
    moWLsigma <- c(4.8227, 6.0082, 18.1716)
    moWLar <- list(c(0.6792, 0.3208), c(1.6711, -0.6711), 1)

    moWL <- new("mixARGaussian", prob = moWLprob, scale = moWLsigma, arcoef = moWLar)

    moWL_A <- new("mixARGaussian"                         # WongLi, model A
                , prob = c(0.5, 0.5)
                , scale = c(5, 1)
                , shift = c(0, 0)
                , arcoef = list(c(0.5), c(1.1))
                  )

    moWL_B <- new("mixARGaussian"                         # WongLi, model B
                , prob = c(0.75, 0.25)
                , scale = c(5, 1)
                , shift = c(0, 0)
                , arcoef = list(c(0.5), c(1.4))
                  )

    moWL_I <- new("mixARGaussian"                          # WongLi, model I
                , prob = c(0.4, 0.3, 0.3)
                , scale = c(1, 1, 5)
                , shift = c(0, 0, -5)
                , arcoef = list(c(0.9, -0.6), c(-0.5), c(1.50, -0.74, 0.12))
                  )

    moWL_II <- new("mixARGaussian"                         # WongLi, model II
                 , prob = c(0.4, 0.3, 0.3)
                 , scale = c(1, 1, 5)
                 , shift = c(5, 0, -5)
                 , arcoef = list(c(0.9, -0.6), c(-0.7, 0), c( 0, 0.80))
                   )

    moWLgen <-
        new("mixARgen", prob=moWLprob, scale=moWLsigma, arcoef=moWLar,
               dist=list(dist_norm))

    moWLt3v <-
        new("mixARgen", prob=moWLprob, scale=moWLsigma, arcoef=moWLar,
               dist=list(fdist_stdt(3,fixed=FALSE)))

    moWLtf <-
        new("mixARgen", prob=moWLprob, scale=moWLsigma, arcoef=moWLar,
              dist=list(generator=function(par) fn_stdt(par,fixed=FALSE),param=c(20,30,40)))

    moT_A <-
        new("mixARgen"
               , prob = c(0.5,0.5)
               , scale = c(1, 2)
               , shift = c(0, 0)
               , arcoef = list(c(-0.5), c(1.1))
                                                                  #  t4, t8
               , dist=list(generator=function(par) fn_stdt(par,fixed=FALSE),param=c(4,8))
               )

    moT_B <-
        new("mixARgen"
               , prob = c(0.3, 0.3, 0.4)
               , scale = c(2, 1, 0.5)
               , shift = c(5, -5, 0)
               , arcoef = list(c(0.5, 0.24), c(-0.9), c(1.5, -0.74, 0.12))
                                        # t4, t6, t10
                 # dist=list(generator=function(par) fn_stdt(par,fixed=FALSE),param=c(4,6,10))
               , dist = distlist(c("stdt", "stdt", "stdt"), c(4,6,10))
               )

    moT_B2 <-
        new("mixARgen"
               , prob = c(0.3, 0.3, 0.4)
               , scale = c(2, 1, 0.5)
               , shift = c(5, -5, 0)
               , arcoef = list(c(0.5, 0.24), c(-0.9), c(1.5, -0.74, 0.12))
                                        # t4, t4, t10
               , dist=list(generator=function(par) ft_stdt(par,
                                                           fixed = c(FALSE, TRUE),
                                                           n = 3,
                                                   tr = function(x,k) if(k<3) x[1] else x[2]),
                           param = c(4, 10))
               )

    moT_B3 <-
        new("mixARgen"
               , prob = c(0.3, 0.3, 0.4)
               , scale = c(2, 1, 0.5)
               , shift = c(5, -5, 0)
               , arcoef = list(c(0.5, 0.24), c(-0.9), c(1.5, -0.74, 0.12))
                                        # t4, t4, t10
               , dist = distlist("stdt", c(4,10), fixed = c(FALSE, TRUE), tr = c(1,1,2))
               )

    moT_C1 <-
        new("mixARgen"
              , prob = c(0.3, 0.3, 0.4)
              , scale = c(2, 1, 0.5)
              , shift = c(5, -5, 0)
              , arcoef = list(c(0.5, 0.24), c(-0.9), c(1.5, -0.74, 0.12))
                                        # t4, t7, N(0,1)
              , dist = distlist(c("stdt", "stdt", "stdnorm"), c(4,7))
              )

    moT_C2 <-
        new("mixARgen"
              , model = exampleModels$WL_Bt_1
              , dist = distlist(c("stdt", "stdt", "stdnorm"), c(4,7))  # t4, t7, N(0,1)
              )

    moT_C3 <-
        new("mixARGaussian", model = exampleModels$WL_Bt_1)

    
    ## these tests show equivalence to old example objects
    expect_identical(c(0.5439,0.4176,0.0385), exampleModels$WL_ibm@prob)
    expect_identical(c(4.8227,6.0082,18.1716), exampleModels$WL_ibm@scale)
    expect_identical(list(c(0.6792,0.3208), c(1.6711,-0.6711), 1),
                     exampleModels$WL_ibm@arcoef@a)

    expect_identical(moWLprob,  exampleModels$WL_ibm@prob)
    expect_identical(moWLsigma, exampleModels$WL_ibm@scale)
    expect_identical(moWLar,    exampleModels$WL_ibm@arcoef@a)

    expect_identical(moWL    , exampleModels$WL_ibm )
    expect_identical(moWL_A  , exampleModels$WL_A   )
    expect_identical(moWL_B  , exampleModels$WL_B   )
    expect_identical(moWL_I  , exampleModels$WL_I   )
    expect_identical(moWL_II , exampleModels$WL_II  )

    expect_identical(moWLgen, exampleModels$WL_ibm_gen)
    ## the rest contain environments and are equal but not identical
    expect_equal(moWLt3v, exampleModels$WL_ibm_t3v)
    expect_equal(moWLtf,  exampleModels$WL_ibm_tf)

    expect_equal(moT_A , exampleModels$WL_At  )
    expect_equal(moT_B , exampleModels$WL_Bt_1)
    expect_equal(moT_B2, exampleModels$WL_Bt_2)
    expect_equal(moT_B3, exampleModels$WL_Bt_3)
    expect_equal(moT_C1, exampleModels$WL_Ct_1)
    expect_equal(moT_C2, exampleModels$WL_Ct_2)
    expect_equal(moT_C3, exampleModels$WL_Ct_3)

})
