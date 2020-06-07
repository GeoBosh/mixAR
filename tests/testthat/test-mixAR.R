library(mixAR)

test_that("mixAR and new() for mixAR work", {

    expect_output(show(new("MixARGaussian", order = c(0, 0, 0))))

    new("MixARGaussian", order = c(3, 2, 1))
    m2 <- new("MixARGaussian", order = c(3, 2, 1),
              arcoef = matrix(c(1:3, c(1:2, 0), c(1, 0, 0)), nrow = 3, byrow = TRUE))
    m3 <- new("MixARGaussian", arcoef = list(1:3, 1:2, 1))
    expect_equal(m2, m3)

    as(m2, "list")
    
    ## expect_identical(m2, m3) # not identical: > class(m2@arcoef@a[[1]])
    ##                                           [1] "numeric"
    ##                                           > class(m3@arcoef@a[[1]])
    ##                                           [1] "integer"
    expect_output(show(m2))
    expect_output(show(exampleModels$WL_At))

    expect_output(show_diff(m2, m3))
    expect_output(show_diff(exampleModels$WL_At, exampleModels$WL_ibm))
    expect_output(show_diff(exampleModels$WL_ibm, exampleModels$WL_At))
    expect_output(show_diff(exampleModels$WL_Bt_1, exampleModels$WL_Bt_2))

    expect_message(new("MixARGaussian", order = c(1, 2, 3), arcoef = list(1:3, 1:2, 1)),
                 "Arg's 'arcoef' and 'order' are not consistent, ignoring 'order'")
    
    new("MixARGaussian", order = c(3, 2, 1), prob = 1)
    new("MixARGaussian", order = c(3, 2, 1), prob = c(0.5, 0.25, 0.25) )
    expect_error(new("MixARGaussian", order = c(3, 2, 1), prob = c(0.5, 0.25)),
                     "length of 'prob' should be 1 or the length of 'order'" )    

    new("MixARGaussian", order = c(3, 2, 1), shift = 1)
    new("MixARGaussian", order = c(3, 2, 1), shift = c(0.5, 0.25, 0.25) )
    expect_error(new("MixARGaussian", order = c(3, 2, 1), shift = c(0.5, 0.25)),
                     "length of 'shift' should be 1 or the length of 'order'" )    

    new("MixARGaussian", order = c(3, 2, 1), scale = 1)
    new("MixARGaussian", order = c(3, 2, 1), scale = c(0.5, 0.25, 0.25) )
    expect_error(new("MixARGaussian", order = c(3, 2, 1), scale = c(0.5, 0.25)),
                     "length of 'scale' should be 1 or the length of 'order'" )    

    new("MixARGaussian", model = exampleModels$WL_At)
    expect_output(show_diff(new("MixARGaussian", model = exampleModels$WL_At),
                            exampleModels$WL_At))

    expect_error(new("MixARGaussian", model = exampleModels$WL_At, order = c(2,1)),
                 # "Arg. 'order' (if present) must be the same as 'model@order'." )
                 "Arg. 'order' .if present. must be the same as 'model@order'" )

    new("MixARGaussian", model = exampleModels$WL_At, prob = c(0.5, 0.5))
    new("MixARGaussian", model = exampleModels$WL_At, prob = 1)
    expect_error(new("MixARGaussian", model = exampleModels$WL_At, prob = c(0.5, 0.25, 0.25)),
                 "Arg. 'prob' .if present. must have the same length as 'model@prob'")

    new("MixARGaussian", model = exampleModels$WL_At, shift = c(0.5, 0.5))
    new("MixARGaussian", model = exampleModels$WL_At, shift = 1)
    expect_error(new("MixARGaussian", model = exampleModels$WL_At, shift = c(0.5, 0.25, 0.25)),
                 "Arg. 'shift' .if present. must have the same length as 'model@shift'")

    new("MixARGaussian", model = exampleModels$WL_At, scale = c(0.5, 0.5))
    new("MixARGaussian", model = exampleModels$WL_At, scale = 1)
    expect_error(new("MixARGaussian", model = exampleModels$WL_At, scale = c(0.5, 0.25, 0.25)),
                 "Arg. 'scale' .if present. must have the same length as 'model@scale'")

    new("MixARGaussian", model = exampleModels$WL_At, arcoef = list(0.9, 0.4))
    expect_error(new("MixARGaussian", model = exampleModels$WL_At, arcoef = list(c(0.9,0.3), 0.4)),
                 "Arg. 'arcoef' \\(if present\\) must be consistent with 'model@order'")
    
    new("MixARGaussian", model = exampleModels$WL_At, arcoef = matrix(c(0.9, 0.4), ncol = 1))
    new("MixARGaussian", model = exampleModels$WL_At, arcoef = matrix(c(0.9, 0.4), ncol = 1),
        order = c(1, 1))
    
    ## Test function mixAR
    
    mixAR(coef = list(prob=c(.5,.5), scale=c(1,2), 
                      arcoef=list(.5, 1.1), shift=c(0,0), order=c(1,1)))
    expect_equal(mixAR(template=c(1,1)), mixAR(coef=list(order=c(1,1))))
    mixAR(m2)
    mixAR(m2, list(prob = c(0.5, 0.25, 0.25)))


    parameters(lm(6:10 ~ 1 + I(1:5))) # equivalent to coef() for non-mixAR objects

    parameters(m2)
    parameters(m2, TRUE)

    v <- 1:4
    expect_error(parameters(v) <- 2:5, "'parameters<-' has no default")

    ## exampleModels$WL_Bt_3
    moT_B3 <-
    new("MixARgen"
               , prob = c(0.3, 0.3, 0.4)
               , scale = c(2, 1, 0.5)
               , shift = c(5, -5, 0)
               , arcoef = list(c(0.5, 0.24), c(-0.9), c(1.5, -0.74, 0.12))
                                        # t4, t4, t10
               , dist = distlist("stdt", c(4,10), fixed = c(FALSE, TRUE), tr = c(1, 1, 2))
               )
    expect_output(show(moT_B3))

    lik_params(moT_B3)
    mix_ncomp(moT_B3)
    row_lengths(moT_B3)

    parameters(moT_B3)
    parameters(moT_B3, TRUE)

    lik_params(moT_B3)
    mix_ncomp(moT_B3)
    row_lengths(moT_B3)

    mo <- moT_B3
    val <- parameters(mo)
    expect_identical(mo@prob, val[4:6]) # c(0.3, 0.3, 0.4)
    val[4:6] <- c(0.5, 0.25, 0.25)
    prob_old <- mo@prob
    parameters(mo) <- val
    expect_identical(mo@prob, val[4:6])
    mo@prob <- prob_old # restore to check if anything else has changed
    expect_identical(mo, moT_B3)

    
    ## demonstrate reuse of existing models
    exampleModels$WL_Bt_1
    ## moT_C2
    new("MixARgen"
                , model = exampleModels$WL_Bt_1
                , dist = distlist(c("stdt", "stdt", "stdnorm"), c(4,7))  # t4, t7, N(0,1)
                  )
    ## moT_C3
    new("MixARGaussian", model = exampleModels$WL_Bt_1 )

    
    expect_error(new("MixARgen" , model = exampleModels$WL_Bt_1 ,
                     dist = c("stdt", "stdt", "stdnorm")),
                 "Argument 'dist' must be a list")
    
    new("MixARgen", model = exampleModels$WL_Bt_1)

    expect_error(new("MixARgen", model = exampleModels$WL_ibm),
                 "Cannot set the distribution since argument 'dist' is missing")


pdf1 <- mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))

## plot the predictive density
## (cdf is used to determine limits on the x-axis)
cdf1 <- mix_cdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
gbutils::plotpdf(pdf1, cdf = cdf1, lq = 0.001, uq = 0.999)

mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
mix_pdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose))
mix_cdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose))
pdf1 <- mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
cdf1 <- mix_cdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
gbutils::plotpdf(pdf1, cdf = cdf1, lq = 0.001, uq = 0.999)

noise_dist(exampleModels$WL_ibm_gen, "cdf")
noise_dist(exampleModels$WL_ibm_gen, "pdf")
noise_dist(exampleModels$WL_ibm_gen, "pdf", expand = TRUE)
noise_dist(exampleModels$WL_ibm_gen, "cdf", expand = TRUE)

pdf1 <- mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
cdf1 <- mix_cdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
gbutils::plotpdf(pdf1, cdf = cdf1, lq = 0.001, uq = 0.999)

pdf1gen <- mix_pdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose))
cdf1gen <- mix_cdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose))
gbutils::plotpdf(pdf1gen, cdf = cdf1gen, lq = 0.001, uq = 0.999)

length(fma::ibmclose)
cdf1gena <- mix_cdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose)[-(369:369)])
pdf1gena <- mix_pdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose)[-(369:369)])
gbutils::plotpdf(pdf1gena, cdf = cdf1gena, lq = 0.001, uq = 0.999)

pdf1a <- mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose)[-(369:369)])
cdf1a <- mix_cdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose)[-(369:369)])
gbutils::plotpdf(pdf1a, cdf = cdf1a, lq = 0.001, uq = 0.999)


cdf1gena <- mix_cdf(exampleModels$WL_ibm_gen, xcond = as.numeric(fma::ibmclose)[-(369:369)])

# cond_loglik(exampleModels$WL_ibm, fma::ibmclose) # doesn't work currently, no method for `ts'
cond_loglik(exampleModels$WL_ibm, as.numeric(fma::ibmclose))
cond_loglik(exampleModels$WL_ibm_gen, as.numeric(fma::ibmclose))

ts1gen <- mixAR_sim(exampleModels$WL_ibm_gen, n = 30, init = c(346, 352, 357), nskip = 0)
plot(ts1gen)

plot(mixAR_sim(exampleModels$WL_ibm_gen, n = 100, init = c(346, 352, 357), nskip = 0),
     type = "l")

plot(diff(mixAR_sim(exampleModels$WL_ibm_gen, n = 100, init = c(346, 352, 357), nskip = 0)),
     type = "l")

noise_dist(exampleModels$WL_ibm_gen, "Fscore")

prob   <- exampleModels$WL_ibm@prob
scale  <- exampleModels$WL_ibm@scale
arcoef <- exampleModels$WL_ibm@arcoef@a

mo_WLt3  <- new("MixARgen", prob = prob, scale = scale, arcoef = arcoef,
                dist = list(fdist_stdt(3)))
mo_WLt30 <- new("MixARgen", prob = prob, scale = scale, arcoef = arcoef,
                dist = list(fdist_stdt(30)))

f <- make_fcond_lik(exampleModels$WL_ibm, as.numeric(fma::ibmclose))
f(as.numeric(fma::ibmclose))
f2 <- make_fcond_lik(exampleModels$WL_At, as.numeric(fma::ibmclose))
f2(as.numeric(fma::ibmclose))

## data(ibmclose, package = "fma") # `ibmclose'
ibmclose <- as.numeric(fma::ibmclose)
length(ibmclose) # 369
max(exampleModels$WL_ibm@order) # 2

## compute point predictions for t = 3,...,369
mix_location(exampleModels$WL_ibm, ibmclose)
## compute one-step point predictions for t = 360,...369
mix_location(exampleModels$WL_ibm, ibmclose, index = 369 - 9:0 )

f <- mix_location(exampleModels$WL_ibm) # a function
## predict the value after the last
f(ibmclose)

## a different way to compute one-step point predictions for t = 360,...369
sapply(369 - 10:1, function(k) f(ibmclose[1:k]))

## the results are the same, but notice that xcond gives past values
## while index above specifies the times for which to compute the predictions.
identical(sapply(369 - 10:1, function(k) f(ibmclose[1:k])),
          mix_location(exampleModels$WL_ibm, ibmclose, index = 369 - 9:0 ))


## conditional variance
f <- mix_variance(exampleModels$WL_ibm) # a function
## predict the value after the last
f(ibmclose)

## a different way to compute one-step point predictions for t = 360,...369
sapply(369 - 10:1, function(k) f(ibmclose[1:k]))

## the results are the same, but notice that xcond gives past values
## while index above specifies the times for which to compute the predictions.
identical(sapply(369 - 10:1, function(k) f(ibmclose[1:k])),
          mix_variance(exampleModels$WL_ibm, ibmclose, index = 369 - 9:0 ))


# interesting example
# bimodal distribution, low kurtosis, 4th moment not much larger than 2nd
moWL <- exampleModels$WL_ibm

mix_location(moWL,xcond = c(500,450))
mix_kurtosis(moWL,xcond = c(500,450))

f1pdf <- mix_pdf(moWL,xcond = c(500,450))
f1cdf <- mix_cdf(moWL,xcond = c(500,450))
gbutils::plotpdf(f1pdf,cdf=f1cdf)
gbutils::plotpdf(f1cdf,cdf=f1cdf)
f1cdf(c(400,480))

mix_variance(moWL,xcond = c(500,450))
mix_central_moment(moWL,xcond = c(500,450), k=2)

sqrt(mix_variance(moWL,xcond = c(500,450)))
sqrt(mix_central_moment(moWL,xcond = c(500,450), k=2))

bycomp <- list(list(0.1, 10,  0.11,                1),
               list(0.2,  20, c(0.11, 0.22),       2),
               list(0.3,  30, c(0.11, 0.22, 0.33), 3) )
bytype <- tomarparambyType(bycomp)
identical(bycomp, tomarparambyComp(bytype)) # TRUE
               
permuteArpar(bycomp)


companion_matrix(4:1)
companion_matrix(4:1, ncol=6)
companion_matrix(4:1, ncol=6, nrow=3)

isStable(exampleModels$WL_I)
isStable(exampleModels$WL_II)


fi0    <- fit_mixAR(fma::ibmclose, exampleModels$WL_ibm, fix = "shift")
expect_output( fit_mixAR(fma::ibmclose, exampleModels$WL_ibm, fix = "shift", init = 2) )
## fi0gen <- fit_mixAR(fma::ibmclose, exampleModels$WL_ibm_gen, fix = "shift")
## fit3   <- fit_mixAR(fma::ibmclose, mo_WLt3,  fix = "shift")
## fi30   <- fit_mixAR(fma::ibmclose, mo_WLt30, fix = "shift")
## 
stdt3v <- fdist_stdt(3, fixed = FALSE)
## mo_WLt3v <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                 dist = list(fdist_stdt(3, fixed = FALSE)))
## fit3v <- fit_mixAR(fma::ibmclose, mo_WLt3v, fix = "shift")
## 
## fit_mixAR(fma::ibmclose, new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                         dist = list(fdist_stdt(10))), fix = "shift")
## fit_mixAR(fma::ibmclose, new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                         dist = list(fdist_stdt(5))), fix = "shift")
## fit_mixAR(fma::ibmclose, new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                         dist = list(fdist_stdt(4))), fix = "shift")
## fit_mixAR(fma::ibmclose, new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                         dist = list(fdist_stdt(3))), fix = "shift")
## fit_mixAR(fma::ibmclose, new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                         dist = list(fdist_stdt(3.5))), fix = "shift")
## fit_mixAR(fma::ibmclose, new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                         dist = list(fdist_stdt(3.8))), fix = "shift")
## 
## mo_WLt30v <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                  dist = list(fdist_stdt(30, fixed = FALSE)))
## 
## fit30v <- fit_mixAR(fma::ibmclose, mo_WLt30v, fix = "shift")
## 
## mo_WLt20v30v40 <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                       dist = list(fn_stdt(c(20, 30, 40), fixed = FALSE)))
## fit20v30v40 <- fit_mixAR(fma::ibmclose, mo_WLt20v30v40, fix = "shift")
## 
## mo_WLt20v30v40 <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
##                       dist = fn_stdt(c(20, 30, 40), fixed = FALSE))
## fit20v30v40 <- fit_mixAR(fma::ibmclose, mo_WLt20v30v40, fix = "shift")
## 
## mo_WLtf <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar, 
##                dist = list(generator = function(par) fn_stdt(par, fixed = FALSE), 
##                            param = c(20, 30, 40)))
## 
## fitmo_WLtf <- fit_mixAR(fma::ibmclose, mo_WLtf, fix = "shift")

companion_matrix(11:14)
companion_matrix(11:15, 6)
expect_warning(companion_matrix(11:15, ncol = 2)) # ncol < length(v)

isStable(exampleModels$WL_I)
isStable(exampleModels$WL_II)
isStable(new("MixARGaussian", order = c(0, 0, 0)))

## missing components are filled with 'filler', extended accordingly
mixAR:::.canonic_coef(list(order = c(2,3)), filler = NA)

# here 'scale' is replicated, the missing 'shift' is inserted
mo <- list(order = c(2,3), prob = c(0.4, 0.6), scale = 1,
           arcoef = list(c(0.5, -0.5), c(1.1,  0.0, -0.5)) )
mixAR:::.canonic_coef(mo, filler = NA)


### seasonal models

probS <- c(0.5, 0.5)
sigmaS <- c(5, 1)
ar1 <- list(c(0.5, -0.5), c(1.1, 0, -0.5))
ar12 <- list(0, c(-0.3, 0.1))
s <- 12

rag <- new("raggedCoefS", a=ar1, as=ar12, s=s)

modelS <- new("MixARGaussian", prob=probS, scale=sigmaS, arcoef=rag)

yS <- mixAR_sim(modelS, n=500, init=rep(0,24))

expect_error(new("MixARGaussian", prob=probS, scale=sigmaS, 
                                      arcoef=list(a=ar1, s=s)))

new("MixARGaussian", prob=probS, scale=sigmaS, arcoef=list(ar1, ar12, s=s))
new("MixARGaussian", prob=probS, scale=sigmaS, arcoef=list(ar1, as=ar12, s=s))
new("MixARGaussian", prob=probS, scale=sigmaS, arcoef=list(a=ar1, ar12, s=s))
new("MixARGaussian", prob=probS, scale=sigmaS, 
    arcoef=list(a=rbind(c(0.5, -0.5, 0), c(1.1, 0, -0.5)), 
                as=rbind(c(0,0), c(-0.3, 0.1)) , s=s))

expect_error(new("MixARGaussian", prob=probS, scale=sigmaS, 
                 arcoef=list(a=rbind(c(0.5, -0.5, 0), c(1.1, 0, -0.5)), 
                             as=matrix(c(0,0), nrow=1) , s=s)))

expect_output(show(modelS))


})

