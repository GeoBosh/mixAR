library(mixAR)

test_that("mixARsim functions work", {

    ## from simuExperiment.Rd
    
    ## explore dist. of the mean of a random sample of length 5.
    ## (only illustration, such simple cases hardly need simuExperiment)
    sim1 <- list(fun="rnorm", args = list(n=5, mean=3, sd = 2))
    est1 <- list(fun=mean, args = list())
    
    # a basic report function
    fsum1 <-  function(x){ wrk <- do.call("c",x)
                           c(n = length(wrk), mean = mean(wrk), sd = sd(wrk))}
    
    a1 <- simuExperiment(TRUE, simu = sim1, est = est1, N = 1000, summary_fun = fsum1)
    
    # explore also the dist. of the sample s.d.
    est2 <- est1
    est2$fun <- function(x) c(xbar = mean(x), s = sd(x))
    
    a2  <- simuExperiment(TRUE, simu = sim1, est = est2, N = 1000)
    
    # keep the raw sample means and s.d.'s for further use
    a2a <- simuExperiment(TRUE, simu = sim1, est = est2, N = 1000, raw = TRUE)
    a2a$Summary
    
    # replicate a2a$Summary
    s5 <- sapply(a2a$Raw, identity)
    apply(s5, 1, mean)
    apply(s5, 1, sd)
    
    hist(s5[1,], prob=TRUE)
    lines(density(s5[1,]))
    curve(dnorm(x, mean(s5[1,]), sd(s5[1,])), add = TRUE, col = "red")
    
    mixAR:::.fsummary(a2a$Raw)
    mixAR:::.fsummary(a2a$Raw, merge = TRUE)


    ## from mixARExperiment.Rd
    set.seed(1234)
    mixARExperiment(exampleModels$WL_II, N = 10)
    mixARExperiment(exampleModels$WL_II, N=10, raw=TRUE)
    mixARExperiment(exampleModels$WL_II, N=10, raw=TRUE, simargs=list(n=500))

    
    ## from test_unswitch.Rd
    aII10data <- mixARExperiment(exampleModels$WL_II, N=10, raw=TRUE)
    aII10 <- test_unswitch(aII10data$Raw, exampleModels$WL_II)
    aII10

    aII10adata <- mixARExperiment(exampleModels$WL_II, N=10, raw=TRUE, simargs=list(n=500))
    aII10a <- test_unswitch(aII10adata$Raw, exampleModels$WL_II)
    aII10a


    ## from mixAR_sim.Rd

    ## simulate a continuation of BJ ibm data
    ts1 <- mixAR_sim(exampleModels$WL_ibm, n = 30, init = c(346, 352, 357), nskip = 0)
    
    ## ts1 <- mixAR_sim(exampleModels$WL_ibm, n = 400, init = c(346, 352, 357), nskip = 0)
    ## plot(diff(ts(ts1)))
    
    ## plot(diff(mixAR_sim(exampleModels$WL_ibm_gen, n = 100, init = c(346, 352, 357), nskip = 0)),
    ##      type = "l")
    
    # a simulation based estimate of the 1-step predictive distribution
    # for the first date after the data.
    s1 <- replicate(1000, mixAR_sim(exampleModels$WL_ibm, n = 1, init = c(346, 352, 357), nskip = 0))
    plot(density(s1))
    
    # load ibm data from BJ
    ## data(ibmclose, package = "fma")
    
    # overlay the 'true' predictive density.
    pdf1 <- mix_pdf(exampleModels$WL_ibm, xcond = as.numeric(fma::ibmclose))
    curve(pdf1, add = TRUE, col = 'blue')
    
    # estimate of 5\% quantile of predictive distribution
    quantile(s1, 0.05)
    
    # Monte Carlo estimate of "expected shortfall"
    # (but the data has not been converted into returns...)
    mean(s1[ s1 <= quantile(s1, 0.05) ])


    ## from  permn.Rd
    m <- matrix(c(11:14,21:24,31:34), ncol=3)
    pm <- permn_cols(m)
    pm[[2]]
    

})
