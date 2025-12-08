# Mixture Autoregressive Models

Model time series using mixture autoregressive (MAR) models. Implemented
are frequentist (EM) and Bayesian methods for estimation, prediction and
model evaluation. See Wong and Li (2002)
\<doi:10.1111/1467-9868.00222\>, Boshnakov (2009)
\<doi:10.1016/j.spl.2009.04.009\>), and the extensive references in the
documentation.

## Details

Package mixAR provides functions for modelling with mixture
autoregressive (MixAR) models. The S4 class `"MixARGaussian"` can be
used when the error distributions of the components are standard
Gaussian. The class `"MixARgen"` admits arbitrary (well, within reason)
distributions for the error components. Both classes inherit from the
virtual class `"MixAR"`.

Estimation can be done with `fit_mixAR`. Currently, the EM algorithm is
used for estimation.

For `"MixARGaussian"` the M-step of the EM algorithm reduces to a system
of linear equations. For `"MixARgen"` the problem is substantially
non-linear. The implementation is fairly general but currently not
optimised for efficiency. The specification of the error distributions
went through several stages and may still be reviewed. However, backward
compatibility will be kept.

## Author

Georgi N. Boshnakov \[aut, cre\], Davide Ravagli \[aut\]

Maintainer: Georgi N. Boshnakov \<georgi.boshnakov@manchester.ac.uk\>

## References

Akinyemi MI (2013). *Mixture autoregressive models: asymptotic
properties and application to financial risk*. Ph.D. thesis, Probability
and Statistics Group, School of Mathematics, University of Manchester.

Boshnakov GN (2009). “Analytic expressions for predictive distributions
in mixture autoregressive models.” *Stat. Probab. Lett.*, **79**(15),
1704-1709.
[doi:10.1016/j.spl.2009.04.009](https://doi.org/10.1016/j.spl.2009.04.009)
.

Boshnakov GN (2011). “On First and Second Order Stationarity of Random
Coefficient Models.” *Linear Algebra Appl.*, **434**(2), 415–423.
[doi:10.1016/j.laa.2010.09.023](https://doi.org/10.1016/j.laa.2010.09.023)
.

Fong PW, Li WK, Yau CW, Wong CS (2007). “On a mixture vector
autoregressive model.” *Can. J. Stat.*, **35**(1), 135-150.

Ravagli D, Boshnakov GN (2020). “Bayesian analysis of mixture
autoregressive models covering the complete parameter space.”
2006.11041, <https://arxiv.org/abs/2006.11041>.

Ravagli D, Boshnakov GN (2020). “Portfolio optimization with mixture
vector autoregressive models.” 2005.13396,
<https://arxiv.org/abs/2005.13396>.

Hossain AS (2012). *Complete Bayesian analysis of some mixture time
series models*. Ph.D. thesis, Probability and Statistics Group, School
of Mathematics, University of Manchester.

Wong CS (1998). *Statistical inference for some nonlinear time series
models*. Ph.D. thesis, University of Hong Kong, Hong Kong.

Wong CS, Li WK (2000). “On a mixture autoregressive model.” *J. R. Stat.
Soc., Ser. B, Stat. Methodol.*, **62**(1), 95-115.

Wong CS, Li WK (2001). “On a logistic mixture autoregressive model.”
*Biometrika*, **88**(3), 833-846.
[doi:10.1093/biomet/88.3.833](https://doi.org/10.1093/biomet/88.3.833) .

Wong CS, Li WK (2001). “On a mixture autoregressive conditional
heteroscedastic model.” *J. Am. Stat. Assoc.*, **96**(455), 982-995.
[doi:10.1198/016214501753208645](https://doi.org/10.1198/016214501753208645)
.

## See also

fit several types of mixAR models:
[`fit_mixAR`](https://geobosh.github.io/mixAR/reference/fit_mixAR-methods.md),
[`bayes_mixAR`](https://geobosh.github.io/mixAR/reference/bayes_mixAR.md),
[`fit_mixARreg`](https://geobosh.github.io/mixAR/reference/fit_mixARreg-methods.md),
[`mixSARfit`](https://geobosh.github.io/mixAR/reference/mixSARfit.md);

Predictive distributions and summaries:
[`mix_pdf`](https://geobosh.github.io/mixAR/reference/mix_pdf-methods.md),
[`mix_cdf`](https://geobosh.github.io/mixAR/reference/mix_pdf-methods.md),
[`mix_qf`](https://geobosh.github.io/mixAR/reference/mix_qf-methods.md),
[`mix_location`](https://geobosh.github.io/mixAR/reference/mix_moment.md),
[`mix_variance`](https://geobosh.github.io/mixAR/reference/mix_moment.md),
[`mix_central_moment`](https://geobosh.github.io/mixAR/reference/mix_moment.md),
[`mix_moment`](https://geobosh.github.io/mixAR/reference/mix_moment.md),
[`mix_kurtosis`](https://geobosh.github.io/mixAR/reference/mix_moment.md),
[`mix_ekurtosis`](https://geobosh.github.io/mixAR/reference/mix_moment.md)

multi-step prediction:
[`multiStep_dist`](https://geobosh.github.io/mixAR/reference/multiStep_dist-methods.md)

## Examples

``` r
## object 'exampleModels' contains a number of models for examples and testing
names(exampleModels)
#>  [1] "WL_ibm"     "WL_A"       "WL_B"       "WL_I"       "WL_II"     
#>  [6] "WL_ibm_gen" "WL_ibm_t3v" "WL_ibm_tf"  "WL_At"      "WL_Bt_1"   
#> [11] "WL_Bt_2"    "WL_Bt_3"    "WL_Ct_1"    "WL_Ct_2"    "WL_Ct_3"   
exampleModels$WL_ibm
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob   shift scale   order ar_1   ar_2   
#> Comp_1 0.5439   0    4.8227   2   0.6792  0.3208
#> Comp_2 0.4176   0    6.0082   2   1.6711 -0.6711
#> Comp_3 0.0385   0   18.1716   1   1.0000        
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 

## some of the models below are available in object 'exampleModels';
## the examples here show how to create them from scratch
mo_WLprob <- c(0.5439, 0.4176, 0.0385)              # model coefficients from Wong&Li
mo_WLsigma <- c(4.8227, 6.0082, 18.1716)
mo_WLar <- list(c(0.6792, 0.3208), c(1.6711, -0.6711), 1)

mo_WL <- new("MixARGaussian", prob = mo_WLprob, scale = mo_WLsigma, arcoef = mo_WLar)
                                        
mo_WL_A <- new("MixARGaussian"                         # WongLi, model A
              , prob = c(0.5, 0.5)
              , scale = c(5, 1)
              , shift = c(0, 0)
              , arcoef = list(c(0.5), c(1.1))
              )

mo_WL_B <- new("MixARGaussian"                         # WongLi, model B
              , prob = c(0.75, 0.25)
              , scale = c(5, 1)
              , shift = c(0, 0)
              , arcoef = list(c(0.5), c(1.4))
              )
                                        
mo_WL_I <- new("MixARGaussian"                          # WongLi, model I
              , prob = c(0.4, 0.3, 0.3)
              , scale = c(1, 1, 5)
              , shift = c(0, 0, -5)
              , arcoef = list(c(0.9, -0.6), c(-0.5), c(1.50, -0.74, 0.12))
              )

mo_WL_II <- new("MixARGaussian"                         # WongLi, model II
               , prob = c(0.4, 0.3, 0.3)
               , scale = c(1, 1, 5)
               , shift = c(5, 0, -5)
               , arcoef = list(c(0.9, -0.6), c(-0.7, 0), c( 0, 0.80))
               )

## MixAR models with arbitrary dist. of the components
## (user interface not finalized)

## Gaussian
mo_WLgen <- new("MixARgen", prob = mo_WLprob, scale = mo_WLsigma, arcoef = mo_WLar,
               dist = list(dist_norm))

## t_3
mo_WLt3v <- new("MixARgen", prob = mo_WLprob, scale = mo_WLsigma, arcoef = mo_WLar,
               dist = list(fdist_stdt(3, fixed = FALSE)))

## t_20, t_30, t_40 (can be used to start estimation)
mo_WLtf <- new("MixARgen", prob = mo_WLprob, scale = mo_WLsigma, arcoef = mo_WLar,
              dist = list(generator =
                              function(par)
                                  fn_stdt(par, fixed = FALSE), param = c(20, 30, 40)))

## data(ibmclose, package = "fma")  # for `ibmclose'

## The examples below are quick but some of them are marked as 'not run'
## to avoid cumulative time of more than 5s on CRAN.

## fit a MAR(2,2,1) model
a0a <- fit_mixAR(as.numeric(fma::ibmclose), c(2, 2, 1), crit = 1e-4)
## same with 2 sets of automatically generated initial values.
# \donttest{
a0b <- fit_mixAR(as.numeric(fma::ibmclose), c(2, 2, 1), 2, crit = 1e-4)
# }

## fix the shift parameters:
a1a <- fit_mixAR(as.numeric(fma::ibmclose), c(2, 2, 1), fix = "shift", crit = 1e-4)
## ... with 3 sets of automatically generated initial values.
# \donttest{
a1b <- fit_mixAR(as.numeric(fma::ibmclose), c(2, 2, 1), 3, fix = "shift", crit = 1e-4)
# }

# \donttest{
## specify the model using a MixAR model object
a1c <- fit_mixAR(as.numeric(fma::ibmclose), a1a$model, init = a0a$model, fix = "shift",
           crit = 1e-4)

## fit a model like mo_WL using as initial values 2 automatically generated sets.
a2 <- fit_mixAR(as.numeric(fma::ibmclose), mo_WL, 2, fix = "shift", permute = TRUE, 
          crit = 1e-4)
# }

moT_B3 <- new("MixARgen"
               , prob = c(0.3, 0.3, 0.4)
               , scale = c(2, 1, 0.5)
               , shift = c(5, -5, 0)
               , arcoef = list(c(0.5, 0.24), c(-0.9), c(1.5, -0.74, 0.12))
                                        # t4, t4, t10
               , dist = distlist("stdt", c(4,10), fixed = c(FALSE, TRUE), tr = c(1, 1, 2))
               )

moT_C1 <- new("MixARgen"
              , prob = c(0.3, 0.3, 0.4)
              , scale = c(2, 1, 0.5)
              , shift = c(5, -5, 0)
              , arcoef = list(c(0.5, 0.24), c(-0.9), c(1.5, -0.74, 0.12))
                                        # t4, t7, N(0,1)
              , dist = distlist(c("stdt", "stdt", "stdnorm"), c(4,7))
              )

## demonstrate reuse of existing models
exampleModels$WL_Bt_1
#> An object of class "MixARgen"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2  ar_3
#> Comp_1 0.3    5   2.0     2    0.5  0.24     
#> Comp_2 0.3   -5   1.0     1   -0.9           
#> Comp_3 0.4    0   0.5     3    1.5 -0.74 0.12
#> 
#> Distributions of the error components:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 6 df
#>  Component 3: Student t with 10 df
#> 
moT_C2 <- new("MixARgen"
              , model = exampleModels$WL_Bt_1
              , dist = distlist(c("stdt", "stdt", "stdnorm"), c(4,7))  # t4, t7, N(0,1)
              )
moT_C3 <- new("MixARGaussian", model = exampleModels$WL_Bt_1 )
```
