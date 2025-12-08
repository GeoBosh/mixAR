# BIC based model selection for MixAR models

BIC calculations for mixture autoregressive models.

## Usage

``` r
mixAR_BIC(y, model, fix = NULL, comp_loglik = TRUE, index)
BIC_comp(x, y)
```

## Arguments

- y:

  a time series.

- model:

  the model for which to calculate BIC, an object inheriting from class
  `MixAR`. Alternatively, an output list from `fit_mixAR`.

- fix:

  If `fix = "shift"` shift parameters are not included in calculation of
  BIC. Default is `NULL`, i.e. shift parameters are included.

- comp_loglik:

  Should the loglikelihood be calculated? Default is `TRUE`. If `FALSE`
  and model is output of `fit_mixAR`, then the loglikelihood is not
  recalculated.

- index:

  Discard the first `1:index` observations. If `missing`, `index` is set
  to the largest AR order.

- x:

  a list containing a combination of `MixAR` objects and/or output lists
  from `fit_mixAR`.

## Details

`mixAR_BIC` calculates the BIC criterion of a given `MixAR` object with
respect to a specified time series.

If `index` is specified, it has to be at least equal to the largest
autoregressive order. The function calculates BIC on the last
`(index + 1):n` data points.

`BIC_comp` calculates the value of BIC for the models listed in `x` with
respect to the specified time series `y`.

If the distributions of the components contain estimated parameters,
then their number is included in the number of parameters for the
calculation of BIC.

## Value

If `comp_loglik = TRUE`, the function calculates BIC based on the given
model, data and `index`.

If `comp_loglik = FALSE` and model is output from `fit_mixAR`, it
returns object `vallogf` from that list.

## Author

Davide Ravagli

## Examples

``` r
model1 <- new("MixARGaussian", prob = c(0.5, 0.5), scale = c(1, 2),
              arcoef = list(-0.5, 1.1))

model2 <- new("MixARGaussian", prob = c(0.5, 0.3, 0.2), scale = c(1, 3, 8),
              arcoef = list(c(-0.5, 0.5), 1, 0.4))

set.seed(123)
y <- mixAR_sim(model1, 400, c(0, 0, 0), nskip = 100)

mixAR_BIC(y, model1)
#> [1] 1770.964

model_fit1 <- fit_mixAR(y, model1)
model_fit2 <- fit_mixAR(y, model2, crit = 1e-4)

mixAR_BIC(y, model_fit1)
#> [1] 1766.659
mixAR_BIC(y, model_fit2)
#> [1] 1782.464

BIC_comp(list(model1, model2, model_fit1, model_fit2), y)
#> Best model:  1 1 
#> [1] 1770.964 2151.311 1766.659 1782.464

mixAR_BIC(y, model_fit1, index = 20)
#> [1] 3.439512
mixAR_BIC(y, model_fit2, index = 20)
#> [1] 3.456183
```
