# Diagnostic checks for mixture autoregressive models

Carry out diagnostic checks and tests on fitted mixAR models.

## Usage

``` r
# S3 method for class 'MixAR'
tsdiag(object, gof.lag = NULL, y, ask = interactive(), ..., 
       plot = interactive(), std.resid = FALSE)

mixAR_diag(model, y, ...)
```

## Arguments

- model,object:

  the model on which to perform the checks, an object from class
  `MixAR`. `model` can also be the output list from `fit_mixAR`.

- gof.lag:

  Goodness of fit lag(s) for the Ljung-Box tests. Vector containing one
  or more positive integers. `max(gof.lag)` is the maximal lag in the
  acf and pacf plots.

  how many lags to compute for acf and pacf? The default is as that of
  `lag.max` for `acf`.

- y:

  a time series, currently a `numeric` vector.

- ask:

  if `TRUE`, ask (using a menu) which plot to present. Otherwise just
  plot the selected plots. `ask` is ignored if only one plot is selected
  with argument `plot`.

- plot:

  if `TRUE`, the default, produce diagnostic plots. If `FALSE` don't
  produce plots. Otherwise, a numeric vector of integers defining a
  subset of plots to consider, see Details.

- std.resid:

  if `TRUE` standardise the ordinary residuals using the conditional
  standard deviations. **NOTE:** the default is currently `FALSE` but it
  may soon be changed to `TRUE`.

- ...:

  for `mixAR_diag`, passed on to `tsdiag`.

## Details

It is recommended to use `tsdiag`. `mixAR_diag` is essentially
deprecated and is still here for compatibility with old code. Moreover,
the `tsdiag` method is more flexible. The only advantage of `mixAR_diag`
is that it accepts also a list for argument `model` but this is
equivalent to calling `tsdiag` with `object = model$model`.

The function calculates several types of residuals, provides diagnostic
plots for each of them, and returns numerical results. The following
choices are currently available:

1.  ACF/PACF of residuals,

2.  ACF/PACF of U_residuals,

3.  ACF/PACF of tau_residuals,

4.  ACF/Histogram of tau_residuals.

In interactive sessions the user is presented with a menu to select
plot(s) from the available ones. The choice can be restricted to a
subset of them by giving argument `plot` a vector of integers. This is
most useful to select a particular plot, with somethinng like `plot = 2`
in the call to `tsdiag`. `plot` is used as an index vector, so
`plot = -1` would remove the first item listed above from the offered
alternatives.

Transformations on the data are performed, as described in Smith (1985).

Four types of residuals are computed:

- ordinary residuals:

  difference (possibly scaled) between observed values and point
  predictions.

- U_residuals/PIT residuals:

  probability integral transform of the data using the CDF of the
  conditional distributions implied by the fitted model. For a good
  model these should resemble an IID sequence uniformly distributed on
  (0,1).

- V_residuals:

  set of transformed `U_residuals` with the quantile function of the
  standard normal distribution (`qnorm`). For a good model these should
  resemble an IID sequence from N(0,1).

- tau_residuals:

  These residuals are calculated as the component specific residual
  `e_tk` divided by its corresponding scale `sigma_k`, according to
  under which component y_t has largest density. Under correct model
  specification, these should be jointly Normal. Shapiro-Wilk test is
  performed on this set of residual to assess the hypothesis.

is applied to V- and tau-residuals.

## Value

returns invisibly a list with class `"tsdiagMixAR"`, currently
containing the following components:

- residuals:

  ordinary residuals,

- U_residuals:

  see Details,

- V_residuals:

  see Details,

- tau_residuals:

  see Details,

- BIC:

  the value of the BIC criterion, a number.

Each component, except `BIC`, is a list containing the residuals in
component `value`, Ljung-Box test in `"Ljung-Box"` and possibly other
tests suitable for the corresponding type of residuals.

## References

Smith JQ (1985). “Diagnostic checks of non-standard time series models.”
*Journal of Forecasting*, **4**(3), 283-291.
[doi:10.1002/for.3980040305](https://doi.org/10.1002/for.3980040305) ,
https://onlinelibrary.wiley.com/doi/pdf/10.1002/for.3980040305,
<https://onlinelibrary.wiley.com/doi/abs/10.1002/for.3980040305>.

Wong CS, Li WK (2000). “On a mixture autoregressive model.” *J. R. Stat.
Soc., Ser. B, Stat. Methodol.*, **62**(1), 95-115.

## Author

Davide Ravagli and Georgi N. Boshnakov

## Note

This function should be used for diagnostic checking of `MixARGaussian`
objects only.

## See also

[`mixAR_BIC`](https://geobosh.github.io/mixAR/reference/mixAR_BIC.md),
[`tsdiag`](https://rdrr.io/r/stats/tsdiag.html)

## Examples

``` r
model1 <- new("MixARGaussian", prob = c(0.5, 0.5), scale = c(1, 2),
              arcoef = list(-0.5, 1.1))
set.seed(123)
y <- mixAR_sim(model1, 400, c(0,0,0), nskip = 100) 

fit1 <- fit_mixAR(y, model1)
d <- tsdiag(fit1$model, c(10, 20, 50), y)
d
#> 
#>  -------------------------------------------------- 
#> Tests on the ordinary residuals
#>  -------------------------------------------------- 
#> $`Ljung-Box`
#>        df   p.value
#> Lag_10 10 0.1363658
#> Lag_20 20 0.1693712
#> Lag_50 50 0.4213514
#> 
#> 
#>  -------------------------------------------------- 
#> Tests on the U_residuals
#>  -------------------------------------------------- 
#> $`Ljung-Box`
#>        df    p.value
#> Lag_10 10 0.43658682
#> Lag_20 20 0.29918541
#> Lag_50 50 0.03460944
#> 
#> $KS
#> 
#>  Asymptotic one-sample Kolmogorov-Smirnov test
#> 
#> data:  cdf
#> D = 0.028344, p-value = 0.9057
#> alternative hypothesis: two-sided
#> 
#> 
#> 
#>  -------------------------------------------------- 
#> Tests on the V_residuals
#>  -------------------------------------------------- 
#> $`Ljung-Box`
#>        df    p.value
#> Lag_10 10 0.68338359
#> Lag_20 20 0.47238093
#> Lag_50 50 0.06870134
#> 
#> $`Shapiro-Wilk`
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  v
#> W = 0.99784, p-value = 0.8928
#> 
#> 
#> 
#>  -------------------------------------------------- 
#> Tests on the tau_residuals
#>  -------------------------------------------------- 
#> $`Ljung-Box`
#>        df   p.value
#> Lag_10 10 0.8708327
#> Lag_20 20 0.9790725
#> Lag_50 50 0.6138649
#> 
#> $`Shapiro-Wilk`
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  err2
#> W = 0.99563, p-value = 0.3328
#> 
#> 
## This will put each plot in a separate file (mydiag01.pdf, ..., mydiag04.pdf)
## pdf("mydiag%02d.pdf", onefile = FALSE)
## d <- tsdiag(fit1$model, c(10, 20, 50), y,  ask = FALSE)
## dev.off()
```
