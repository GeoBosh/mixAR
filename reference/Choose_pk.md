# Choose the autoregressive order of MixAR components

Reversible Jump MCMC algorithm to choose the optimal autoregressive
order of each component of a mixture autoregressive model.

## Usage

``` r
Choose_pk(y, model, fix_shift = FALSE, tau, pmax, method, par = NULL, nsim)
```

## Arguments

- y:

  a time series. Currently a `numeric` vector.

- model:

  an object inheriting from class `"MixAR"`.

- fix_shift:

  whether the shift/mean parameter should be kept fixed to its starting
  value or not. Default is `FALSE`.

- tau:

  tuning parameters for Metropolis-Hastings algorithm in sampling AR
  coefficients.

- pmax:

  the largest autoregressive order allowed for each component.

- method:

  character vector of length 1. Method for calculating probability of
  new AR order to be increased/decreased by 1 unit. Currently available
  `"Ratio"`, `"Poisson"` and `"NULL"`. Default is `"NULL"`.

- par:

  `numeric`, parameter for tuning probabilities according to `method`.
  Ignored if `method` is "NULL".

- nsim:

  `numeric`, the number of iterations.

## Value

- out:

  a dataframe with `g+1` columns. The first `g` columns contain the
  autoregressive orders of the components, the last column how often a
  model is preferred, divided by `nsim`.

- fix_shift:

  the choice made for the shift/mean parameters.

- method:

  the method used to increase/decrease AR orders.

## References

Ravagli D, Boshnakov GN (2020). “Bayesian analysis of mixture
autoregressive models covering the complete parameter space.”
2006.11041, <https://arxiv.org/abs/2006.11041>.

## Author

Davide Ravagli

## Note

`Choose_pk` currenlty supports class `"MixARGaussian"` only.

## See also

[`bx_dx`](https://geobosh.github.io/mixAR/reference/bx_dx.md) for more
details on the `method`

## Examples

``` r
model <- new("MixARGaussian", 
             prob   = exampleModels$WL_At@prob,      # c(0.5, 0.5)
             scale  = exampleModels$WL_At@scale,     # c(1, 2)        
             arcoef = list(-0.5, 1) )
             # note: arcoef != list(-0.5, 1.1) == exampleModels$WL_At@arcoef@a

set.seed(1234)
n <- 50 # 200
y <- mixAR_sim(model, n, rep(0, max(model@order)), nskip = 100)

nsim <- 25 # 100
pk_star <- Choose_pk(y, model, tau = c(.15, .25), pmax = 5, method = "NULL", nsim = nsim)
```
