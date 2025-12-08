# Calculate marginal loglikelihood at high density points of a MAR model.

The function implements the method by Chib (1995) and Chib and Jeliazkov
(2001) for calculation of the marginal loglikelihood of a mixture
autoregressive model. It automatically finds high density values for
model parameters, and evaluates the likelihood at such points.

## Usage

``` r
marg_loglik(y, model, tau, nsim, prob_mod)
```

## Arguments

- y:

  a time series (currently a numeric vector).

- model:

  object of formal class `MixAR`, containing initial values for the
  parameters. Currently available for `MixARGaussian` objects only.

- tau:

  tuning parameter for Metropolis-Hasting move to update autoregressive
  parameters.

- nsim:

  sample size on which to evaluate highest density values.

- prob_mod:

  this is currently the output from `Choose_pk`: the proportion of times
  the "best model" was chosen.

## Details

`nsim` is the sample size on which to evaluate highest density values
for each set of parameters. For example, choosing `nsim=1000` results in
`1000*(g+3)` (1000 iterations for each autoregressive component, plus
1000 for mean and scale parameters and mixing weights).

## Value

A list containing the following elements:

- marg_loglik:

  value of the marginal loglikelihood.

- phi_hd:

  set of highest density autoregressive parameters.

- prec_hd:

  set of highest density precision parameters.

- mu_hd:

  set of highest density mean parameters.

- weig_hd:

  set of highest density mixing weights.

## References

Chib S (1995). “Marginal likelihood from the Gibbs output.” *J. A. Stat.
Ass.*, **90**(432), 1313-1321.

Chib S, Jeliazkov I (2001). “Marginal likelihood from the
Metropolis-Hastings output.” *J. A. Stat. Ass.*, **96**(453), 270-281.

## Author

Davide Ravagli

## Examples

``` r
prob <- c(0.5, 0.5)
sigma <- c(1, 2)
arco <- list(-0.5, 1)

model <- new("MixARGaussian", prob = prob, scale = sigma, arcoef = arco)

set.seed(1234)
y <- mixAR_sim(model, 250, rep(0, max(model@order)), nskip = 100)  # data

nsim <- 10 # 50
marg_loglik(y, model, tau = c(.15, .25), nsim = nsim, 0.5)
#> $marg_loglik
#> [1] -541.8047
#> 
#> $phi_hd
#> $phi_hd[[1]]
#> [1] -0.4470663
#> 
#> $phi_hd[[2]]
#> [1] 0.9891284
#> 
#> 
#> $prec_hd
#> [1] 0.7083760 0.2737235
#> 
#> $mu_hd
#> [1]  0.04563084 -0.76579127
#> 
#> $weig_hd
#> [1] 0.4996393 0.5003607
#> 
```
