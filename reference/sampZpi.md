# Sampling functions for Bayesian analysis of mixture autoregressive models

Sampling functions for Bayesian analysis of mixture autoregressive
models. Draws observations from posterior distributions of the latent
variables `Z_t`s and the parameters of mixture autoregressive models.

## Usage

``` r
sampZpi(y, pk, prob, mu, AR, sigma, nsim, d)
sampMuShift(y, pk, prec, nk, shift, z, AR, nsim)
sampSigmaTau(y, pk, prec, nk, AR, mu, z, a, c, nsim)
```

## Arguments

- y:

  a time series (currently a `numeric` vector).

- pk:

  `numeric` vector of length `g`. The autoregressive order of each
  component.

- prob:

  `numeric` vector of length `g`. Current mixing weights.

- mu:

  `numeric` vector of length `g`. Current mean parameters.

- shift:

  `numeric` vector of length `g`. Current shift parameters.

- AR:

  `list` of `g` elements. Autoregressive coefficients for each
  component.

- sigma:

  `numeric` vector of length `g`. Current scale parameters

- nsim:

  dessired sample size.

- d:

  `numeric` vector of length `g`. Hyperparameters for Dirichlet prior.
  If missing, a vector of `1`s is generated. If length is `1`, creates a
  vector of length `g` with given number.

- prec:

  `numeric` vector of length `g`. Current precision parameters.

- nk:

  output from `sampZpi`. Component sum of latent variables Z_t.

- z:

  output `latentZ` from `sampZpi`. A matrix containing the simulated
  latent variables.

- a,c:

  hyperparameters.

## Details

`sampZpi` draws observations from posterior distributions of the latent
variables `Z_t`s and mixing weights of a Mixture autoregressive model.

`sampSigmaTau` draws observations from posterior distributions of the
precisions `tau_k` of a Mixture autoregressive model, and obtains scales
`sigma_k` by transformation.

`sampMuShift` Draws observations from posterior distributions of the
means `mu_k` of a Mixture autoregressive model, and obtains shifts
`phi_k0` by transformation.

## Value

for `sampZpi`, a list containing the following elements:

- mix_weights:

  `matrix` with `nrow = nsim` and `ncol = g`: sampled mixing weights.

- latentZ:

  `matrix` with `nrow = n - p` and `ncol = g`, `n` equal to `length(y)`
  and `p` equal to `max(pk`: the simulated latent variables `Z_t` at the
  last of `nsim` iterations (functional). Specifically, each row
  contains `1` for exactly one component, and is filled with `0`.

- nk:

  Vector of length `g`. Column sums of `latentZ`.

for `sampMuShift`, a list containing the following elements:

- shift:

  `matrix` with `nrow = nsim` and `ncol = g`: simulated shift
  parameters, obtained by transformation of the means.

- mu:

  `matrix` with `nrow = nsim` and `ncol = g`: simulated mean parameters.

for `sampSigmaTau`, a list containing the following elements:

- scale:

  `matrix` with `nrow = nsim` and `ncol = g`: scale parameters, obtained
  by transformation of precisions.

- precision:

  `matrix` with `nrow = nsim` and `ncol = g`: precision parameters.

- lambda:

  `numeric` vector of length `nsim` simulated values of hyperparameter
  lambda, due to hierarchical setup.

## Author

Davide Ravagli

## Examples

``` r
model <- new("MixARGaussian",
             prob   = exampleModels$WL_At@prob,      # c(0.5, 0.5)
             scale  = exampleModels$WL_At@scale,     # c(1, 2)        
             arcoef = exampleModels$WL_At@arcoef@a ) # list(-0.5, 1.1)
             
prob  <- model@prob
sigma <- model@scale
prec  <- 1 / sigma ^ 2
g     <- length(model@prob)
d     <- rep(1, g)
pk    <- model@arcoef@p
p     <- max(pk)
shift <- mu <- model@shift
         
AR <- model@arcoef@a       

model
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob shift scale order ar_1
#> Comp_1 0.5    0     1     1   -0.5
#> Comp_2 0.5    0     2     1    1.1
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 

set.seed(1234)
n <- 50 # 500
nsim <- 50

y <- mixAR_sim(model, n = n, init = 0)
x  <- sampZpi(y, pk, prob, shift, AR, sigma, nsim = nsim, d)
x1 <- sampMuShift(y, pk, prec, nk = x$nk, shift, z = x$latentZ, AR, nsim = nsim)
x2 <- sampSigmaTau(y, pk, prec, nk = x$nk, AR, mu = x1$mu, z = x$latentZ,
                   a = 0.2, c = 2, nsim = nsim)
```
