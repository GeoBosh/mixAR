# Internal functions for estimation of MixAR models with Gaussian components

Internal functions for EM estimation of MixAR models with Gaussian
components: sums of products and crossproducts; M-step for MixAR
estimation; estimation of autoregressive part of the model.

## Usage

``` r
tauCorrelate(y, tau, order)
tau2arcoef(y, tau, order, est_shift = TRUE)
mixMstep(y, tau, order, index, est_shift = TRUE)
```

## Arguments

- y:

  time series.

- tau:

  conditional probabilties for the observations to belong to each of the
  components, a `MixComp` object.

- order:

  order of the MixAR model, numeric vector of length the number of
  mixture components.

- index:

  indices of the observations to include in the likelihood calculations,
  typically `(p+1):n`, where `p` is `max(order)` and `n=length(y)`.

- est_shift:

  if TRUE include shifts (intercepts) in the AR components, otherwise
  set them to zero.

## Details

`mixMstep` performs an M-step for estimation of MixAR models with
Gaussian components.

`tauCorrelate` computes crossproducts needed for EM estimation of MixAR
models with Gaussian components.

`tau2arcoef` computes the AR coefficients by solving Yule-Walker-type
equations for each component.

## Value

For `mixMstep`, a MixAR model, an object of class `MixARGaussian`.

For `tauCorrelate`, a named list with the following components:

- Stau:

&nbsp;

- Stauy:
- Stauyy:

For `tau2arcoef`, a list with two components:

- shift:

  the shift (intercept) terms, a numeric vector

- arcoef:

  the AR coefficients as a list, whose i-th component contains the
  coefficients for component i (as a numeric vector)
