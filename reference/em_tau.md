# Compute probabilities for the observations to belong to each of the components

Supporting function for EM algorithm. Update the conditional
probabilities of the components of the MixAR model (for E-step of EM
algorithm).

## Usage

``` r
em_tau(stdetk, prob, scale, pdf = dnorm)

em_tau_safe(stdetk, prob, scale, pdf = dnorm)
```

## Details

`em_tau` and `em_tau_safe` compute the conditional probabilities of the
components of the MixAR model (for the E-step of the EM algorithm). The
two functions do the same computations but `em_tau_safe`, in addition,
protects agains `NaN`'s and infinite values in argument `stdetk` or
obtained during computations.

## Arguments

- stdetk:

  standardised component residuals, a `MixComp` object.

- prob:

  current estimates of the probabilities of the components, a numeric
  vector of length equal to the number of components in the model.

- scale:

  scales (standard deviations) of the noise components, a numeric vector
  of length equal to the number of components in the model.

- pdf:

  densities of the noise components.

## Author

Georgi N. Boshnakov
