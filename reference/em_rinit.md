# Gaussian EM-step with random initialisation

Gaussian EM-step with random initialisation.

## Usage

``` r
em_rinit(y, order, partempl)

etk2tau(etk)
```

## Arguments

- y:

  time series.

- order:

  MixAR order, vector of length the number of components.

- partempl:

  parameter template, a list containing one element for each mixture
  component, see
  [`randomArCoefficients`](https://geobosh.github.io/mixAR/reference/randomArCoefficients.md).

- etk:

  MixAR component residuals, a matrix.

## Details

`em_rinit` generates random MAR residuals, performs a non-distributional
E-step, and a Gaussian M-step.

`etk2tau` estimates `tau` from component residuals only. Note that this
is unlike
[`em_tau`](https://geobosh.github.io/mixAR/reference/em_tau.md), which
also needs the noise pdf's, as well as estimates of the mixture
probabilities.

`em_rinit` uses `etk2tau` to start the EM algorithm.

## Value

for `em_rinit`, an object from class `"MixARGaussian"`

for `etk2tau`, a matrix representing `tau` (i-th row contains
probabilities corresponding to the i-th observation)

## Author

Georgi N. Boshnakov
