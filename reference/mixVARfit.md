# Fit mixture vector autoregressive models

Provides EM-estimation of mixture autoregressive models for multivariate
time series

## Usage

``` r
mixVARfit(y, model, fix = FALSE, tol = 10^-6, verbose = FALSE)
```

## Arguments

- y:

  a data matrix.

- model:

  an object of class `"MixVAR"` with initial values of parameter for EM
  estimation.

- tol:

  Threshold for convergence criterion.

- fix:

  if TRUE, fix the shift parameters.

- verbose:

  if `TRUE` print information during the optimisation.

## Details

Estimation is done under the assumption of multivariate Gaussian
innovations.

## Value

An object of class `MixVARGaussian` with EM estimates of model
parameters.

## References

Fong PW, Li WK, Yau CW, Wong CS (2007). “On a Mixture Vector
Autoregressive Model.” *The Canadian Journal of Statistics / La Revue
Canadienne de Statistique*, **35**(1), 135–150. ISSN 03195724,
[doi:10.1002/cjs.5550350112](https://doi.org/10.1002/cjs.5550350112) .

## Author

Davide Ravagli

## See also

[`fit_mixVAR-methods`](https://geobosh.github.io/mixAR/reference/fit_mixVAR-methods.md)
for examples
