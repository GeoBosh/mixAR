# Utility function for MixAR

Calculates residuals under a certain component of the mixture.

## Usage

``` r
err_k(AR, mu, y, z, p, pk)
```

## Arguments

- AR:

  a `list` with autoregressive parameters.

- mu:

  component means.

- y:

  a time series (currently a numeric vector).

- z:

  a vector of allocation to a specific component.

- p:

  maximum autoregressive order

- pk:

  autoregressive order of the component.

## Value

- e:

  a vector containing component specific residuals.

## Author

Davide Ravagli

## Note

This is built as a utility function.
