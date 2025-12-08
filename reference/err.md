# Calculate component specific error terms under MixAR model

Returns a matrix which columns correspond to the error terms. Each
column's row will assume value 0 if the observation was not "assigned"
to that component.

## Usage

``` r
err(AR, mu, y, z, pk)
```

## Arguments

- AR:

  a `list` with autoregressive components.

- mu:

  Component means.

- y:

  a time series (currently a numeric vector).

- z:

  a `matrix` which columns contain observations "assignment". Hence each
  row will contain one 1 and (g-1) 0's

- pk:

  autoregressive orders.

## Value

- e:

  a matrix which columns correspond to component specific "residuals",
  which are equal to 0 for observations not assigned to such component.

## Author

Davide Ravagli

## Note

This function is built for use within sampling functions.
