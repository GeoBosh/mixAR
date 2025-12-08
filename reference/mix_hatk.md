# Compute component predictions for MixAR models

Function and methods to compute component predictions for MixAR models

## Usage

``` r
mix_hatk(model, x, index, xcond)
```

## Arguments

- model:

  a model.

- x:

  time series.

- index:

  a vector of positive integers specifying the indices for which to
  compute the residuals, has a natural default.

- xcond:

  the past values needed for the conditional distribution, a numeric
  vector of length at least the maximal AR order of the components.

## Author

Georgi N. Boshnakov

## Methods

- `signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing")`:

## See also

class
`"`[`MixAR`](https://geobosh.github.io/mixAR/reference/MixAR-class.md)`"`
