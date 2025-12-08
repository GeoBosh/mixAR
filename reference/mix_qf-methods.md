# Conditional quantile functions of MixAR models

Gives conditional quantile functions of mixture autoregressive models.

## Usage

``` r
mix_qf(model, p, x, index, xcond)
```

## Arguments

- model:

  mixAR model.

- p:

  vector of probabilities.

- x:

  time series.

- index:

  vector of positive integers.

- xcond:

  the past values needed for the conditional distribution, a numeric
  vector of length at least the maximal AR order of the components.

## Value

depending on the arguments, a function for computing quantiles or a
numeric vector representing quantiles, see sections 'Details' and
'Methods

## Author

Georgi N. Boshnakov

## Methods

- `signature(model = "MixARGaussian", p = "missing", x = "missing", index = "missing", xcond = "numeric")`:

- `signature(model = "MixARGaussian", p = "numeric", x = "missing", index = "missing", xcond = "numeric")`:

- `signature(model = "MixARGaussian", p = "numeric", x = "numeric", index = "numeric", xcond = "missing")`:

## See also

[`mix_pdf`](https://geobosh.github.io/mixAR/reference/mix_pdf-methods.md),
[`mix_cdf`](https://geobosh.github.io/mixAR/reference/mix_pdf-methods.md);

[`mix_moment`](https://geobosh.github.io/mixAR/reference/mix_moment.md)
for examples
