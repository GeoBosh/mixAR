# Filter time series with MixAR filters

Filter time series with MixAR filters, a generic function with no
default method (currently).

## Usage

``` r
mixFilter(x, coef, index, shift = 0, residual = FALSE, scale = 1)
```

## Arguments

- x:

  time series

- coef:

  the filter coefficients

- index:

  indices for which to calculate the filtered values.

- shift:

  optional shifts (intercept) terms.

- residual:

  If FALSE (default) calculate “predictions”, if TRUE calculate
  “residuals”.

- scale:

  optional scale factor(s), makes sense only when `residual=TRUE`,
  corresponds to `scale` in the specification of a MixAR model.

## Value

a `MixComp` object

## Author

Georgi N. Boshnakov

## Methods

- `signature(x = "ANY", coef = "ANY", index = "ANY")`:

  This method simply prints an error message and stops.

- `signature(x = "numeric", coef = "raggedCoef", index = "numeric")`:

## See also

[`raghat1`](https://geobosh.github.io/mixAR/reference/raghat1.md)
[`mix_ek`](https://geobosh.github.io/mixAR/reference/mix_ek.md)
[`mix_hatk`](https://geobosh.github.io/mixAR/reference/mix_hatk.md)
