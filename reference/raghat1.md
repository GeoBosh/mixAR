# Filter a time series with options to shift and scale

Filter a time series with options to shift and scale. This function is
used by mixFilter.

## Usage

``` r
raghat1(filter, x, index, shift = 0, residual = FALSE, scale = 1)
```

## Arguments

- filter:

  The coefficients of the filter, numeric, see Details.

- x:

  time series, numeric.

- index:

  indices for which to compute the filtered values, numeric.

- shift:

  a constant to be added to each filtered element, a number.

- residual:

  if TRUE calculate a \`residual', otherwise calculate a \`hat' value.

- scale:

  if `scale != 1` calculate scaled residuals by divividing by this
  value. Probably meaningful only if `residual=TRUE`.

## Details

This function is used by `mixFilter`. Applies an autoregressive filter
to a time series for indices specified by `index`.

Note that \`filter' here is equivalent to calculating one-step
predictions (or residuals if `residual=TRUE`) from autoregressions.

`index` should not specify indices smaller than `length(filter)+1` or
larger than `length(x)+1`. The value `length(x)+1` can legitimately be
used to calculate a prediction (but not a residual of course) for the
first value after the end of the series.

## Value

A numeric vector of length equal to `length(index)`.

## Note

This should probably use `filter` but for the purposes of this package
`filter` is usually short and the calculation is vectorised w.r.t.
`index`, so should not be terribly slow.
