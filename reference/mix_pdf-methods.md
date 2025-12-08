# Conditional pdf's and cdf's of MixAR models

Gives conditional probability densities and distribution functions of
mixture autoregressive models.

## Methods

`mix_pdf` gives a probability density, `mix_cdf` a distribution
function. If argument `x` is supplied, the functions are evaluated for
the specified values of `x`, otherwise function objects are returned and
can be used for further computations, eg for graphs.

`mix_pdf` and `mix_cdf` have methods with the following signatures.

- `signature(model = "MixARGaussian", x = "missing", index = "missing", xcond = "numeric")`:

  Return (as a function of one argument) the conditional density
  (respectively cdf), \\f(x\|xcond)\\, of \\X\_{t+1}\\ given the past
  values `xcond`. The values in `xcond` are in natural time order, e.g.
  the last value in `xcond` is \\x\_{t}\\. `xcond` must contain enough
  values for the computation of the conditional density (cdf) but if
  more are given, only the necessary ones are used.

- `signature(model = "MixARGaussian", x = "numeric", index = "missing", xcond = "numeric")`:

  Compute the conditional density (respectively cdf) at the values given
  by `x`.

- `signature(model = "MixARGaussian", x = "numeric", index = "numeric", xcond = "missing")`:

  Compute conditional densities (respectively cdf) for times specified
  in `index`. For each \\t\in{}\\`index` the past values needed for the
  computation of the pdf (cdf) are `...,x[t-2],x[t-1]`.

- `signature(model = "MixARgen", x = "missing", index = "missing", xcond = "numeric")`:

- `signature(model = "MixARgen", x = "numeric", index = "missing", xcond = "numeric")`:

- `signature(model = "MixARgen", x = "numeric", index = "numeric", xcond = "missing")`:

## Author

Georgi N. Boshnakov

## See also

[`mix_moment`](https://geobosh.github.io/mixAR/reference/mix_moment.md)
for examples and computation of summary statistics of the predictive
distributions

[`mix_qf`](https://geobosh.github.io/mixAR/reference/mix_qf-methods.md)
for computation of quantiles.
