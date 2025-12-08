# Function and methods to compute component residuals for MixAR models

Compute component residuals for MixAR models.

## Usage

``` r
mix_ek(model, x, index, xcond, scale)
```

## Arguments

- model:

  a model.

- x:

  time series.

- index:

  a vector of positive integer specifying the indices for which to
  compute the residuls, has a natural default.

- xcond:

  the past values needed for the conditional distribution, a numeric
  vector of length at least the maximal AR order of the components.

- scale:

  logical or missing, if `TRUE` standardise the residuals.

## Details

`mix_ek` computes component residuals from MixAR models.

It is highly desirable to use it along with
[`mix_hatk`](https://geobosh.github.io/mixAR/reference/mix_hatk.md) and
the underlying function
[`mixFilter`](https://geobosh.github.io/mixAR/reference/mixFilter.md).
Doing this ensures transparent code and easy maintenance. Also, more
efficient implementation can be introduced without changing other code.

## Author

Georgi N. Boshnakov

## Methods

- `signature(model = "MixAR", x = "numeric", index = "missing", xcond = "numeric", scale = "logical")`:

- `signature(model = "MixAR", x = "numeric", index = "missing", xcond = "numeric", scale = "missing")`:

- `signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing", scale = "logical")`:

- `signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing", scale = "missing")`:

## See also

[`mixFilter`](https://geobosh.github.io/mixAR/reference/mixFilter.md)
which is used by `mix_ek` to do the job,
[`MixComp-class`](https://geobosh.github.io/mixAR/reference/MixComp-class.md)
for easy manipulation of the returned object.

class
`"`[`MixAR`](https://geobosh.github.io/mixAR/reference/MixAR-class.md)`"`
