# Give natural limits for parameters of a MixAR model.

Give the natural lower and upper limits for the parameters of a MixAR
model.

## Usage

``` r
lik_params_bounds(model)
```

## Arguments

- model:

  a `MixAR` model

## Details

The function gives crude limits: the probabilities of the components are
between 0 and 1, the standard deviations of the components are
non-negative. For the other parameters the limits are
\\(-\infty,\infty)\\.

There are further restrictions, e.g. the sum of the probabilities should
be less than or equal to 1 and the autoregression coefficients normally
are restricted to a particular region, but these are not indicated in
the returned value.

## Value

A list with two components describing the limits on the parameters. The
order of the parameters is as the one returned by
[`lik_params`](https://geobosh.github.io/mixAR/reference/lik_params.md).

- lower:

  lower limits, a numeric vector

- upper:

  upper limits, a numeric vector

## See also

[`lik_params`](https://geobosh.github.io/mixAR/reference/lik_params.md)
