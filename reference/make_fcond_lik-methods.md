# Create a function for computation of conditional likelihood

Create a function for the computation of the conditional likelihood of
MixAR models for a given time series. The methods for this generic
function defined in package mixAR are described here.

## Usage

``` r
make_fcond_lik(model, ts)
```

## Arguments

- model:

  a `MixAR` model

- ts:

  the time series

## Details

The returned value is a function, say `f(x)`, whose only argument is a
numeric vector of parameters with the arrangement of
[`lik_params`](https://geobosh.github.io/mixAR/reference/lik_params.md),
for which it computes the conditional loglikelihood. `f` can be given to
optimisation routines.

Argument `model` is an object inheriting from `MixAR` and determines the
structure of the MixAR model for the function, `f`, that it creates. So,
properties of the model, such as number of components, AR order, and
distribution of the noise components are fixed when `f` is created and
only the numeric values of the parameters are changed by calls to it.

## Value

a function of one argument, the parameters of a MixAR model as a numeric
vector with the arrangement of
[`lik_params`](https://geobosh.github.io/mixAR/reference/lik_params.md),
for which it computes the conditional loglikelihood

## Todo

The environment of the returned function contains the time series and
the model object (initially argument `model`, later the model used in
the last call to `f`). So, these things can be extracted from `f`. Is it
necessary to create convenience functions?

## Methods

- `signature(model = "MixAR", ts = "numeric")`:

## See also

[`mix_pdf`](https://geobosh.github.io/mixAR/reference/mix_pdf-methods.md),
[`mix_cdf`](https://geobosh.github.io/mixAR/reference/mix_pdf-methods.md)
