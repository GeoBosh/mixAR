# Vector of parameters of a MixAR model

Give a numeric vector containing non-redundant parameters of a MixAR
model in a form suitable for use by optimisation routines. The methods
defined in package mixAR for this generic function are described here.

## Usage

``` r
lik_params(model)
```

## Arguments

- model:

  a MixAR model.

## Details

`lik_params` gives the parameters of a MixAR model as a numeric vector.

This is a generic function. Parameters common to all MixAR models are
arranged as described below. There are no other parameters when the
error distributions do not contain parameters of their own. Methods for
sub-classes with additional parameters should append them after the
common parameters.

If \\k\\ is the number of components and \\\pi_i\\ is the probability
associated with the \\i\\th component, then the parameters are put in a
vector as follows:

1.  component probabilities, \\\pi_1,\ldots,\pi\_{k-1}\\, (note:
    \\\pi\_{k}\\ is not included)

2.  scales, \\\sigma_1,\ldots,\sigma\_{k}\\,

3.  shifts, \\\mu_1,\ldots,\mu\_{k}\\,

4.  AR coefficients of the 1st component,

5.  AR coefficients of the 2nd component,

6.  ...

7.  AR coefficients of the \\k\\th component.

## Value

A numeric vector containing all parameters except the probability
associated with the last component.

## Author

Georgi N. Boshnakov

## Note

The probability associated with the \\k\\th component is omitted as it
is redundant. This makes it possible to try unconstrained optimisation
though it is not likely to give useful results since there are other
restrictions on the probabilities.

## Methods

- `signature(model = "MixAR")`:

- `signature(model = "MixARgen")`:
