# M-step for models from class MixARgen

M-step for models from class MixARgen. This function is for use by other
functions.

## Usage

``` r
mixgenMstep(y, tau, model, index, fix = NULL, comp_sigma = FALSE,
            method = "BBsolve", maxit = 100, trace = FALSE,
            lessverbose = TRUE, ...)
```

## Arguments

- y:

  time series, a numeric vector.

- tau:

  conditional probabilities, an object of class "MixComp".

- model:

  the current model, an object from a subclass of class "MixAR".

- index:

  indices of observations for which to compute residuals, a vector of
  positive integers, see 'Details'.

- method:

  optimisation or equation solving method for package BB

- ...:

  arguments to pass on to optimisation functions, not thought over yet.
  Do not use until this notice is removed.

- comp_sigma:

  If TRUE optimise the scale parameters using univariate optimisation.
  (note: does not work with argument 'fix' yet.)

- fix:

  specify parameters to be held fixed during optimisation, see
  'Details'.

- maxit:

  maximal number of iterations for BB optimisers and solvers. Meant
  mainly for testing.

- trace:

  if TRUE, BB optimisers and solvers will print information about their
  proceedings. Meant mainly for testing.

- lessverbose:

  if TRUE, print a dot instead of more verbose information.

## Details

`mixgenMstep` is an implementation of the M-step of the EM algorithm for
mixture autoregressive models specified by objects of class "MixARgen".
The function was build and modified incrementally with the main goal of
providing flexibility. Speed will be addressed later.

By default optimisation is done with respect to all parameters. Argument
`fix` may be a list with elements "prob", "shift", "scale" and "arcoef".
These elements should be logical vectors containing `TRUE` in the
positions of the fixed parameters. Elements with no fixed parameters may
be omitted. (Currently the "prob" element is ignored, i.e. it is not
possible to fix any of the component probabilities.)

If `fix = "shift"` the shift parameters are kept fixed. This is
equivalent to `fix = list(shift = rep(TRUE,g))`.

The parameters (if any) of the distributions of the error components are
estimated by default. Currently the above method cannot be used to fix
some of them. This can be achieved however by modifying the distribution
part of the model since that incorporates information about the
parameters and whether they are fixed or not.

## See also

[`fit_mixAR`](https://geobosh.github.io/mixAR/reference/fit_mixAR-methods.md)
and
[`mixARgenemFixedPoint`](https://geobosh.github.io/mixAR/reference/mixARemFixedPoint.md)
which are meant to be called by users.
