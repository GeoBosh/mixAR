# Optimise scale parameters in MixARgen models

Optimise the scale parameters in MixAR models from class MixARgen.
Internal function.

## Usage

``` r
em_est_dist(tau, etk, parscore, sigma, nu, logpdf)
```

## Arguments

- tau:

  conditional probabilities, an object of class "MixComp", see
  'Details'.

- etk:

  component residuals, see 'Details'.

- parscore:

  the score function(s), see 'Details'.

- sigma:

  current values of the scale parameters, a numeric vector.

- nu:

  current values of the parameters. w.r.t. which optimisation is done.

- logpdf:

  the log of pdf as a function of the parameters.

## Details

One or more of the error distributions of a MixAR model may have
parameters that are considered unknown. In that case `em_est_dist` can
be used to optimise with respect to them.

The representation of the error distributions in "MixARgen" models
carries all the necessary information about parameters. `em_est_dist`
works by extracting their current values from `logpdf`, passes them to
the optimisation function (or equation solver) and stores the result
back into `logpdf`. `em_est_dist` is quite general, as long as `logpdf`
is prepared according to the conventions it expects (this is so if they
are valid elements of the `dist` slot of "MixARgen" objects).

## Value

the new values of the parameters
