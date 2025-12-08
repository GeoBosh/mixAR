# Update the scale parameters of MixAR models

Calculates estimates of scale parameters of MixAR models from
conditional probabilities and mixture \`residuals'. Used in EM
algorithm.

## Usage

``` r
tauetk2sigmahat(tau, etk)

em_est_sigma(tau, etk, Fscore, sigma,
             dontfix = rep(TRUE, length(sigma)), compwise = FALSE)
```

## Arguments

- tau:

  the conditional probabilities for the groups, a `"MixComp"` object.

- etk:

  component "residuals", MixComp object(?).

- Fscore:

  the score function(s) of the noise distributions.

- sigma:

  current values of the scale parameters.

- compwise:

  if `TRUE` solve the equations component-wise, see 'Details.

- dontfix:

  a logical vector containig `TRUE` in the positions of elements of
  `sigma` that are to be estimated.

## Details

`tauetk2sigmahat` calculates estimates of the scale parameters for a
MixAR time series with Gaussian components. There is an explicit formula
in that case.

`em_est_sigma` calculates estimates of the scale parameters in the
general case. The non-linear equations are solved using functions from
package `BB`. The equations for the components can often be solved
independently. When that is the case, `compwise` may speed things up a
little.

## Value

The new values of the scale parameters, a numeric vector
