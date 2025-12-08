# The E-step of the EM algorithm for MixAR models

Compute conditional probabilities for the E-step of the EM algorithm for
MixAR models. Internal function.

## Usage

``` r
mixAR_cond_probs(model, y, indx = NULL)
```

## Arguments

- model:

  an object from a sub-class of "MixAR".

- y:

  the time series, a numeric vector.

- indx:

  indices of elements for which to compute residuals.

## Details

This is essentially the E-step for the MixAR models.

## Value

the conditional probabilities, an object from class "MixComp".
