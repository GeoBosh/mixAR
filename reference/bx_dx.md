# RJMCMC move for AR order selection of mixture autoregressive models

Computes probabilities for deciding whether the AR order should be
increased or decreased by 1 at each iteration in Bayesian analysis of
mixture autoregressive models.

## Usage

``` r
bx_dx(method = c("Ratio", "Poisson", "NULL"), par, pk)
```

## Arguments

- method:

  the method used for updating probabilities. If `"Ratio"` or
  `"Poisson"`, probabilities depend on `par` and decrease as `pk`
  increases. If `"NULL"`, probabilities are set to 0.5. See Examples for
  details.

- par:

  tuning parameter for calculating updating probabilities.

- pk:

  autoregressive order of the selected component.

## Value

A list of `2` elements:

- bx:

  The probability of increasing the autoregressive order by 1.

- dx:

  The probability of decreasing the autoregressive order by 1,
  calculated as `1 - bx`.

## References

Ravagli D, Boshnakov GN (2020). “Bayesian analysis of mixture
autoregressive models covering the complete parameter space.”
2006.11041, <https://arxiv.org/abs/2006.11041>.

## Author

Davide Ravagli

## Note

This function is for use within
[`Choose_pk`](https://geobosh.github.io/mixAR/reference/Choose_pk.md).
