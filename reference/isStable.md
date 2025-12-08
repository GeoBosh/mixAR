# Check if a MixAR model is stable

Checks if a MixAR model is stable. This is also the second order
stationarity condition.

## Usage

``` r
isStable(x)
```

## Arguments

- x:

  the model

## Details

If each component of a MixAR model corresponds to a stable
autoregression model, then the MixAR model is also stable. However, the
MixAR model may be stable also when some of its components correspond to
integrated or explosive AR models, see the references.

## Value

True if the model is stable (second order stationary), FALSE otherwise.

## References

Boshnakov GN (2011). “On First and Second Order Stationarity of Random
Coefficient Models.” *Linear Algebra Appl.*, **434**(2), 415–423.
[doi:10.1016/j.laa.2010.09.023](https://doi.org/10.1016/j.laa.2010.09.023)
.

Wong CS, Li WK (2000). “On a mixture autoregressive model.” *J. R. Stat.
Soc., Ser. B, Stat. Methodol.*, **62**(1), 95-115.

## Examples

``` r
isStable(exampleModels$WL_I)
#> [1] TRUE
isStable(exampleModels$WL_II)
#> [1] TRUE
```
