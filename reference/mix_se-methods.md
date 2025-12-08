# Compute standard errors of estimates of MixAR models

Compute standard errors of estimates of MixAR models.

## Usage

``` r
mix_se(x, model, fix_shift)
```

## Arguments

- x:

  time series.

- model:

  MixAR model, an object inheriting from class “MixAR”.

- fix_shift:

  `logical`. Should the shift paramters be fixed? Default is `FALSE`.

## Details

For formulas used in the computation, see Wong (1998) .

## Value

a list with components:

- standard_errors:

  Standard error of parameter estimates,

- covariance_matrix:

  The covariance matrix, obtained as inverse of the information matrix,

- Complete_Information:

  Complete information matrix,

- Missing_Information:

  Missing information matrix.

## References

Wong CS (1998). *Statistical inference for some nonlinear time series
models*. Ph.D. thesis, University of Hong Kong, Hong Kong.

## Author

Davide Ravagli

## Methods

- `signature(x = "ANY", model = "list")`:

- `signature(x = "ANY", model = "MixAR")`:

- `signature(x = "ANY", model = "MixARGaussian")`:

## Examples

``` r
## Example with IBM data

## data(ibmclose, package = "fma")

moWLprob <- exampleModels$WL_ibm@prob    # 2019-12-15; was: c(0.5339,0.4176,0.0385)     
moWLsigma <- exampleModels$WL_ibm@scale  #                  c(4.8227,6.0082,18.1716)
moWLar <- list(-0.3208, 0.6711,0)        # @Davide - is this from some model?

moWLibm <- new("MixARGaussian", prob = moWLprob, scale = moWLsigma, arcoef = moWLar)

IBM <- diff(fma::ibmclose)
mix_se(as.numeric(IBM), moWLibm, fix_shift = TRUE)$'standard_errors'
#> $Component_1
#>       Estimate Standard Error
#> prob    0.5439     0.09005847
#> AR_1   -0.3208     0.07477196
#> scale   4.8227     0.49799538
#> 
#> $Component_2
#>       Estimate Standard Error
#> prob    0.4176     0.08671523
#> AR_1    0.6711     0.12662929
#> scale   6.0082     0.77726571
#> 
#> $Component_3
#>       Estimate Standard Error
#> prob    0.0385     0.03263437
#> AR_1    0.0000     0.80123158
#> scale  18.1716     5.70693125
#> 
```
