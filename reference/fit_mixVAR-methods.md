# Fit mixture vector autoregressive models

Estimate a MixVAR model for a multivariate time series. This is a
generic function. The methods defined in package MixAR are described
here.

## Usage

``` r
fit_mixVAR(x, model, fix, ...)
```

## Arguments

- x:

  a multivariate time series (currently a numeric matrix).

- model:

  model, object inheriting from MixVAR class.

- fix:

  if TRUE, fix the shift parameters.

- ...:

  additional arguments for the methods (not currently used).

## Value

a MixVAR model.

## Details

`model` specifies the model to fit. If `model` inherits from `"MixVAR"`,
it is used as a template. Estimation is done via EM-Algorithm, using the
function `mixVARfit`.

Currently the default method for `fit_mixAR` just throws error, since
there seems no suitable default task to do.

## Methods

- `signature(x = "ANY", model = "MixVAR")`:

- `signature(x = "ANY", model = "ANY")`:

## See also

[`mixVARfit`](https://geobosh.github.io/mixAR/reference/mixVARfit.md)

## Examples

``` r
AR <- list()
AR[[1]] <- array(c(0.5, -0.3, -0.6, 0, 0, 0.5, 0.4, 0.5, -0.3), dim = c(3, 3, 1))
AR[[2]] <- array(c(-0.5, 0.3, 0, 1, 0, -0.5, -0.4, -0.2, 0.5), dim = c(3, 3, 1))

prob <- c(0.75, 0.25)
shift <- cbind(c(0, 0, 0), c(0, 0, 0))

Sigma1 <- cbind(c(1, 0.5, -0.4), c(0.5, 2, 0.8), c(-0.4, 0.8, 4))
Sigma2 <- cbind(c(1, 0.2, 0), c(0.2,  2, -0.15), c(0, -0.15, 4))
Sigma <- array(c(Sigma1, Sigma2), dim = c(3, 3, 2))

m <- new("MixVARGaussian", prob = prob, vcov = Sigma, arcoef = AR, shift = shift)

set.seed(1234)
y <- mixVAR_sim(m, n = 100, init = matrix(0, ncol = 3), nskip = 50, flag = FALSE)

fit_mixVAR(y, m, tol = 1e-3)
#> $model
#> (To see the internal structure of the object, use function 'str'.)
#> 
#> An object of class "MixVARGaussian"
#> Number of components: 2 
#> Number of series: 3 
#>             prob      shift order        ar_1                        
#> Comp_1 0.7561508  0.1281656     1  0.41214162  0.15489065  0.36858610
#>                   0.1476945       -0.67459251  0.23338114  0.53169958
#>                  -0.4193473       -0.98634266  0.58632537 -0.33231352
#> Comp_2 0.2438492  0.3997921     1 -0.26384005  0.40212431 -0.14272653
#>                   0.6901770       -0.04967386 -0.06820107 -0.07706285
#>                  -0.5439589       -0.04438110  0.09917752  0.34190926
#>               vcov                       
#> Comp_1  1.28998427  0.3532316 -1.43884725
#>         0.35323165  1.4588477  0.84376732
#>        -1.43884725  0.8437673  5.09397281
#> Comp_2  1.18935913  0.5697533 -0.07065855
#>         0.56975325  0.9346697 -0.99381873
#>        -0.07065855 -0.9938187  5.28724834
#> 
#> Distributions of the error components:
#>  Multivariate standard Gaussian
#> 
#> 
#> $vallogf
#> [1] -477.3159
#> 
mixVARfit(y, m, tol = 1e-3)
#> $model
#> (To see the internal structure of the object, use function 'str'.)
#> 
#> An object of class "MixVARGaussian"
#> Number of components: 2 
#> Number of series: 3 
#>             prob      shift order        ar_1                        
#> Comp_1 0.7561684  0.1281484     1  0.41212929  0.15486430  0.36858362
#>                   0.1476893       -0.67459978  0.23335828  0.53169430
#>                  -0.4192959       -0.98629155  0.58640262 -0.33230476
#> Comp_2 0.2438316  0.3997948     1 -0.26381786  0.40218964 -0.14274202
#>                   0.6900980       -0.04958495 -0.06808290 -0.07707954
#>                  -0.5439389       -0.04455661  0.09893087  0.34194016
#>               vcov                       
#> Comp_1  1.28997846  0.3532545 -1.43887869
#>         0.35325450  1.4588626  0.84369248
#>        -1.43887869  0.8436925  5.09408617
#> Comp_2  1.18937746  0.5697202 -0.07050862
#>         0.56972024  0.9345942 -0.99356299
#>        -0.07050862 -0.9935630  5.28685385
#> 
#> Distributions of the error components:
#>  Multivariate standard Gaussian
#> 
#> 
#> $vallogf
#> [1] -477.2795
#> 
```
