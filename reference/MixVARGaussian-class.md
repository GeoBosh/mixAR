# MixVAR models with multivariate Gaussian noise components

Class MixVARGaussian represents MixAR models with multivariate Gaussian
noise components.

## Objects from the Class

Objects can be created by calls of the form
`new("MixVARGaussian", ...)`, giving the elements of the model as named
arguments, see the examples below.

`arcoef` contains the AR coefficients, one numeric array for each
mixture component. It can be given as a
`"`[`raggedCoefV`](https://geobosh.github.io/mixAR/reference/raggedCoefV-class.md)`"`
object or as a list of numeric arrays.

The AR order of the model is inferred from `arcoef` argument. If
argument `order` is given, it is checked for consistency with `arcoef`.
The `shift` slot defaults to a matrix of zeroes and the `vcov` slot to
an array of identity matrices, one for each component.

The distribution of the noise components is standard multivariate
Gaussian, N(0,1).

## Slots

All slots except `arcoef` are numeric vectors of length equal to the
number of components in the model.

- `prob`::

  probabilities of the mixture components,

- `order`::

  AR orders of the components,

- `shift`::

  the shift (intercept) terms of the AR components,

- `vcov`::

  covariance matrices of the noise terms of the AR components,

- `arcoef`::

  The AR components, object of class `"raggedCoefV"`.

## Extends

Class
`"`[`MixAR`](https://geobosh.github.io/mixAR/reference/MixAR-class.md)`"`,
directly.

## Methods

- fit_mixAR:

  `signature(x = "ANY", model = "MixARGaussian")`: ...

## Author

Davide Ravagli

## See also

class
`"`[`MixAR`](https://geobosh.github.io/mixAR/reference/MixAR-class.md)`"`

## Examples

``` r
showClass("MixVARGaussian")
#> Class "MixVARGaussian" [package "mixAR"]
#> 
#> Slots:
#>                                                                   
#> Name:         prob       order       shift        vcov      arcoef
#> Class:     numeric     numeric      matrix       array raggedCoefV
#> 
#> Extends: "MixVAR"

## Create array of covariance matrices
Sigma1 <- cbind(c(0.0013, 0.0011), c(0.0011, 0.0012))
Sigma2 <- cbind(c(0.0072, 0.0047), c(0.0047, 0.0039))
Sigma  <- array(c(Sigma1, Sigma2), dim=c(2,2,2))

## Create list of AR coefficients
AR <- list()
AR[[1]] <- array(c(0.0973, -0.0499,  0.2927,  0.4256,  ## VAR(2;4)
                  -0.0429,  0.0229, -0.1515, -0.1795,
                  -0.0837, -0.1060, -0.1530,  0.1947,
                  -0.1690, -0.0903,  0.1959,  0.0955), dim=c(2,2,4))
AR[[2]] <- array(c(0.3243,  0.2648,  0.4956,  0.2870,  ## VAR(2;3)
                  -0.1488,  0.0454, -0.0593, -0.3629,
                   0.1314,  0.0274,  0.0637,  0.0485), dim=c(2,2,3))

## Create vector of mixing weights
prob <- c(0.6376, 0.3624)

## Create matrix of shift parameters
shift <- cbind(c(0.0044, 0.0020), c(-0.0039, -0.0014))

## Build "MixVARGaussian" model
new("MixVARGaussian", prob=prob, vcov=Sigma, arcoef=AR, shift=shift)
#> (To see the internal structure of the object, use function 'str'.)
#> 
#> An object of class "MixVARGaussian"
#> Number of components: 2 
#> Number of series: 2 
#>          prob   shift order    ar_1           ar_2            ar_3        
#> Comp_1 0.6376  0.0044     4  0.0973 0.2927 -0.0429 -0.1515 -0.0837 -0.1530
#>                0.0020       -0.0499 0.4256  0.0229 -0.1795 -0.1060  0.1947
#> Comp_2 0.3624 -0.0039     3  0.3243 0.4956 -0.1488 -0.0593  0.1314  0.0637
#>               -0.0014        0.2648 0.2870  0.0454 -0.3629  0.0274  0.0485
#>           ar_4          vcov       
#> Comp_1 -0.1690 0.1959 0.0013 0.0011
#>        -0.0903 0.0955 0.0011 0.0012
#> Comp_2                0.0072 0.0047
#>                       0.0047 0.0039
#> 
#> Distributions of the error components:
#>  Multivariate standard Gaussian
#> 
```
