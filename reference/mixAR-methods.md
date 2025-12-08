# Create MixAR objects

Generic function with methods for creating MixAR objects.

## Usage

``` r
mixAR(template, coef, ..., filler = NA_real_)
```

## Arguments

- template:

  an object to be used as a template for the new object, typically
  inheriting from `"MixAR"`. Alternatively, missing or a numeric vector
  specifying the order of the MixAR model, see Details.

- coef:

  parameters for the new object a list with components `"arcoef"`,
  `"order"`, `"prob"`, `"shift"`, and `"scale"`.

- ...:

  further arguments for methods.

- filler:

  value for unspecified parameters, default is `NA_real_`.

## Details

`mixAR` provides an alternative to the function `new` for specifying
MixAR models.

If `template` is numeric vector, it is taken to specify the AR order of
the model and the number of mixture components. A Gaussian MixAR model
is created with parameters filled initially with NA's and then updated
with values given by `coef`. `coef` does not need to have values for all
parameters and may be missing altogether. If NA's are not suitable for
initialisation, a suitable value can be specified with `filler`.

If `template` is a MixAR object, then the new object will have the class
of `template`. The new object is set initially to a copy of `template`
and then updated with parameters specified by `coef` (and maybe others
for some methods).

In principle, the numeric parameters are vectors of length the number of
components of the MixAR model. For convenience, single values are
replicated to the number of components. For this to work, at least one
component must be specified completely, for example the order. It is an
error for the parameters to imply conflicting number of components.

## Methods

- `signature(template = "ANY")`:

- `signature(template = "MixAR")`:

## See also

class
`"`[`MixARGaussian`](https://geobosh.github.io/mixAR/reference/MixARGaussian-class.md)`"`,
class
`"`[`MixARgen`](https://geobosh.github.io/mixAR/reference/MixARgen-class.md)`"`

## Examples

``` r
mixAR(coef = list(prob = c(.5,.5), scale = c(1,2), 
                  arcoef = list(.5, 1.1), shift = c(0,0), order = c(1,1)))
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob shift scale order ar_1
#> Comp_1 0.5    0     1     1   0.5 
#> Comp_2 0.5    0     2     1   1.1 
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 

mixAR(template = c(1,1))
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob shift scale order ar_1
#> Comp_1  NA   NA    NA     1    NA 
#> Comp_2  NA   NA    NA     1    NA 
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
mixAR(coef = list(order = c(1,1))) # same
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob shift scale order ar_1
#> Comp_1  NA   NA    NA     1    NA 
#> Comp_2  NA   NA    NA     1    NA 
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 

m2 <- new("MixARGaussian", order = c(3, 2, 1),
          arcoef = matrix(c(1:3, c(1:2, 0), c(1, 0, 0)), nrow = 3, byrow = TRUE))
m2a <- mixAR(m2, list(prob = c(0.5, 0.25, 0.25)))
show_diff(m2, m2a)
#>        prob shift scale order ar_1 ar_2 ar_3
#> Comp_1  NA    0     0     3     0    0    0 
#> Comp_2  NA    0     0     2     0    0      
#> Comp_3  NA    0     0     1     0           
```
