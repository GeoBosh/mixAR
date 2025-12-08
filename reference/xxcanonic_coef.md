# Put core MixAR coefficients into a canonical form, internal function

Put core MixAR coefficients into a canonical form, internal function.

## Usage

``` r
.canonic_coef(coef, filler)
```

## Arguments

- coef:

  the core coefficients of a MixAR model, a list.

- filler:

  a value for filling unspecified entries, such as `NA_real_`.

## Details

This is an internal function called by `mixAR` to put MixAR parameters
into a canonical form. `"shift"`, `"scale"` and `"prob"` are normally
vectors with one element for each component but they may also be given
as a single number, in which case it is taken to be the common value for
all parameters and `.canonic_coef` extends them correspondingly. Also,
the AR coefficients may be specified in a number of ways and this
function converts them to the format used by the MixAR classes.

## Value

a list with the folowing components:

- order:

  MixAR order, a vector of integers

- prob:

  MixAR probabilities, vector of length`length(order)`

- shift:

  MixAR shift parameters, vector of length`length(order)`

- scale:

  MixAR scle parameters, vector of length`length(order)`

- arcoef:

  AR coefficients as a `"raggedCoef"` object

## Author

Georgi N. Boshnakov

## See also

[`mixAR`](https://geobosh.github.io/mixAR/reference/mixAR-methods.md)

## Examples

``` r
## missing components are filled with 'filler', extended accordingly
mixAR:::.canonic_coef(list(order = c(2,3)), filler = NA)
#> $order
#> [1] 2 3
#> 
#> $prob
#> [1] NA NA
#> 
#> $shift
#> [1] NA NA
#> 
#> $scale
#> [1] NA NA
#> 
#> $arcoef
#> An object of class "raggedCoef"
#> Number of rows: 2 
#> Components' lengths: 2 3 
#> 
#>             co_1 co_2 co_3
#> Component_1  NA   NA      
#> Component_2  NA   NA   NA 
#> 
#> 

# here 'scale' is replicated, the missing 'shift' is inserted
mo <- list(order = c(2,3), prob = c(0.4, 0.6), scale = 1,
           arcoef = list(c(0.5, -0.5), c(1.1,  0.0, -0.5)) )
mixAR:::.canonic_coef(mo, filler = NA)
#> $order
#> [1] 2 3
#> 
#> $prob
#> [1] 0.4 0.6
#> 
#> $scale
#> [1] 1 1
#> 
#> $arcoef
#> An object of class "raggedCoef"
#> Number of rows: 2 
#> Components' lengths: 2 3 
#> 
#>             co_1 co_2 co_3
#> Component_1 0.5  -0.5     
#> Component_2 1.1   0.0 -0.5
#> 
#> 
#> $shift
#> [1] NA NA
#> 
```
