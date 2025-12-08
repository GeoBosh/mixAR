# Class `"raggedCoefV"` — ragged list

Ragged list used to hold coefficients of MixVAR models.

## Objects from the Class

Objects are created by calls of the form
`new("raggedCoefV", a = list(v1, v2 ,...)`.

## Slots

- `a`::

  Object of class `"list"` containing AR values. Each element of the
  list must be `"array"`.

- `p`::

  Object of class `"numeric"` containing the length of arrays in `a` (AR
  orders). If missing, it is generated based on lengths of elements of
  `a`.

## Methods

Indexing with "\[" and "\[\[" works on slot `a`.

"\[" and "\[\[" can be use alternatively. Specifically, "\[\]" and
"\[\[\]\]" produce the same result, the complete list of AR
coefficients. Similarly, `[i,]`, `[i]` and `[[i]]` all return the i^th
element of the list, the array for i^th component. `[,j]` returns an
array with j^th lag autoregressive parameters for each component.

- \[:

  `signature(x = "raggedCoefV", i = "missing", j = "ANY", drop = "ANY")`:
  ...

- \[:

  `signature(x = "raggedCoefV", i = "missing", j = "numeric", drop = "ANY")`:
  ...

- \[:

  `signature(x = "raggedCoefV", i = "numeric", j = "missing", drop = "ANY")`:
  ...

- \[:

  `signature(x = "raggedCoefV", i = "numeric", j = "numeric", drop = "ANY")`:
  ...

- \[\[:

  `signature(x = "raggedCoefV", i = "missing", j = "ANY")`: ...

- \[\[:

  `signature(x = "raggedCoefV", i = "numeric", j = "ANY")`: ...

- initialize:

  `signature(.Object = "raggedCoefV")`: ...

- show:

  `signature(object = "raggedCoefV")`: ...

## Author

Davide Ravagli

## See also

class
`"`[`MixVAR`](https://geobosh.github.io/mixAR/reference/MixVAR-class.md)`"`

## Examples

``` r
showClass("raggedCoefV")
#> Class "raggedCoefV" [package "mixAR"]
#> 
#> Slots:
#>                       
#> Name:        a       p
#> Class:    list numeric

AR <- list()
AR[[1]] <- array(c(0.0973, -0.0499,  0.2927,  0.4256,  ## VAR(2;4)
                  -0.0429,  0.0229, -0.1515, -0.1795,
                  -0.0837, -0.1060, -0.1530,  0.1947,
                  -0.1690, -0.0903,  0.1959,  0.0955), dim=c(2,2,4))
AR[[2]] <- array(c(0.3243,  0.2648,  0.4956,  0.2870,  ## VAR(2;3)
                  -0.1488,  0.0454, -0.0593, -0.3629,
                   0.1314,  0.0274,  0.0637,  0.0485), dim=c(2,2,3))
                   
new("raggedCoefV", AR)
#> An object of class "raggedCoefV"
#> Number of rows: 2 
#> Number of series: 2 
#> Components' lengths: 4 3 
#> 
#> , , 1
#> 
#>                co_1           co_2            co_3            co_4       
#> Component_1  0.0973 0.2927 -0.0429 -0.1515 -0.0837 -0.1530 -0.1690 0.1959
#> Component_2 -0.0499 0.4256  0.0229 -0.1795 -0.1060  0.1947 -0.0903 0.0955
#> 
#> , , 2
#> 
#>               co_1           co_2           co_3        co_4 
#> Component_1 0.3243 0.4956 -0.1488 -0.0593 0.1314 0.0637      
#> Component_2 0.2648 0.2870  0.0454 -0.3629 0.0274 0.0485      
#> 
#> Component orders: 4 3 
#> 
new("raggedCoefV", a=AR, p=c(4,3))
#> An object of class "raggedCoefV"
#> Number of rows: 2 
#> Number of series: 2 
#> Components' lengths: 4 3 
#> 
#> , , 1
#> 
#>                co_1           co_2            co_3            co_4       
#> Component_1  0.0973 0.2927 -0.0429 -0.1515 -0.0837 -0.1530 -0.1690 0.1959
#> Component_2 -0.0499 0.4256  0.0229 -0.1795 -0.1060  0.1947 -0.0903 0.0955
#> 
#> , , 2
#> 
#>               co_1           co_2           co_3        co_4 
#> Component_1 0.3243 0.4956 -0.1488 -0.0593 0.1314 0.0637      
#> Component_2 0.2648 0.2870  0.0454 -0.3629 0.0274 0.0485      
#> 
#> Component orders: 4 3 
#> 
```
