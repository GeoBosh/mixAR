# Set or extract the parameters of MixAR objects

Set or extract the parameters of MixAR objects.

## Usage

``` r
parameters(model, namesflag = FALSE, drop = character(0))

parameters(model) <- value
# S4 method for class 'MixAR'
parameters(model) <- value
# S4 method for class 'ANY'
parameters(model) <- value
```

## Arguments

- model:

  a model.

- namesflag:

  if TRUE, generate names.

- drop:

  names of parameters not to include in the returned value, a character
  vector. The default is to return all parameters, see Details.

- value:

  values of the parameters, numeric.

## Details

This is a generic function. The dispatch is on argument `model`. The
default calls `coef`.

`parameters` extracts the parameters of a MixAR object. It returns a
numeric vector. If `namesflag` is `TRUE` the returned vector is named,
so that the parameters can be referred to by names. Argument `drop` is a
character vector giving names of parameters not to be included in the
returned value.

This function can be useful for setting parameters from optimisation
routines.

`set_parameters` is deprecated, use `parameters(model) <- value`
instead.

## Value

a vector of parameters, maybe with names.

## Methods

- `signature(model = "ANY")`:

- `signature(model = "MixAR")`:

## Examples

``` r
parameters(exampleModels$WL_ibm)
#>  [1]  2.0000  2.0000  1.0000  0.5439  0.4176  0.0385  0.0000  0.0000  0.0000
#> [10]  4.8227  6.0082 18.1716  0.6792  0.3208  1.6711 -0.6711  1.0000
parameters(exampleModels$WL_ibm, namesflag = TRUE)
#>  order1  order2  order3   prob1   prob2   prob3  shift1  shift2  shift3  scale1 
#>  2.0000  2.0000  1.0000  0.5439  0.4176  0.0385  0.0000  0.0000  0.0000  4.8227 
#>  scale2  scale3   ar_11   ar_12   ar_21   ar_22   ar_31 
#>  6.0082 18.1716  0.6792  0.3208  1.6711 -0.6711  1.0000 
## drop orders
parameters(exampleModels$WL_ibm, namesflag = TRUE, drop = "order")
#>   prob1   prob2   prob3  shift1  shift2  shift3  scale1  scale2  scale3   ar_11 
#>  0.5439  0.4176  0.0385  0.0000  0.0000  0.0000  4.8227  6.0082 18.1716  0.6792 
#>   ar_12   ar_21   ar_22   ar_31 
#>  0.3208  1.6711 -0.6711  1.0000 
## drop orders and mixing weights
parameters(exampleModels$WL_ibm, namesflag = TRUE, drop = c("order", "prob"))
#>  shift1  shift2  shift3  scale1  scale2  scale3   ar_11   ar_12   ar_21   ar_22 
#>  0.0000  0.0000  0.0000  4.8227  6.0082 18.1716  0.6792  0.3208  1.6711 -0.6711 
#>   ar_31 
#>  1.0000 

parameters(exampleModels$WL_I,  namesflag = TRUE)
#> order1 order2 order3  prob1  prob2  prob3 shift1 shift2 shift3 scale1 scale2 
#>   2.00   1.00   3.00   0.40   0.30   0.30   0.00   0.00  -5.00   1.00   1.00 
#> scale3  ar_11  ar_12  ar_21  ar_31  ar_32  ar_33 
#>   5.00   0.90  -0.60  -0.50   1.50  -0.74   0.12 
parameters(exampleModels$WL_II, namesflag = TRUE)
#> order1 order2 order3  prob1  prob2  prob3 shift1 shift2 shift3 scale1 scale2 
#>    2.0    2.0    2.0    0.4    0.3    0.3    5.0    0.0   -5.0    1.0    1.0 
#> scale3  ar_11  ar_12  ar_21  ar_22  ar_31  ar_32 
#>    5.0    0.9   -0.6   -0.7    0.0    0.0    0.8 
```
