# Show differences between two models

Show differences between two MixAR models in a way that enables quick
comparison between them. This is a generic function, package mixAR
defines methods for `MixAR` models.

## Usage

``` r
show_diff(model1, model2)
```

## Arguments

- model1,model2:

  the MixAR models to be compared.

## Details

`show_diff()` is a generic function with dispatch on both arguments.

`show_diff()` prints the differences between two models in convenient
form for comparison. The methods for MixAR models allow to see
differences between similar models at a glance.

## Value

The function is called for the side effect of printing the differences
between the two models and has no useful return value.

## Author

Georgi N. Boshnakov

## Methods

- `signature(model1 = "MixAR", model2 = "MixAR")`:

- `signature(model1 = "MixARGaussian", model2 = "MixARgen")`:

- `signature(model1 = "MixARgen", model2 = "MixARGaussian")`:

- `signature(model1 = "MixARgen", model2 = "MixARgen")`:

## Examples

``` r
## the examples reveal that the models below
##        differ only in the noise distributions
show_diff(exampleModels$WL_Ct_3, exampleModels$WL_Bt_1)
#>        prob shift scale order ar_1 ar_2 ar_3
#> Comp_1   0    0     0     2     0    0      
#> Comp_2   0    0     0     1     0           
#> Comp_3   0    0     0     3     0    0    0 
#> 
#> Distributions of the error components:
#> 
#>  Model 1
#>   All components: Standard normal distribution
#> 
#>  Model 2
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 6 df
#>  Component 3: Student t with 10 df
show_diff(exampleModels$WL_Bt_1, exampleModels$WL_Ct_3)
#>        prob shift scale order ar_1 ar_2 ar_3
#> Comp_1   0    0     0     2     0    0      
#> Comp_2   0    0     0     1     0           
#> Comp_3   0    0     0     3     0    0    0 
#> 
#> Distributions of the error components:
#> 
#>  Model 1
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 6 df
#>  Component 3: Student t with 10 df
#> 
#>  Model 2
#>   All components: Standard normal distribution
show_diff(exampleModels$WL_Ct_2, exampleModels$WL_Bt_3)
#>        prob shift scale order ar_1 ar_2 ar_3
#> Comp_1   0    0     0     2     0    0      
#> Comp_2   0    0     0     1     0           
#> Comp_3   0    0     0     3     0    0    0 
#> 
#> Distributions of the error components:
#> 
#>  Model 1:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 7 df
#>  Component 3: Standard normal distribution
#> 
#>  Model 2:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 4 df
#>  Component 3: Student t with 10 df
```
