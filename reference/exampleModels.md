# MixAR models for examples and testing

MixAR models for examples and testing.

## Usage

``` r
exampleModels
```

## Details

Coefficients of models from the examples in Wong and Li (2000) .
Variations on these with different noise distributions are used
throughout the examples in mixAR. The models are from classes inheriting
from class `"MixAR"`.

`exampleModels` is a list with the following components:

[TABLE]

Each component is a MixAR model, i.e. an object inheriting from class
`"MixAR"`.

## Source

Wong CS, Li WK (2000). “On a mixture autoregressive model.” *J. R. Stat.
Soc., Ser. B, Stat. Methodol.*, **62**(1), 95-115.

## Examples

``` r
## use these instead of moWL, moWL_A, moWL_B, etc.
exampleModels$WL_ibm
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob   shift scale   order ar_1   ar_2   
#> Comp_1 0.5439   0    4.8227   2   0.6792  0.3208
#> Comp_2 0.4176   0    6.0082   2   1.6711 -0.6711
#> Comp_3 0.0385   0   18.1716   1   1.0000        
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 

exampleModels$WL_A
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob shift scale order ar_1
#> Comp_1 0.5    0     5     1   0.5 
#> Comp_2 0.5    0     1     1   1.1 
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
exampleModels$WL_B
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob shift scale order ar_1
#> Comp_1 0.75   0     5     1   0.5 
#> Comp_2 0.25   0     1     1   1.4 
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
# what is the difference between A and B?
show_diff(exampleModels$WL_A, exampleModels$WL_B)
#>        prob  shift scale order ar_1
#> Comp_1 -0.25   0     0     1    0.0
#> Comp_2  0.25   0     0     1   -0.3

exampleModels$WL_I
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2  ar_3
#> Comp_1 0.4    0     1     2    0.9 -0.60     
#> Comp_2 0.3    0     1     1   -0.5           
#> Comp_3 0.3   -5     5     3    1.5 -0.74 0.12
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
exampleModels$WL_II
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2
#> Comp_1 0.4    5     1     2    0.9 -0.6
#> Comp_2 0.3    0     1     2   -0.7  0.0
#> Comp_3 0.3   -5     5     2    0.0  0.8
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#show_diff(exampleModels$WL_I, exampleModels$WL_II)

exampleModels$WL_ibm_gen
#> An object of class "MixARgen"
#> Number of components: 3 
#>        prob   shift scale   order ar_1   ar_2   
#> Comp_1 0.5439   0    4.8227   2   0.6792  0.3208
#> Comp_2 0.4176   0    6.0082   2   1.6711 -0.6711
#> Comp_3 0.0385   0   18.1716   1   1.0000        
#> 
#> Distributions of the error components:
#>  Component 1: Standard normal distribution
#> 
exampleModels$WL_ibm_t3v
#> An object of class "MixARgen"
#> Number of components: 3 
#>        prob   shift scale   order ar_1   ar_2   
#> Comp_1 0.5439   0    4.8227   2   0.6792  0.3208
#> Comp_2 0.4176   0    6.0082   2   1.6711 -0.6711
#> Comp_3 0.0385   0   18.1716   1   1.0000        
#> 
#> Distributions of the error components:
#>  Component 1: Student t with 3 df
#> 
exampleModels$WL_ibm_tf
#> An object of class "MixARgen"
#> Number of components: 3 
#>        prob   shift scale   order ar_1   ar_2   
#> Comp_1 0.5439   0    4.8227   2   0.6792  0.3208
#> Comp_2 0.4176   0    6.0082   2   1.6711 -0.6711
#> Comp_3 0.0385   0   18.1716   1   1.0000        
#> 
#> Distributions of the error components:
#>  Component 1: Student t with 20 df
#>  Component 2: Student t with 30 df
#>  Component 3: Student t with 40 df
#> 
#show_diff(exampleModels$WL_ibm_gen, exampleModels$WL_ibm_t3v)

exampleModels$WL_At
#> An object of class "MixARgen"
#> Number of components: 2 
#>        prob shift scale order ar_1
#> Comp_1 0.5    0     1     1   -0.5
#> Comp_2 0.5    0     2     1    1.1
#> 
#> Distributions of the error components:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 8 df
#> 

exampleModels$WL_Bt_1
#> An object of class "MixARgen"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2  ar_3
#> Comp_1 0.3    5   2.0     2    0.5  0.24     
#> Comp_2 0.3   -5   1.0     1   -0.9           
#> Comp_3 0.4    0   0.5     3    1.5 -0.74 0.12
#> 
#> Distributions of the error components:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 6 df
#>  Component 3: Student t with 10 df
#> 
exampleModels$WL_Bt_2
#> An object of class "MixARgen"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2  ar_3
#> Comp_1 0.3    5   2.0     2    0.5  0.24     
#> Comp_2 0.3   -5   1.0     1   -0.9           
#> Comp_3 0.4    0   0.5     3    1.5 -0.74 0.12
#> 
#> Distributions of the error components:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 4 df
#>  Component 3: Student t with 10 df
#> 
exampleModels$WL_Bt_3
#> An object of class "MixARgen"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2  ar_3
#> Comp_1 0.3    5   2.0     2    0.5  0.24     
#> Comp_2 0.3   -5   1.0     1   -0.9           
#> Comp_3 0.4    0   0.5     3    1.5 -0.74 0.12
#> 
#> Distributions of the error components:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 4 df
#>  Component 3: Student t with 10 df
#> 
## what is different between Bt_2 and Bt_1? (df of component 2)
show_diff(exampleModels$WL_Bt_2, exampleModels$WL_Bt_1)
#>        prob shift scale order ar_1 ar_2 ar_3
#> Comp_1   0    0     0     2     0    0      
#> Comp_2   0    0     0     1     0           
#> Comp_3   0    0     0     3     0    0    0 
#> 
#> Distributions of the error components:
#> 
#>  Model 1:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 4 df
#>  Component 3: Student t with 10 df
#> 
#>  Model 2:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 6 df
#>  Component 3: Student t with 10 df

exampleModels$WL_Ct_1
#> An object of class "MixARgen"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2  ar_3
#> Comp_1 0.3    5   2.0     2    0.5  0.24     
#> Comp_2 0.3   -5   1.0     1   -0.9           
#> Comp_3 0.4    0   0.5     3    1.5 -0.74 0.12
#> 
#> Distributions of the error components:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 7 df
#>  Component 3: Standard normal distribution
#> 
exampleModels$WL_Ct_2
#> An object of class "MixARgen"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2  ar_3
#> Comp_1 0.3    5   2.0     2    0.5  0.24     
#> Comp_2 0.3   -5   1.0     1   -0.9           
#> Comp_3 0.4    0   0.5     3    1.5 -0.74 0.12
#> 
#> Distributions of the error components:
#>  Component 1: Student t with 4 df
#>  Component 2: Student t with 7 df
#>  Component 3: Standard normal distribution
#> 
exampleModels$WL_Ct_3
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob shift scale order ar_1 ar_2  ar_3
#> Comp_1 0.3    5   2.0     2    0.5  0.24     
#> Comp_2 0.3   -5   1.0     1   -0.9           
#> Comp_3 0.4    0   0.5     3    1.5 -0.74 0.12
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 

## The models were created with something like:
moWLprob <- c(0.5439,0.4176,0.0385)
moWLsigma <- c(4.8227,6.0082,18.1716)
moWLar <- list(c(0.6792,0.3208), c(1.6711,-0.6711), 1)

moWL <- new("MixARGaussian", prob = moWLprob, scale = moWLsigma,
            arcoef = moWLar)
moWLgen <- new("MixARgen", prob = moWLprob, scale = moWLsigma,
               arcoef = moWLar, dist = list(dist_norm))
## clean up a bit
rm(moWLprob, moWLsigma, moWLar, moWL, moWLgen)
```
