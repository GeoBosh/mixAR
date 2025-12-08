# Class `"MixAR"` — mixture autoregressive models

Mixture autoregressive models

## Objects from the Class

A virtual Class: no objects can be created from it.

Derived classes add distribution properties, e.g. use class
`"`[`MixARGaussian`](https://geobosh.github.io/mixAR/reference/MixARGaussian-class.md)`"`
for MixAR models with Gaussian error components.

## Slots

- `prob`::

  the mixing probabilities, `"numeric"`.

- `order`::

  the AR orders, `"numeric"`.

- `shift`::

  intercept terms, `"numeric"`.

- `scale`::

  scaling factor, `"numeric"`.

- `arcoef`::

  autoregressive coefficients, an object from class `"raggedCoef"`
  containing one row for each mixture component.

## Methods

- fit_mixAR:

  `signature(x = "ANY", model = "MixAR", init = "list")`: ...

- fit_mixAR:

  `signature(x = "ANY", model = "MixAR", init = "missing")`: ...

- fit_mixAR:

  `signature(x = "ANY", model = "MixAR", init = "MixAR")`: ...

- fit_mixAR:

  `signature(x = "ANY", model = "MixAR", init = "numeric")`: ...

- fit_mixAR:

  `signature(x = "ANY", model = "MixARGaussian", init = "MixAR")`: ...

- get_edist:

  `signature(model = "MixAR")`: ...

- initialize:

  `signature(.Object = "MixAR")`: ...

- lik_params:

  `signature(model = "MixAR")`: ...

- make_fcond_lik:

  `signature(model = "MixAR", ts = "numeric")`: ...

- mix_ek:

  `signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing", scale = "missing")`:
  ...

- mix_ek:

  `signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing", scale = "logical")`:
  ...

- mix_ek:

  `signature(model = "MixAR", x = "numeric", index = "missing", xcond = "numeric", scale = "missing")`:
  ...

- mix_ek:

  `signature(model = "MixAR", x = "numeric", index = "missing", xcond = "numeric", scale = "logical")`:
  ...

- mix_hatk:

  `signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing")`:
  ...

- mix_ncomp:

  `signature(x = "MixAR")`: ...

- mixAR:

  `signature(template = "MixAR")`: ...

- noise_dist:

  `signature(model = "MixAR")`: ...

- noise_params:

  `signature(model = "MixAR")`: ...

- noise_rand:

  `signature(model = "MixAR")`: ...

- parameters:

  `signature(model = "MixAR")`: ...

- row_lengths:

  `signature(x = "MixAR")`: ...

## Author

Georgi N. Boshnakov

## See also

[`mixAR`](https://geobosh.github.io/mixAR/reference/mixAR-methods.md),
classes
`"`[`MixARGaussian`](https://geobosh.github.io/mixAR/reference/MixARGaussian-class.md)`"`,
`"`[`MixARgen`](https://geobosh.github.io/mixAR/reference/MixARgen-class.md)`"`

## Examples

``` r
## some models from subclasses of (virtual) class "MixAR"
names(exampleModels)
#>  [1] "WL_ibm"     "WL_A"       "WL_B"       "WL_I"       "WL_II"     
#>  [6] "WL_ibm_gen" "WL_ibm_t3v" "WL_ibm_tf"  "WL_At"      "WL_Bt_1"   
#> [11] "WL_Bt_2"    "WL_Bt_3"    "WL_Ct_1"    "WL_Ct_2"    "WL_Ct_3"   
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

## modify an existing model, here change the mixture weights
mixAR(exampleModels$WL_A, coef = list((prob = c(0.4, 0.6))))
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob shift scale order ar_1
#> Comp_1 0.5    0     5     1   0.5 
#> Comp_2 0.5    0     1     1   1.1 
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
```
