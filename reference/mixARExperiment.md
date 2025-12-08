# Simulation experiments with MixAR models

Perform simulation experiments with MixAR models.

## Usage

``` r
mixARExperiment(model, imodel = NULL, simargs = NULL, estargs = NULL, fix, ...)
```

## Arguments

- model:

  the underlying model, an object inheriting from "MixAR", see
  'Details'.

- imodel:

  initial model, an object inheriting from "MixAR", see 'Details'.

- simargs:

  additional arguments for the simulation function, a list, see
  'Details'.

- estargs:

  additional arguments for the estimation function, a list, see
  'Details'.

- fix:

  which arguments to keep fixed during estimation; currently if
  `fix = "shift"` the shift parameters are kept fixed, otherwise all
  parameters are estimated.

- ...:

  additional arguments to pass on to
  [`simuExperiment`](https://geobosh.github.io/mixAR/reference/simuExperiment.md).

## Details

`mixARExperiment` is a wrapper for `simuExperiment`.

The simulation function is `mixARsim` and the estimation function is
`fit_mixAR`. The simulation function gets `model` as first argument. The
first argument of the estimation function is `imodel`, if supplied, or
`model` otherwise. Some appropriate defaults for other arguments are
also supplied. Arguments `simargs` and `estargs` are needed only to
override defaults.

If `fix = "shift"`, the shifts components are not estimated and are kept
fixed. This may be useful when fitting the model to differenced series,
for example.

`mixARExperiment` sets `keep` and `summary_fun` to some defaults
(currently `mixAR:::.fsumA` for the latter).

## Value

A list with one or more elements, depending on the arguments `...`
passed on to
[`simuExperiment`](https://geobosh.github.io/mixAR/reference/simuExperiment.md).

- Summary:

  a summary of the experiment, by default sample means and standard
  deviations of the estimates. This is a list of MixAR models.

- Raw:

  A list of the estimated models.

## Author

Georgi N. Boshnakov

## Note

This is an initial version of this function, some of its arguments may
change. Additional elements may be made available in the returned value,
so refer to those by name, not with a numeric index.

## See also

[`simuExperiment`](https://geobosh.github.io/mixAR/reference/simuExperiment.md)

## Examples

``` r
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

set.seed(1234)
n = 20 # toy size
N = 5 # toy number of simulations
mixARExperiment(exampleModels$WL_II, N = N, estargs = list(crit = 1e-4))
#> $Summary
#> $Summary$mean
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale    order ar_1        ar_2       
#> Comp_1 0.3882967  5.0028946 1.021856   2    0.89954799 -0.60315169
#> Comp_2 0.3090158 -0.0128337 1.004509   2   -0.71928615  0.01317786
#> Comp_3 0.3026876 -5.0170104 4.836239   2    0.01190528  0.75452662
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Summary$sd
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift     scale      order ar_1       ar_2      
#> Comp_1 0.05378105 0.1790953 0.10561787   2   0.02816997 0.01971022
#> Comp_2 0.04804369 0.1224686 0.09446427   2   0.01197268 0.03195056
#> Comp_3 0.02473113 0.5024960 0.30765087   2   0.03203031 0.04304680
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Summary$meanUnswitched
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale    order ar_1        ar_2       
#> Comp_1 0.3026876 -5.0170104 4.836239   2    0.01190528  0.75452662
#> Comp_2 0.3090158 -0.0128337 1.004509   2   -0.71928615  0.01317786
#> Comp_3 0.3882967  5.0028946 1.021856   2    0.89954799 -0.60315169
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Summary$sdUnswitched
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift     scale      order ar_1       ar_2      
#> Comp_1 0.02473113 0.5024960 0.30765087   2   0.03203031 0.04304680
#> Comp_2 0.04804369 0.1224686 0.09446427   2   0.01197268 0.03195056
#> Comp_3 0.05378105 0.1790953 0.10561787   2   0.02816997 0.01971022
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
mixARExperiment(exampleModels$WL_II, N = N, raw = TRUE, estargs = list(crit = 1e-4))
#> $Summary
#> $Summary$mean
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2        
#> Comp_1 0.3991983  4.96425073 0.9785516   2    0.90412861 -0.601076230
#> Comp_2 0.2900017  0.03637379 0.9309377   2   -0.70624513  0.005645736
#> Comp_3 0.3108000 -5.38749282 4.9194780   2    0.01698498  0.830893484
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Summary$sd
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift     scale      order ar_1       ar_2      
#> Comp_1 0.03730998 0.1166968 0.11403847   2   0.00884525 0.03419575
#> Comp_2 0.03688219 0.1416702 0.06994315   2   0.01909112 0.01423444
#> Comp_3 0.04579375 0.5929002 0.39136839   2   0.05757455 0.06553143
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Summary$meanUnswitched
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2        
#> Comp_1 0.3108000 -5.38749282 4.9194780   2    0.01698498  0.830893484
#> Comp_2 0.2900017  0.03637379 0.9309377   2   -0.70624513  0.005645736
#> Comp_3 0.3991983  4.96425073 0.9785516   2    0.90412861 -0.601076230
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Summary$sdUnswitched
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift     scale      order ar_1       ar_2      
#> Comp_1 0.04579375 0.5929002 0.39136839   2   0.05757455 0.06553143
#> Comp_2 0.03688219 0.1416702 0.06994315   2   0.01909112 0.01423444
#> Comp_3 0.03730998 0.1166968 0.11403847   2   0.00884525 0.03419575
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $Raw
#> $Raw[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2       
#> Comp_1 0.4080660  5.06510556 0.9920789   2    0.90623811 -0.62284318
#> Comp_2 0.3160655 -0.03767196 0.8907312   2   -0.70256045 -0.00501553
#> Comp_3 0.2758685 -5.36236312 4.8943008   2    0.03608485  0.80581955
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale    order ar_1        ar_2        
#> Comp_1 0.4050840  5.0944308 1.122035   2    0.90916115 -0.594592459
#> Comp_2 0.2949899  0.2607166 1.033626   2   -0.69533994  0.007404252
#> Comp_3 0.2999261 -6.2292221 5.171451   2    0.09393146  0.912366961
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale    order ar_1        ar_2         
#> Comp_1 0.4452899  4.8965489 0.910946   2    0.90080237 -0.5626733638
#> Comp_2 0.2367076 -0.0150589 0.888185   2   -0.70188135  0.0005465691
#> Comp_3 0.3180025 -4.6585352 5.185955   2    0.03536553  0.8193486283
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2        
#> Comp_1 0.3959729  4.95125605 0.8273562   2    0.89071347 -0.577854481
#> Comp_2 0.3298400  0.07896581 0.9729592   2   -0.69198674 -0.004230706
#> Comp_3 0.2741871 -5.05927494 5.0946720   2   -0.04690491  0.874691463
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1       ar_2      
#> Comp_1 0.3415786  4.8139123 1.0403412   2    0.9137280 -0.6474177
#> Comp_2 0.2724055 -0.1050826 0.8691871   2   -0.7394572  0.0295241
#> Comp_3 0.3860159 -5.6280687 4.2510102   2   -0.0335520  0.7422408
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
mixARExperiment(exampleModels$WL_II, N = N, raw = TRUE,
    simargs = list(n = n), estargs = list(crit = 1e-4) )
#> $Summary
#> $Summary$mean
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2       
#> Comp_1 0.3691747  4.94049495 0.5424302   2    0.87460652 -0.59833037
#> Comp_2 0.3002207 -0.02115004 0.2831587   2   -0.69420027 -0.01061726
#> Comp_3 0.3306046 -7.10692678 1.6750352   2    0.02235797  0.88243256
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Summary$sd
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift     scale     order ar_1       ar_2      
#> Comp_1 0.0870959 0.5538342 0.1621501   2   0.06056077 0.05125050
#> Comp_2 0.1035992 1.3167560 0.2281381   2   0.07110076 0.02435567
#> Comp_3 0.1563916 6.1293510 2.0237210   2   0.48342231 0.25101835
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Summary$meanUnswitched
#> [1] NA
#> 
#> $Summary$sdUnswitched
#> [1] NA
#> 
#> 
#> $Raw
#> $Raw[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale        order ar_1       ar_2        
#> Comp_1 0.3318619  4.7445112 6.714888e-01   2    0.7896087 -0.637257969
#> Comp_2 0.1666667  0.4144435 5.024296e-15   2   -0.6567292 -0.005871863
#> Comp_3 0.5014715 -7.6204700 2.536809e+00   2   -0.5898504  0.549462614
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift     scale     order ar_1       ar_2       
#> Comp_1 0.2718974 5.7142529 0.4723228   2    0.8872741 -0.62303718
#> Comp_2 0.3547748 0.5607002 0.5131166   2   -0.7095910  0.02428491
#> Comp_3 0.3733278 0.6456413 4.7808811   2    0.2683984  0.83179455
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale        order ar_1       ar_2       
#> Comp_1 0.4118738   5.0325194 7.109825e-01   2    0.9122396 -0.53648642
#> Comp_2 0.4214595   0.3539875 5.124229e-01   2   -0.6433690 -0.02979583
#> Comp_3 0.1666667 -16.4566060 7.470737e-15   2    0.6800289  1.12554827
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale        order ar_1       ar_2       
#> Comp_1 0.4971034  5.0281088 5.497379e-01   2    0.9432734 -0.54971515
#> Comp_2 0.3362300  0.9099383 2.545710e-01   2   -0.6489908 -0.03750718
#> Comp_3 0.1666667 -6.4161578 4.381280e-15   2   -0.2339274  1.13927098
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift     scale     order ar_1        ar_2        
#> Comp_1 0.3331372  4.183083 0.3076192   2    0.84063684 -0.645155144
#> Comp_2 0.2219724 -2.344820 0.1356830   2   -0.81232139 -0.004196316
#> Comp_3 0.4448904 -5.687041 1.0574859   2   -0.01285965  0.766086370
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
```
