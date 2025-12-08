# A test for 'unswitch'

A test for 'unswitch'.

## Usage

``` r
test_unswitch(models, true_model, allperm, ...)
```

## Arguments

- models:

  a list of MixAR models to process.

- true_model:

  the MixAR model used in the experiment.

- allperm:

  which permutations to try.

- ...:

  additional arguments to pass on to
  [`unswitch`](https://geobosh.github.io/mixAR/reference/unswitch.md).

## Details

This function permutes ('switches') randomly each model in `models` and
calls `unswitch` to 'unswitch' the switched models. It calculates
summary statistics for the three sets of models.

The summary statistics for the 'switched' models will often be
meaningless. Those for the 'unswitched' ones should be good.

The quality of the statistics for argument `models` itself depend on how
they are obtained and on the specifics of the model. They are good if,
as is often done is mixture experiments, estimation is initialised with
the true values of the parameters, since there is little scope for
switching in that case. If initial values are chosen randomly, then
there may still be little switching, largely dependent on the
probabilities of the components.

## Value

A list with the summaries for the 3 sets of models.

## Author

Georgi N. Boshnakov

## Examples

``` r
## N is small for to make the examples run instantly.
set.seed(1234)
aII10data <- mixARExperiment(exampleModels$WL_II, N = 5, raw = TRUE,
                 estargs = list(crit = 1e-4))
aII10 <- test_unswitch(aII10data$Raw, exampleModels$WL_II)
aII10
#> $mean
#>           original  u_original      messed    u_messed
#> order1  2.00000000  2.00000000  2.00000000  2.00000000
#> order2  2.00000000  2.00000000  2.00000000  2.00000000
#> order3  2.00000000  2.00000000  2.00000000  2.00000000
#> prob1   0.38829668  0.38829668  0.36331927  0.38829668
#> prob2   0.30901576  0.30901576  0.31668405  0.30901576
#> prob3   0.30268756  0.30268756  0.31999668  0.30268756
#> shift1  5.00289458  5.00289458  2.96024801  5.00289458
#> shift2 -0.01283370 -0.01283370 -0.82408181 -0.01283370
#> shift3 -5.01701045 -5.01701045 -2.16311576 -5.01701045
#> scale1  1.02185577  1.02185577  1.00185852  1.02185577
#> scale2  1.00450866  1.00450866  2.56104168  1.00450866
#> scale3  4.83623855  4.83623855  3.29970278  4.83623855
#> ar_11   0.89954799  0.89954799  0.24984373  0.89954799
#> ar_12  -0.60315169 -0.60315169 -0.36719379 -0.60315169
#> ar_21  -0.71928615 -0.71928615 -0.09321343 -0.71928615
#> ar_22   0.01317786  0.01317786  0.18427759  0.01317786
#> ar_31   0.01190528  0.01190528  0.03553682  0.01190528
#> ar_32   0.75452662  0.75452662  0.34746899  0.75452662
#> 
#> $sd
#>          original u_original     messed   u_messed
#> order1 2.00000000 2.00000000 2.00000000 2.00000000
#> order2 2.00000000 2.00000000 2.00000000 2.00000000
#> order3 2.00000000 2.00000000 2.00000000 2.00000000
#> prob1  0.05378105 0.05378105 0.07296112 0.05378105
#> prob2  0.04804369 0.04804369 0.05363909 0.04804369
#> prob3  0.02473113 0.02473113 0.04010385 0.02473113
#> shift1 0.17909526 0.17909526 2.77592816 0.17909526
#> shift2 0.12246861 0.12246861 3.92143063 0.12246861
#> shift3 0.50249596 0.50249596 4.72377077 0.50249596
#> scale1 0.10561787 0.10561787 0.12026810 0.10561787
#> scale2 0.09446427 0.09446427 2.09072731 0.09446427
#> scale3 0.30765087 0.30765087 2.11395212 0.30765087
#> ar_11  0.02816997 0.02816997 0.88649436 0.02816997
#> ar_12  0.01971022 0.01971022 0.32445588 0.01971022
#> ar_21  0.01197268 0.01197268 0.66828327 0.01197268
#> ar_22  0.03195056 0.03195056 0.55423397 0.03195056
#> ar_31  0.03203031 0.03203031 0.58085911 0.03203031
#> ar_32  0.04304680 0.04304680 0.62889207 0.04304680
#> 
#> $summaries
#> $summaries$original
#> $summaries$original$mean
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
#> $summaries$original$sd
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
#> 
#> $summaries$u_original
#> $summaries$u_original$mean
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
#> $summaries$u_original$sd
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
#> 
#> $summaries$messed
#> $summaries$messed$mean
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale    order ar_1        ar_2      
#> Comp_1 0.3633193  2.9602480 1.001859   2    0.24984373 -0.3671938
#> Comp_2 0.3166840 -0.8240818 2.561042   2   -0.09321343  0.1842776
#> Comp_3 0.3199967 -2.1631158 3.299703   2    0.03553682  0.3474690
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $summaries$messed$sd
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift    scale     order ar_1      ar_2     
#> Comp_1 0.07296112 2.775928 0.1202681   2   0.8864944 0.3244559
#> Comp_2 0.05363909 3.921431 2.0907273   2   0.6682833 0.5542340
#> Comp_3 0.04010385 4.723771 2.1139521   2   0.5808591 0.6288921
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $summaries$u_messed
#> $summaries$u_messed$mean
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
#> $summaries$u_messed$sd
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
#> 
#> 
#> $Raw
#> $Raw$original
#> $Raw$original[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2        
#> Comp_1 0.3665446  4.85396931 0.9767540   2    0.89144569 -0.600549202
#> Comp_2 0.2942500  0.05641133 0.9378551   2   -0.73756372  0.008297454
#> Comp_3 0.3392054 -5.00465861 5.2267672   2    0.02201604  0.779793289
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$original[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift        scale     order ar_1        ar_2       
#> Comp_1 0.4838707  4.994434593 0.9049234   2    0.88123929 -0.59333704
#> Comp_2 0.2362973 -0.004242097 0.9893591   2   -0.71844053  0.03999962
#> Comp_3 0.2798320 -5.324341677 4.4337937   2   -0.01675496  0.77532485
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$original[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2      
#> Comp_1 0.3720827  5.2076908 0.9687927   2    0.91560067 -0.6048382
#> Comp_2 0.3194904 -0.2079841 0.9077054   2   -0.70391125 -0.0338954
#> Comp_3 0.3084269 -4.6891375 4.6672338   2    0.05210517  0.6796020
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$original[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale    order ar_1        ar_2       
#> Comp_1 0.3550308  4.8017282 1.090907   2    0.93981275 -0.58214983
#> Comp_2 0.3655736  0.1170995 1.147201   2   -0.71804244  0.04584891
#> Comp_3 0.2793957 -5.6688162 4.828737   2   -0.02470486  0.78142635
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$original[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale    order ar_1        ar_2        
#> Comp_1 0.3639545  5.15665006 1.167902   2    0.86964157 -0.634884135
#> Comp_2 0.3294676 -0.02545309 1.040423   2   -0.71847281  0.005638708
#> Comp_3 0.3065779 -4.39809822 5.024661   2    0.02686497  0.756486635
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $Raw$u_original
#> $Raw$u_original[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2        
#> Comp_1 0.3665446  4.85396931 0.9767540   2    0.89144569 -0.600549202
#> Comp_2 0.2942500  0.05641133 0.9378551   2   -0.73756372  0.008297454
#> Comp_3 0.3392054 -5.00465861 5.2267672   2    0.02201604  0.779793289
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_original[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift        scale     order ar_1        ar_2       
#> Comp_1 0.4838707  4.994434593 0.9049234   2    0.88123929 -0.59333704
#> Comp_2 0.2362973 -0.004242097 0.9893591   2   -0.71844053  0.03999962
#> Comp_3 0.2798320 -5.324341677 4.4337937   2   -0.01675496  0.77532485
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_original[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2      
#> Comp_1 0.3720827  5.2076908 0.9687927   2    0.91560067 -0.6048382
#> Comp_2 0.3194904 -0.2079841 0.9077054   2   -0.70391125 -0.0338954
#> Comp_3 0.3084269 -4.6891375 4.6672338   2    0.05210517  0.6796020
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_original[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale    order ar_1        ar_2       
#> Comp_1 0.3550308  4.8017282 1.090907   2    0.93981275 -0.58214983
#> Comp_2 0.3655736  0.1170995 1.147201   2   -0.71804244  0.04584891
#> Comp_3 0.2793957 -5.6688162 4.828737   2   -0.02470486  0.78142635
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_original[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale    order ar_1        ar_2        
#> Comp_1 0.3639545  5.15665006 1.167902   2    0.86964157 -0.634884135
#> Comp_2 0.3294676 -0.02545309 1.040423   2   -0.71847281  0.005638708
#> Comp_3 0.3065779 -4.39809822 5.024661   2    0.02686497  0.756486635
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $Raw$messed
#> $Raw$messed[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2        
#> Comp_1 0.2942500  0.05641133 0.9378551   2   -0.73756372  0.008297454
#> Comp_2 0.3665446  4.85396931 0.9767540   2    0.89144569 -0.600549202
#> Comp_3 0.3392054 -5.00465861 5.2267672   2    0.02201604  0.779793289
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$messed[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift        scale     order ar_1        ar_2       
#> Comp_1 0.4838707  4.994434593 0.9049234   2    0.88123929 -0.59333704
#> Comp_2 0.2362973 -0.004242097 0.9893591   2   -0.71844053  0.03999962
#> Comp_3 0.2798320 -5.324341677 4.4337937   2   -0.01675496  0.77532485
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$messed[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2      
#> Comp_1 0.3194904 -0.2079841 0.9077054   2   -0.70391125 -0.0338954
#> Comp_2 0.3084269 -4.6891375 4.6672338   2    0.05210517  0.6796020
#> Comp_3 0.3720827  5.2076908 0.9687927   2    0.91560067 -0.6048382
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$messed[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale    order ar_1        ar_2       
#> Comp_1 0.3550308  4.8017282 1.090907   2    0.93981275 -0.58214983
#> Comp_2 0.3655736  0.1170995 1.147201   2   -0.71804244  0.04584891
#> Comp_3 0.2793957 -5.6688162 4.828737   2   -0.02470486  0.78142635
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$messed[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale    order ar_1        ar_2        
#> Comp_1 0.3639545  5.15665006 1.167902   2    0.86964157 -0.634884135
#> Comp_2 0.3065779 -4.39809822 5.024661   2    0.02686497  0.756486635
#> Comp_3 0.3294676 -0.02545309 1.040423   2   -0.71847281  0.005638708
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $Raw$u_messed
#> $Raw$u_messed[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2        
#> Comp_1 0.3665446  4.85396931 0.9767540   2    0.89144569 -0.600549202
#> Comp_2 0.2942500  0.05641133 0.9378551   2   -0.73756372  0.008297454
#> Comp_3 0.3392054 -5.00465861 5.2267672   2    0.02201604  0.779793289
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_messed[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift        scale     order ar_1        ar_2       
#> Comp_1 0.4838707  4.994434593 0.9049234   2    0.88123929 -0.59333704
#> Comp_2 0.2362973 -0.004242097 0.9893591   2   -0.71844053  0.03999962
#> Comp_3 0.2798320 -5.324341677 4.4337937   2   -0.01675496  0.77532485
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_messed[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2      
#> Comp_1 0.3720827  5.2076908 0.9687927   2    0.91560067 -0.6048382
#> Comp_2 0.3194904 -0.2079841 0.9077054   2   -0.70391125 -0.0338954
#> Comp_3 0.3084269 -4.6891375 4.6672338   2    0.05210517  0.6796020
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_messed[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale    order ar_1        ar_2       
#> Comp_1 0.3550308  4.8017282 1.090907   2    0.93981275 -0.58214983
#> Comp_2 0.3655736  0.1170995 1.147201   2   -0.71804244  0.04584891
#> Comp_3 0.2793957 -5.6688162 4.828737   2   -0.02470486  0.78142635
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_messed[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale    order ar_1        ar_2        
#> Comp_1 0.3639545  5.15665006 1.167902   2    0.86964157 -0.634884135
#> Comp_2 0.3294676 -0.02545309 1.040423   2   -0.71847281  0.005638708
#> Comp_3 0.3065779 -4.39809822 5.024661   2    0.02686497  0.756486635
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> 

aII10adata <- mixARExperiment(exampleModels$WL_II, N = 5, raw = TRUE,
                  simargs = list(n = 100), estargs = list(crit = 1e-4))
aII10a <- test_unswitch(aII10adata$Raw, exampleModels$WL_II)
aII10a
#> $mean
#>           original  u_original      messed    u_messed
#> order1  2.00000000  2.00000000  2.00000000  2.00000000
#> order2  2.00000000  2.00000000  2.00000000  2.00000000
#> order3  2.00000000  2.00000000  2.00000000  2.00000000
#> prob1   0.39000243  0.39000243  0.32024244  0.39000243
#> prob2   0.28708074  0.28708074  0.33503454  0.28708074
#> prob3   0.32291682  0.32291682  0.34472301  0.32291682
#> shift1  5.06529341  5.06529341  0.59525658  5.06529341
#> shift2  0.09187578  0.09187578 -0.06471025  0.09187578
#> shift3 -5.44155059 -5.44155059 -0.81492773 -5.44155059
#> scale1  0.95696922  0.95696922  1.43560482  0.95696922
#> scale2  0.77942512  0.77942512  2.39384993  0.77942512
#> scale3  4.71168578  4.71168578  2.61862535  4.71168578
#> ar_11   0.89468687  0.89468687  0.06955169  0.89468687
#> ar_12  -0.60333356 -0.60333356 -0.09022949 -0.60333356
#> ar_21  -0.71752101 -0.71752101  0.14859335 -0.71752101
#> ar_22   0.01672051  0.01672051  0.05028241  0.01672051
#> ar_31  -0.05676030 -0.05676030 -0.09773947 -0.05676030
#> ar_32   0.76647278  0.76647278  0.21980681  0.76647278
#> 
#> $sd
#>          original u_original     messed   u_messed
#> order1 2.00000000 2.00000000 2.00000000 2.00000000
#> order2 2.00000000 2.00000000 2.00000000 2.00000000
#> order3 2.00000000 2.00000000 2.00000000 2.00000000
#> prob1  0.05577215 0.05577215 0.06187516 0.05577215
#> prob2  0.03674940 0.03674940 0.03398549 0.03674940
#> prob3  0.03976739 0.03976739 0.08661957 0.03976739
#> shift1 0.18254361 0.18254361 4.73087227 0.18254361
#> shift2 0.22843254 0.22843254 5.47197555 0.22843254
#> shift3 1.41934674 1.41934674 4.19442938 1.41934674
#> scale1 0.11292854 0.11292854 1.23565245 0.11292854
#> scale2 0.15928874 0.15928874 2.15235042 0.15928874
#> scale3 0.82317699 0.82317699 2.42889801 0.82317699
#> ar_11  0.01172396 0.01172396 0.79702750 0.01172396
#> ar_12  0.01369262 0.01369262 0.55568922 0.01369262
#> ar_21  0.02605250 0.02605250 0.73105344 0.02605250
#> ar_22  0.01968793 0.01968793 0.66049080 0.01968793
#> ar_31  0.13594030 0.13594030 0.67243539 0.13594030
#> ar_32  0.06765766 0.06765766 0.61326789 0.06765766
#> 
#> $summaries
#> $summaries$original
#> $summaries$original$mean
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1       ar_2       
#> Comp_1 0.3900024  5.06529341 0.9569692   2    0.8946869 -0.60333356
#> Comp_2 0.2870807  0.09187578 0.7794251   2   -0.7175210  0.01672051
#> Comp_3 0.3229168 -5.44155059 4.7116858   2   -0.0567603  0.76647278
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $summaries$original$sd
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift     scale     order ar_1       ar_2      
#> Comp_1 0.05577215 0.1825436 0.1129285   2   0.01172396 0.01369262
#> Comp_2 0.03674940 0.2284325 0.1592887   2   0.02605250 0.01968793
#> Comp_3 0.03976739 1.4193467 0.8231770   2   0.13594030 0.06765766
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $summaries$u_original
#> $summaries$u_original$mean
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1       ar_2       
#> Comp_1 0.3900024  5.06529341 0.9569692   2    0.8946869 -0.60333356
#> Comp_2 0.2870807  0.09187578 0.7794251   2   -0.7175210  0.01672051
#> Comp_3 0.3229168 -5.44155059 4.7116858   2   -0.0567603  0.76647278
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $summaries$u_original$sd
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift     scale     order ar_1       ar_2      
#> Comp_1 0.05577215 0.1825436 0.1129285   2   0.01172396 0.01369262
#> Comp_2 0.03674940 0.2284325 0.1592887   2   0.02605250 0.01968793
#> Comp_3 0.03976739 1.4193467 0.8231770   2   0.13594030 0.06765766
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $summaries$messed
#> $summaries$messed$mean
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale    order ar_1        ar_2       
#> Comp_1 0.3202424  0.59525658 1.435605   2    0.06955169 -0.09022949
#> Comp_2 0.3350345 -0.06471025 2.393850   2    0.14859335  0.05028241
#> Comp_3 0.3447230 -0.81492773 2.618625   2   -0.09773947  0.21980681
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $summaries$messed$sd
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift    scale    order ar_1      ar_2     
#> Comp_1 0.06187516 4.730872 1.235652   2   0.7970275 0.5556892
#> Comp_2 0.03398549 5.471976 2.152350   2   0.7310534 0.6604908
#> Comp_3 0.08661957 4.194429 2.428898   2   0.6724354 0.6132679
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $summaries$u_messed
#> $summaries$u_messed$mean
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1       ar_2       
#> Comp_1 0.3900024  5.06529341 0.9569692   2    0.8946869 -0.60333356
#> Comp_2 0.2870807  0.09187578 0.7794251   2   -0.7175210  0.01672051
#> Comp_3 0.3229168 -5.44155059 4.7116858   2   -0.0567603  0.76647278
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $summaries$u_messed$sd
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift     scale     order ar_1       ar_2      
#> Comp_1 0.05577215 0.1825436 0.1129285   2   0.01172396 0.01369262
#> Comp_2 0.03674940 0.2284325 0.1592887   2   0.02605250 0.01968793
#> Comp_3 0.03976739 1.4193467 0.8231770   2   0.13594030 0.06765766
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> 
#> $Raw
#> $Raw$original
#> $Raw$original[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2       
#> Comp_1 0.3755441  4.9250821 0.9337884   2    0.88861157 -0.59840666
#> Comp_2 0.2747094  0.2482667 0.9039959   2   -0.71501178  0.01461083
#> Comp_3 0.3497465 -7.1592837 4.9309416   2   -0.01210352  0.74561766
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$original[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1       ar_2        
#> Comp_1 0.4829257  5.25235146 1.0820603   2    0.9001051 -0.601072316
#> Comp_2 0.2326677 -0.03276334 0.6269954   2   -0.7244085  0.009926353
#> Comp_3 0.2844067 -3.89112250 4.5578013   2   -0.2950199  0.677241028
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$original[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2       
#> Comp_1 0.3820311  4.8320621 0.9966950   2    0.89136117 -0.60037773
#> Comp_2 0.3315081  0.4098836 0.7082044   2   -0.74326638  0.04536445
#> Comp_3 0.2864608 -5.2500869 5.9093101   2    0.03696473  0.85438281
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$original[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2       
#> Comp_1 0.3776702  5.09986693 0.9938957   2    0.88143774 -0.59023444
#> Comp_2 0.3039395 -0.02626864 0.6665486   2   -0.73024993  0.02241541
#> Comp_3 0.3183903 -6.60835858 3.6291641   2   -0.03313735  0.74642509
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$original[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2        
#> Comp_1 0.3318412  5.2171044 0.7784067   2    0.91191878 -0.626576631
#> Comp_2 0.2925791 -0.1397394 0.9913812   2   -0.67466840 -0.008714492
#> Comp_3 0.3755798 -4.2989013 4.5312118   2    0.01949449  0.808697322
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $Raw$u_original
#> $Raw$u_original[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2       
#> Comp_1 0.3755441  4.9250821 0.9337884   2    0.88861157 -0.59840666
#> Comp_2 0.2747094  0.2482667 0.9039959   2   -0.71501178  0.01461083
#> Comp_3 0.3497465 -7.1592837 4.9309416   2   -0.01210352  0.74561766
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_original[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1       ar_2        
#> Comp_1 0.4829257  5.25235146 1.0820603   2    0.9001051 -0.601072316
#> Comp_2 0.2326677 -0.03276334 0.6269954   2   -0.7244085  0.009926353
#> Comp_3 0.2844067 -3.89112250 4.5578013   2   -0.2950199  0.677241028
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_original[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2       
#> Comp_1 0.3820311  4.8320621 0.9966950   2    0.89136117 -0.60037773
#> Comp_2 0.3315081  0.4098836 0.7082044   2   -0.74326638  0.04536445
#> Comp_3 0.2864608 -5.2500869 5.9093101   2    0.03696473  0.85438281
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_original[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2       
#> Comp_1 0.3776702  5.09986693 0.9938957   2    0.88143774 -0.59023444
#> Comp_2 0.3039395 -0.02626864 0.6665486   2   -0.73024993  0.02241541
#> Comp_3 0.3183903 -6.60835858 3.6291641   2   -0.03313735  0.74642509
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_original[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2        
#> Comp_1 0.3318412  5.2171044 0.7784067   2    0.91191878 -0.626576631
#> Comp_2 0.2925791 -0.1397394 0.9913812   2   -0.67466840 -0.008714492
#> Comp_3 0.3755798 -4.2989013 4.5312118   2    0.01949449  0.808697322
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $Raw$messed
#> $Raw$messed[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2       
#> Comp_1 0.3755441  4.9250821 0.9337884   2    0.88861157 -0.59840666
#> Comp_2 0.3497465 -7.1592837 4.9309416   2   -0.01210352  0.74561766
#> Comp_3 0.2747094  0.2482667 0.9039959   2   -0.71501178  0.01461083
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$messed[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1       ar_2        
#> Comp_1 0.2326677 -0.03276334 0.6269954   2   -0.7244085  0.009926353
#> Comp_2 0.2844067 -3.89112250 4.5578013   2   -0.2950199  0.677241028
#> Comp_3 0.4829257  5.25235146 1.0820603   2    0.9001051 -0.601072316
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$messed[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2       
#> Comp_1 0.3820311  4.8320621 0.9966950   2    0.89136117 -0.60037773
#> Comp_2 0.3315081  0.4098836 0.7082044   2   -0.74326638  0.04536445
#> Comp_3 0.2864608 -5.2500869 5.9093101   2    0.03696473  0.85438281
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$messed[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2       
#> Comp_1 0.3183903 -6.60835858 3.6291641   2   -0.03313735  0.74642509
#> Comp_2 0.3776702  5.09986693 0.9938957   2    0.88143774 -0.59023444
#> Comp_3 0.3039395 -0.02626864 0.6665486   2   -0.73024993  0.02241541
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$messed[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2        
#> Comp_1 0.2925791 -0.1397394 0.9913812   2   -0.67466840 -0.008714492
#> Comp_2 0.3318412  5.2171044 0.7784067   2    0.91191878 -0.626576631
#> Comp_3 0.3755798 -4.2989013 4.5312118   2    0.01949449  0.808697322
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> $Raw$u_messed
#> $Raw$u_messed[[1]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2       
#> Comp_1 0.3755441  4.9250821 0.9337884   2    0.88861157 -0.59840666
#> Comp_2 0.2747094  0.2482667 0.9039959   2   -0.71501178  0.01461083
#> Comp_3 0.3497465 -7.1592837 4.9309416   2   -0.01210352  0.74561766
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_messed[[2]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1       ar_2        
#> Comp_1 0.4829257  5.25235146 1.0820603   2    0.9001051 -0.601072316
#> Comp_2 0.2326677 -0.03276334 0.6269954   2   -0.7244085  0.009926353
#> Comp_3 0.2844067 -3.89112250 4.5578013   2   -0.2950199  0.677241028
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_messed[[3]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2       
#> Comp_1 0.3820311  4.8320621 0.9966950   2    0.89136117 -0.60037773
#> Comp_2 0.3315081  0.4098836 0.7082044   2   -0.74326638  0.04536445
#> Comp_3 0.2864608 -5.2500869 5.9093101   2    0.03696473  0.85438281
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_messed[[4]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift       scale     order ar_1        ar_2       
#> Comp_1 0.3776702  5.09986693 0.9938957   2    0.88143774 -0.59023444
#> Comp_2 0.3039395 -0.02626864 0.6665486   2   -0.73024993  0.02241541
#> Comp_3 0.3183903 -6.60835858 3.6291641   2   -0.03313735  0.74642509
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $Raw$u_messed[[5]]
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob      shift      scale     order ar_1        ar_2        
#> Comp_1 0.3318412  5.2171044 0.7784067   2    0.91191878 -0.626576631
#> Comp_2 0.2925791 -0.1397394 0.9913812   2   -0.67466840 -0.008714492
#> Comp_3 0.3755798 -4.2989013 4.5312118   2    0.01949449  0.808697322
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> 
#> 
```
