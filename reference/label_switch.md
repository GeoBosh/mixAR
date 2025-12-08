# A posteriori relabelling of a Markov chain

Takes the output from a MCMC simulation of parameters of a mixture, and
detects whether labels switch has occured while sampling, using the
method by Celeux (2000).

## Usage

``` r
label_switch(x, m)
```

## Arguments

- x:

  output from an MCMC sampling of a mixture. A `matrix`, each column
  corresponds to one component of the mixture.

- m:

  the number of observations in the sample that will be used to
  initialise the algorithm. `m~100` is recommended.

## Details

Function can be directly executed when `x` is one of `mix_weights`,
`scale`, `precision`, `shift` or `mu` from `bayes_mixAR` output.
`ARcoeff` cannot be input as it is, but element from the list may be
used.

## Value

A list of 2:

- x:

  The input matrix, with adjusted labels

- true_perm:

  The "true" permutation at each iteration.

## References

Celeux G (2000). *Bayesian Inference of Mixture: The Label Switching
Problem.*. Payne R., Green P. (eds) COMPSTAT. Physica, Heidelberg.

## Author

Davide Ravagli

## Note

There is no absolute choice on what `x` should be to obtain the "true"
permutation at any given point. User is subject to make the most
suitable choice, given output of their MCMC.

## See also

[`bayes_mixAR`](https://geobosh.github.io/mixAR/reference/bayes_mixAR.md)

## Examples

``` r
model <- new("MixARGaussian",
             prob   = exampleModels$WL_At@prob,      # c(0.5, 0.5)
             scale  = exampleModels$WL_At@scale,     # c(1, 2)        
             arcoef = exampleModels$WL_At@arcoef@a ) # list(-0.5, 1.1)

y <- mixAR_sim(model, n = 300, init = rep(0, which.max(model@order)))

## just examples, use larger numbers in practice
nsim <- 30   # 200
burnin <- 10  # 100
x <- bayes_mixAR(y, model, fix_shift = FALSE, tau = c(.15, .25),
              nsim = nsim, burnin = burnin)

label_switch(x$mix_weights, m = 5)
#> $x
#>            [,1]      [,2]
#>  [1,] 0.5370141 0.4629859
#>  [2,] 0.5585030 0.4414970
#>  [3,] 0.5655156 0.4344844
#>  [4,] 0.5025084 0.4974916
#>  [5,] 0.5033404 0.4966596
#>  [6,] 0.5451949 0.4548051
#>  [7,] 0.5332204 0.4667796
#>  [8,] 0.5077244 0.4922756
#>  [9,] 0.5596734 0.4403266
#> [10,] 0.6183201 0.3816799
#> [11,] 0.5155798 0.4844202
#> [12,] 0.5543813 0.4456187
#> [13,] 0.5037202 0.4962798
#> [14,] 0.5281592 0.4718408
#> [15,] 0.5438278 0.4561722
#> [16,] 0.5156774 0.4843226
#> [17,] 0.5332348 0.4667652
#> [18,] 0.5088264 0.4911736
#> [19,] 0.5397674 0.4602326
#> [20,] 0.6130280 0.3869720
#> 
#> $true_perm
#>       [,1] [,2]
#>  [1,]    1    2
#>  [2,]    1    2
#>  [3,]    1    2
#>  [4,]    1    2
#>  [5,]    1    2
#>  [6,]    1    2
#>  [7,]    1    2
#>  [8,]    2    1
#>  [9,]    2    1
#> [10,]    2    1
#> [11,]    2    1
#> [12,]    2    1
#> [13,]    1    2
#> [14,]    1    2
#> [15,]    1    2
#> 
```
