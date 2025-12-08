# Functions for the standard normal distribution

The noise distributions are specified by a list of functions for the
density, quantiles, etc. This object demonstrates this for the standard
normal distribution.

## Usage

``` r
dist_norm
```

## Format

This is a list of functions or names of functions for calculations
related to the standard normal distribution. Currently it has elements
with the following names: "pdf", "cdf", "rand", "logpdf", "Fscore",
"xFscore", "Parscore", "get_param", "set_param", "any_param", "show" .

## Details

`dist_norm` may be used to specify the noise distribution for `MixAR`
models. It can be used as a template if other distributions are needed,
see also `fdist_stdnorm`.

## See also

[`fdist_stdnorm`](https://geobosh.github.io/mixAR/reference/fnoise.md)

## Examples

``` r
dist_norm
#> $pdf
#> [1] "dnorm"
#> 
#> $cdf
#> [1] "pnorm"
#> 
#> $rand
#> [1] "rnorm"
#> 
#> $logpdf
#> function (x) 
#> log(dnorm(x))
#> <bytecode: 0x55d854a2cb50>
#> <environment: 0x55d83f255c10>
#> 
#> $Fscore
#> function (x) 
#> -x
#> <bytecode: 0x55d854a2c4c0>
#> <environment: 0x55d83f255c10>
#> 
#> $xFscore
#> function (x) 
#> -x^2
#> <bytecode: 0x55d854a31750>
#> <environment: 0x55d83f255c10>
#> 
#> $Parscore
#> function () 
#> 0
#> <bytecode: 0x55d854a31130>
#> <environment: 0x55d83f255c10>
#> 
#> $get_param
#> function () 
#> numeric()
#> <bytecode: 0x55d854a30c28>
#> <environment: 0x55d83f255c10>
#> 
#> $set_param
#> function () 
#> stop("Standard normal has no parameters to set.")
#> <bytecode: 0x55d854a306e8>
#> <environment: 0x55d83f255c10>
#> 
#> $any_param
#> function () 
#> param_flag
#> <bytecode: 0x55d854a30170>
#> <environment: 0x55d83f255c10>
#> 
#> $show
#> function () 
#> "Standard normal distribution"
#> <bytecode: 0x55d854a2fc68>
#> <environment: 0x55d83f255c10>
#> 
dist_norm$pdf
#> [1] "dnorm"
dist_norm$cdf
#> [1] "pnorm"
```
