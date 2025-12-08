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
#> <bytecode: 0x561e21dbe118>
#> <environment: 0x561e12091458>
#> 
#> $Fscore
#> function (x) 
#> -x
#> <bytecode: 0x561e21dc18b8>
#> <environment: 0x561e12091458>
#> 
#> $xFscore
#> function (x) 
#> -x^2
#> <bytecode: 0x561e21dc1308>
#> <environment: 0x561e12091458>
#> 
#> $Parscore
#> function () 
#> 0
#> <bytecode: 0x561e21dc0ce8>
#> <environment: 0x561e12091458>
#> 
#> $get_param
#> function () 
#> numeric()
#> <bytecode: 0x561e21dc07e0>
#> <environment: 0x561e12091458>
#> 
#> $set_param
#> function () 
#> stop("Standard normal has no parameters to set.")
#> <bytecode: 0x561e21dc02a0>
#> <environment: 0x561e12091458>
#> 
#> $any_param
#> function () 
#> param_flag
#> <bytecode: 0x561e21dbfd28>
#> <environment: 0x561e12091458>
#> 
#> $show
#> function () 
#> "Standard normal distribution"
#> <bytecode: 0x561e21dc3650>
#> <environment: 0x561e12091458>
#> 
dist_norm$pdf
#> [1] "dnorm"
dist_norm$cdf
#> [1] "pnorm"
```
