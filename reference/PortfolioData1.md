# Closing prices of four stocks

Closing prices of four stocks.

## Usage

``` r
data("PortfolioData1")
```

## Format

A data frame with 867 observations on the following 4 variables.

- `DELL`:

  numeric, Dell Technologies Inc.

- `MSFT`:

  numeric, Microsoft Corporation.

- `INTC`:

  numeric, Intel Corporation.

- `IBM`:

  numericr, International Business Machine Corporation.

## Details

Time series of daily adjusted close prices of the above stocks from 2
January 2016 to 29 January 2020 (867 observations).

## Source

`https://finance.yahoo.com/`

## Examples

``` r
data(PortfolioData1)
dim(PortfolioData1)
#> [1] 867   4
head(PortfolioData1)
#>       DELL     MSFT     INTC      IBM
#> 1 23.80953 54.17237 32.16791 139.7352
#> 2 23.67110 54.21002 32.12197 140.5364
#> 3 24.08638 54.22884 32.36999 139.3867
#> 4 24.16944 54.27589 32.48022 139.3519
#> 5 24.91694 54.48295 32.51696 139.5784
#> 6 25.24917 54.53941 32.28732 138.5245
```
