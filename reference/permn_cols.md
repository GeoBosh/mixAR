# All permutations of the columns of a matrix

All permutations of the columns of a matrix

## Usage

``` r
permn_cols(m)
```

## Arguments

- m:

  a matrix

## Details

This function is a wrapper for `permn` from package \`combinat'.

## Value

a list with one element for each permutation of the columns. Each
element of the list is an unnamed list with two components:

1.  the permutation, a vector of positive integers,

2.  a matrix obtained by permuting the columns of `m`.

## Author

Georgi N. Boshnakov

## Examples

``` r
m <- matrix(c(11:14,21:24,31:34), ncol=3)
pm <- permn_cols(m)
pm[[2]]
#> [[1]]
#> [1] 1 3 2
#> 
#> [[2]]
#>      [,1] [,2] [,3]
#> [1,]   11   31   21
#> [2,]   12   32   22
#> [3,]   13   33   23
#> [4,]   14   34   24
#> 
```
