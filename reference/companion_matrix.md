# Create a companion matrix from a vector

Create a companion matrix from a vector.

## Usage

``` r
companion_matrix(v, ncol = length(v), nrow = ncol)
```

## Arguments

- v:

  the first row of the matrix, a numeric vector or a matrix with one
  row.

- ncol:

  number of columns, a number.

- nrow:

  number of rows, a number.

## Details

With the default settings, a square \\m\times m\\ matrix is returned,
where \\m\\ is the length of `v`. If `ncol>`\\m\\, the vector is amended
with 0's. It is an error for `ncol` to be smaller than \\m\\.

Argument `nrow` may be used to get a rectangular matrix, although the
term "companion" is normally used only for square matrices.

## Value

a matrix

## Examples

``` r
companion_matrix(4:1)
#>      [,1] [,2] [,3] [,4]
#> [1,]    4    3    2    1
#> [2,]    1    0    0    0
#> [3,]    0    1    0    0
#> [4,]    0    0    1    0
companion_matrix(4:1, ncol=6)
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    4    3    2    1    0    0
#> [2,]    1    0    0    0    0    0
#> [3,]    0    1    0    0    0    0
#> [4,]    0    0    1    0    0    0
#> [5,]    0    0    0    1    0    0
#> [6,]    0    0    0    0    1    0
companion_matrix(4:1, ncol=6, nrow=3)
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    4    3    2    1    0    0
#> [2,]    1    0    0    0    0    0
#> [3,]    0    1    0    0    0    0
```
