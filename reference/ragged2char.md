# Convert a ragged list into a matrix of characters

The function transforms a ragged list into a matrix of characters. It is
used in mixAR for output visualisation purposes.

## Usage

``` r
ragged2char(raglist, filler = NA_character_)
```

## Arguments

- raglist:

  A ragged list (from `raggedCoef`).

- filler:

  The character filling order mismatches.

## Details

`ragged2char` converts a ragged list into a character matrix with as
many columns as the longest component of the list, filling the missing
entries with the values of argument `filler`. The latter defaults to
`NA`.

In MixAR context the `i`-th row represents the AR coefficients for the
`i`-th component.

## Value

a character matrix

## Author

Georgi N. Boshnakov

## See also

[`raggedCoef`](https://geobosh.github.io/mixAR/reference/raggedCoef-class.md)

## Examples

``` r
li1 <- list(1.5, c(1.51, 1.522), c(10.311, 1.31, 1.313))
ragged2char(li1)
#>      [,1]     [,2]    [,3]   
#> [1,] " 1.500" NA      NA     
#> [2,] " 1.510" "1.522" NA     
#> [3,] "10.311" "1.310" "1.313"
ragged2char(li1, "")
#>      [,1]     [,2]    [,3]   
#> [1,] " 1.500" ""      ""     
#> [2,] " 1.510" "1.522" ""     
#> [3,] "10.311" "1.310" "1.313"
```
