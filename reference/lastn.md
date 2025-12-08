# Extract the last n elements of a vector

Extract the last n elements of a vector

## Usage

``` r
lastn(x, n)
```

## Arguments

- x:

  a vector

- n:

  number of elements to keep, an integer

## Details

I did not know about the function
[`tail()`](https://rdrr.io/r/utils/head.html) when I did this one, but
the two functions are not completely equivalent (and `tail` is generic).

If `n` is equal to `length(x)`, `x` is returned as is. If `n` is equal
to zero or is negative, a length zero vector is returned.

It is an error for `n` to be larger than `length(x)`.

## Value

a vector containing the last `n` elements of `x`, see Details

## See also

See Also as [`tail`](https://rdrr.io/r/utils/head.html)

## Examples

``` r
lastn(1:10, 3) # 8:10
#> [1]  8  9 10
lastn(letters, 5) # "v" "w" "x" "y" "z"
#> [1] "v" "w" "x" "y" "z"
```
