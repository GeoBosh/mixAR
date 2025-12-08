# Small utilities for ragged objects

Small utilities for ragged objects. Modify the elements of raggedCoef
objects, extract them as a vector.

## Usage

``` r
rag_modify(rag, v)
ragged2vec(x)
```

## Arguments

- rag:

  the `raggedCoef` object to be modified.

- v:

  vector of values to replace the old ones.

- x:

  a `raggedCoef` object.

## Details

An error will occur if the length of `v` is not equal to `sum(rag@p)`.

`rag_modify` is, in a sense, the inverse of `ragged2vec`.

## Value

for `rag_modify`, a `raggedCoef` object of the same order as `rag` but
with coefficients replaced by the new values.

for `ragged2vec`, a numeric vector.

## Examples

``` r
rag1 <- new("raggedCoef", list(1, 2:3, 4:6))
a1 <- (1:6)^2
rag1a <- rag_modify(rag1, a1)

rag2 <- new("raggedCoef", list(1, numeric(0), 4:6)) # a zero-length ccomponent
a2 <- (1:4)^2
rag2a <- rag_modify(rag2, a2)
```
