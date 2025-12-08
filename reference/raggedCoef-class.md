# Class `"raggedCoef"` — ragged list objects

Some models have several several vectors of parameters, possibly of
different lengths, such that in some circumstances they are thought of
as lists, in others as matrices after suitable padding with zeroes.
Class `"raggedCoef"` represents such ragged lists. In package `"MixAR"`
it is used to hold the autoregressive coefficients of MixAR models.

## Usage

``` r
raggedCoef(p, value = NA_real_)
```

## Arguments

- p:

  orders, vector of integers.

- value:

  typically, a list, but see Details.

## Details

Class `"raggedCoef"` is for objects that can be considered as both,
lists and matrices. The elements of the list are vectors, possibly of
different lengths. When the object is viewed as a matrix, each element
of the list (suitably padded with zeroes or `NA`s) represents a row of a
matrix.

The recommended way to create objects from class `"raggedCoef"` is with
the function `raggedCoef`.

If `value` is a "raggedCoef" object it is returned. If value is a list,
it is converted to "raggedCoef" using `new()`. If argument `p` is
missing, it is inferred from the lengths of the elements of the list. If
argument `p` is not missing, a consistency check is made to ensure that
the order of the object is as specified by `p`.

Otherwise, if `value` is of length one, it is replicated to form a
ragged list with i-th element a vector of length `p[i]`. Although not
checked, the intention here is that `value` is from some atomic class.
The default for `value` is `NA_real_` to give a convenient way to create
a ragged list.

Finally, if none of the above applies, `value` is effectively assumed to
be a vector of length `sum(p)`, although other cases are admissible (but
I don't remember if this was intended). In this case, `value` is
reshaped into a ragged list to match `p`. This is convenient when, for
example, the elements of a ragged array are obtained from an
optimisation routine which expects plain vector.

## Objects from the Class

Below we describe the `"initialize"` method that underlies
`new("raggedCoef", ...)`. The recommended way to create `"raggedCoef"`
objects is with the function `raggedCoef`, see section Details.

Objects can also be created by calls of the form `new("raggedCoef", v)`,
where `v` is a list whose elements are numeric vectors, or
`new("raggedCoef", v1, v2, ...)`, where `v1, v2, ...` are numeric
vectors. The two forms are equivalent if `v = list(v1, v2, ...)`.

The elements of the list `v` may be named. Similarly, named arguments
can be used in the second form, say
`new("raggedCoef", name1 = v1, name2 = v2, ...)`. In both cases the
names are preserved in the internal representaion, but not used.

If the arguments are not as specified above the result should be
considered undefined. Currently, if there are other arguments after the
list `v`, they are ignored with a warning. If the first argument is not
a list then all arguments must be `numeric` and an error is raised if
this is not the case. For completeness, we mention that exactly two
arguments named `a`, and `p` are also accepted by `new()`, eg
`new("raggedCoef", p = c(1, 2), a = list(3, 4:5))`, but these are
assigned to the slots without any checking. so it is most flexible (and
recommended) to use `raggedCoef()` instead.

## Slots

- `a`::

  Object of class `"list"` containing the values.

- `p`::

  Object of class `"numeric"` containing the lengths of the components
  of `a`.

## Methods

Indexing with "\[" treats a `raggedCoef` object as a matrix, while
"\[\[" treats the object as list (it works on slot `a`).

Note that there is a difference between `x[2,]` (or the equivalent
`x[2]`) and `x[[2]]`—the former gives a vector of length `max(p)`, so
potentially padded with zeroes, while the latter gives the component
with its “natural” length.

The replacement variants of "\[" and "\[\[" do not change the structure
of the object and issue errors if the replacement value would result in
that. In situations where the checks are deemed redundant, direct
assignments to the corresponding slots may be used.

- \[:

  `signature(x = "raggedCoef", i = "missing", j = "missing", drop = "ANY")`:

- \[:

  `signature(x = "raggedCoef", i = "missing", j = "numeric", drop = "ANY")`:

- \[:

  `signature(x = "raggedCoef", i = "numeric", j = "missing", drop = "ANY")`:

- \[:

  `signature(x = "raggedCoef", i = "numeric", j = "numeric", drop = "ANY")`:
  Indexing with "\[" treats a `raggedCoef` object as a matrix with one
  row for each component and number of columns equal to `max(p)`.
  However, `x[2]` is equivalent to `x[2,]` which is different from the
  treatment of `matrix` objects in base R.

- \[\[:

  `signature(x = "raggedCoef", i = "ANY", j = "missing")`:

- \[\[:

  `signature(x = "raggedCoef", i = "ANY", j = "ANY")`: `"[["` extracts
  the corresponding element of slot `a`.

- \[\[\<-:

  `signature(x = "raggedCoef", i = "ANY", j = "ANY", value = "numeric")`:
  Replace the j-th element of i-th row with `value`. All arguments must
  be scalars.

- \[\[\<-:

  `signature(x = "raggedCoef", i = "ANY", j = "missing", value = "numeric")`:

- \[\<-:

  `signature(x = "raggedCoef", i = "ANY", j = "missing", value = "numeric")`:
  Replace the i-th row with `value`. Argument `i` must be a scalar while
  the length of `value` must be the same as that of `x@a[[i]]`. The
  methods for "\[" and "\[\[" with this signature coinside.

- \[\<-:

  `signature(x = "raggedCoef", i = "ANY", j = "missing", value = "list")`:
  The elements of `value` must have the same lengths as the elements
  they are replacing.

- \[\<-:

  `signature(x = "raggedCoef", i = "ANY", j = "missing", value = "matrix")`:
  This is essentially the reverse od the corresponding non-replacement
  operator. `value` must have at least as many columns as the longest
  element of `x` that is replaced.

- \[\<-:

  `signature(x = "raggedCoef", i = "ANY", j = "ANY", value = "numeric")`:
  ...

- \[\<-:

  `signature(x = "raggedCoef", i = "missing", j = "missing", value = "list")`:
  ...

- \[\<-:

  `signature(x = "raggedCoef", i = "missing", j = "missing", value = "matrix")`:
  ...

- \[\<-:

  `signature(x = "raggedCoef", i = "missing", j = "missing", value = "numeric")`:
  ...

- initialize:

  `signature(.Object = "raggedCoef")`: Creates objects of class
  `raggedCoef`. This method is used internally by `new()`. Users should
  use `new()` for creation of objects from this class, see the examples.

- show:

  `signature(object = "raggedCoef")`: ...

- mixFilter:

  `signature(x = "numeric", coef = "raggedCoef", index = "numeric")`:
  Apply a mixture filter to a time series.

- row_lengths:

  `signature(x = "raggedCoef")`: Gives `x@p`, which is the same as
  `lengths(x@a)`.

- length:

  `signature(x = "raggedCoef")`: Gives the total number of coefficients
  (`sum(x@p)`).

- anyNA:

  `signature(x = "raggedCoef")`: Are there `NA`'s in `x@a`?

- dim:

  `signature(x = "raggedCoef")`: The dimension of the object, when
  viewed as a matrix. The presence of this method also ensures that
  [`nrow()`](https://rdrr.io/r/base/nrow.html) and related functions
  give the expected result.

## Author

Georgi N. Boshnakov

## Note

Slot `p` is redundant but convenient.

## See also

class
`"`[`MixARGaussian`](https://geobosh.github.io/mixAR/reference/MixARGaussian-class.md)`"`

## Examples

``` r
ragged1 <- list(1, 2:3, 4:6)
ragged2 <- list(a = 1, b = 2:3, c = 4:6)

raggedCoef(1:3)          # only order given, fill with NA's
#> An object of class "raggedCoef"
#> Number of rows: 3 
#> Components' lengths: 1 2 3 
#> 
#>             co_1 co_2 co_3
#> Component_1  NA           
#> Component_2  NA   NA      
#> Component_3  NA   NA   NA 
#> 
raggedCoef(1:3, 0)       # fill with a number (zero in this case)
#> An object of class "raggedCoef"
#> Number of rows: 3 
#> Components' lengths: 1 2 3 
#> 
#>             co_1 co_2 co_3
#> Component_1   0           
#> Component_2   0    0      
#> Component_3   0    0    0 
#> 

## init with a list
raggedCoef(ragged1)
#> An object of class "raggedCoef"
#> Number of rows: 3 
#> Components' lengths: 1 2 3 
#> 
#>             co_1 co_2 co_3
#> Component_1   1           
#> Component_2   2    3      
#> Component_3   4    5    6 
#> 
raggedCoef(value = ragged1) 
#> An object of class "raggedCoef"
#> Number of rows: 3 
#> Components' lengths: 1 2 3 
#> 
#>             co_1 co_2 co_3
#> Component_1   1           
#> Component_2   2    3      
#> Component_3   4    5    6 
#> 

## error, since the shape of ragged1 is not c(2, 2, 3):
## raggedCoef(c(2, 2, 3), value = ragged1)

## init with a flattened list
raggedCoef(p = 1:3, value = 1:6)
#> An object of class "raggedCoef"
#> Number of rows: 3 
#> Components' lengths: 1 2 3 
#> 
#>             co_1 co_2 co_3
#> Component_1   1           
#> Component_2   2    3      
#> Component_3   4    5    6 
#> 

## specify each component separately
ragA <- new("raggedCoef", 1, 2:3, 4:6)
ragB <- new("raggedCoef", list(1, 2:3, 4:6))  # same
identical(ragA, ragB) #TRUE
#> [1] TRUE

## extract as a matrix
ragA[]
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    2    3    0
#> [3,]    4    5    6

## extract the 2nd component
ragA[2]      # c(2, 3, 0) ("[" pads with 0's)
#> [1] 2 3 0
ragA[[2]]    # c(2, 3)    ("[[" does not pad)
#> [1] 2 3

## get the 2nd and 3rd components as a matrix
ragA[2:3, ]    # "[" treats object (almost) as matrix
#>      [,1] [,2] [,3]
#> [1,]    2    3    0
#> [2,]    4    5    6
ragA[2:3]      # same (though not as for "matrix")
#>      [,1] [,2] [,3]
#> [1,]    2    3    0
#> [2,]    4    5    6

## names are kept in the list but currently not used 
ragC  <- new("raggedCoef", list(a = 1, b = 2:3, c = 4:6))
ragC1 <- new("raggedCoef", a = 1, b = 2:3, c = 4:6)
identical(ragC, ragC1) # TRUE
#> [1] TRUE
names(ragC@a) # [1] "a" "b" "c"
#> [1] "a" "b" "c"

length(ragA)
#> [1] 6
dim(ragA)
#> [1] 3 3
c(nrow(ragA), ncol(ragA))
#> [1] 3 3
c(NROW(ragA), NCOL(ragA))
#> [1] 3 3
```
