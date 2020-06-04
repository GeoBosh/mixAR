test_that("mixComp class and methods are ok", {

    ## TODO: need more meaningful examples (for the Rd file, here no problem)
## dim, nrow, ncol
a <- new("mixComp", m = matrix(c(1:7, 11:17, 21:27), ncol = 3))
a
dim(a)
nrow(a)
ncol(a)
mix_ncomp(a)

-a
a - 1:7
1:7 - a

a + 1:7 
1:7 + a
2*a
a*2
a^2
sqrt(a)

b <- new("mixComp", m = matrix(rnorm(21), ncol = 3))


2/a
1:7 / a
a * b

## apply a function to the columns of a mixComp object
pnorm %of% b
"pnorm" %of% b # same
pnorm * b # same
"pnorm" * b # same but deprecated

## apply a separate function to to each column
flist <- list(function(x) pnorm(x),
              function(x) pt(x, df = 5),
              function(x) pt(x, df = 4) )
flist %of% b

f1 <- function(x) pnorm(x)     
f2 <- function(x) pt(x, df = 5)
f3 <- function(x) pt(x, df = 4)

c("f1", "f2", "f3") %of% b

c("f1", "f2", "f3") * b

"f1" * b

pnorm %of% (matrix(1:14/14, ncol = 2))
"pnorm" %of% (matrix(1:14/14, ncol = 2))

inner(1:7, a)
inner(a, 1:3)
inner(a, 1:3, "%%")
inner(a, 1:3, "%%", "+")

})
