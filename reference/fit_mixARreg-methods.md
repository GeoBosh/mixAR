# Fit time series regression models with mixture autoregressive residuals

Estimate a linear regression model for time series with residuals from a
mixture autoregressive process.

## Usage

``` r
fit_mixARreg(x, y, mixARmodel, EMinit, ...)

mixARreg(x, y, mixARmodel, tol = 1e-6, niter = 200)
```

## Arguments

- x:

  the response time series (currently a numeric vector).

- y:

  `data.frame`, `matrix` or `numeric` vector. If either of the first
  two, each column must contain one covariate (currently numeric). A
  check for matching lengths between `x` and `y` is done.

- mixARmodel:

  An object inheriting from class `"MixAR"`, giving initial values for
  EM-estimation of mixture autoregressive parameters. Currenlty only
  `"MixARGaussian"` is supported.

- EMinit:

  starting values for EM estimation of MixAR parameters. If present,
  must be a named list, containing at least `prob` and `scale` as
  numeric vectors, and a list for `arcoef`.

- tol:

  threshold for convergence criterion.

- ...:

  passed on to `MixARreg`.

- niter:

  maximal number of iterations.

## Details

`fit_mixARreg` is a generic function. Currently there is no default
method for `fit_mixARreg`. Arguments `y`, `mixARmodel`, `EMinit` can be
given in a number of ways, see individual methods for details.

Argument `mixARmodel` gives the details of the the MixAR part of the
model and initial values for the parameters. For `fit_mixARreg` this can
alternatively be done with a list using argument `EMinit`. Currently, at
least one of the two must be supplied, and if both are present `EMinit`
is ignored.

`mixARreg` performs a two-step estimation of a linear regression model
with mixture autoregressive residuals. It is the workhorse for
`fit_mixARreg` which calls it to do the computations.

## Value

- reg:

  The summary output of the regression part of the model.

- mixARmodel:

  Estimates of the mixture autoregressive part of the model.

- niter:

  The number of iterations until convergence.

## Methods

- `signature(x = "ANY", y = "data.frame", mixARmodel = "MixAR", EMinit = "missing") `:

  Covariates `y` are supplied as `data.frame`: each column corresponds
  to one covariate. Initialization of `MixAR` paramters is done using
  input `mixARmodel`

- `signature(x = "ANY", y = "matrix", mixARmodel = "MixAR", EMinit = "missing")`
  :

  Covariates `y` are supplied as `matrix`: each column corresponds to
  one covariate. Initialization of `MixAR` paramters is done using input
  `mixARmodel`

- `signature(x = "ANY", y = "numeric", mixARmodel = "MixAR", EMinit = "missing")`
  :

  Covariates `y` is supplied as `numeric`: this method handles the
  simple regression case with a single covairate. Initialization of
  `MixAR` paramters is done using input `mixARmodel`

- `signature(x = "ANY", y = "ANY", mixARmodel = "missing", EMinit = "list")`:

  `EMinit` must be a named list (see 'Arguments').

- `signature(x = "ANY", y = "ANY", mixARmodel = "MixAR", EMinit = "list")`:

  When both `mixARmodel` and `EMinit` are supplied, the second is
  ignored.

## Author

Davide Ravagli and Georgi N. Boshnakov

## Note

Estimation is done using the function `mixARreg` within each method.

## See also

[`fit_mixAR`](https://geobosh.github.io/mixAR/reference/fit_mixAR-methods.md)

## Examples

``` r
## Simulate covariates
set.seed(1234)
n <- 50 # for CRAN
y <- data.frame(rnorm(n, 7, 1), rt(n, 3), rnorm(n, 3, 2))

## Build mixAR part
model <- new("MixARGaussian", 
             prob   = exampleModels$WL_At@prob,      # c(0.5, 0.5)
             scale  = exampleModels$WL_At@scale,     # c(1, 2)        
             arcoef = exampleModels$WL_At@arcoef@a ) # list(-0.5, 1.1)

## Simulate from MixAR part
u <- mixAR_sim(model, n, 0)

x <- 10 + y[, 1] + 3 * y[, 2] + 2 * y[, 3] + u

## Fit model

## Using MixARGaussian
fit_mixARreg(x = x, y = y, mixARmodel = model, niter = 3)
#> $reg
#> 
#> Call:
#> lm(formula = data.frame(xstar, y))
#> 
#> Coefficients:
#>    (Intercept)  rnorm.n..7..1.        rt.n..3.  rnorm.n..3..2.  
#>          7.325           1.558           2.831           1.805  
#> 
#> 
#> $mixARmodel
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob      shift scale    order ar_1     
#> Comp_1 0.5566597   0   1.057166   1   -0.475807
#> Comp_2 0.4433403   0   1.906334   1    1.005682
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $niter
#> [1] 3
#> 
#> $convergence
#> [1] 1
#> 

## Using EMinit
EMinit <- list(prob = exampleModels$WL_At@prob, scale = exampleModels$WL_At@scale,
               arcoef = exampleModels$WL_At@arcoef@a)
fit_mixARreg(x = x, y = y, EMinit = EMinit, niter = 3)
#> $reg
#> 
#> Call:
#> lm(formula = data.frame(xstar, y))
#> 
#> Coefficients:
#>    (Intercept)  rnorm.n..7..1.        rt.n..3.  rnorm.n..3..2.  
#>          7.325           1.558           2.831           1.805  
#> 
#> 
#> $mixARmodel
#> An object of class "MixARGaussian"
#> Number of components: 2 
#>        prob      shift scale    order ar_1     
#> Comp_1 0.5566597   0   1.057166   1   -0.475807
#> Comp_2 0.4433403   0   1.906334   1    1.005682
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 
#> 
#> $niter
#> [1] 3
#> 
#> $convergence
#> [1] 1
#> 
```
