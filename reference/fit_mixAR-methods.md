# Fit mixture autoregressive models

Estimate a MixAR model for a time series. This is a generic function.
The methods defined in package mixAR are described here.

## Usage

``` r
fit_mixAR(x, model, init, fix, ...)
```

## Arguments

- x:

  a time series.

- model:

  model, object inheriting from MixAR class.

- init:

  what initializations to do, see Details.

- fix:

  which parameters to fix, see Details.

- ...:

  additional arguments for the methods.

## Details

Method dispatch is done on the first three arguments: `x`, `model` and
`init`.

`model` specifies the model to fit. If `model` inherits from `"MixAR"`,
it is used as a template. If `init` is missing, the parameters of
`model` are also used as initial values. `model` can also be a numeric
vector specifying the order of a MixAR model with Gaussian components.

Argument `init` can be used to give initial values in variety of ways.
If it is a MixAR object it doesn't need to be of the same class as
`model`, to allow using as initial values common parameters of different
MixAR models. A positive integer value of `init` asks to run the fitting
procedure `init` times, each time generating random initial values.

`init` can also be a list. In that case, each component of the list
should itself be an acceptable value for `init` and the fitting
procedure is run with each component of `init`.

Argument `fix` can be given in a number of ways. Note however that
currently there is no method dispatch on it.

Currently the default method for `fit_mixAR` just throws error, since
there seems no suitable default task to do.

See individual methods for further details.

## Value

a MixAR model or a list of MixAR models, depending on the arguments.

## Methods

- `signature(x = "ANY", model = "ANY", init = "ANY")`:

  The default method throws error.

- `signature(x = "ANY", model = "MixAR", init = "missing")`:

  This is equivalent to setting `init = model`.

- `signature(x = "ANY", model = "MixAR", init = "MixAR")`:

  `model` is a template for the result, `init` specifies initial values
  for the parameters. In principle, `model` and `init` may be from
  different classes, to allow for example using AR coefficients from a
  Gaussian fit for other distributions.

- `signature(x = "ANY", model = "MixAR", init = "numeric")`:

  `init` must be a single positive integer here. The model is fitted
  `init` times, each time starting with a new set of randomly generated
  initial values. If `select` is `TRUE`, the default, the model with the
  largest likelihood is returned, otherwise a list containing the `init`
  fitted models is returned.

- `signature(x = "ANY", model = "MixAR", init = "list")`:

  Each element of the list `init` should be an acceptable value for
  `init`. The model is fitted with the initial value set to each element
  of `init`. A list containing the fitted models is returned.

- `signature(x = "ANY", model = "MixARGaussian", init = "MixAR")`:

- `signature(x = "ANY", model = "numeric", init = "missing")`:

  This is equivalent to setting `init = 1`.

- `signature(x = "ANY", model = "numeric", init = "numeric")`:

  A numeric `model` should be a vector of non-negative integers
  specifying the order of the MixAR model. The distribution of the
  components is assumed Gaussian.

## Examples

``` r
## model coefficients from Wong&Li (IBM fit)
prob  <- exampleModels$WL_ibm@prob     # c(0.5439, 0.4176, 0.0385)
sigma <- exampleModels$WL_ibm@scale    # c(4.8227, 6.0082, 18.1716)
ar    <- exampleModels$WL_ibm@arcoef@a # list(c(0.6792, 0.3208), c(1.6711, -0.6711), 1)

## data(ibmclose, package = "fma")  # `ibmclose'

mot30 <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
             dist = distlist("stdt", c(30, 30, 30)))

mot20_30_40 <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
                   dist = distlist("stdt", c(20, 30, 40)))

mo_t20_t30_norm <- new("MixARgen", prob = prob, scale = sigma, arcoef = ar,
                   dist = distlist(c("stdt", "stdt", "stdnorm"), c(20, 30)))

## Gaussian components
fi0 <- fit_mixAR(fma::ibmclose, exampleModels$WL_ibm, fix = "shift", crit = 1e-4)
fi0$model
#> An object of class "MixARGaussian"
#> Number of components: 3 
#>        prob       shift scale     order ar_1      ar_2      
#> Comp_1 0.54287126   0    4.797900   2   0.6825951  0.3184695
#> Comp_2 0.42094949   0    5.938168   2   1.6731621 -0.6755995
#> Comp_3 0.03617925   0   17.749431   1   0.9889259           
#> 
#> Distributions of the error components:
#>  standard Gaussian
#> 

if(FALSE){ # don't run on CRAN to save a couple of seconds
## remove minniter/maxniter below for realistic results.

## std-t components
fi1 <- fit_mixAR(fma::ibmclose, mot30, fix = "shift",
                 crit = 1e-4, minniter = 1, maxniter = 3)
fi1$model

## 1st and 2nd components std-t, 3rd Gaussian
fi2 <- fit_mixAR(fma::ibmclose, mo_t20_t30_norm, fix = "shift",
                 crit = 1e-4, minniter = 1, maxniter = 3)
fi2$model
}
```
