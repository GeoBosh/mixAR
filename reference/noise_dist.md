# Internal mixAR functions

Functions for the distributions of the components of MixAR models

## Usage

``` r
get_edist(model)
noise_dist(model, what, expand = FALSE)
noise_rand(model, expand = FALSE)
noise_params(model)
set_noise_params(model, nu)
```

## Arguments

- model:

  a model.

- what:

  the property, a character string.

- expand:

  if TRUE, expand the list to length equal to the number of components,
  see Details.

- nu:

  degrees of freedom.

## Details

`get_edist` gives the distributions of the noise components of `model`.
`noise_dist` gives property `what` of the noise distribution.
`noise_rand` gives a list of functions for simulation from the component
distributions.

In each case, the list contains one element for each component but if it
is of length one, then the only element is common for all components. To
force a complete list even in this case, use `expand = TRUE`.

`noise_params` gives the parameters of the model as a numeric vector.

The distribution is specified as a list. Element "dist" contain the
distribution. Element "generator" is a function that generates a
distribution like the one specified. If "dist" is absent or NULL, the
generator is called to generate a distribution object.

Initially the distribution itself was used for slot `dist`. For
compatibility with old code using that format, this is still supported.

## See also

[`fdist_stdnorm`](https://geobosh.github.io/mixAR/reference/fnoise.md),
[`fdist_stdt`](https://geobosh.github.io/mixAR/reference/fnoise.md),
[`fn_stdt`](https://geobosh.github.io/mixAR/reference/fnoise.md).
