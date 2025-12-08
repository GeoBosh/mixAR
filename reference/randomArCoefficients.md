# Random initial values for MixAR estimation

Translations of functions from my Mathematica sources. Not used
currently?

## Usage

``` r
randomArCoefficients(ts, wv, pk, pmax, partempl, sub_size = 10,
                     condthr = 10, nattempt = 10, startfrom = pmax + 1)

randomMarParametersKernel(ts, ww, pk, pmax, partempl, ...)

randomMarResiduals(ts, p, partempl)

tsDesignMatrixExtended(ts, p, ind, partempl)
```

## Arguments

- ts:

  time series.

- wv,ww:

  a vector of weights (?).

- pk:

  the AR order of the requested component.

- pmax:

  the maximal AR order in the model. Needed since it cannot be
  determined by functions working on a single component.

- partempl:

  parameter template, a list containing one element for each mixture
  component, see Details.

- sub_size:

  the size of the subsample to use, default is 10.

- condthr:

  threshold for the condition number.

- nattempt:

  if `condthr` is not reached after `nattempt` attempts, the function
  returns the results from the last subset tried.

- startfrom:

  the starting index (in `ts`) to use for subsampling, default is
  `pmax + 1`.

- ...:

  arguments to pass on to `randomArCoefficients()`.

- p:

  a vector of non-negative integers, the MixAR order.

- ind:

  a vector of positive integers specifying the indices of the
  observations to use for the “response” variable.

## Details

`randomArCoefficients` tries small subsamples (not necessarilly
contiguous) from the observations in search of a cluster hopefully
belonging to one mixture component and estimates the corresponding shift
and AR parameters.

`randomMarResiduals` selects random parameters for each mixture
component and returns the corresponding residuals.
`randomMarParametersKernel` is a helper function which does the
computation for one component.

`tsDesignMatrixExtended` forms the extended design matrix corresponding
to a subsample. This is used for least square estimation of the
parameters.

## Author

Georgi N. Boshnakov

## See also

[`tomarparambyComp`](https://geobosh.github.io/mixAR/reference/tomarparambyComp.md)
