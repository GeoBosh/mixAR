# Class `"MixVAR"` — mixture vector autoregressive models

Mixture vector autoregressive models

## Objects from the Class

A virtual Class: No objects may be created from it.

Derived classes add distribution properties, e.g. use class
`"`[`MixVARGaussian`](https://geobosh.github.io/mixAR/reference/MixVARGaussian-class.md)`"`
for MixVAR models with Gaussian error components.

## Slots

- `prob`::

  the mixing probabilities, an object of class `"numeric"`

- `order`::

  Object of class `"numeric"` \~~

- `shift`::

  Object of class `"matrix"` \~~

- `vcov`::

  Object of class `"array"` \~~

- `arcoef`::

  Object of class `"raggedCoefV"` \~~

## Methods

- fit_mixVAR:

  `signature(x = "ANY", model = "MixAR")`: ...

## Author

Davide Ravagli

## See also

class
`"`[`MixVARGaussian`](https://geobosh.github.io/mixAR/reference/MixVARGaussian-class.md)`"`
