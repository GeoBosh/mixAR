# Methods for function `get_edist` in package mixAR

Methods for function `get_edist` in package mixAR

## Methods

`get_edist` gives the error (or noise) distribution of MixAR objects.

Currently the distribution is returned as a list of functions. The list
contains one element for each component. If the error distributions of
all components are the same, then the list may contain a single element
representing the common error distribution.

Note that the distribution is not necessarily stored in slot `dist` in
this format, see the description of this slot in class
`"`[`MixARgen`](https://geobosh.github.io/mixAR/reference/MixARgen-class.md)`"`.
Such a slot may even not exist if the distribution of the error
components is fixed as is the case for class `MixARGaussian`.

Each subclass of `MixAR` needs to define a method for `get_edist`.

- `signature(model = "MixAR")`:

  Issue an error message and stop.

  This method is invoked for subclasses of `MixAR` that have not defined
  their own method for `get_edist`. This is an error.

- `signature(model = "MixARGaussian")`:

  Return an object representing the fact that the error distributions of
  the components of `MixARGaussian` objects are standard normal.

- `signature(model = "MixARgen")`:

  Return an object representing the error distributions of the
  components of `MixARgen` objects. The distributions are not
  necessarilly the same for such objects.
