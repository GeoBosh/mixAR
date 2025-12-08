# Methods for function `initialize` in package mixAR

Methods for function `initialize` in package mixAR.

## Methods

These methods are for internal use by the user-level function `new()` to
create objects from the corresponding classes. The creation of objects
from a class and examples can be found in the description of the
corresponding class.

- `signature(.Object = "MixAR")`:

  Objects from class `MixAR` cannot be created since it is virtual. This
  method is called by initialisation methods of non-virtual subclasses
  of `MixAR` to set up its slots.

- `signature(.Object = "raggedCoef")`:

  Creates objects from class
  `"`[`raggedCoef`](https://geobosh.github.io/mixAR/reference/raggedCoef-class.md)`"`.

- `signature(.Object = "MixARgen")`:

  Creates objects from class `MixARgen`.
