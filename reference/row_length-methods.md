# Methods for function `row_lengths` in package mixAR

Determine the lengths of the \`rows' of a ragged object.

## Methods

Some objects in this package contain (effectively) lists of vectors.
These vectors are considered \`rows' and this function returns their
lengths (as a vector).

- `signature(x = "ANY")`:

  The default method. Applies `length` to the elements of the argument
  (2020-03-28: now using `lengths(x)`).

- `signature(x = "raggedCoef")`:

  Returns the lengths of the rows of the components, a numeric vector.

- `signature(x = "MixAR")`:

  Returns the AR orders of the model components, a numeric vector.
