# Conditional mean of MixAR models

Conditional mean of MixAR models.

## Methods

- `signature(model = "MixAR", x = "missing", index = "missing", xcond = "missing")`:

  This method returns a function with argument `xcond`, suitable for
  calls with many values of `xcond`.

- `signature(model = "MixAR", x = "missing", index = "missing", xcond = "numeric")`:

- `signature(model = "MixAR", x = "numeric", index = "missing", xcond = "missing")`:

- `signature(model = "MixAR", x = "numeric", index = "numeric", xcond = "missing")`:

## See also

[`mix_location`](https://geobosh.github.io/mixAR/reference/mix_moment.md)
for examples and related functions
