# Relabel the components of a MixAR model

Relabel the components of a MixAR model.

## Usage

``` r
mixAR_switch(model, perm)
mixAR_permute(model, perm)
```

## Arguments

- model:

  a MixAR model

- perm:

  a permutation for relabeling

## Details

If the permutation is the identity permutation the model is returned as
is. Otherwise the order of the components is changed according to
`perm`. Basically, `perm` is used as index, e.g. `prob[perm]`, etc.

## Note

Currently the function only reorders the "usual" components. Subclasses
of "MixAR" may contain other parameters (e.g. different error
distributions). So this function may not be appropriate for them.
