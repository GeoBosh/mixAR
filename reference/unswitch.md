# Dealing with label switching in MixAR experiments

Deal with label switching in MixAR experiments.

## Usage

``` r
unswitch(models, true_model, Nref = 100, simargs = NULL)
```

## Arguments

- models:

  a list of MixAR models to process.

- true_model:

  the MixAR model used in the experiment.

- Nref:

  length of the reference series

- simargs:

  arguments for simulation of the reference series, see 'Details'.

## Value

a list of the unswitched models.

## References

Boshnakov, Georgi N., (2012) Label switching in MixAR models (in
preparation).

## Author

Georgi N. Boshnakov
