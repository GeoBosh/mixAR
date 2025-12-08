# Support for EM estimation of MixAR models, internal function.

Solve the linear sub-system arising in the M-step of the EM algorithm
for MixAR models with Gaussian noise components. This gives estimates of
the parameters of the \\k\\th AR component of the model.

## Usage

``` r
mixSubsolve(k, pk, Stau, Stauy, Stauyy, shift, tol = 1e-07)
```

## Arguments

- k:

  which component, an integer.

- pk:

  AR order of the \\k\\th component.

- Stau,Stauy,Stauyy:

  sums and cross-sums needed to form the system.

- shift:

  If `TRUE`, estimate also a shift, otherwise set the shift to 0.

- tol:

  tolerance, only used in case of trouble, see Details.

## Details

`mixSubsolve` forms and solves a linear subsytem to obtain estimates of
the AR parameters (and the shift, if `shift` is `TRUE`) for the `k`-th
MixAR component.

First, [`solve()`](https://rdrr.io/r/base/solve.html) is tried. If it
reports that the system is (numerically) singular, a solution is
computed using SVD with tolerance `tol`.

Note that argument `tol` is not used in the call to
[`solve()`](https://rdrr.io/r/base/solve.html). The net effect is that
[`solve()`](https://rdrr.io/r/base/solve.html) computes the solution
with its very small default tollerance. Probably I wanted discontinuity
during optimisation, which could result if using a larger tolerance, and
only resort to that if absolutely needed.

I don't remember why I used pseudo-inverse in `mixSubsolve()`, when
[`solve()`](https://rdrr.io/r/base/solve.html) has a similar `tol`
argument for its QR-decomposition.

## Author

Georgi N. Boshnakov
