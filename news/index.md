# Changelog

## mixAR 0.22.8.9000

- fixed a typo in a [`stop()`](https://rdrr.io/r/base/stop.html) string
  (fixes issue [\#1](https://github.com/GeoBosh/mixAR/issues/1),
  reported by [@beliavski](https://github.com/beliavski)).

- added ORCID ids of the authors.

## mixAR 0.22.8

CRAN release: 2023-12-19

- fixed ‘Lost braces’ NOTE on CRAN.

- fixed “Documented arguments not in in Rd file ‘mixAR-internal.Rd’:”
  NOTE on CRAN.

## mixAR 0.22.7

CRAN release: 2022-05-03

- fixed CRAN warning “`\item` in `\describe` must have non-empty label”
  in a couple of Rd files.

## mixAR 0.22.6

CRAN release: 2022-01-23

- `fit_mixAR` was failing for some mixAR models with Gaussian components
  when the AR order of a component was zero. (reported by Lu Cao)

- improved printing of `mixARgen` objects.

- The function returned by the methods for `multiStep_dist` with
  `N = "missing"` now checks the length of the supplied `xcond` argument
  and throws error if it is shorter than the maximal AR order `p`. If it
  is longer, the last `p` values in it are used. This has always been
  the case when `N` is not missing.

- new function `mix_qf` computes conditional quantiles, analogous to
  `mix_pdf` and `mix_cdf`.

- numerous fixes and improvements.

## mixAR 0.22.5

CRAN release: 2021-01-04

- in tests, include `check.environment = FALSE` in calls to
  expect_equal() to accommodate for a change in
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) in `R-devel`.

## mixAR 0.22.4

CRAN release: 2020-06-29

- `tsdiag` has been extensively revamped. It now is more user friendly
  and offers more diagnostics. The object returned by it has a class and
  a print method.

- `tsdiag` now works also for models with non-Gaussian components.

- `mixAR_BIC` now takes into account estimated noise parameters, useful
  for non-Gaussian noise components.

- updated README.md.

- relaxed some comparison tests raising “Additional issues” on CRAN.

## mixAR 0.22.3

CRAN release: 2020-06-22

- there are now no commented out examples

- now optimisation functions write to the console only when asked.

- further reduced the time taken by `R CMD check`, mainly by slashing
  the time for the examples.

## mixAR 0.22.2 (Not on CRAN)

- reduced the time taken by `R CMD check`.

## mixAR 0.22.1 (Not on CRAN)

- added missing import from stats4.

- in `README.md`, now using absolute paths to linked pages on the
  package website.

## mixAR 0.22.0 (Not on CRAN)

- first public version
