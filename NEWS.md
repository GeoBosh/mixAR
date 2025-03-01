# mixAR 0.22.8.9000 (CRAN)

- fixed a typo in a `stop()` string 
  (fixes issue #1, reported by @beliavski).


# mixAR 0.22.8 (CRAN)

- fixed 'Lost braces' NOTE on CRAN.

- fixed "Documented arguments not in \usage in Rd file 'mixAR-internal.Rd':"
  NOTE on CRAN.


# mixAR 0.22.7 (CRAN)

- fixed CRAN warning "`\item` in `\describe` must have non-empty label" in a
  couple of Rd files.


# mixAR 0.22.6 (CRAN)

- `fit_mixAR` was failing for some mixAR models with Gaussian components when
  the AR order of a component was zero. (reported by Lu Cao)

- improved printing of `mixARgen` objects.

- The function returned by the methods for `multiStep_dist` with `N = "missing"`
  now checks the length of the supplied `xcond` argument and throws error if it
  is shorter than the maximal AR order `p`. If it is longer, the last `p` values
  in it are used.  This has always been the case when `N` is not missing.

- new function `mix_qf` computes conditional quantiles, analogous to `mix_pdf`
  and `mix_cdf`.

- numerous fixes and improvements.


# mixAR 0.22.5 (CRAN)

- in tests, include `check.environment = FALSE` in calls to expect_equal() to
  accommodate for a change in `all.equal()` in `R-devel`.


# mixAR 0.22.4 (CRAN)

- `tsdiag` has been extensively revamped. It now is more user friendly and
   offers more diagnostics. The object returned by it has a class and a print
   method.

- `tsdiag` now works also for models with non-Gaussian components.

- `mixAR_BIC` now takes into account estimated noise parameters, useful for
   non-Gaussian noise components.

- updated README.md.

- relaxed some comparison tests raising "Additional issues" on CRAN.


# mixAR 0.22.3 (CRAN)

- there are now no commented out examples

- now optimisation functions write to the console only when asked.

- further reduced the time taken by `R CMD check`, mainly by slashing the time
  for the examples.


# mixAR 0.22.2

- reduced the time taken by `R CMD check`.


# mixAR 0.22.1

- added missing import from stats4.

- in `README.md`, now using absolute paths to linked pages on the package website.


# mixAR 0.22.0

- first public version
