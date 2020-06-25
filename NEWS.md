# mixAR 0.22.4

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
