## Test environments
* local Ubuntu 18.04 install, R 3.6.1
* win-builder (devel and release)
* travis CI (ubuntu; oldrel, release, and devel)
* appveyor CI (windows; release only)

## R CMD check results
* There were 0 ERRORs.

* There were 0 WARNINGs.

* There were 0 NOTEs.

## Downstream dependencies
* Nothing to report.

## Additional Notes
* Updated submission of package, from version v0.5.0 to v0.6.1
* This release fixes broken functionality and unit tests in anticipation of R
  4.0.0: inheritance of `Array` class by `Matrix` objects, re: use of `class()`.
* Also renames an exported function to avoid method override of `plot.lm()`.
