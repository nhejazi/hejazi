## Resubmission
This is a resubmission. In this version I have:

* ATTN: `R CMD check --as-cran` returns 0 ERRORS, 1 WARNING, 3 NOTES, described
  here:

  1. 0 ERRORS -- all functions and examples in the package are working fine.

  2. 1 WARNING -- there appears to be a LaTeX compilation issue according to the
     output generated but there is no evidence of this in the file `Rdlatex.log`
     and the manual appears to compile correctly.

  3. 3 NOTES -- (1) New Submission to CRAN, (2) "checking package dependencies,
     no repository set", and (3) "checking top-level files," which refers to the
     existence of this file, which is meant to allow a communication with a CRAN
     maintainer. All of these appear benign; please contact if changes required.

* Added a reference to the file LICENSE in the file DESCRIPTION.

* Revised most functions to resolve any outstanding warnings and/or errors.

