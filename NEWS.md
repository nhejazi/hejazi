December 10, 2019:
* Fixes for compatibility with R 4.0.0, including removal or re-writing of
   functions making use of `class`
* Functions removed or renamed:
  * `compFun` - removed
  * `factornum` - renamed to `factor_to_num`
  * `qrD` - removed
* Functoins altered:
  * `discrete_by_quantile` - now uses `is.factor` instead of `class`
* Functions added:
  * `nll` - risk under cross-entropy loss
* Bump of version number to 0.6.0 for updated release on CRAN

May 02, 2018:
* Fix a broken unit test for R 3.5.0, changing the use of `identical` for a
    combination of `expect_true` and `setequal`

March 23, 2018:
* Fix a broken test (due to use of `all.equal` instead of `identical`) in
    R-devel, based on report by CRAN
* Add hex sticker for package based on art work by GL

September 20, 2016:
* Addition of several functions, re-written DESCRIPTION file.
* Update of package version to v0.4.0.
* This version update to be released on CRAN.

July 05, 2016:
* Major changes, including addition of unit tests and Travis-CI coverage, added
    to the package for v0.3.5.
* This version update not to be released on CRAN.

March 25, 2016:
* The first public release of this package (v0.3.0) is made available on CRAN.
