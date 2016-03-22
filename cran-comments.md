# Comments from CRAN submission on March 15, 2016

## R CMD check results

There were no WARNINGs.

There was 1 ERROR:

* Undefined global functions or variables: `na.omit ppoints qnorm quantile time`

  possible solution:
  add `importFrom("stats", "na.omit", "ppoints", "qnorm", "quantile", "time")` 
  to the NAMESPACE file.

There was 1 NOTE:

* Non-standard file/directory found at top level: ‘cran-comments.md’
