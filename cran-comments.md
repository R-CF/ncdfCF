## R CMD check results

0 errors | 0 warnings | 0 notes

* This minor release has significant enhancements in code efficiency and logic and adds new features.

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems
(This reports the first line of each new failure)

* CFtime
  checking running R code from vignettes ...

This is a known failure due to a mutual dependency. A patch for CFtime is ready and will be applied and submitted to CRAN as soon as this ncdfCF package release is available to build against.
