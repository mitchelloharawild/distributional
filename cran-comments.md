## Test environments
* local ubuntu 24.04 install, R 4.5.2
* ubuntu-latest (on GitHub actions), R-devel, R-release, R-oldrel-1, R-oldrel-2, R-oldrel-3, R-oldrel-4
* macOS-latest (on GitHub actions), R-release
* windows-latest (on GitHub actions), R-release, R-oldrel-4
* win-builder, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* Found the following (possibly) invalid URLs (404) for `dist_cdf()`,
  `dist_density()`, and `dist_tweedie()`: these are new functions in this
  release, and their pkgdown reference pages will go live once this release
  is accepted.

Reverse dependency checks have been performed and there were no changes to worse.
