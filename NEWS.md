# distributional 0.2.2

## New features

* Added `is_distribution()` to identify if an object is a distribution.

## Improvements

* Improved NA structure of distributions, allowing it to work with `is.na()` and
  `vctrs` vector resizing / filling functionality.
  

# distributional 0.2.1

A small performance and methods release. Some issues with truncated
distributions have been fixed, and some more distribution methods have been
added which improve performance of common tasks.

## New features

### Probability distributions

* Added `dist_missing()` for representing unknown or missing (NA) distributions.

## Improvements

* Documentation improvements.
* Added `cdf()` method for `dist_sample()` which uses the emperical cdf.
* `dist_mixture()` now preserves `dimnames()` if all distributions have the same
  `dimnames()`.
* Added `density()` and `generate()` methods for sample distributions.
* Added `skewness()` method for `dist_sample()`.
* Improved performance for truncated Normal and sample distributions (#49).
* Improved vectorisation of distribution methods.

## Bug fixes

* Fixed issue with computing the median of `dist_truncated()` distributions.
* Fixed format method for `dist_truncated()` distributions with no upper or 
  lower limit.
* Fixed issue with naming <hilo> objects giving an invalid structure. It now
  gives an informative error (#23).
* Fixed documentation for Negative Binomial distribution (#46).

# distributional 0.2.0

## New features

### Probability distributions

* Added `dist_wrap()` for wrapping distributions not yet added in the package.

### Methods

* Added `likelihood()` for computing the likelihood of observing a sample from a
  distribution.
* Added `skewness()` for computing the skewness of a distribution.
* Added `kurtosis()` for computing the kurtosis of a distribution.
* The `density()`, `cdf()` and `quantile()` methods now accept a `log` argument 
  which will use/return probabilities as log probabilities.
  
## Improvements

* Improved documentation for most distributions to include equations for the
  region of support, summary statistics, density functions and moments. This is
  the work of @alexpghayes in the `distributions3` package.
* Documentation improvements
* Added support for displaying distributions with `View()`.
* `hilo()` intervals can no longer be added to other intervals, as this is a
  common mistake when aggregating forecasts.
* Incremented `d` for `numDeriv::hessian()` when computing mean and variance of 
  transformed distributions.
  
## Deprecated features

* Graphics functionality provided by `autoplot.distribution()` is now deprecated
  in favour of using the `ggdist` package. The `ggdist` package allows 
  distributions produced by distributional to be used directly with ggplot2 as
  aesthetics.
  
# distributional 0.1.0

First release. 

## New features

### Object classes

* `distribution`: Distributions are represented in a vectorised format using the
  [vctrs](https://cran.r-project.org/package=vctrs) package. This makes 
  distributions suitable for inclusion in model prediction output. A 
  `distribution` is a container for distribution-specific S3 classes.
* `hilo`: Intervals are also stored in a vector. A `hilo` consists of a `lower`
  bound, `upper` bound, and confidence `level`. Each numerical element can be
  extracted using `$`, for example my_hilo$lower to obtain the lower bounds.
* `hdr`: Highest density regions are currently stored as lists of `hilo` values.
  This is an experimental feature, and is likely to be expanded upon in an
  upcoming release.

### Generic functions

Values of interest can be computed from the distribution using generic functions.
The first release provides 9 functions for interacting with distributions:

* `density()`: The probability density/mass function (equivalent to `d...()`).
* `cdf()`: The cumulative distribution function (equivalent to `p...()`).
* `generate()`: Random generation from the distribution (equivalent to `r...()`).
* `quantile()`: Compute quantiles of the distribution (equivalent to `q...()`).
* `hilo()`: Compute probability intervals of probability distribution(s).
* `hdr()`: Compute highest density regions of probability distribution(s).
* `mean()`: Obtain the mean(s) of probability distribution(s).
* `median()`: Obtain the median(s) of probability distribution(s).
* `variance()`: Obtain the variance(s) of probability distribution(s).

### Graphics

* Added an `autoplot()` method for visualising the probability density function
  ([`density()`]) or cumulative distribution function ([`cdf()`]) of one or more
  distribution.
* Added `geom_hilo_ribbon()` and `geom_hilo_linerange()` geometries for ggplot2.
  These geoms allow uncertainty to be shown graphically with `hilo()` intervals.

### Probability distributions

* Added 20 continuous probability distributions: 
  `dist_beta()`, `dist_burr()`, `dist_cauchy()`, `dist_chisq()`, 
  `dist_exponential()`, `dist_f()`, `dist_gamma()`, `dist_gumbel()`, 
  `dist_hypergeometric()`, `dist_inverse_exponential()`, `dist_inverse_gamma()`,
  `dist_inverse_gaussian()`, `dist_logistic()`, `dist_multivariate_normal()`, 
  `dist_normal()`, `dist_pareto()`, `dist_student_t()`, 
  `dist_studentized_range()`, `dist_uniform()`, `dist_weibull()`
* Added 8 discrete probability distributions: 
  `dist_bernoulli()`, `dist_binomial()`, `dist_geometric()`, 
  `dist_logarithmic()`, `dist_multinomial()`, `dist_negative_binomial()`,
  `dist_poisson()`, `dist_poisson_inverse_gaussian()`
* Added 3 miscellaneous probability distributions: 
  `dist_degenerate()`, `dist_percentile()`, `dist_sample()`

### Distribution modifiers

* Added `dist_inflated()` which inflates a specific value of a distribution by
  a given probability. This can be used to produce zero-inflated distributions.
* Added `dist_transformed()` for transforming distributions. This can be used
  to produce log distributions such as logNormal: 
  `dist_transformed(dist_normal(), transform = exp, inverse = log)`
* Added `dist_mixture()` for producing weighted mixtures of distributions.
* Added `dist_truncated()` to impose boundaries on a distribution's domain via
  truncation.
