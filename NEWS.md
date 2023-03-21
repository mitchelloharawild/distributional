# distributional 0.3.2

Small patch to resolve issues with CRAN checks.

## Bug fixes

* Fixed object structure resulting from transforming sample distributions (#81).
* Improved reliability of `quantile(<dist_mixture>)`.
* Defined `cdf(<dist_sample>)` as Pr(X <= x), not Pr(X < x).
* Fixed S3 generic argument name `p` for `log_quantile()`.

# distributional 0.3.1

## New features

* Add Math and Ops methods for sample distribution, which applies the functions
  directly to the samples.
* Added `mean` and `sd` as aliases for `mu` and `sigma` respectively in 
  `dist_normal()` and `dist_student_t()` to match arguments of the stats package
  interface (#76).
* Added `scale` argument for alternative specification for `dist_burr()` and
  `dist_gamma()`.

## Improvements

* Generics introduced by this package now allow `na.rm` and other parameters to
  be passed to distribution methods, even if these parameters aren't used. The
  package no longer checks the usage of `...` with the `ellipsis` package, if
  you'd like to check that all `...` are used, you can write your own wrapping
  functions.
* Lists of functions can now be used in `dist_transformed()`, allowing the
  transformation to differ for each distribution.
* `covariance()` and other matrix output functions of multivariate distributions
  now name the result using the distribution's dimension names.
* Improve handling of mixture distribution quantiles at boundaries {0,1}.

## Bug fixes

* Fixed issue with computing multiple values from a univariate distribution with
  named dimensions (#79).

# distributional 0.3.0

## New features

### Probability distributions

* Added `dist_categorical()` for the Categorical distribution.
* Added `dist_lognormal()` for the log-normal distribution. Mathematical 
  conversion shortcuts have also been added, so `exp(dist_normal())` produces
  `dist_lognormal()`.

### Generics

* Added `parameters()` generic for obtaining the distribution's parameters.
* Added `family(<distribution>)` for getting the distribution's family name.
* Added `covariance()` to return the covariance of a distribution.
* Added `support()` to identify the distribution's region of support (#8).
* Added `log_likelihood()` for computing the log-likelihood of observing a 
  sample from a distribution.
  
## Improvements

* `variance()` now always returns a variance. It will not default to providing
  a covariance matrix for matrices. This also applies to multivariate 
  distributions such as `dist_multivariate_normal()`. The covariance can now
  be obtained using the `covariance()` function.
* `dist_wrap()` can now search for distribution functions in any environment,
  not just packages. If the `package` argument is `NULL`, it will search the
  calling environment for the functions. You can also provide a package name as
  before, and additionally an arbitrary environment to this argument.
* `median()` methods will now ignore the `na.rm` option when it does not apply
  to that distribution type (#72).
* `dist_sample()` now allows for missing values to be stored. Note that 
  `density()`, `quantile()` and `cdf()` will remove these missing values by
  default. This behaviour can be changed with the `na.rm` argument.
* `<hilo>` objects now support non-numeric and multivariate distributions. 
  `<hilo>` vectors that have different bound types cannot be mixed (#74).
* Improved performance of default methods of `mean()` and `variance()`, which
  no longer use sampling based means and variances for univariate continuous
  distributions (#71, @mjskay)
* `dist_binomial()` distributions now return integers for `quantile()` and
  `generate()` methods.
* Added conditional examples for distributions using functions from supported
  packages.

## Bug fixes

* Fixed fallback `format()` function for distributions classes that have not
  defined this method (#67).

## Breaking changes

* `variance()` on a `dist_multivariate_normal()` will now return the diagonal
  instead of the complete variance-covariance matrix.
* `dist_bernoulli()` will now return logical values for `quantile()` and 
  `generate()`.

# distributional 0.2.2

## New features

* Added `is_distribution()` to identify if an object is a distribution.

## Improvements

* Improved NA structure of distributions, allowing it to work with `is.na()` and
  `vctrs` vector resizing / filling functionality.
* Added `as.character(<hilo>)` method, allowing datasets containing `hilo()`
  objects to be saved as a text file (#57).

## Bug fixes

* Fixed issue with `hdr()` range `size` incorrectly being treated as `100-size`,
  giving 5% ranges for 95% sizes and vice-versa (#61).

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
