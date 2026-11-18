# distributional (development version)

## Improvements

* Distributions are now stored and computed on in a vectorised way using the
  vecvec package. A vector of distributions is held as one record vector per
  family, and every method (`density()`, `cdf()`, `quantile()`, `generate()`,
  `format()`, moments, `support()`, arithmetic, and so on) handles all
  distributions of a family in one call rather than looping over them one at a
  time. This makes working with long vectors of distributions much faster.
* Arithmetic and mathematical functions on distributions (such as `x + 1`,
  `x * y` and `exp(x)`) are computed once for each family rather than once for
  each distribution.
* Arithmetic gives a missing result when either operand is missing, whichever
  side the missing distribution is on. `dist_normal(1:2) + dist_missing(2)` and
  `dist_missing(2) + dist_normal(1:2)` are both missing, where the first
  previously gave a convolution with a missing distribution.
* Arithmetic on an empty distribution vector now gives an empty distribution
  vector, such as `dist_normal(1:2)[0] + 1`.
* `as.character()` now works for distributions, and returns `format()`.
* The ordering operators `<`, `<=`, `>` and `>=` between a distribution and a
  number, or between two (independent) distributions, now give a Bernoulli
  distribution of the probability that the comparison is true (#16). For
  example, `mean(dist_normal() > 1.96)` is 0.025. These previously returned a
  transformed distribution. The difference between `>=` and `>` (and `<` and
  `<=`) is the probability mass at the number, including the point masses of
  mixed distributions such as `dist_inflated(dist_normal(), 0.2)` and of
  mixtures of continuous and discrete distributions. As in `ppois()`, a number
  within rounding error of an integer is treated as that integer, so that
  `0.1 * dist_poisson(3) >= 0.3` includes the mass at 3. See
  `?Ops.distribution`.
* `&`, `|` and `!` now combine Bernoulli distributions (such as the results of
  comparisons) as independent events, with logicals treated as certain events.
  For example `(x > 0) & (y > 0)` is a Bernoulli distribution of
  P(X > 0) P(Y > 0).
* `==` and `!=` now compare distributions by family and parameters, and give a
  logical vector. A number is compared as a `dist_degenerate()`, so
  `dist_degenerate(1) == 1` is `TRUE` and `dist_normal(1) == 1` is `FALSE`.
  They previously returned a transformed distribution. See
  `?Ops.distribution`.
* `c()` of many distributions is faster, and distributions from the same family
  are stored together even when they are not adjacent.
* `cdf()` and `quantile()` gain a `tail` argument (`"lower"` or `"upper"`).
  `cdf(x, q, tail = "upper")` gives the survival function P(X > q), and can be
  combined with `log = TRUE` (for example `-cdf(x, q, tail = "upper", log = TRUE)`
  is the cumulative hazard). Upper tail probabilities keep their precision
  where `1 - cdf()` would round to 0.
* Added closed-form `support()` methods for all parametric distributions,
  avoiding the slower numerical default that evaluates `quantile()`,
  `density()` and `generate()`. Degenerate parameterisations (such as
  `dist_binomial(n, 0)`, `dist_normal(mu, 0)` and `dist_uniform(a, a)`) now
  report a single point of support, and the closed upper bound of
  `dist_gev()` with `shape <= -1` is now identified.
* Added `support()` methods for `dist_truncated()` (the base support
  intersected with `[lower, upper]`), `dist_inflated()` (the base support
  widened to include the inflation point, keeping integer support when
  inflating at an integer) and `dist_missing()`.

## Breaking changes

* Points (`at`, `q`) for multivariate distributions must now be a matrix with
  one column for each variate (such as `cdf(x, cbind(4, 4))`), or a list of
  such matrices. Bare vectors, deprecated in 0.9.0, are now an error, as is a
  matrix with the wrong number of columns or points for distributions with
  different numbers of variates.
* The class of a distribution is now an S7 class, so `class(x)` is
  `c("distributional::distribution", "distribution", "vecvec::vecvec", "double", "S7_object")`.
  `inherits(x, "distribution")` and `is_distribution()` still work, but code
  that compares the whole class vector should be checked.
* `vctrs::vec_data()` and `unclass()` no longer give a list with one object for
  each distribution. Use `as.list()` to get the distributions individually.
* `dimnames()` and `dimnames<-()` for distributions are deprecated in favour of
  `variates()` and `variates<-()`.
* `parameters()` now returns one row for every distribution, with `NA` in the
  rows of missing distributions and in the columns of other families.
  Previously it errored when the vector contained a missing distribution.
* `min()`, `max()`, `range()` and `prod()` of distributions now give an error.
  They previously returned a distribution (for example `min(x)` returned the
  first element). The cumulative functions `cumsum()`, `cumprod()`, `cummax()`
  and `cummin()` also give an error, where `cumsum()` previously returned a
  transformed distribution. `sum()` is unchanged.

### Breaking changes for extension packages

Packages that define their own distribution families with `new_dist()` need to
update their methods. `new_dist()` keeps its signature, and see `?new_dist`,
`?class_distribution`, `?variates` and `vignette("broadcasting")` for details.

* Methods for a family (such as `density.dist_myfamily()`) are now called with
  a record holding `n` distributions of that family, rather than a single
  distribution, and must return a result of size `n`: a vector, a matrix with
  `n` rows, a list of length `n`, or a record of size `n`. Never assume
  `n == 1`.
* `x[["par"]]` and `x$par` now return a vector of length `n` rather than a
  single value. Fields that are lists (such as closures or parameters which
  differ in length between distributions) are always returned as a list of
  length `n`. A record is not a list, so `unclass(x)`, `names(x)` and
  `lapply(x, ...)` no longer give the parameters.
* `generate()` methods are called with a vector of `n` sizes in `times`, and
  must return a list of `n` vectors.
* `Ops` and `Math` methods for a family receive whole records, with any numeric
  operand as a vector paired with the distributions by position, and must
  return a record.
* Families which wrap other distributions now store a `distribution` vector in
  their `dist` field, rather than a single `dist_*` object.

## Bug fixes

* The `skewness()` of `dist_beta()` now uses the correct formula.
* `dist_multinomial()` and `dist_dirichlet()` `quantile()` now return the
  marginal quantiles of each variate.
* `density()` of `dist_multinomial()` now returns 0 when the counts do not sum
  to `size`.
* `quantile(x, p, log = TRUE)` no longer errors with "`q` must be a vector,
  not a function". A leftover argument check validated `q` (resolving to
  `base::q()`) instead of `p`.
* `dist_truncated()` now keeps the probability mass at the `lower` bound of
  discrete distributions, so that the truncated `density()`, `cdf()` and
  `quantile()` respect the documented closed support `[lower, upper]`.
  This now includes truncated zero-inflated count distributions.
  It also keeps the point mass at the `lower` bound of mixed distributions,
  so `cdf(dist_truncated(dist_inflated(dist_normal(), 0.2, 0), lower = 0), 0)`
  is now 1/3, where it was 0.
* `dist_gpd()` with `shape < 0` now has a consistent upper bound: `support()`
  only includes it when the density there is positive (`shape <= -1`), and
  `density()` now returns `1/scale` (`shape = -1`) or `Inf` (`shape < -1`)
  there instead of 0. The density near the upper bound is also computed
  without floating point cancellation.
* `cdf()` of a decreasing transformation of a discrete distribution now
  includes the probability mass at the threshold. `cdf(-dist_poisson(3), -2)`
  is now P(Y >= 2) = 0.80, where it was P(Y > 2) = 0.58.
* `density()` of a transformation of a discrete distribution is now the
  probability mass at the preimage, without the Jacobian of the inverse that
  is used for continuous distributions. `density(2 * dist_poisson(3), 4)` is
  now P(Y = 2) = 0.22, where it was half of that, and the `likelihood()` of
  transformed counts is now that of the counts.
* `quantile()` of a decreasing transformation of a discrete distribution is
  now correct at the probabilities of its support points, so that it inverts
  `cdf()`. At p = P(Y >= 2), `quantile(-dist_poisson(3), p)` is now -2, where
  it was -1.

## Other changes

* The `type` argument of the `quantile()` method for `dist_sample()` is now
  numeric (one of the `stats::quantile()` types, default `7`).
* Seeded random draws from `generate()` may differ from previous versions for
  `dist_gpd()`, `dist_mixture()`, `dist_truncated()` and `dist_inflated()`,
  because they now generate all of their draws in a single vectorised call.
* `generate()` methods for custom distribution classes must return a list with
  one set of draws per distribution (a single distribution used to return the
  draws directly).

# distributional 0.9.0

## New features

* Added a `vignette("broadcasting")` to describe how statistics apply to vectors
  of distributions differently based on the form of the statistical operation's
  argument (scalar, vector, or list).
  
* Added `dist_density()`, a distribution characterised by the values of its
  probability density function at a set of points. This is useful for
  representing a distribution known only through its density, such as a kernel
  density estimate or a posterior evaluated over a grid.

* Added `dist_cdf()`, which describes a distribution by the cumulative
  probabilities at a set of values. Interpolation is done linearly, and the
  results are equivalent to `dist_quantile()` with swapped arguments.

* Added `dist_tweedie()`, for the Tweedie compound Poisson-Gamma
  distribution with power parameter in `(1, 2)`, using the `tweedieDistr`
  package (@StefanoDamato).

* Added `recursive` option for `parameters()`, which recursively extracts the
  parameters of inner distributions to return a flat data frame (#161).

* Added `recursive` option for `family()`, which identifies the families of
  distributions embedded within another (e.g. `dist_mixture()`) returning a
  nested list describing the full family tree (#110).
  
## Breaking changes

* `generate()`'s `times` argument no longer accepts length-n vectors, and 
  instead requires `times = list(c(...))` for consistency with other operations.

# distributional 0.8.1

## Bug fixes

* Conditionalised `chooseOpsMethod()` S3 registration so the package can load
  on R versions before 4.3.0 (#157).

# distributional 0.8.0

## New features

* `dist_mixture()` gains a `type` argument supporting both probability mixtures
  (`type = "probability"`, the default, existing behaviour) and quantile
  mixtures (`type = "quantile"`). A quantile mixture (Vincentization) defines
  the quantile function as a weighted average of the component quantile
  functions: Q(p) = sum(w_i * Q_i(p)). Its CDF and density are computed
  numerically from this quantile function.

* Added `dist_convolved()` for the distribution of the sum of two independent
  random variables, computed via FFT-based numerical convolution (#106).
  The `+` and `-` operators now automatically produce a `dist_convolved` object
  for distribution families without a known closed-form sum. Distributions with 
  known closed-form sums (e.g. `dist_normal(1, 2) + dist_normal(3, 1)`) continue
  to produce exact results.

* Added `dist_quantile()` as the primary interface for the quantile/percentile 
  distribution, accepting probabilities on the \[0, 1\] scale. 
  `dist_percentile()` is now an alias that accepts the \[0, 100\] scale (#116).

# distributional 0.7.1

## Improvements

* Non-finite boundaries for truncated distributions now use open brackets.
* Added vectorised `p` support in `quantile()` for inflated distributions (#151).

## Bug fixes

* Fixed `density()` error when multivariate distribution parameters contain names (#150).

# distributional 0.7.0

## Improvements

* Fixed default argument `sigma` for `dist_multivariate_normal()` (#118).
* Improved formatting of support regions for limited multivariate support regions.

## New features

### Probability distributions

* Added `dist_dirichlet()` for the Dirichlet distribution (#146).
* Added `dist_horseshoe()` for the Horseshoe distribution.

# distributional 0.6.0

Extensively updated documentation for each distribution to include additional details about the statistics associated with each distribution.

## New features

* Added `has_symmetry()` generic to identify if a distribution is symmetric.

### Probability distributions

* Added `dist_laplace()` for the Laplace distribution.
* Added `dist_multivariate_t()` for the multivariate t-distribution.

## Improvements

* Extended documentation for many distributions to include detailed 
  mathematical formulas for the statistics of each distribution (#141).
* `hdr()` now produces exact results for symmetric distributions (#123).
* `hdr()` uses more quantiles by default (512 -> 4096) for more accurate HDRs (#123).
* Transformed distributions now work better for monotonic increasing functions 
  when the domain of the untransformed distribution is R (#129).
* Added `cdf()` and `quantile()` methods for dist_categorical() distributions with
  ordered factors (#136).
* Added `support()` method for `dist_gpd()` distributions.
* Added `cdf()` method for `dist_multinomial()` distributions.

## Bug fixes

* Fixed `dist_gamma()` not allowing the `scale` parameter to be used (#132).
* Fixed `dist_negative_binomial()` calculation of skewness and kurtosis for p!=0.5.
* Fixed floating point issue that caused `dist_mixture()` to incorrectly throw an error when weights did sum to 1 (@statasaurus, #134)

## Breaking changes

* The `type` argument for `dist_multivariate_normal()`'s `quantile()` method 
  has been renamed to `kind` to avoid name clashes with `quantile.default()`.

# distributional 0.5.0

## New features

* `support()` now shows whether the interval of support is open or 
  closed (@venpopov, #97).
* Added default method for `cdf()` which estimates the CDF using Monte Carlo
  integration (@robjhyndman, #122).

### Probability distributions

* Added `dist_gk()` for g-and-k distributions.
* Added `dist_gh()` for g-and-h distributions.
* Added `dist_gev()` for the Generalised Extreme Value distribution and
  `dist_gpd()` for the Generalised Pareto distribution (@robjhyndman, #124).

## Improvements

* `dist_mixture()` now displays the components of the mixture when the output
  width is sufficiently wide (@statasaurus, #112).
* `generate()` now respects `dimnames()` for multivariate distributions.
* `dist_mixture()` now supports multivariate distributions (@robjhyndman, #122).

## Bug fixes

* Fixed error when using '-' as a unary operator on a distribution different from
 `dist_normal()` (@venpopov, #95)
* Density for transformed distributions now correctly gives 0 instead of NaNs for 
  values outside the support of the distribution (@venpopov, #97)
* Fixed `quantile()` and `cdf()` for transformed distributions with 
  monotonically decreasing transformations (#100).
* Fixed multivariate `dist_sample()` methods not structuring multivariate 
  results correctly as matrices.
* The `cdf()` method for `dist_multivariate_normal()` now gives the P(X <= q)
  rather than P(X > q) for consistency with all other `cdf()` methods.
* The `quantile()` method for `dist_multivariate_normal()` now correctly gives
  the boundaries when `p=0` or `p=1` when `type="equicoordinate"`.
* The `quantile()` method for `dist_multivariate_normal()` now only square roots
  the diagonal elements when `type="marginal"`.

# distributional 0.4.0

## Breaking changes

* All graphics related functionality has been removed from the package in favour
  of the ggdist (https://cran.r-project.org/package=ggdist) package. This
  breaking change was done to substantially reduce the package's dependencies,
  focusing the functionality on representing vectors of distributions.

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
