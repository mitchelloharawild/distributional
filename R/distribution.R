#' Construct distributions
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' Allows extension package developers to define a new distribution class
#' compatible with the distributional package.
#'
#' @param ... Parameters of the distribution (named).
#' @param class The class of the distribution for S3 dispatch.
#' @param dimnames The names of the variables in the distribution (optional).
#'
#' @export
new_dist <- function(..., class = NULL, dimnames = NULL){
  # `...` is passed straight through (not materialised via `list(...)` first)
  # so that `rlang`'s `!!!` splicing - used by e.g. `dist_wrap()` to forward
  # its recycled parameter list - is resolved by `vec_recycle_common()`'s own
  # dots capture, rather than being evaluated here as literal `!(!(!x))`.
  args <- vctrs::vec_recycle_common(...)
  if (!length(args)) {
    # No fields given: this is a bare, zero-length prototype (used all over
    # the family files for `vec_is(dist, new_dist())`-style type-checks)
    # rather than the construction of an actual distribution.
    return(class_distribution())
  }
  # `vctrs::new_rcrd()` fields must be proper vectors, so a literal `NULL`
  # argument (a handful of families use it as an "unset" sentinel for an
  # optional parameter, e.g. `dist_student_t(ncp = NULL)`) can't be stored
  # directly; recycle it to a `NULL`-per-position list field instead.
  is_null_arg <- vapply(args, is.null, logical(1L))
  if (any(is_null_arg)) {
    n <- if (all(is_null_arg)) 1L else vctrs::vec_size(args[!is_null_arg][[1L]])
    args[is_null_arg] <- lapply(args[is_null_arg], function(z) vector("list", n))
  }
  slot <- vctrs::new_rcrd(args, class = c(class, "dist_default"))
  if (!is.null(dimnames)) attr(slot, "vars") <- dimnames
  class_distribution(list(slot))
}

# ------------------------------------------------------------------------
# Family record field-access shims
# ------------------------------------------------------------------------
# A distribution family is stored as one `vctrs_rcrd` "slot" holding every
# distribution of that family in the vector (rather than one S3 object per
# distribution, as before the vecvec migration - see `_dev/vecvec.md`).
# `x[["par"]]`/`x$par` (used throughout the `dist_*` family files) therefore
# now return a *vector* of `length(x)` parameter values rather than a scalar.
#
# List-valued fields (e.g. `dist_transformed()`'s `transform`/`inverse`
# closures, or ragged parameters) are always returned as a list with one
# entry per distribution, whatever the size of `x`, so a method never needs to
# special-case the single-distribution slot.

#' @export
#' @method [[ dist_default
`[[.dist_default` <- function(x, i, ...) {
  if (is.character(i)) {
    dist_field_get(x, i)
  } else {
    NextMethod()
  }
}
#' @export
`$.dist_default` <- function(x, name) {
  dist_field_get(x, name)
}
# `x$field`/`x[["field"]]` on a plain-list S3 object (the pre-migration
# per-element representation) silently returned NULL for a name that wasn't
# present (base list `$` semantics), which family Ops/Math methods rely on to
# probe for fields that only exist on *some* families (e.g. `dist_default`
# methods testing `e1$dist` to detect a nested-distribution operand).
# `vctrs::field()` errors instead, so guard it here to keep that behaviour.
dist_field_get <- function(x, name) {
  if (!(name %in% vctrs::fields(x))) return(NULL)
  vctrs::field(x, name)
}

# Pre-migration `x[-(1:2)]`-style field dropping (e.g. `dist_wrap()` removing
# its `dist`/`env` bookkeeping fields to get the wrapped distribution's own
# parameters) relied on the raw record being a plain named list, where `[`
# subsets *fields*. `vctrs_rcrd`'s `[` subsets *elements* instead, so this
# helper gives the fields-as-named-list view directly.
dist_fields_except <- function(x, exclude) {
  flds <- setdiff(vctrs::fields(x), exclude)
  stats::setNames(lapply(flds, function(f) dist_field_get(x, f)), flds)
}
#' @export
#' @method [[<- dist_default
`[[<-.dist_default` <- function(x, i, value) {
  vctrs::field(x, i) <- value
  x
}
#' @export
`$<-.dist_default` <- function(x, name, value) {
  vctrs::field(x, name) <- value
  x
}

# ------------------------------------------------------------------------
# Compatibility helpers used by family constructors/methods
# ------------------------------------------------------------------------

# Extract the raw (family-level) record for a single logical position of a
# `distribution` vector - the closest equivalent to the pre-migration
# `vec_data(x)[[i]]`. Used by nested-distribution constructors
# (`dist_convolved()`, `dist_mixture()`) that need to build up per-element
# component lists. Returns a `dist_na` placeholder for an NA (missing) index,
# matching the pre-migration behaviour of patching NULL elements.
dist_elem <- function(x, i) {
  xi <- x[i]
  if (length(xi@x) == 0L) {
    structure(list(), class = c("dist_na", "dist_default"))
  } else {
    xi@x[[1L]]
  }
}
dist_first_record <- function(x) dist_elem(x, 1L)

# Call the generic `.f` on the `distribution` vector `x` with its first
# argument in `...` (for example `q = q`) paired with the distributions by
# position, as the family methods receive their own broadcast argument. Other
# arguments in `...` are passed unchanged. The argument has size `vec_size(x)`
# or 1. Unlike `cdf(x, q)`, which would map a size-`n` `q` over all `n`
# distributions, this gives one result for each distribution. `.ptype` is the
# result type used when there are no distributions to compute (see
# `apply_slots()`).
#
# Nested families (truncated, inflated, ...) store their inner distributions as
# a `distribution` vector field, so `dist_paired(cdf, x[["dist"]], q = q)` calls
# the inner family methods once per family with all of their distributions.
dist_paired <- function(.f, x, ..., .ptype = NULL) {
  args <- list2(...)
  apply_slots(x, .f, args, 1L, .ptype)
}

# Combine a list of raw (family-level) records into a `distribution` vector -
# the inverse of `dist_elem()`. Used by `Ops.dist_default()`/
# `Math.dist_default()` (default.R) which build a `dist_transformed()`/
# `dist_convolved()` result from one or two single-element operands.
wrap_dist <- function(x, dimnames = NULL){
  out <- do.call(c, lapply(x, function(z) class_distribution(list(z))))
  if (!is.null(dimnames)) variates(out) <- dimnames
  out
}

# vctrs finds these by the first class only, which is the S7 class name
# (registered in .onLoad() in zzz.R)
vec_ptype_abbr_distribution <- function(x, ...){
  "dist"
}

# vecvec's own `format.vecvec::vecvec()` default calls `dim(x)`/`dimnames(x)`
# to shape its result as an array - but `dimnames.distribution()` returns
# *variate names* (a character vector), not array dimnames (a list), so
# letting that default run corrupts `format()` for any distribution with
# variate names set (`dimnames<-`/`variates<-`). Define our own instead,
# which never touches array shape - matching the pre-migration behaviour,
# where `format()` was always just per-element character formatting.
#' @export
format.distribution <- function(x, ...) {
  apply_dist(x, format, ..., .ptype = character())
}
vec_ptype_full_distribution <- function(x, ...){
  "distribution"
}

#' @importFrom pillar pillar_shaft new_pillar_shaft get_max_extent
#' @export
pillar_shaft.distribution <- function(x, ...) {
  dist = format(x)
  dist_min = format(x, digits = 1)

  pillar::new_pillar_shaft(
    list(dist = dist,
         dist_min = dist_min),
    width = pillar::get_max_extent(dist),
    min_width = pillar::get_max_extent(dist_min),
    class = "pillar_distribution"
  )
}

#' @export
#' @importFrom pillar new_ornament
format.pillar_distribution <- function(x, width, ...) {
  if (get_max_extent(x$dist) <= width) {
    ornament <- x$dist
  } else {
    ornament <- x$dist_min
  }

  pillar::new_ornament(ornament, align = "right")
}

# ------------------------------------------------------------------------
# Variate names ("dimnames" pre-migration)
# ------------------------------------------------------------------------
# vecvec uses dim()/dimnames() for array shape, so the pre-migration meaning
# (names of a multivariate distribution's variates) moves to variates().
# `dimnames()`/`dimnames<-()` on a distribution become a deprecated shim,
# supported only while the vector has a single family slot. See
# `_dev/vecvec.md` #4.

#' Names of a multivariate distribution's variates
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param x The distribution(s).
#' @param value A character vector of variate names.
#'
#' @export
variates <- function(x) {
  # Variate names live on the (sole) family slot; a mixed-family vector has
  # no single place to store them, so it simply has none.
  if (length(x@x) != 1L) return(NULL)
  attr(x@x[[1L]], "vars")
}
#' @rdname variates
#' @export
`variates<-` <- function(x, value) {
  if (length(x@x) > 1L) {
    abort("`variates<-()` currently only supports distributions with a single family.")
  }
  if (length(x@x) == 1L) attr(x@x[[1L]], "vars") <- value
  x
}

#' @export
`dimnames<-.distribution` <- function(x, value){
  lifecycle::deprecate_soft("0.9.0", "`dimnames<-.distribution`()", "`variates<-`()")
  variates(x) <- value
  x
}
#' @export
dimnames.distribution <- function(x){
  lifecycle::deprecate_soft("0.9.0", "dimnames.distribution()", "variates()")
  variates(x)
}

#' @method [[ distribution
#' @export
`[[.distribution` <- function(x, i, ...) x[i, ...]

# vecvec stores the index as the S7 object's underlying data (`class_vecvec`
# has `parent = S7::class_integer`), so `typeof(x) == "integer"` and base R's
# `is.numeric()` (which falls back to the base type when no method matches)
# would otherwise report a `distribution` as numeric - which, among other
# things, makes `testthat`'s (2e) `all.equal()`-based `expect_equal()` try to
# subtract/compare distributions as if they were plain numbers. Restore the
# pre-migration `FALSE`.
#' @export
is.numeric.distribution <- function(x) FALSE

# Before the vecvec migration, `distribution` was a plain list of per-element
# S3 list-objects, so base R's `all.equal.list()` recursed through it (and
# through each per-element object, itself a plain list) for free - which is
# what `expect_equal()` (testthat 2e) relies on. The vecvec-backed container
# no longer has that structure (its `typeof()` is "integer", not "list"), so
# provide an explicit method that compares element-by-element via each raw
# family record's fields (also plain lists once unclassed), matching the
# pre-migration comparison.
#' @export
all.equal.distribution <- function(target, current, ...) {
  if (!is_distribution(current)) {
    return(paste0("Modes: ", mode(target), ", ", mode(current)))
  }
  if (length(target) != length(current)) {
    return(sprintf("Lengths (%d, %d) differ", length(target), length(current)))
  }
  msgs <- character(0)
  for (i in seq_len(length(target))) {
    a <- dist_elem(target, i)
    b <- dist_elem(current, i)
    if (!identical(class(a), class(b))) {
      msgs <- c(msgs, sprintf("Element %d: family classes differ: %s, %s",
                               i, paste(class(a), collapse = "/"), paste(class(b), collapse = "/")))
      next
    }
    cmp <- all.equal(unclass(a), unclass(b), ...)
    if (!isTRUE(cmp)) msgs <- c(msgs, sprintf("Element %d: %s", i, paste(cmp, collapse = "; ")))
  }
  if (length(msgs) == 0L) TRUE else msgs
}

#' The probability density/mass function
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Computes the probability density function for a continuous distribution, or
#' the probability mass function for a discrete distribution.
#'
#' @param x The distribution(s).
#' @param at The point at which to compute the density/mass.
#' @param ... Additional arguments passed to methods.
#' @param log If `TRUE`, probabilities will be given as log probabilities.
#'
#' @seealso `vignette("broadcasting", package = "distributional")` for how the
#' `at` argument is combined with the distributions.
#'
#' @importFrom stats density
#' @export
density.distribution <- function(x, at, ..., log = FALSE){
  # Validated before log_density(), which is one call further from the user,
  # so that the deprecation for vector `at` is reported in the user's frame
  at <- validate_arg_dim(x, at, arg = "at")
  if(log) return(log_density(x, at, ...))
  at <- broadcast_arg(at, arg = "at")
  broadcast_dist(x, density, at = at, ..., .ptype = double())
}

log_density <- function(x, at, ...) {
  UseMethod("log_density")
}
#' @export
log_density.distribution <- function(x, at, ...){
  at <- broadcast_arg(validate_arg_dim(x, at, arg = "at"), arg = "at")
  broadcast_dist(x, log_density, at = at, ..., .ptype = double())
}

#' Distribution Quantiles
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Computes the quantiles of a distribution.
#'
#' @inheritParams density.distribution
#' @param p The probability of the quantile.
#' @param ... Additional arguments passed to methods.
#' @param tail Which tail `p` is the probability of: `"lower"` (the default),
#' \eqn{P(X \le x)}, or `"upper"`, \eqn{P(X > x)}. So `quantile(x, p, tail = "upper")` equals
#' `quantile(x, 1 - p)` without the loss of precision from computing `1 - p`.
#'
#' @seealso `vignette("broadcasting", package = "distributional")` for how the
#' `p` argument is combined with the distributions.
#'
#' @examples
#' # Upper tail quantiles
#' quantile(dist_normal(), 0.025, tail = "upper")
#' quantile(dist_normal(), 1e-20, tail = "upper")
#'
#' @importFrom stats quantile
#' @export
quantile.distribution <- function(x, p, ..., tail = c("lower", "upper"), log = FALSE){
  tail <- match.arg(tail)
  if(log) return(log_quantile(x, p, ..., tail = tail))
  p <- broadcast_arg(p, .ptype = double(), arg = "p")
  broadcast_dist(x, quantile, p = p, ..., tail = tail, .ptype = double())
}
log_quantile <- function(x, p, ...) {
  UseMethod("log_quantile")
}
#' @export
log_quantile.distribution <- function(x, p, ...){
  p <- broadcast_arg(p, .ptype = double(), arg = "p")
  broadcast_dist(x, log_quantile, p = p, ..., .ptype = double())
}

#' The cumulative distribution function
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams density.distribution
#' @param q The quantile at which the cdf is calculated.
#' @param tail Which tail the probabilities are of: `"lower"` (the default),
#' \eqn{P(X \le q)}, or `"upper"`, \eqn{P(X > q)}, which is the survival
#' function. Where possible the upper
#' tail is computed directly, so it keeps its precision where
#' `1 - cdf(x, q)` would round to zero. For multivariate distributions the
#' upper tail is the complement \eqn{1 - P(X \le q)}, not the joint survival
#' function.
#'
#' @seealso `vignette("broadcasting", package = "distributional")` for how the
#' `q` argument is combined with the distributions.
#'
#' @examples
#' # The survival function keeps its precision far into the upper tail
#' cdf(dist_normal(), 10, tail = "upper")
#' 1 - cdf(dist_normal(), 10)
#'
#' # The cumulative hazard function, -log(S(q))
#' -cdf(dist_weibull(2, 1), 3, tail = "upper", log = TRUE)
#'
#' @name cdf
#' @export
cdf <- function (x, q, ..., tail = c("lower", "upper"), log = FALSE){
  tail <- match.arg(tail)
  if(log) return(log_cdf(x, q, ..., tail = tail))
  UseMethod("cdf")
}
#' @rdname cdf
#' @export
cdf.distribution <- function(x, q, ..., tail = c("lower", "upper")){
  tail <- match.arg(tail)
  q <- broadcast_arg(validate_arg_dim(x, q, arg = "q"), arg = "q")
  broadcast_dist(x, cdf, q = q, ..., tail = tail, .ptype = double())
}
log_cdf <- function(x, q, ...) {
  UseMethod("log_cdf")
}
#' @export
log_cdf.distribution <- function(x, q, ...){
  q <- broadcast_arg(validate_arg_dim(x, q, arg = "q"), arg = "q")
  broadcast_dist(x, log_cdf, q = q, ..., .ptype = double())
}

#' Randomly sample values from a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Generate random samples from probability distributions.
#'
#' @param x The distribution(s).
#' @param times The number of samples. A scalar is broadcast to every
#' distribution. Per-distribution sample sizes must be given as a list
#' containing a single vector (`list(c(...))`) recycled against the
#' distributions (length 1 or `length(x)`) - a bare vector of length > 1 is
#' deprecated for this, as it is ambiguous with mapping the same vector onto
#' every distribution.
#' @param ... Additional arguments used by methods.
#'
#' @seealso `vignette("broadcasting", package = "distributional")` for how the
#' `times` argument is combined with the distributions.
#'
#' @export
generate.distribution <- function(x, times, ...){
  if (is.list(times)) {
    if (length(times) != 1L) {
      abort("`times` must be a scalar, a vector, or a list containing a single vector of per-distribution values (such as `times = list(c(...))`).")
    }
    times <- times[[1L]]
  } else if (length(times) > 1L) {
    lifecycle::deprecate_warn(
      when = "0.9.0",
      what = I("Using a vector for the `times` argument of `generate()`"),
      with = I("a list (such as `times = list(<numeric[n]>)`) for sample sizes per distribution")
    )
  }
  times <- vec_cast(times, integer())
  times <- vec_recycle(times, size = length(x))

  out <- dist_paired(generate, x, times = times, ..., .ptype = list())
  set_variate_names(out, variates(x))
}

#' The (log) likelihood of a sample matching a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @name likelihood
#' @export
likelihood <- function (x, ...){
  UseMethod("likelihood")
}

#' @rdname likelihood
#' @param sample A list of samples to compare to the distribution(s), recycled
#' against the distributions (length 1 or `length(x)`) so that each sample is
#' compared with its distribution by position.
#' @param log If `TRUE`, the log-likelihood will be computed.
#' @export
likelihood.distribution <- function(x, sample, ..., log = FALSE){
  if(vec_is(sample, numeric())) {
    warn("The `sample` argument of `likelihood()` should contain a list of numbers.
The same sample will be used for each distribution, i.e. `sample = list(sample)`.")
    sample <- list(sample)
  }
  if(log) return(log_likelihood(x, sample, ...))
  # Each sample is paired with a distribution by position (or recycled)
  sample <- vec_recycle(sample, length(x), x_arg = "sample")
  dist_paired(likelihood, x, sample = sample, ..., .ptype = double())
}

#' @rdname likelihood
#' @export
log_likelihood <- function(x, ...) {
  UseMethod("log_likelihood")
}
#' @export
log_likelihood.distribution <- function(x, sample, ...){
  sample <- vec_recycle(sample, length(x), x_arg = "sample")
  dist_paired(log_likelihood, x, sample = sample, ..., .ptype = double())
}

#' Extract the parameters of a distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Some distributions (such as [dist_inflated()] and [dist_transformed()])
#' are defined in terms of another distribution, which returns a distribution
#' as one of their parameters. By default, this underlying distribution is 
#' returned as-is in a column of the `parameters()` result. Setting 
#' `recursive = TRUE` will recursively compute the parameters of these inner
#' distributions, returning a flat data frame of all parameters.
#'
#' @param x The distribution(s).
#' @param recursive If `TRUE`, parameters which are themselves
#' distributions (such as the base distribution of a
#' [dist_inflated()] or [dist_transformed()]) are recursively expanded
#' into their own parameters instead of being returned as a distribution.
#' @param ... Additional arguments used by methods.
#'
#' @return A data frame of parameters, with one row per distribution and
#' one column per parameter. The result never contains data frame (or
#' distribution) columns.
#'
#' @name parameters
#' @examples
#' dist <- c(
#'   dist_normal(1:2),
#'   dist_poisson(3),
#'   dist_multinomial(size = c(4, 3),
#'   prob = list(c(0.3, 0.5, 0.2), c(0.1, 0.5, 0.4)))
#'   )
#' parameters(dist)
#'
#' # Distribution-valued parameters (such as the inflated distribution
#' # below) are returned as-is by default.
#' infl_dist <- dist_inflated(dist_negative_binomial(10, 0.6), prob = 0.5)
#' parameters(infl_dist)
#'
#' # With recursive = TRUE, the inflated distribution's parameters are
#' # expanded into a flat data frame.
#' parameters(infl_dist, recursive = TRUE)
#' @export
parameters <- function(x, ...) {
  UseMethod("parameters")
}

#' @rdname parameters
#' @export
parameters.distribution <- function(x, recursive = FALSE, ...) {
  loc <- locate_slots(x)
  if (!length(loc$slots)) return(vctrs::new_data_frame(list(), n = loc$n))

  rows <- vector("list", loc$n)
  for (k in seq_along(loc$slots)) {
    pos <- loc$pos[[k]]
    if (!length(pos)) next
    vals <- loc$slots[[k]][loc$local[pos]]
    rows[pos] <- vctrs::vec_chop(parameters(vals))
  }
  present <- !vapply(rows, is.null, logical(1L))
  x <- vec_rbind(!!!rows[present])
  if (recursive) {
    # Identify distributions
    dist_col <- which(vapply(x, inherits, logical(1L), what = "distribution"))
    # Recurse parameters()
    x[dist_col] <- lapply(x[dist_col], parameters, recursive = TRUE)
    # Flatten result
    x <- unpack(x, dist_col)
  }
  new_data_frame(x)
}

#' Extract the name of the distribution family
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Some distributions (such as [dist_mixture()], [dist_convolved()],
#' [dist_inflated()] and [dist_transformed()]) are defined in terms of one or
#' more other distributions. By default, `family()` only identifies the
#' outermost distribution. Setting `recursive = TRUE` will also identify the
#' families of these embedded distributions, returning a nested list
#' describing the full family tree of each distribution.
#'
#' @param object The distribution(s).
#' @param recursive If `TRUE`, the families of any distributions embedded
#' within `object` (such as the components of a [dist_mixture()], or the
#' base distribution of a [dist_inflated()] or [dist_transformed()]) are
#' also identified, and returned as a nested list instead of a flat
#' character vector.
#' @param ... Additional arguments used by methods.
#'
#' @return If `recursive = FALSE` (the default), a character vector giving
#' the family name of each distribution. If `recursive = TRUE`, a list with
#' one element per distribution: a distribution with no embedded
#' distributions is represented by a character scalar (as before), while a
#' distribution built from others is represented by a named list with a
#' `family` element and one element per embedded distribution (containing
#' its recursive `family()` result, or a list of such results if that
#' element holds more than one embedded distribution).
#'
#' @examples
#' dist <- c(
#'   dist_normal(1:2),
#'   dist_poisson(3),
#'   dist_multinomial(size = c(4, 3),
#'   prob = list(c(0.3, 0.5, 0.2), c(0.1, 0.5, 0.4)))
#'   )
#' family(dist)
#'
#' # Distributions composed of other distributions (such as the mixture
#' # below) only identify the outer family by default.
#' mix_dist <- dist_mixture(dist_normal(0, 1), dist_normal(5, 2),
#'   weights = c(0.3, 0.7))
#' family(mix_dist)
#'
#' # With recursive = TRUE, the families of the mixture's components are
#' # also identified.
#' family(mix_dist, recursive = TRUE)
#'
#' @importFrom stats family
#' @export
family.distribution <- function(object, recursive = FALSE, ...) {
  loc <- locate_slots(object)
  if (recursive) {
    # Family trees are ragged, so describe each distribution individually.
    out <- rep(list(NA_character_), loc$n)
    for (k in seq_along(loc$slots)) {
      pos <- loc$pos[[k]]
      vals <- loc$slots[[k]][loc$local[pos]]
      for (j in seq_along(pos)) {
        out[[pos[j]]] <- family(vals[j], recursive = TRUE)
      }
    }
    return(out)
  }
  out <- rep(NA_character_, loc$n)
  for (k in seq_along(loc$slots)) {
    pos <- loc$pos[[k]]
    if (length(pos)) out[pos] <- family(loc$slots[[k]][loc$local[pos]])
  }
  out
}

#' Region of support of a distribution
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @name support
#' @export
support <- function(x, ...) {
  UseMethod("support")
}

#' @rdname support
#' @export
support.distribution <- function(x, ...) {
  apply_dist(x, support, ..., .ptype = new_support_region())
}

#' Mean of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the empirical mean of the probability distribution. If the method
#' does not exist, the mean of a random sample will be returned.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
mean.distribution <- function(x, ...){
  apply_dist(x, mean, ..., .ptype = double())
}

#' Variance
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A generic function for computing the variance of an object.
#'
#' @param x An object.
#' @param ... Additional arguments used by methods.
#'
#' @details
#'
#' The implementation of `variance()` for numeric variables coerces the input to
#' a vector then uses [`stats::var()`] to compute the variance. This means that,
#' unlike [`stats::var()`], if `variance()` is passed a matrix or a 2-dimensional
#' array, it will still return the variance ([`stats::var()`] returns the
#' covariance matrix in that case).
#'
#' @seealso [`variance.distribution()`], [`covariance()`]
#'
#' @export
variance <- function(x, ...){
  UseMethod("variance")
}
#' @export
variance.default <- function(x, ...){
  stop(
    "The variance() method is not supported for objects of type ",
    paste(deparse(class(x)), collapse = "")
  )
}
#' @rdname variance
#' @export
variance.numeric <- function(x, ...){
  stats::var(as.vector(x), ...)
}
#' @rdname variance
#' @export
variance.matrix <- function(x, ...){
  diag(stats::cov(x, ...))
}

#' Variance of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the empirical variance of the probability distribution. If the method
#' does not exist, the variance of a random sample will be returned.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
variance.distribution <- function(x, ...){
  apply_dist(x, variance, ..., .ptype = double())
}

#' Covariance
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A generic function for computing the covariance of an object.
#'
#' @param x An object.
#' @param ... Additional arguments used by methods.
#'
#' @seealso [`covariance.distribution()`], [`variance()`]
#'
#' @export
covariance <- function(x, ...){
  UseMethod("covariance")
}
#' @export
covariance.default <- function(x, ...){
  stop(
    "The covariance() method is not supported for objects of type ",
    paste(deparse(class(x)), collapse = "")
  )
}
#' @rdname variance
#' @export
covariance.numeric <- function(x, ...){
  stats::cov(x, ...)
}
#' Covariance of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the empirical covariance of the probability distribution. If the
#' method does not exist, the covariance of a random sample will be returned.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
covariance.distribution <- function(x, ...){
  apply_dist(x, covariance, ..., .ptype = list())
}

#' Skewness of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
skewness <- function(x, ...) {
  UseMethod("skewness")
}
#' @rdname skewness
#' @export
skewness.distribution <- function(x, ...){
  apply_dist(x, skewness, ..., .ptype = double())
}

#' Kurtosis of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @export
kurtosis <- function(x, ...) {
  UseMethod("kurtosis")
}
#' @rdname kurtosis
#' @export
kurtosis.distribution <- function(x, ...){
  apply_dist(x, kurtosis, ..., .ptype = double())
}

#' Median of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns the median (50th percentile) of a probability distribution. This is
#' equivalent to `quantile(x, p=0.5)`.
#'
#' @param x The distribution(s).
#' @param na.rm Unused, included for consistency with the generic function.
#' @param ... Additional arguments used by methods.
#'
#' @importFrom stats median
#' @export
median.distribution <- function(x, na.rm = FALSE, ...){
  apply_dist(x, median, na.rm = na.rm, ..., .ptype = double())
}

#' Probability intervals of a probability distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Returns a `hilo` central probability interval with probability coverage of
#' `size`. By default, the distribution's [`quantile()`] will be used to compute
#' the lower and upper bound for a centered interval
#'
#' @param x The distribution(s).
#' @param size The size of the interval (between 0 and 100).
#' @param ... Additional arguments used by methods.
#'
#' @seealso [`hdr.distribution()`]
#'
#' @importFrom stats median
#' @export
hilo.distribution <- function(x, size = 95, ...){
  size <- broadcast_arg(size, .ptype = double(), arg = "size")
  broadcast_dist(x, hilo, size = size, ..., .ptype = new_hilo())
}

#' Highest density regions of probability distributions
#'
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' This function is highly experimental and will change in the future. In
#' particular, improved functionality for object classes and visualisation tools
#' will be added in a future release.
#'
#' Computes minimally sized probability intervals highest density regions.
#'
#' @param x The distribution(s).
#' @param size The size of the interval (between 0 and 100).
#' @param n The resolution used to estimate the distribution's density.
#' @param ... Additional arguments used by methods.
#'
#' @export
hdr.distribution <- function(x, size = 95, n = 512, ...){
  size <- broadcast_arg(size, .ptype = double(), arg = "size")
  broadcast_dist(x, hdr, size = size, n = n, ..., .ptype = new_hdr())
}

#' @export
sum.distribution <- function(x, ...){
  Reduce(`+`, x)
}

# ------------------------------------------------------------------------
# Ops / Math (container level)
# ------------------------------------------------------------------------
# Arithmetic dispatches element-by-element to the family-level `Ops.dist_*`/
# `Math.dist_*` methods. Unlike the stats generics (`apply_dist()`), these are
# not yet vectorised per storage slot.

#' @export
Ops.distribution <- function(e1, e2) {
  if (missing(e2)) {
    n <- length(e1)
    out <- lapply(seq_len(n), function(i) get(.Generic)(dist_elem(e1, i)))
    return(dist_combine_ops(out))
  }

  dist_e1 <- is_distribution(e1)
  dist_e2 <- is_distribution(e2)
  n <- max(length(e1), length(e2))
  e1 <- vctrs::vec_recycle(e1, n)
  e2 <- vctrs::vec_recycle(e2, n)

  is_bool <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = , `<=` = , `>=` = TRUE, FALSE)

  out <- vector("list", n)
  for (i in seq_len(n)) {
    a <- if (dist_e1) dist_elem(e1, i) else e1[i]
    b <- if (dist_e2) dist_elem(e2, i) else e2[i]
    out[[i]] <- get(.Generic)(a, b)
  }
  if (is_bool) return(unlist(out))
  dist_combine_ops(out)
}

dist_combine_ops <- function(out) {
  if (!length(out)) return(logical())
  do.call(c, lapply(out, function(z) class_distribution(list(z))))
}

#' @export
Math.distribution <- function(x, ...) {
  n <- length(x)
  out <- lapply(seq_len(n), function(i) get(.Generic)(dist_elem(x, i), ...))
  dist_combine_ops(out)
}

# `is.nan()`/`is.infinite()`/`is.finite()` are their own S3 generics, not
# members of the base R "Math" group (unlike pre-migration, where the old
# `vctrs_vctr`-based `distribution` routed them through vctrs' *own*
# `vec_math()` protocol into `vec_math.distribution()`). Left unhandled here,
# they fall through to `` `is.infinite.vecvec::vecvec` `` etc., which apply
# per storage slot - and a raw family record (a `vctrs_rcrd`) has no
# `vec_math()` support, so that errors. A distribution is never NaN/infinite
# and always finite, matching the old special-casing.
#' @export
is.nan.distribution <- function(x) rep_len(FALSE, length(x))
#' @export
is.infinite.distribution <- function(x) rep_len(FALSE, length(x))
#' @export
is.finite.distribution <- function(x) rep_len(TRUE, length(x))

# ------------------------------------------------------------------------
# vctrs integration
# ------------------------------------------------------------------------

# vecvec's c() stores other inputs as raw slots, so cast them to distributions
# first (numbers become `dist_degenerate()`, `NA` a missing distribution).
# Called as a function, because `S7::method(c, ...) <-` would also assign a
# local `c` in the namespace, masking base::c() for the rest of the package.
S7::`method<-`(c, class_distribution, value = function(..., recursive = FALSE) {
  dots <- lapply(list2(...), vctrs::vec_cast, to = new_dist())
  inject(vecvec_c(!!!dots))
})

vec_ptype2_distribution <- function(x, y, ...) new_dist()
vec_cast_to_distribution <- function(x, to, ...) {
  if (is_distribution(x)) return(x)
  if (is.numeric(x)) return(dist_degenerate(x))
  vctrs::stop_incompatible_cast(x, to, x_arg = "", to_arg = "")
}
vec_cast_from_distribution <- function(x, to, ...) {
  if (is.character(to)) return(format(x))
  vctrs::stop_incompatible_cast(x, to, x_arg = "", to_arg = "")
}

#' Test if the object is a distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function returns `TRUE` for distributions and `FALSE` for all other objects.
#'
#' @param x An object.
#'
#' @return TRUE if the object inherits from the distribution class.
#' @rdname is-distribution
#' @examples
#' dist <- dist_normal()
#' is_distribution(dist)
#' is_distribution("distributional")
#' @export
is_distribution <- function(x) {
  inherits(x, "distribution")
}


#' Check if a distribution is symmetric
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Determines whether a probability distribution is symmetric around its center.
#'
#' @param x The distribution(s).
#' @param ... Additional arguments used by methods.
#'
#' @return A logical value indicating whether the distribution is symmetric.
#'
#' @examples
#' # Normal distribution is symmetric
#' has_symmetry(dist_normal(mu = 0, sigma = 1))
#' has_symmetry(dist_normal(mu = 5, sigma = 2))
#'
#' # Beta distribution symmetry depends on parameters
#' has_symmetry(dist_beta(shape1 = 2, shape2 = 2))  # symmetric
#' has_symmetry(dist_beta(shape1 = 2, shape2 = 5))  # not symmetric
#'
#' @export
has_symmetry <- function(x, ...) {
  UseMethod("has_symmetry")
}

#' @export
has_symmetry.distribution <- function(x, ...) {
  apply_dist(x, has_symmetry, ..., .ptype = logical())
}
