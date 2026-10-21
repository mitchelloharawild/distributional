#' The Categorical distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Categorical distributions are used to represent events with multiple
#' outcomes, such as what number appears on the roll of a dice. This is also
#' referred to as the 'generalised Bernoulli' or 'multinoulli' distribution.
#' The Categorical distribution is a special case of the [Multinomial()]
#' distribution with `n = 1`.
#'
#' @param prob A list of probabilities of observing each outcome category.
#' @param outcomes The list of vectors where each value represents each outcome.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_categorical")`
#'
#'   In the following, let \eqn{X} be a Categorical random variable with
#'   probability parameters `prob` = \eqn{\{p_1, p_2, \ldots, p_k\}}.
#'
#'   The Categorical probability distribution is widely used to model the
#'   occurance of multiple events. A simple example is the roll of a dice, where
#'   \eqn{p = \{1/6, 1/6, 1/6, 1/6, 1/6, 1/6\}} giving equal chance of observing
#'   each number on a 6 sided dice.
#'
#'   **Support**: \eqn{\{1, \ldots, k\}}{{1, ..., k}}
#'
#'   **Mean**: Not defined for unordered categories. For ordered categories with
#'   integer outcomes \eqn{\{1, 2, \ldots, k\}}, the mean is:
#'
#'   \deqn{
#'     E(X) = \sum_{i=1}^{k} i \cdot p_i
#'   }{
#'     E(X) = sum(i * p_i) for i = 1 to k
#'   }
#'
#'   **Variance**: Not defined for unordered categories. For ordered categories
#'   with integer outcomes \eqn{\{1, 2, \ldots, k\}}, the variance is:
#'
#'   \deqn{
#'     \text{Var}(X) = \sum_{i=1}^{k} i^2 \cdot p_i - \left(\sum_{i=1}^{k} i \cdot p_i\right)^2
#'   }{
#'     Var(X) = sum(i^2 * p_i) - [sum(i * p_i)]^2
#'   }
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X = i) = p_i
#'   }{
#'     P(X = i) = p_i
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The c.d.f is undefined for unordered categories. For ordered categories
#'   with outcomes \eqn{x_1 < x_2 < \ldots < x_k}, the c.d.f is:
#'
#'   \deqn{
#'     P(X \le x_j) = \sum_{i=1}^{j} p_i
#'   }{
#'     P(X <= x_j) = sum(p_i) for i = 1 to j
#'   }
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \sum_{i=1}^{k} e^{tx_i} \cdot p_i
#'   }{
#'     E(e^(tX)) = sum(e^(t * x_i) * p_i) for i = 1 to k
#'   }
#'
#'   **Skewness**: Approximated numerically for ordered categories.
#'
#'   **Kurtosis**: Approximated numerically for ordered categories.
#'
#' @seealso [stats::Multinomial]
#'
#' @examples
#' dist <- dist_categorical(prob = list(c(0.05, 0.5, 0.15, 0.2, 0.1), c(0.3, 0.1, 0.6)))
#'
#' dist
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' # The outcomes aren't ordered, so many statistics are not applicable.
#' cdf(dist, 0.6)
#' quantile(dist, 0.7)
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#' 
#' # Some of these statistics are meaningful for ordered outcomes
#' dist <- dist_categorical(list(rpois(26, 3)), list(ordered(letters)))
#' dist
#' cdf(dist, "m")
#' quantile(dist, 0.5)
#'
#' dist <- dist_categorical(
#'   prob = list(c(0.05, 0.5, 0.15, 0.2, 0.1), c(0.3, 0.1, 0.6)),
#'   outcomes = list(letters[1:5], letters[24:26])
#' )
#'
#' generate(dist, 10)
#'
#' density(dist, "a")
#' density(dist, "z", log = TRUE)
#'
#' @name dist_categorical
#' @export
dist_categorical <- function(prob, outcomes = NULL){
  prob <- lapply(prob, function(x) x/sum(x))
  prob <- as_list_of(prob, .ptype = double())
  if(is.null(outcomes)) {
    new_dist(p = prob, class = "dist_categorical")
  } else {
    new_dist(p = prob, x = outcomes, class = "dist_categorical")
  }
}

#' @export
format.dist_categorical <- function(x, digits = 2, ...){
  sprintf(
    "Categorical[%s]",
    vapply(
      categorical_p(x), function(p) format(length(p), digits = digits, ...),
      character(1L)
    )
  )
}

# The list fields, by position. `x[["p"]]` would drop the list for a single
# distribution, so these read the fields directly.
categorical_p <- function(x) as.list(x[["p"]])
categorical_x <- function(x) {
  if ("x" %in% vctrs::fields(x)) as.list(x[["x"]])
}

# The outcomes of each distribution (the positions when none are given)
categorical_outcomes <- function(x) {
  outcomes <- categorical_x(x)
  if (is.null(outcomes)) {
    return(lapply(categorical_p(x), seq_along))
  }
  outcomes
}

# Combine one result per distribution, which may be factors with different
# levels, into a single vector
categorical_combine <- function(res, ordered) {
  if (!any(ordered)) return(rep_len(NA_real_, length(res)))
  res[!ordered] <- list(NA)
  tryCatch(
    vctrs::vec_c(!!!res, .name_spec = zap()),
    error = function(e) {
      vctrs::vec_c(!!!lapply(res, as.character), .name_spec = zap())
    }
  )
}

#' @export
density.dist_categorical <- function(x, at, ...){
  at <- vctrs::vec_recycle(at, vctrs::vec_size(x))
  outcomes <- categorical_x(x)
  prob <- categorical_p(x)
  vapply(seq_along(prob), function(i) {
    a <- at[[i]]
    if(!is.null(outcomes)) a <- match(a, outcomes[[i]])
    if(is.na(a) || a <= 0) return(NA_real_)
    prob[[i]][a]
  }, numeric(1L))
}

#' @export
quantile.dist_categorical <- function(x, p, ..., tail = "lower"){
  p <- vctrs::vec_recycle(p, vctrs::vec_size(x))
  if(tail == "upper") p <- 1 - p
  outcomes <- categorical_outcomes(x)
  ordered <- vapply(outcomes, is.ordered, logical(1L))
  res <- .mapply(
    function(prob, o, p, ord) {
      if(!ord) return(NA_real_)
      k <- length(prob)
      o[pmin(1L + findInterval(p, cumsum(prob), left.open = TRUE), k)]
    },
    list(categorical_p(x), outcomes, p, ordered), NULL
  )
  if(!any(ordered)) return(rep_len(NA_real_, length(res)))
  categorical_combine(res, ordered)
}

#' @export
cdf.dist_categorical <- function(x, q, ..., tail = "lower"){
  q <- vctrs::vec_recycle(q, vctrs::vec_size(x))
  outcomes <- categorical_x(x)
  probs <- categorical_p(x)
  vapply(seq_along(probs), function(i) {
    o <- outcomes[[i]]
    if(!is.ordered(o)) return(NA_real_)
    prob <- probs[[i]]
    cum <- if (tail == "lower") {
      cumsum(prob)
    } else {
      # P(X > x_i) as the sum of the probabilities above x_i
      c(rev(cumsum(rev(prob)))[-1L], 0)
    }
    cum[match(q[[i]], o)]
  }, numeric(1L))
}

#' @export
generate.dist_categorical <- function(x, times, ...){
  .mapply(
    function(p, o, times) {
      z <- sample.int(length(p), size = times, prob = p, replace = TRUE)
      if(is.null(o)) z else o[z]
    },
    list(categorical_p(x), categorical_x(x) %||% vector("list", vctrs::vec_size(x)), times),
    NULL
  )
}

#' @export
support.dist_categorical <- function(x, ...) {
  region <- categorical_outcomes(x)
  n <- length(region)
  new_support_region(
    lapply(region, function(r) vctrs::vec_init(r, n = 0L)),
    lapply(region, list),
    rep_len(list(c(TRUE, TRUE)), n)
  )
}

#' @export
mean.dist_categorical <- function(x, ...){
  rep_len(NA_real_, vctrs::vec_size(x))
}

#' @export
covariance.dist_categorical <- function(x, ...){
  rep_len(NA_real_, vctrs::vec_size(x))
}

#' @export
skewness.dist_categorical <- function(x, ...) {
  rep_len(NA_real_, vctrs::vec_size(x))
}

#' @export
kurtosis.dist_categorical <- function(x, ...) {
  rep_len(NA_real_, vctrs::vec_size(x))
}
