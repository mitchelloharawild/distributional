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
    format(length(x[["p"]]), digits = digits, ...)
  )
}

#' @export
density.dist_categorical <- function(x, at, ...){
  if(!is.null(x[["x"]])) at <- match(at, x[["x"]])
  at[at <= 0] <- NA_real_
  x[["p"]][at]
}

#' @export
quantile.dist_categorical <- function(x, p, ...){
  if(is.ordered(x[["x"]])) {
    return(x[["x"]][findInterval(p, cumsum(x[["p"]]))])
  }
  rep_len(NA_real_, length(p))
}

#' @export
cdf.dist_categorical <- function(x, q, ...){
  if(is.ordered(x[["x"]])) {
    return(cumsum(x[["p"]])[match(q, x[["x"]])])
  }
  rep_len(NA_real_, length(q))
}

#' @export
generate.dist_categorical <- function(x, times, ...){
  z <- sample(
    x = seq_along(x[["p"]]), size = times, prob = x[["p"]], replace = TRUE
  )
  if(is.null(x[["x"]])) return(z)
  x[["x"]][z]
}

#' @export
support.dist_categorical <- function(x, ...) {
  region <- if(is.null(x[["p"]])) seq_along(x[["p"]]) else x[["x"]]
  new_support_region(
    list(vctrs::vec_init(region, n = 0L)),
    list(region),
    list(c(TRUE, TRUE))
  )
}

#' @export
mean.dist_categorical <- function(x, ...){
  NA_real_
}

#' @export
covariance.dist_categorical <- function(x, ...){
  NA_real_
}

#' @export
skewness.dist_categorical <- function(x, ...) {
  NA_real_
}

#' @export
kurtosis.dist_categorical <- function(x, ...) {
  NA_real_
}