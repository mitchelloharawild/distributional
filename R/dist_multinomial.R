#' The Multinomial distribution
#'
#' \lifecycle{maturing}
#'
#' The multinomial distribution is a generalization of the binomial
#' distribution to multiple categories. It is perhaps easiest to think
#' that we first extend a [dist_bernoulli()] distribution to include more
#' than two categories, resulting in a categorical distribution.
#' We then extend repeat the Categorical experiment several (\eqn{n})
#' times.
#'
#' @inheritParams stats::dmultinom
#'
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X = (X_1, ..., X_k)} be a Multinomial
#'   random variable with success probability `p` = \eqn{p}. Note that
#'   \eqn{p} is vector with \eqn{k} elements that sum to one. Assume
#'   that we repeat the Categorical experiment `size` = \eqn{n} times.
#'
#'   **Support**: Each \eqn{X_i} is in \eqn{{0, 1, 2, ..., n}}.
#'
#'   **Mean**: The mean of \eqn{X_i} is \eqn{n p_i}.
#'
#'   **Variance**: The variance of \eqn{X_i} is \eqn{n p_i (1 - p_i)}.
#'     For \eqn{i \neq j}, the covariance of \eqn{X_i} and \eqn{X_j}
#'     is \eqn{-n p_i p_j}.
#'
#'   **Probability mass function (p.m.f)**:
#'
#'   \deqn{
#'     P(X_1 = x_1, ..., X_k = x_k) = \frac{n!}{x_1! x_2! ... x_k!} p_1^{x_1} \cdot p_2^{x_2} \cdot ... \cdot p_k^{x_k}
#'   }{
#'     P(X_1 = x_1, ..., X_k = x_k) = n! / (x_1! x_2! ... x_k!) p_1^x_1 p_2^x_2 ... p_k^x_k
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   Omitted for multivariate random variables for the time being.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = \left(\sum_{i=1}^k p_i e^{t_i}\right)^n
#'   }{
#'     E(e^(tX)) = (p_1 e^t_1 + p_2 e^t_2 + ... + p_k e^t_k)^n
#'   }
#'
#' @seealso [stats::Multinomial]
#'
#' @examples
#' dist <- dist_multinomial(size = c(4, 3), prob = list(c(0.3, 0.5, 0.2), c(0.1, 0.5, 0.4)))
#'
#' dist
#' mean(dist)
#' variance(dist)
#'
#' generate(dist, 10)
#'
#' # TODO: Needs fixing to support multiple inputs
#' # density(dist, 2)
#' # density(dist, 2, log = TRUE)
#'
#' @name dist_multinomial
#' @export
dist_multinomial <- function(size, prob){
  size <- vec_cast(size, double())
  prob <- lapply(prob, function(x) x/sum(x))
  prob <- as_list_of(prob, .ptype = double())
  new_dist(s = size, p = prob, class = "dist_multinomial")
}

#' @export
print.dist_multinomial <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_multinomial <- function(x, digits = 2, ...){
  sprintf(
    "Multinomial(%s)[%s]",
    format(x[["s"]], digits = digits, ...),
    format(length(x[["p"]]), digits = digits, ...)
  )
}

#' @export
density.dist_multinomial <- function(x, at, ...){
  stats::dmultinom(at, x[["s"]], x[["p"]])
}

#' @export
log_density.dist_multinomial <- function(x, at, ...){
  stats::dmultinom(at, x[["s"]], x[["p"]], log = TRUE)
}

#' @export
generate.dist_multinomial <- function(x, times, ...){
  t(stats::rmultinom(times, x[["s"]], x[["p"]]))
}

#' @export
mean.dist_multinomial <- function(x, ...){
  x[["s"]]*x[["p"]]
}

#' @export
variance.dist_multinomial <- function(x, ...){
  s <- x[["s"]]
  p <- x[["p"]]
  v <- numeric(length(p)^2)
  for(i in seq_along(p)){
    for(j in seq_along(p)){
      v[(i-1)*length(p) + j] <- if(i == j) s*p[i]*(1-p[j]) else -s*p[i]*p[j]
    }
  }
  list(matrix(v, nrow = length(p)))
}

#' @export
dim.dist_multinomial <- function(x){
  length(x[["p"]])
}
