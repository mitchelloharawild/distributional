#' The Multinomial distribution
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The multinomial distribution is a generalization of the binomial
#' distribution to multiple categories. It is perhaps easiest to think
#' that we first extend a [dist_bernoulli()] distribution to include more
#' than two categories, resulting in a [dist_categorical()] distribution.
#' We then extend repeat the Categorical experiment several (\eqn{n})
#' times.
#'
#' @param size The number of draws from the Categorical distribution.
#' @param prob The probability of an event occurring from each draw.
#'
#' @details
#'
#' `r pkgdown_doc_link("dist_multinomial")`
#'
#'   In the following, let \eqn{X = (X_1, ..., X_k)} be a Multinomial
#'   random variable with success probability `prob` = \eqn{p}. Note that
#'   \eqn{p} is vector with \eqn{k} elements that sum to one. Assume
#'   that we repeat the Categorical experiment `size` = \eqn{n} times.
#'
#'   **Support**: Each \eqn{X_i} is in \eqn{\{0, 1, 2, ..., n\}}{{0, 1, 2, ..., n}}.
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
#'     P(X_1 = x_1, ..., X_k = x_k) = \frac{n!}{x_1! x_2! \cdots x_k!} p_1^{x_1} \cdot p_2^{x_2} \cdot \ldots \cdot p_k^{x_k}
#'   }{
#'     P(X_1 = x_1, ..., X_k = x_k) = n! / (x_1! x_2! ... x_k!) p_1^x_1 p_2^x_2 ... p_k^x_k
#'   }
#'
#'   where \eqn{\sum_{i=1}^k x_i = n} and \eqn{\sum_{i=1}^k p_i = 1}.
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   \deqn{
#'     P(X_1 \le q_1, ..., X_k \le q_k) = \sum_{\substack{x_1, \ldots, x_k \ge 0 \\ x_i \le q_i \text{ for all } i \\ \sum_{i=1}^k x_i = n}} \frac{n!}{x_1! x_2! \cdots x_k!} p_1^{x_1} \cdot p_2^{x_2} \cdot \ldots \cdot p_k^{x_k}
#'   }{
#'     P(X_1 <= q_1, ..., X_k <= q_k) = sum over all (x_1, ..., x_k) with x_i <= q_i and sum x_i = n of n! / (x_1! ... x_k!) p_1^x_1 ... p_k^x_k
#'   }
#'
#'   The c.d.f. is computed as a finite sum of the p.m.f. over all integer vectors
#'   in the support that satisfy the componentwise inequalities.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{t'X}) = \left(\sum_{i=1}^k p_i e^{t_i}\right)^n
#'   }{
#'     E(e^(t'X)) = (p_1 e^t_1 + p_2 e^t_2 + ... + p_k e^t_k)^n
#'   }
#'
#'   where \eqn{t = (t_1, ..., t_k)} is a vector of the same dimension as \eqn{X}.
#'
#'   **Skewness**: The skewness of \eqn{X_i} is
#'
#'   \deqn{
#'     \frac{1 - 2p_i}{\sqrt{n p_i (1 - p_i)}}
#'   }{
#'     (1 - 2p_i) / sqrt(n p_i (1 - p_i))
#'   }
#'
#'   **Excess Kurtosis**: The excess kurtosis of \eqn{X_i} is
#'
#'   \deqn{
#'     \frac{1 - 6p_i(1 - p_i)}{n p_i (1 - p_i)}
#'   }{
#'     (1 - 6p_i(1 - p_i)) / (n p_i (1 - p_i))
#'   }
#'
#' @seealso [stats::dmultinom()], [stats::rmultinom()]
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
#' density(dist, list(d = rbind(cbind(1,2,1), cbind(0,2,1))))
#' density(dist, list(d = rbind(cbind(1,2,1), cbind(0,2,1))), log = TRUE)
#' 
#' cdf(dist, cbind(1,2,1))
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
format.dist_multinomial <- function(x, digits = 2, ...){
  sprintf(
    "Multinomial(%s)[%s]",
    format(x[["s"]], digits = digits, ...),
    format(length(x[["p"]]), digits = digits, ...)
  )
}

#' @export
density.dist_multinomial <- function(x, at, ...){
  if(is.list(at)) return(vapply(at, density, numeric(1L), x = x, ...))
  stats::dmultinom(at, x[["s"]], x[["p"]])
}

#' @export
log_density.dist_multinomial <- function(x, at, ...){
  if(is.list(at)) return(vapply(at, log_density, numeric(1L), x = x, ...))
  stats::dmultinom(at, x[["s"]], x[["p"]], log = TRUE)
}

#' @export
generate.dist_multinomial <- function(x, times, ...){
  t(stats::rmultinom(times, x[["s"]], x[["p"]]))
}

#' @export
mean.dist_multinomial <- function(x, ...){
  matrix(x[["s"]]*x[["p"]], nrow = 1)
}

#' @export
covariance.dist_multinomial <- function(x, ...){
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

#' @export
cdf.dist_multinomial <- function(x, q, ...) {
  # Extract parameters
  size <- x$s
  prob <- x$p
  k <- length(prob)

  # Validate dimensions
  if (ncol(q) != k) {
    stop("Number of columns in q must match the number of categories in the multinomial distribution")
  }

  n_queries <- nrow(q)
  result <- numeric(n_queries)

  for (i in seq_len(n_queries)) {
    q_i <- q[i, ]

    # Any negative quantile => CDF = 0
    if (any(q_i < 0)) {
      result[i] <- 0
      next
    }

    # Discrete CDF: floor input values
    q_i <- floor(q_i)

    # Max counts for the first (k-1) categories
    max_vec <- pmin(q_i[1:(k - 1)], size)

    # Build ranges for first (k-1) categories
    ranges <- lapply(max_vec, function(m) 0:m)

    # Enumerate only the first (k-1) coordinates
    partial <- expand.grid(ranges, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

    # Compute the last category as the remainder
    sums <- rowSums(partial)
    xk <- size - sums

    # Valid if 0 <= xk <= q_k
    valid_idx <- which(xk >= 0 & xk <= q_i[k])

    if (!length(valid_idx)) {
      result[i] <- 0
      next
    }

    # Construct full count vectors
    counts <- cbind(as.matrix(partial[valid_idx, , drop = FALSE]), xk[valid_idx])

    # Sum multinomial pmf over valid outcomes
    probs <- apply(counts, 1L, stats::dmultinom, size = size, prob = prob)
    result[i] <- sum(probs)
  }

  result
}