#' The Normal distribution
#'
#' \lifecycle{stable}
#'
#' The Normal distribution is ubiquitous in statistics, partially because
#' of the central limit theorem, which states that sums of i.i.d. random
#' variables eventually become Normal. Linear transformations of Normal
#' random variables result in new random variables that are also Normal. If
#' you are taking an intro stats course, you'll likely use the Normal
#' distribution for Z-tests and in simple linear regression. Under
#' regularity conditions, maximum likelihood estimators are
#' asymptotically Normal. The Normal distribution is also called the
#' gaussian distribution.
#'
#' @param mu The mean (location parameter) of the distribution, which is also
#'   the mean of the distribution. Can be any real number.
#' @param sigma The standard deviation (scale parameter) of the distribution.
#'   Can be any positive number. If you would like a Normal distribution with
#'   **variance** \eqn{\sigma^2}, be sure to take the square root, as this is a
#'   common source of errors.
#'
#' @details
#'
#'   We recommend reading this documentation on
#'   <https://pkg.mitchelloharawild.com/distributional/>, where the math
#'   will render nicely.
#'
#'   In the following, let \eqn{X} be a Normal random variable with mean
#'   `mu` = \eqn{\mu} and standard deviation `sigma` = \eqn{\sigma}.
#'
#'   **Support**: \eqn{R}, the set of all real numbers
#'
#'   **Mean**: \eqn{\mu}
#'
#'   **Variance**: \eqn{\sigma^2}
#'
#'   **Probability density function (p.d.f)**:
#'
#'   \deqn{
#'     f(x) = \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-(x - \mu)^2 / 2 \sigma^2}
#'   }{
#'     f(x) = 1 / (2 \pi \sigma^2) exp(-(x - \mu)^2 / (2 \sigma^2))
#'   }
#'
#'   **Cumulative distribution function (c.d.f)**:
#'
#'   The cumulative distribution function has the form
#'
#'   \deqn{
#'     F(t) = \int_{-\infty}^t \frac{1}{\sqrt{2 \pi \sigma^2}} e^{-(x - \mu)^2 / 2 \sigma^2} dx
#'   }{
#'     F(t) = integral_{-\infty}^t 1 / (2 \pi \sigma^2) exp(-(x - \mu)^2 / (2 \sigma^2)) dx
#'   }
#'
#'   but this integral does not have a closed form solution and must be
#'   approximated numerically. The c.d.f. of a standard Normal is sometimes
#'   called the "error function". The notation \eqn{\Phi(t)} also stands
#'   for the c.d.f. of a standard Normal evaluated at \eqn{t}. Z-tables
#'   list the value of \eqn{\Phi(t)} for various \eqn{t}.
#'
#'   **Moment generating function (m.g.f)**:
#'
#'   \deqn{
#'     E(e^{tX}) = e^{\mu t + \sigma^2 t^2 / 2}
#'   }{
#'     E(e^(tX)) = e^(\mu t + \sigma^2 t^2 / 2)
#'   }
#'
#' @seealso [stats::Normal]
#'
#' @examples
#' dist <- dist_normal(mu = 1:5, sigma = 3)
#'
#' dist
#' mean(dist)
#' variance(dist)
#' skewness(dist)
#' kurtosis(dist)
#'
#' generate(dist, 10)
#'
#' density(dist, 2)
#' density(dist, 2, log = TRUE)
#'
#' cdf(dist, 4)
#'
#' quantile(dist, 0.7)
#'
#' @export
dist_normal <- function(mu = 0, sigma = 1){
  mu <- vec_cast(mu, double())
  sigma <- vec_cast(sigma, double())
  if(any(sigma[!is.na(sigma)] < 0)){
    abort("Standard deviation of a normal distribution must be non-negative")
  }
  new_dist(mu = mu, sigma = sigma, class = "dist_normal")
}

#' @export
print.dist_normal <- function(x, ...){
  cat(format(x, ...))
}

#' @export
format.dist_normal <- function(x, digits = 2, ...){
  sprintf(
    "N(%s, %s)",
    format(x[["mu"]], digits = digits, ...),
    format(x[["sigma"]]^2, digits = digits, ...)
  )
}

#' @export
density.dist_normal <- function(x, at, ...){
  stats::dnorm(at, x[["mu"]], x[["sigma"]])
}

#' @export
log_density.dist_normal <- function(x, at, ...){
  stats::dnorm(at, x[["mu"]], x[["sigma"]], log = TRUE)
}

#' @export
quantile.dist_normal <- function(x, p, ...){
  stats::qnorm(p, x[["mu"]], x[["sigma"]])
}
#' @export
log_quantile.dist_normal <- function(x, p, ...){
  stats::qnorm(p, x[["mu"]], x[["sigma"]], log.p = TRUE)
}

#' @export
cdf.dist_normal <- function(x, q, ...){
  stats::pnorm(q, x[["mu"]], x[["sigma"]])
}
#' @export
log_cdf.dist_normal <- function(x, q, ...){
  stats::pnorm(q, x[["mu"]], x[["sigma"]], log.p = TRUE)
}

#' @export
generate.dist_normal <- function(x, times, ...){
  stats::rnorm(times, x[["mu"]], x[["sigma"]])
}

#' @export
mean.dist_normal <- function(x, ...){
  x[["mu"]]
}

#' @export
variance.dist_normal <- function(x, ...){
  x[["sigma"]]^2
}

#' @export
skewness.dist_normal <- function(x, ...) 0

#' @export
kurtosis.dist_normal <- function(x, ...) 0

#' @export
Ops.dist_normal <- function(e1, e2){
  ok <- switch(.Generic, `+` = , `-` = , `*` = , `/` = TRUE, FALSE)
  if (!ok) {
    return(NextMethod())
  }
  if(.Generic == "/" && inherits(e2, "dist_normal")){
    abort(sprintf("Cannot divide by a normal distribution"))
  }
  if(.Generic %in% c("-", "+") && missing(e2)){
    e2 <- e1
    e1 <- if(.Generic == "+") 1 else -1
    .Generic <- "*"
  }
  if(.Generic == "-"){
    .Generic <- "+"
    e2 <- -e2
  }
  else if(.Generic == "/"){
    .Generic <- "*"
    e2 <- 1/e2
  }

  # Ops between two normals
  if(inherits(e1, "dist_normal") && inherits(e2, "dist_normal")){
    if(.Generic == "*"){
      abort(sprintf("Multiplying two normal distributions is not supported."))
    }

    e1$mu <- e1$mu + e2$mu
    e1$sigma <- sqrt(e1$sigma^2 + e2$sigma^2)
    return(e1)
  }

  # Ops between a normal and scalar
  if(inherits(e1, "dist_normal")){
    dist <- e1
    scalar <- e2
  } else {
    dist <- e2
    scalar <- e1
  }
  if(!is.numeric(scalar)){
    abort(sprintf("Cannot %s a `%s` with a normal distribution",
                  switch(.Generic, `+` = "add", `-` = "subtract", `*` = "multiply", `/` = "divide"),
                  class(scalar)))
  }

  if(.Generic == "+"){
    dist$mu <- dist$mu + scalar
  }
  else if(.Generic == "*"){
    dist$mu <- dist$mu * scalar
    dist$sigma <- dist$sigma * abs(scalar)
  }
  dist
}
