#' Convolve distributions
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The [`density()`], [`mean()`], and [`variance()`] methods are approximate as
#' they are based on numerical derivatives.
#'
#' @param x,y A univariate distribution vector.
#' @param n the number of equally spaced points at which the convolution is to be estimated. This should be a power of two.
#' @param trunc The truncation of the support region if support of `x`, `y` are infinite.
#'
#' @examples
#'
#' 1
#'
#' @export
dist_convolved <- function(x, y, n = 512, trunc = 1e-6){
  vec_is(y, new_dist())
  vec_is(x, new_dist())

  trunc_lims <- function(x) {
    lwr <- quantile(x, 0)
    lwr_inf <- is.infinite(lwr)
    lwr[lwr_inf] <- quantile(x[lwr_inf], trunc)

    upr <- quantile(x, 1)
    upr_inf <- is.infinite(upr)
    upr[upr_inf] <- quantile(x[upr_inf], 1-trunc)

    cbind(lwr, upr)
  }

  browser()
  dist <- vec_recycle_common(x, y)
  dlim <- lapply(dist, trunc_lims)

  dlim <- cbind(
    lwr = pmin(dlim[[1L]][,1L], dlim[[2L]][,1L]),
    upr = pmax(dlim[[1L]][,2L], dlim[[2L]][,2L])
  )

  dconvolve <- function(x, y, lwr, upr) {
    xgrid <- seq(lwr, upr, length=n)
    d <- convolve(
      density(x, xgrid),
      rev(density(y, xgrid))
    )
    # dx <- (upr - lwr) / n
  }

  x <- seq(qlower, qupper, length=n)
  dx <- (qupper - qlower) / n

  dens <- density(dist, x)
  dens_z <- convolve(dens[[1]], rev(dens[[2]]), type = "open") / n * (max(x)-min(x))

  xn <- seq(2*qlower, 2*qupper, length=n*2-1)
  approxfun(xn, dens_z, rule = 2)

  new_dist(dist = vec_data(dist),
           transform = transform, inverse = inverse,
           dimnames = dimnames(dist), class = "dist_transformed")
}
