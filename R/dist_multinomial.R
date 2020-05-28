#' The Multinomial distribution
#'
#' \lifecycle{maturing}
#'
#' @inheritParams stats::dmultinom
#'
#' @seealso [stats::Multinom]
#'
#' @examples
#' dist_multinomial(size = c(4, 3), prob = list(c(0.3, 0.5, 0.2), c(0.1, 0.5, 0.4)))
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
dim.dist_mvnorm <- function(x){
  length(x[["p"]])
}
