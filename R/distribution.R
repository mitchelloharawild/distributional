#' @export
new_dist <- function(x, ..., class = NULL){
  vctrs::new_vctr(list(x), class = "distribution")
}

#' @export
vec_size.distribution <- function(x){
  sum(lapply(vec_data(x), vec_size))
}

#' @export
vec_ptype_abbr.distribution <- function(x, ...){
  "dist"
}

#' @export
format.distribution <- function(x, ...){
  do.call(vec_c, lapply(vec_data(x), format, ...))
}
