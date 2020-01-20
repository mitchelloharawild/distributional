transpose <- function(.l) {
  if(is_empty(.l)) return(.l)
  inner_names <- names(.l[[1]])
  result <- lapply(seq_along(.l[[1]]), function(i) {
    lapply(.l, .subset2, i)
  })
  set_names(result, inner_names)
}
