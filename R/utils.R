transpose <- function(.l) {
  if(is_empty(.l)) return(.l)
  inner_names <- names(.l[[1]])
  result <- lapply(seq_along(.l[[1]]), function(i) {
    lapply(.l, .subset2, i)
  })
  set_names(result, inner_names)
}


dist_apply <- function(x, .f, ...){
  x <- vec_data(x)
  # TODO: Use better approach to quieten vec_rbind
  capture.output(out <- do.call(vctrs::vec_rbind, lapply(x, .f, ...)), type = "message")
  if(ncol(out) == 1)
    out[[1]]
  else
    out
}
