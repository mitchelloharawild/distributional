transpose <- function(.l) {
  if(is_empty(.l)) return(.l)
  inner_names <- names(.l[[1]])
  result <- lapply(seq_along(.l[[1]]), function(i) {
    lapply(.l, .subset2, i)
  })
  set_names(result, inner_names)
}


dist_apply <- function(x, .f, ...){
  dn <- dimnames(x)
  x <- vec_data(x)
  # TODO: Use better approach to quieten vec_rbind
  out <- mapply(.f, x, ..., SIMPLIFY = FALSE)
  suppressMessages(out <- do.call(vctrs::vec_rbind, out))
  if(!is.null(dn)){
    colnames(out) <- dn
  }
  if(ncol(out) == 1)
    out[[1]]
  else
    out
}

# inlined from https://github.com/r-lib/cli/blob/master/R/utf8.R
is_utf8_output <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (!is_null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is_latex_output()
  }
}

is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }
  get("is_latex_output", asNamespace("knitr"))()
}
