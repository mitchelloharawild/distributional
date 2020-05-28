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
  out <- mapply(.f, x, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if(length(out[[1]]) > 1){
    out <- suppressMessages(vctrs::vec_rbind(!!!out))
  } else {
    out <- vctrs::vec_c(!!!out)
  }
  if(is.data.frame(out) && !is.null(dn)){
    # Set dimension names
    colnames(out) <- dn
  }
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

require_package <- function(pkg){
  if(!requireNamespace(pkg, quietly = TRUE)){
    abort(
      sprintf('The `%s` package must be installed to use this functionality. It can be installed with install.packages("%s")', pkg, pkg)
    )
  }
}
