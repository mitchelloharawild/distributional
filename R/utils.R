transpose <- function(.l) {
  if(is_empty(.l)) return(.l)
  inner_names <- names(.l[[1L]])
  result <- lapply(seq_along(.l[[1L]]), function(i) {
    lapply(.l, .subset2, i)
  })
  set_names(result, inner_names)
}

transpose_c <- function(.l) {
  stopifnot(is_list_of(.l))
  .ptype <- vec_init(attr(.l, "ptype"), 1L)
  if(is_empty(.l)) return(.l)
  inner_names <- names(.l[[1L]])
  .l <- vec_recycle_common(!!!.l)
  result <- lapply(seq_along(.l[[1L]]), function(i) {
    unname(vec_c(!!!lapply(.l, vec_slice, i), .ptype = .ptype))
  })
  set_names(result, inner_names)
}

split_matrix_rows <- function(x) {
  lapply(seq_len(nrow(x)), function(i) x[i,,drop=FALSE])
}

# Declare a function's argument as allowing list inputs for mapping values
arg_listable <- function(x, .ptype) {
  if(is.list(x)) {
    x <- as_list_of(as.list(x), .ptype)
    if(is.matrix(attr(x, "ptype"))) {
      x <- lapply(x, split_matrix_rows)
      x <- as_list_of(x, .ptype)
    }
    if(is.null(names(x))) {
      names(x) <- vec_as_names(character(vec_size(x)), repair = "unique")
    }
  } else if(is.matrix(x)) {
    x <- split_matrix_rows(x)
  } else {
    vec_assert(x, .ptype)
  }
  # Declares list arguments to be unpacked for dist_apply()
  class(x) <- c("arg_listable", class(x))
  x
}

validate_recycling <- function(x, arg) {
  if(is_list_of(arg)) return(lapply(arg, validate_recycling, x = x))
  if(!any(vec_size(arg) == c(1, vec_size(x)))) {
    abort(
      sprintf("Cannot recycle input of size %i to match the distributions (size %i).",
              vec_size(arg), vec_size(x)
      )
    )
  }
}

dist_apply <- function(x, .f, ...){
  dn <- dimnames(x)
  x <- vec_data(x)
  dist_is_na <- vapply(x, is.null, logical(1L))
  x[dist_is_na] <- list(structure(list(), class = c("dist_na", "dist_default")))

  args <- dots_list(...)
  is_arg_listable <- vapply(args, inherits, FUN.VALUE = logical(1L), "arg_listable")
  unpack_listable <- multi_arg <- FALSE
  if(any(is_arg_listable)) {
    if(sum(is_arg_listable) > 1) abort("Only distribution argument can be unpacked at a time.\nThis shouldn't happen, please report a bug at https://github.com/mitchelloharawild/distributional/issues/")
    arg_pos <- which(is_arg_listable)

    if(unpack_listable <- is_list_of(args[[arg_pos]])) {
      validate_recycling(x, args[[arg_pos]])
      .unpack_names <- names(args[[arg_pos]])
      args[[arg_pos]] <- transpose_c(args[[arg_pos]])
    } else if (multi_arg <- (length(args[[arg_pos]]) > 1)){
      args[[arg_pos]] <- list(unclass(args[[arg_pos]]))
    }
  }

  out <- do.call(mapply, c(.f, list(x), args, SIMPLIFY = FALSE, USE.NAMES = FALSE))
  # out <- mapply(.f, x, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)

  if(unpack_listable) {
    # TODO - update and repair multivariate distribution i/o with unpacking
    out <- as_list_of(out)
    if (rbind_mat <- is.matrix(attr(out, "ptype"))) {
      out <- as_list_of(lapply(out, split_matrix_rows))
    }
    out <- transpose_c(out)
    if(rbind_mat) {
      out <- lapply(out, function(x) `colnames<-`(do.call(rbind, x), dn))
    }
    names(out) <- .unpack_names
    out <- new_data_frame(out, n = vec_size(x))
  # } else if(length(out[[1]]) > 1) {
  #   out <- suppressMessages(vctrs::vec_rbind(!!!out))
  } else if(multi_arg) {
    if(length(dn) > 1) out <- lapply(out, `colnames<-`, dn)
  } else {
    out <- vec_c(!!!out)
    out <- if(vec_is_list(out))
      lapply(out, set_matrix_dimnames, dn = dn)
    else
      set_matrix_dimnames(out, dn)
  }
  out
}

set_matrix_dimnames <- function(x, dn) {
  if((is.matrix(x) || is.data.frame(x)) && !is.null(dn)){
    # Set dimension names
    colnames(x) <- dn
  }
  x
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

#' @keywords internal
pkgdown_doc_link <- function(name) {
  require_package("pkgdown")
  if (pkgdown::in_pkgdown()) {
    # When pkgdown builds, return nothing
    return("")
  }

  url <- sprintf(
    "https://pkg.mitchelloharawild.com/distributional/reference/%s.html",
    name
  )

  # Plain text + markdown-style link; roxygen will convert this to Rd.
  # Example rendered text:
  #   We recommend reading this documentation on [pkgdown](https://...)
  #   https://...
  paste0(
    "We recommend reading this documentation on ",
    "[pkgdown](", url, ") which renders math nicely.\n\\url{",
    url, "}\n"
  )
}

restore_rng <- function(expr, seed = NULL) {
  old_seed <- .GlobalEnv$.Random.seed
  # Set new temporary seed
  set.seed(seed)
  # Restore previous seed
  on.exit(.GlobalEnv$.Random.seed <- old_seed)

  expr
}

near <- function(x, y) {
  tol <- .Machine$double.eps^0.5
  abs(x - y) < tol
}
