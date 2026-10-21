transpose <- function(.l) {
  if(is_empty(.l)) return(.l)
  inner_names <- names(.l[[1L]])
  result <- lapply(seq_along(.l[[1L]]), function(i) {
    lapply(.l, .subset2, i)
  })
  set_names(result, inner_names)
}

unpack <- function(data, cols, sep = ".") {
  cols <- if (is.numeric(cols)) names(data)[cols] else cols
  rn <- attr(data, "row.names")
  data <- unclass(data)
  is_target <- logical(length(data))
  is_target[match(cols, names(data))] <- TRUE   # one vectorized position lookup
  data[cols] <- lapply(cols, function(col) {
    inner <- data[[col]]
    stopifnot(is.data.frame(inner))
    inner <- unclass(inner)
    names(inner) <- paste(col, names(inner), sep = sep)
    inner
  })
  pieces <- vector("list", length(data))
  pieces[!is_target] <- lapply(which(!is_target), function(j) data[j])
  pieces[is_target]  <- data[is_target]
  data <- do.call(c, pieces)                    # one concatenation, not one per column
  attr(data, "row.names") <- rn
  class(data) <- "data.frame"
  data
}

# Locate every logical position of a `distribution` vector within its
# storage slots: which slot it belongs to (`pos`, one integer vector of
# positions per slot) and its position within that slot (`local`). NA
# (missing) index positions are excluded from every `pos[[k]]`, so a family
# method never sees them - see `_dev/vecvec.md` #3.3.
locate_slots <- function(x) {
  idx <- S7::S7_data(x)
  slots <- x@x
  n <- length(idx)
  if (!length(slots) || !n) {
    return(list(n = n, slots = slots, pos = vector("list", length(slots)), local = integer(n)))
  }
  starts <- c(0L, cumsum(lengths(slots)))
  not_na <- !is.na(idx)
  slot_id <- integer(n)
  slot_id[not_na] <- findInterval(idx[not_na], starts, left.open = TRUE)
  local <- integer(n)
  local[not_na] <- idx[not_na] - starts[slot_id[not_na]]
  pos <- lapply(seq_along(slots), function(k) which(not_na & slot_id == k))
  list(n = n, slots = slots, pos = pos, local = local)
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

restore_rng <- function(expr, seed) {
  # The RNG may not have been used yet, in which case it should be left unused
  had_seed <- env_has(.GlobalEnv, ".Random.seed")
  old_seed <- .GlobalEnv$.Random.seed
  # Restore previous seed
  on.exit(
    if (had_seed) {
      .GlobalEnv$.Random.seed <- old_seed
    } else {
      env_unbind(.GlobalEnv, ".Random.seed")
    }
  )
  # Set new temporary seed
  if (!missing(seed)) set.seed(seed)

  expr
}

near <- function(x, y) {
  tol <- .Machine$double.eps^0.5
  abs(x - y) < tol
}
