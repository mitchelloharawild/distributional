#' The `distribution` vecvec class
#'
#' @description
#' `distribution` vectors are stored as a [vecvec::class_vecvec]: a list of
#' typed "slots" (one `vctrs::vctrs_rcrd()` per distribution family present in
#' the vector) plus an index locating each logical distribution in
#' its slot. This lets family methods (`density.dist_normal()`, and so on)
#' operate on a *vector* of same-family distributions in one call, instead of
#' looping element-by-element. See `_dev/vecvec.md` for the design.
#'
#' `class_distribution` also carries the legacy `"distribution"` S3 class
#' string (inserted after the S7 class name, which must stay first for vctrs)
#' so that the existing `<generic>.distribution` S3 methods in this package - and, for now, any
#' extension package that dispatches on `"distribution"` - keep working
#' unchanged. See `_dev/vecvec.md` section 9.5 for the trade-offs of this choice.
#'
#' @param x A list of distribution family record vectors (each inheriting
#'   from `"dist_default"`).
#' @param i An index locating each logical distribution within `x`.
#'   Defaults to `1:sum(lengths(x))` (no compression/repetition).
#'
#' @keywords internal
#' @importFrom vecvec class_vecvec
#' @export
class_distribution <- S7::new_class(
  "distribution",
  parent = vecvec::class_vecvec,
  constructor = function(x = list(), i = seq_len(sum(lengths(x)))) {
    if (length(x) > 1L) {
      merged <- merge_slots(x, i)
      x <- merged$x
      i <- merged$i
    }
    obj <- S7::new_object(vecvec::class_vecvec(x, i))
    # vctrs dispatches on the first class only, where vecvec_register() has
    # registered the coercion methods (as "distributional::distribution")
    class(obj) <- append(class(obj), "distribution", after = 1L)
    obj
  }
)

# vecvec's c() method, which `c.distribution` calls after casting its inputs
# (S7 can't look up a parent's method for an external generic like c())
vecvec_c <- function(...) utils::getS3method("c", "vecvec::vecvec")(...)

# vecvec only merges adjacent slots of the same type, so a vector alternating
# between families would have one slot per element. Merge every slot with the
# same class and attributes (other than the field names, so including the
# variate names in `"vars"`) into the first of them, and remap the index `i`
# (which locates distributions in the concatenation of the slots, `NA`
# staying `NA`).
merge_slots <- function(x, i) {
  n <- length(x)
  key <- vapply(x, function(s) paste(class(s), collapse = "/"), character(1L))
  if (!anyDuplicated(key)) return(list(x = x, i = i))

  # Group slots with the same class and identical attributes (except names)
  attrs <- lapply(x, function(s) {
    a <- attributes(s)
    a[names(a) != "names"]
  })
  group <- integer(n)
  firsts <- integer()
  for (k in seq_len(n)) {
    for (f in firsts[key[firsts] == key[k]]) {
      if (identical(attrs[[k]], attrs[[f]]) && identical(vctrs::fields(x[[k]]), vctrs::fields(x[[f]]))) {
        group[k] <- group[f]
        break
      }
    }
    if (group[k] == 0L) {
      firsts <- c(firsts, k)
      group[k] <- length(firsts)
    }
  }
  if (length(firsts) == n) return(list(x = x, i = i))

  sizes <- vapply(x, vctrs::vec_size, integer(1L))
  old_start <- cumsum(sizes) - sizes
  # Position of each old slot's elements in the merged slots
  new_x <- vector("list", length(firsts))
  new_start <- integer(n)
  group_total <- 0L
  for (g in seq_along(firsts)) {
    members <- which(group == g)
    new_x[[g]] <- if (length(members) == 1L) x[[members]] else combine_records(x[members])
    sz <- sizes[members]
    new_start[members] <- group_total + cumsum(sz) - sz
    group_total <- group_total + sum(sz)
  }
  map <- integer(sum(sizes))
  for (k in seq_len(n)) {
    map[old_start[k] + seq_len(sizes[k])] <- new_start[k] + seq_len(sizes[k])
  }
  list(x = new_x, i = map[i])
}

# Combine records of one family with identical attributes and field names (as
# `vctrs::vec_c()` would, but without its overhead per record, which dominates
# when combining many distributions).
combine_records <- function(xs) {
  fields <- lapply(vctrs::fields(xs[[1L]]), function(f) {
    vctrs::list_unchop(lapply(unname(xs), .subset2, f))
  })
  attributes(fields) <- attributes(xs[[1L]])
  fields
}
