#' The `distribution` vecvec class
#'
#' @description
#' `distribution` vectors are stored as a [vecvec::class_vecvec]: a list of
#' typed "slots" (one `vctrs::vctrs_rcrd()` per distribution family present in
#' the vector) plus an integer index locating each logical distribution in
#' its slot. This lets family methods (`density.dist_normal()`, and so on)
#' operate on a *vector* of same-family distributions in one call, instead of
#' looping element-by-element. See `_dev/vecvec.md` for the design.
#'
#' `class_distribution` also carries the legacy `"distribution"` S3 class
#' string (prepended to the S7-generated class vector) so that the existing
#' `<generic>.distribution` S3 methods in this package - and, for now, any
#' extension package that dispatches on `"distribution"` - keep working
#' unchanged. See `_dev/vecvec.md` section 9.5 for the trade-offs of this choice.
#'
#' @param x A list of distribution family record vectors (each inheriting
#'   from `"dist_default"`).
#' @param i An integer index locating each logical distribution within `x`.
#'   Defaults to `1:sum(lengths(x))` (no compression/repetition).
#'
#' @keywords internal
#' @importFrom vecvec class_vecvec
#' @export
class_distribution <- S7::new_class(
  "distribution",
  parent = vecvec::class_vecvec,
  constructor = function(x = list(), i = seq_len(sum(lengths(x)))) {
    obj <- S7::new_object(vecvec::class_vecvec(vecvec_flatten_adj(x), i))
    class(obj) <- c("distribution", class(obj))
    obj
  }
)

# vecvec's own slot-merging helper (adjacent same-type slots produced by c()
# etc. are combined into one). Not exported by vecvec, so re-exposed here
# under our own name rather than reaching into vecvec:::.
vecvec_flatten_adj <- utils::getFromNamespace("vecvec_flatten_adj", "vecvec")

#' Distribution families with vectorised (ported) methods
#'
#' @description
#' A distribution family is "ported" once its `<generic>.dist_<family>`
#' methods accept a *vector* of `n` distributions and vectorised (length `n`
#' or recyclable) arguments, returning a length-`n` result in one call -
#' rather than assuming `x` is always a single distribution.
#'
#' `apply_dist()` assumes every family is ported. This registry is only
#' consulted by `format()` and `generate()`, which still loop
#' element-by-element over families that are not listed here. See
#' `_dev/dist-port.md` for the families still to be ported.
#'
#' @noRd
dist_ported_families <- c("dist_normal", "dist_poisson", "dist_na")

is_ported_dist <- function(x) {
  class(x)[1L] %in% dist_ported_families
}
