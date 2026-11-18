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

