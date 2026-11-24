# Per-call overhead on short vectors: ~200 us per generic, 94% of it in `vecvec_mapply`

Measured on `vecvec-port` f654e11 (after the mixture and mixed-dimension
fixes), `d1 <- dist_normal(0.3, 1)`, median of 500+ calls, machine under load.

| call | now | 0.9.0 |
|---|---|---|
| `family(d1)` | 200 us | 13 us |
| `quantile(d1, .5)` | 226 us | 43 us |
| `density(d1, 0)` / `cdf(d1, 0)` | 257 / 283 us | |
| `parameters(d1)` / `support(d1)` | 233 / 570 us | |
| `quantile(d, .5)` with 100 normals | 240 us | |

The cost is fixed per call, not per element: 100 normals cost the same as one.
ggdist calls these once per group on length-1 vectors, so it pays the fixed
cost for each group.

## Where it goes (`quantile(d1, .5)`)

The family method itself takes 6 us. Everything else is the dispatch layer:

| step | us |
|---|---|
| `vecvec::vecvec_mapply()` total | 112-184 |
| of which `vecvec_align()` | 22 |
| of which `vec_slice()` of the slot | 13 |
| of which building the result with `class_vecvec()` (S7 construct + validate) | ~49 |
| `slot_dims()` (only the vector-`at` path) | 38 |
| `broadcast_arg()`, `variates()` | 2-4 |

`apply_slots()` runs `vecvec_mapply()` and then immediately `unvecvec()`s the
result, so for one slot the S7 vecvec is built only to be taken apart.

## Prototype fast path

In `apply_slots()`, when `x` has a single slot and no NA index there is
nothing to group or align: slice the slot once and call `.f` directly.

```r
apply_slots <- function(x, .f, args, pos = NULL, .ptype = NULL) {
  if (length(x@x) == 1L) {
    idx <- vecvec:::vv_index(x)
    if (!anyNA(idx)) {
      a <- if (!is.null(pos)) args[[pos]]
      if (is.null(pos) || vec_size(a) %in% c(1L, length(x))) {
        vals <- vec_slice(x@x[[1L]], as.vector(idx))
        return(inject(.f(vals, !!!args)))
      }
    }
  }
  ...  # existing code
}
```

(`vv_index` is internal to vecvec; an exported accessor would be better.)

Patched in place with `assignInNamespace()` and timed against the unpatched
build:

| call | before | after |
|---|---|---|
| `quantile(d1, .5)` | 210 us | 51 us |
| `density(d1, 0)` | 242 us | 92 us |
| `cdf(d1, 0)` | ~283 us | 105 us |

`quantile(x, .5)` on 100 normals gave identical results (`identical()`), and
`quantile(x, c(.1, .9))` matched (`all.equal()`). The distributional test suite was not
run with the patch, and mixed-family vectors (more than one slot) still take
the existing path.

The remaining ~45 us per call is `broadcast_dist()`, `broadcast_arg()` and S3
dispatch. `support()` (570 us) and `parameters()` build data frames and
support regions, so they need a separate look.

## Equivalent fix in vecvec

`vecvec_mapply()` could take the same single-slot shortcut, or `class_vecvec()`
could skip `validate()` for results built internally. That would speed up
every package that uses `vecvec_mapply()`.

## Status

Applied in `R/apply.R` (single-slot fast path using `vecvec:::vv_index`). Distributional suite: 2579 passed, 0 failed. After: `quantile` 52 us, `family` 38 us, `density` 100 us, `support` 70 us, `parameters` 40 us on a length-1 vector.
