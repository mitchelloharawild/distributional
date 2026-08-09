# vecvec migration: proof-of-concept status

Branch: `vecvec-poc`. Implements phases 0-1 of `_dev/vecvec.md`, plus phase 2
for two families (`dist_normal`, `dist_poisson`), as a proof of concept.

## What's ported and vectorised

`dist_normal` and `dist_poisson` are registered in the "ported families"
list (`R/00_classes.R`). For these, `dist_apply()` makes **one vectorised
call per storage slot**, not one call per distribution:

```r
d <- dist_normal(rnorm(1e5), 1)
density(d, 0)   # exactly 1 call to stats::dnorm(), confirmed via trace()
```

Benchmark (`density()` over 1e5 Normals, at a scalar point):

| | time |
|---|---|
| pre-migration `distributional` (main) | 0.448s |
| this branch | 0.043s |

A mixed-family vector (`c(dist_normal(...), dist_poisson(...))`) also makes
exactly one `dnorm()` call and one `dpois()` call, regardless of length -
dispatch cost is O(#families), not O(#distributions), as intended.

## What changed structurally

- `R/00_classes.R`: `class_distribution` (S7, `parent = vecvec::class_vecvec`),
  keeping the legacy `"distribution"` S3 class string prepended to the class
  vector (see "Key decisions" below). `dist_ported_families` registry.
- `new_dist()` builds one `vctrs::new_rcrd()` slot instead of one S3 object
  per element; unchanged signature/behaviour for callers.
- `dist_apply()` splits by storage slot; ported families get one vectorised
  call, everything else falls back to a per-element loop identical in
  behaviour to the pre-migration implementation - so all ~50 families keep
  working via their **existing, unmodified scalar method bodies**.
- `.onLoad()` registers `vec_ptype2`/`vec_cast` via `vecvec::vecvec_register()`.
- `variates()`/`variates<-()` added; `dimnames()`/`dimnames<-()` on a
  distribution are soft-deprecated shims to them (per `_dev/vecvec.md` #4).

## Test suite

908 of 909 expectations pass (all pre-existing tests, plus 17 in-test
`vec_data()` internals updated to the new `dist_elem()` helper - the
audit `_dev/vecvec.md` phase 0 called for). `R CMD check` is clean (0
errors/warnings/notes).

The one remaining failure (`test-dist-degenerate.R`) is a narrow, deep
limitation: `vctrs::vec_cast(1, new_dist())` doesn't reach our registered
S3 `vec_cast` method, because `vec_cast()`'s C-level dispatch doesn't
consult runtime-`registerS3method()`-registered methods for this pairing
the way `UseMethod()` does (confirmed by calling the registered method
directly, which works fine). The much more common path for combining a
distribution with a bare number - `c(dist_normal(1,2), 5)`, which goes
through `` `c.vecvec::vecvec` `` rather than `vec_cast()` directly - already
works correctly. Worth a closer look upstream in vecvec/vctrs before this
goes further; out of scope to chase down further here.

## Key decisions made along the way (not all pre-planned in `_dev/vecvec.md`)

1. **Kept the `"distribution"` S3 class string** as an alias, prepended to
   the S7-generated class vector (`_dev/vecvec.md` open question #5, "Test
   whether S7 tolerates it before ruling it out" - confirmed it does). This
   means the ~20 `<generic>.distribution` methods and all family-level
   `<generic>.dist_<family>` methods keep their existing names completely
   unchanged - S3 dispatch just finds `distribution` first in the class
   chain and falls back to `` .vecvec::vecvec `` methods (mean, sort, Ops
   defaults, etc.) only when we don't override them. This turned out to be
   *much* less invasive than doing everything via `S7::method(generic,
   class_distribution) <-` / `` `generic.pkg::class` `` as first drafted in
   the plan, at the cost of the compatibility risk the plan's open question
   flags (revdeps keying off the bare `"distribution"` string still work;
   ggdist's specific concern doesn't apply here since we kept the string).
2. **`is.numeric()` had to be overridden to `FALSE`.** `class_vecvec`'s
   parent is `S7::class_integer` (the index is the object's base data), so
   `is.numeric(distribution)` was unexpectedly `TRUE` - which made
   testthat's (2e) `all.equal()`-based `expect_equal()` try to arithmetically
   diff two distributions as if they were plain numbers. Not mentioned in
   the plan; worth flagging for the real migration since it likely affects
   other numeric-detecting code in the wild (`is.numeric()` checks in
   dplyr/ggplot2-adjacent code, etc.), not just tests.
3. **`all.equal.distribution()` had to be added.** Pre-migration, `all.equal`
   on two distributions fell through to `all.equal.list()` for free (the old
   class chain included `"list"`). The vecvec-backed container's `typeof()`
   is `"integer"`, so that free ride disappears; added an explicit method
   that compares element-by-element via each raw record's fields.
4. **Nested-distribution families need their `dist` field stored as a *list
   of raw records* (`dist_list()`), not a nested `distribution` vector**,
   contradicting `_dev/vecvec.md` §3.5's suggestion ("each stores a
   `distribution` vecvec of length n, not `vec_data(dist)`"). Storing it as
   a container works fine for scalar arguments, but any bare multi-value
   argument (`quantile(x[["dist"]], c(0.1, 0.9))`) then goes through
   `dist_apply()`'s container-level "constant broadcast" reshaping and comes
   back as a 1-element list instead of a plain vector - which breaks
   internal helpers like `dist_transformed()`'s `monotonic_dir()` that
   assume a plain vector. Pre-migration, `.subset2()` (used by the old
   `transpose()`) bypassed custom `[[` methods entirely and always produced
   raw records for exactly this reason; `dist_list()`/`dist_elem()`/
   `dist_transpose_list()` restore that. §3.5 should be revisited before the
   real migration reaches phase 4.
5. **`generate()`'s new list-return contract (`_dev/vecvec.md` #6.10) is
   "infectious" at family boundaries.** Any not-yet-ported family whose
   method calls `generate()` on a *ported* nested distribution now gets a
   list back instead of a vector (`dist_transformed`, `dist_inflated`,
   `dist_mixture` all do this). Fixed by unwrapping defensively
   (`if (is.list(r)) r <- r[[1L]]`) at each call site rather than only when
   the inner family happens to be ported - keeps those call sites correct
   regardless of which families get ported later.
6. **`vctrs::field()` errors on a missing field name**, unlike the old
   per-element plain-list `$`/`[[` (which silently returned `NULL`) - several
   `Ops.dist_default`-style methods probe for fields (e.g. `e1$dist`) that
   only exist on *some* families. `dist_field_get()` (backing the
   `` `$.dist_default` ``/`` `[[.dist_default` `` shims) restores the old
   silent-`NULL` behaviour.
7. **A handful of scalar-constant / scalar-only bodies had to be fixed for
   both ported families**, exactly as `_dev/vecvec.md` §3.4 anticipated:
   `skewness.dist_normal`/`kurtosis.dist_normal`/`has_symmetry.dist_normal`
   returned a bare constant regardless of `length(x)`; `has_symmetry.dist_default`
   used `isTRUE()` on what's now a vector; `hdr.dist_default` branched on
   `if (has_symmetry(x))` (errors for `length(x) > 1`); `support.dist_default`
   assumed scalar `quantile()`/`density()` calls. Fixed with a "loop
   internally via `vec_slice()`/`Map()` when `vec_size(x) != 1`" pattern,
   which is cheap insurance for any other not-yet-audited `dist_default`
   fallback a newly-ported family might exercise.
8. **`new_dist()` needed two edge-case fixes** beyond the plan's sketch:
   empty-call construction (`new_dist()` with no fields, used all over the
   family files as `vec_is(dist, new_dist())` for type-checking) can't build
   a zero-field `vctrs::new_rcrd()`, so it short-circuits to an empty
   `class_distribution()`; and a literal `NULL` argument (`dist_student_t`'s
   optional `ncp`) can't be a field directly, so it's recycled to a
   `NULL`-per-position list field instead.
9. **`vecvec::vecvec_register()`'s `f_cast_to`/`f_cast_from` parameters are
   wired opposite to what their names suggest** (confirmed empirically by
   inspecting the registered S3 method table) - `f_cast_to` is actually
   invoked when casting *from* a distribution, and `f_cast_from` when casting
   *to* one. Documented inline in `zzz.R`; worth a docs fix upstream in
   vecvec.
10. **`is.nan`/`is.infinite`/`is.finite` are not part of base R's `Math`
    group generic**, unlike vctrs' own `vec_math()` protocol that the old
    `vctrs_vctr`-based `distribution` used (where `vec_math.distribution`'s
    special-casing covered them for free). Needed explicit
    `is.nan.distribution()`/`is.infinite.distribution()`/
    `is.finite.distribution()` methods, or they fall through to
    `` `is.infinite.vecvec::vecvec` ``, which applies per storage slot and
    errors on a raw family record (ggdist's `stat_dist_halfeye()` hits this
    via one of the package's own `dist_truncated()` `@examples`).

## What's *not* done (by design, out of PoC scope)

- Only 2 of ~50 families are vectorised. Every other family works correctly
  via the per-element legacy fallback in `dist_apply()`, `format()`, and
  `Ops`/`Math` - i.e. functionally unchanged, not yet faster.
- `Ops.distribution`/`Math.distribution` (arithmetic) are a plain
  per-element loop over the whole vector, not split/vectorised per slot -
  correct, but doesn't yet get the vectorisation speedup `dist_apply()`
  gives the stats generics. Flagged as a phase-2 follow-up in
  `_dev/vecvec.md` and left alone here to keep risk down.
- Phase 3 (list-parameter/multivariate families) and phase 4 (nested
  families: `dist_transformed`, `dist_truncated`, `dist_inflated`,
  `dist_mixture`, `dist_convolved`) are untouched beyond the mechanical
  compatibility fixes above - their own method bodies are unmodified and
  still scalar/legacy-path.
- The upstream vecvec blocking issues (§5 items 1-3 in `_dev/vecvec.md`)
  turned out to already be fixed in the installed vecvec version
  (1.2.0.9000, reinstalled from `~/github/vecvec` source) - `[<-`,
  `duplicated()`, and an element-aligned apply (`vecvec_apply_aligned()`)
  all exist already. `dist_apply()` here doesn't use
  `vecvec_apply_aligned()` directly though (see decision log in the
  original design discussion) - it's a bespoke implementation matching
  `_dev/vecvec.md` §3.3's sketch, since distributional's broadcasting rules
  (scalar / bare-vector-broadcasts-whole / `list(<vec>)`-pairs-per-element)
  don't map cleanly onto `aligned()`'s pure per-element-alignment model.

## Suggested next steps for a real migration

1. Decide on §3.5 per the finding in point 4 above before starting phase 4.
2. Consider vectorising `Ops.distribution`/`Math.distribution` per-slot
   (mirrors `dist_apply()`'s split) as an early phase-2 follow-up alongside
   more family ports - it's the next biggest unvectorised code path after
   the stats generics.
3. File the `vecvec_register()` parameter-naming and `vec_cast()` FFI
   dispatch findings upstream.
4. Continue phase 2 with the ~35 other closed-form univariate families -
   based on this PoC, most should port with minimal body changes (just the
   "remove scalar assumptions" pass done here for `dist_normal`/`dist_poisson`),
   since their `density`/`cdf`/`quantile` bodies already call vectorised
   base R p/d/q functions directly.
