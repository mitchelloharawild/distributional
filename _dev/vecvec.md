# Migrating `distribution` to a vecvec class

Status: **proof-of-concept underway** - see `_dev/vecvec-poc-status.md` for
what's implemented so far (`dist_normal`/`dist_poisson` ported and vectorised
on branch `vecvec-poc`, ~908/909 pre-existing tests passing) and decisions
made that revise this plan.
Target release: 0.9.0 (major breaking release)
Related: [vecvec](https://pkg.mitchelloharawild.com/vecvec/), [mixtime](https://pkg.mitchelloharawild.com/mixtime/) (`class_mixtime`)

---

## 1. Why

Today `distribution` is a `vctrs_vctr` list, with **one S3 object per element**:

```r
new_dist(mu = 1:5, sigma = 3, class = "dist_normal")
# vctrs::new_vctr(list(<dist_normal>, <dist_normal>, ... x5), class = "distribution")
```

Every statistic is therefore a `mapply()` over elements (`dist_apply()` in `R/utils.R`),
each call computing a scalar. `density(dist_normal(rnorm(1e5)), 0)` performs 1e5
`stats::dnorm()` calls instead of one vectorised call, and the argument-broadcasting
machinery (`arg_listable()`, `transpose_c()`, `validate_recycling()`) exists purely to
re-shape arguments into that per-element loop.

vecvec stores a mixed-type vector as *(list of typed vectors, integer index)* — exactly the
structure needed here, where "type" is the distribution family. Moving to it means:

- **Vectorised family methods.** `density.dist_normal()` becomes one `dnorm()` call over all
  Normal distributions in the vector, regardless of how many other families are mixed in.
- **Dispatch cost is O(#families)** rather than O(#distributions).
- **Compressed storage.** `rep(dist, 1000)` replicates only the index vector; arithmetic on a
  replicated distribution operates on the unique values (see the vecvec README).
- **Shared, tested infrastructure.** Subsetting, `[<-`, `c()`, `rep()`, array `dim()`,
  vctrs proxy/restore, and NA handling come from vecvec instead of being hand-rolled here.
- **`dist_apply()`, `arg_listable()`, `transpose()`, `transpose_c()` shrink or disappear.**
- Consistency with mixtime, which already uses this pattern (`class_mixtime`), and with the
  vecvec documentation, which already names distributional as a target consumer.

The cost is that **every one of the ~50 family files must be re-written from scalar to
vectorised methods**, and the change is visibly breaking for extension packages.

---

## 2. Current architecture (what has to move)

| Piece | File | Role |
|---|---|---|
| `new_dist()`, `wrap_dist()`, `enclass_dist()` | `R/distribution.R` | build one S3 list-object per element, wrap in a `vctrs_vctr` |
| `dist_apply()` | `R/utils.R` | the whole dispatch layer: `mapply()` over elements + argument broadcasting + output re-shaping |
| `arg_listable()`, `transpose_c()`, `validate_recycling()`, `validate_arg_dim()` | `R/utils.R` | broadcasting semantics documented in `vignette("broadcasting")` |
| `<generic>.distribution` methods | `R/distribution.R` | 20 thin wrappers that all call `dist_apply()` |
| `<generic>.dist_<family>` methods | `R/dist_*.R` | ~470 scalar methods across 50 families |
| `dist_default` fallbacks | `R/default.R` | numeric fallbacks (integration, Monte Carlo, root-finding) — all scalar |
| `vec_arith.*`, `vec_math.*`, `vec_ptype2.*`, `vec_cast.*` | `R/distribution.R` | vctrs integration |
| `dimnames()` / `dimnames<-` | `R/distribution.R` | stores the *variate names* of multivariate distributions in the `vars` attribute of the whole vector |
| NULL element = `NA` | `R/distribution.R`, `dist_apply()` | missing distributions are `NULL` list elements, patched to `dist_na` on the fly |

Structural facts worth keeping in mind:

- Parameters are accessed as `x[["mu"]]` inside family methods (list indexing of the element).
- Nested-distribution families (`dist_transformed`, `dist_truncated`, `dist_inflated`,
  `dist_mixture`, `dist_convolved`) store the inner distribution as a raw `dist_*` object
  (`vec_data(dist)`), plus closures (`transform`, `inverse`) per element.
- `dimnames` is a property of the *whole vector*, so a vector cannot currently mix
  distributions with different numbers of variates.

---

## 3. Target architecture

### 3.1 Outer class

```r
# R/00_classes.R (new)
#' @importFrom vecvec class_vecvec
class_distribution <- S7::new_class("distribution", parent = vecvec::class_vecvec)
```

Storage becomes:

```r
c(dist_normal(1:3), dist_poisson(4:5))
#> @x : list(<dist_normal[3]>, <dist_poisson[2]>)
#> @.data (index) : 1:5
```

Verified in a prototype: `format()`, `print()`, `[`, `[[`, `c()`, `rep()`, `dim<-`,
data-frame columns, NA indices and slot pruning on subset all work out of the box with
record-style slots.

### 3.2 Inner (per-family) vectors — recommended: `vctrs_rcrd`

Each slot is a **vectorised** distribution of one family. Recommendation:

```r
new_dist <- function(..., class = NULL, dimnames = NULL) {
  args <- vec_recycle_common(...)
  slot <- vctrs::new_rcrd(args, vars = dimnames, class = c(class, "dist_default"))
  class_distribution(list(slot))
}
```

`new_dist()`'s **signature and user-facing behaviour are unchanged** — it still takes
recycled parameter vectors and returns a distribution of that length. Only the internal
layout changes (1 record vector instead of n S3 objects).

Why `vctrs_rcrd` rather than S7 classes per family (the mixtime approach):

- Satisfies `vec_is()`, which vecvec requires of every slot.
- `[`, `length()`, `vec_c()`, `format()`, `vec_ptype2()` all work already; `vecvec_flatten_adj()`
  merges adjacent slots of the same family automatically via `c()`.
- Matrix/list-valued parameters (`sigma` of `dist_mvnorm`, `dist` of `dist_mixture`) fit as
  list fields.
- Family methods stay S3, so the ~470 existing methods keep their file layout and
  `@export` tags; only their bodies change.

**Field-access shims** keep method bodies nearly untouched:

```r
`[[.dist_default`  <- function(x, i, ...) if (is.character(i)) field(x, i) else NextMethod()
`$.dist_default`   <- function(x, name) field(x, name)
`[[<-.dist_default` <- function(x, i, value) { field(x, i) <- value; x }
`$<-.dist_default`  <- function(x, name, value) { field(x, name) <- value; x }
```

With these, `x[["mu"]]` and `e1$mu <- ...` (used throughout `Ops.dist_normal()` etc.) keep
working — they just return/assign vectors instead of scalars.

### 3.3 The new dispatch layer

`dist_apply()` is rewritten to split by slot instead of mapping over elements:

```r
dist_apply <- function(x, .f, ...) {
  i        <- S7::S7_data(x)
  starts   <- c(0L, cumsum(lengths(x@x)))
  slot_id  <- findInterval(i, starts, left.open = TRUE)
  local    <- i - starts[slot_id]
  args     <- rlang::dots_list(...)

  res <- lapply(seq_along(x@x), function(k) {
    pos    <- which(slot_id == k)                 # positions in the output
    args_k <- lapply(args, slice_arg, pos)        # scalar -> recycled, vector -> sliced,
                                                  # list   -> unpacked (see below)
    rlang::inject(.f(x@x[[k]][local[pos]], !!!args_k))
  })

  scatter(res, split(seq_along(i), slot_id), n = length(i))  # restore original order,
                                                             # fill NA index positions
}
```

Broadcasting semantics from `vignette("broadcasting")` are preserved, and become *simpler*
to implement:

| `at`/`p`/`q`/`size`/`times` form | Current | New |
|---|---|---|
| scalar | recycled by `mapply` | passed through, family method recycles it |
| vector of length n | recycled by `mapply` | sliced to `pos` per slot, one vectorised call |
| `list(<vector>)` (per-distribution multiple values) | `arg_listable()` + `transpose_c()` + `mapply` | repeat the slot indices (`rep(local[pos], lengths(arg))`), one vectorised call, `split()` results back |
| matrix (multivariate points) | `split_matrix_rows()` then per-element | slice rows to `pos`, pass matrix to the slot method |

Two useful consequences: NA index positions never reach a family method (the `dist_na`
patching inside `dist_apply()` and `generate.distribution()` disappears), and the
`arg_listable`/`transpose_c` machinery is replaced by a `rep()` + `split()` pair.

**Optional phase-2 optimisation:** deduplicate `(local, arg)` pairs before calling `.f`, as
`Ops.vecvec::vecvec` already does, so `rep(dist_normal(0,1), 1e6)` stays cheap.

### 3.4 Method contract for family methods (the actual porting work)

`x` is now a *vector* of n distributions of one family; arguments are length 1 or n.

| Generic | Returns |
|---|---|
| `density`, `log_density`, `cdf`, `log_cdf`, `quantile`, `log_quantile` | numeric length n (or list of n for multivariate/matrix output) |
| `generate` | **list of n vectors** (`times` is length n) — *contract change*, see §6 |
| `mean`, `median`, `variance`, `skewness`, `kurtosis` | numeric length n |
| `covariance` | numeric length n, or list of n matrices (multivariate) |
| `support` | `support_region` of length n |
| `hilo`, `hdr` | `hilo`/`hdr` of length n |
| `format` | character length n |
| `family` | character length n (cheap: `rep(fam, n)`) |
| `parameters` | data frame with n rows |
| `dim` | integer length n (number of variates) |
| `has_symmetry` | logical length n |
| `Ops`, `Math` | a distribution slot vector of the same length |

Most univariate closed-form families are *already* written in a way that vectorises (e.g.
`stats::dnorm(at, x[["mu"]], x[["sigma"]])` is correct unchanged once `x[["mu"]]` returns a
vector). The genuinely scalar ones are:

- `R/default.R` fallbacks — `stats::integrate()`, `stats::optim()`, `stats::uniroot()`,
  Monte-Carlo `cdf()`, `hdr()`. These need an explicit `vapply()`/`Map()` loop *inside* the
  method (the loop moves from the framework into the fallback, where it belongs).
- Nested-distribution families (§3.5).
- `dist_wrap` (`do.call()` per element unless the wrapped d/p/q/r are vectorised — they
  usually are, so this may vectorise for free).
- Families with per-element list parameters: `dist_multinomial`, `dist_dirichlet`,
  `dist_mvnorm`, `dist_mvt`, `dist_categorical`, `dist_sample`, `dist_percentile`/`dist_quantile`,
  `dist_density`, `dist_cdf` — these keep a `Map()`/`vapply()` loop but over list fields.

### 3.5 Nested-distribution families

`dist_transformed`, `dist_truncated`, `dist_inflated`, `dist_mixture`, `dist_convolved` hold
inner distributions. In the new layout each stores a **`distribution` vecvec** in a field:

```r
dist_transformed <- function(dist, transform, inverse) {
  new_dist(
    dist = dist,                 # a distribution vecvec of length n, not vec_data(dist)
    transform = list_of_fns,     # list field, length n
    inverse   = list_of_fns,
    class = "dist_transformed"
  )
}
```

`new_rcrd()` accepts a `distribution` field as long as `vec_size()`/`vec_slice()` work on it
(they do, via vecvec's proxy/restore). Inner methods then call the *vectorised*
`density(x[["dist"]], at)` and get n values back in one call — recursion becomes vectorised
too. `dist_mixture` keeps a list-of-distributions field (one entry per element, each a
length-K distribution) and stays loop-based over elements initially.

This is the highest-risk part of the migration and should be a later phase (§7).

### 3.6 Methods distributional must override from vecvec

vecvec provides defaults that call `unvecvec()` — which **errors for distributions** because
two families have no common ptype (confirmed in prototype). All of these need a
`class_distribution` method:

| vecvec default | Needed override |
|---|---|
| `mean`, `median`, `quantile` (`R/statistic.R`) | route to `dist_apply()` |
| `var`, `cov`, `cor` (S7 generics masking stats) | distributional keeps its own `variance()`/`covariance()`; decide whether to also give `var`/`cov`/`cor` methods or let them error informatively |
| `xtfrm` / `sort` (`R/sort.R`) | error, or order by a documented statistic (e.g. median) — currently unsupported, so erroring is fine |
| `Summary` (`R/group-methods.R`) | `sum()` = `Reduce("+")` (convolution); `min`/`max`/`range`/`prod` should error |
| `Ops` comparison operators | vecvec `unvecvec()`s the result of `==`, `<`, … — distributions need `==`/`!=` via `vec_proxy_equal()` and an error for ordering ops |
| `as.character`, `as.double`, … (`R/coercion.R`) | `as.character()` should give `format()`; numeric coercions should error |
| `duplicated`, `anyDuplicated` | broken upstream for record slots (§5) |

### 3.7 vctrs integration

Replace the hand-written `vec_ptype2.distribution.*` / `vec_cast.distribution.*` /
`vec_arith.*` / `vec_math.*` with vecvec's registration hook, mixtime-style:

```r
.onLoad <- function(...) {
  S7::methods_register()
  vecvec::vecvec_register(
    class_distribution,
    f_ptype2  = vec_ptype2_distribution,   # distribution + numeric -> distribution
    f_cast_to = vec_cast_to_distribution   # numeric -> dist_degenerate()
  )
  if (getRversion() >= "4.3.0") register_s3_method("base", "chooseOpsMethod", "dist_default")
  invisible()
}
```

`vec_arith`/`vec_math` are no longer used — arithmetic goes through
`Ops.vecvec::vecvec` (which already computes on unique operand pairs) down to
`Ops.dist_<family>`, and `Math.vecvec::vecvec` down to `Math.dist_default`.
`chooseOpsMethod` registration must be extended to `distributional::distribution` so mixed
dispatch (e.g. with `hilo`) stays predictable.

Printing: `pillar_shaft` and `vec_ptype_abbr` must be re-registered on
`"distributional::distribution"`; `format.distribution` is replaced by an S7
`method(format, class_distribution)` that delegates to the slots (vecvec's own `format`
method already does exactly this, so it may only need overriding for the `width` argument
used by `pillar_shaft`).

---

## 4. `dimnames` clash (needs an API decision)

vecvec uses `dim()`/`dimnames()` for **array shape** (it supports matrix-shaped vecvecs).
distributional currently uses `dimnames()` for the **variate names** of a multivariate
distribution, stored on the whole vector. These cannot both keep the name.

Recommendation: move variate names onto the slot (`vars` attribute of the record) and expose
them through a new generic:

```r
variates(x)          # character vector of variate names, or NULL
variates(x) <- value
```

`dimnames(x)` / `dimnames(x) <- value` on a distribution get a deprecation shim that forwards
to `variates()` while the vector has a single slot, and errors otherwise.

Upside: because variate names live per slot, a distribution vector can finally **mix
distributions of different dimension** (e.g. `c(dist_normal(0,1), dist_mvnorm(...))`), which
is impossible today. `dist_apply()` picks up `variates()` per slot when setting `colnames()`
on matrix output, replacing the single `dn <- dimnames(x)` lookup.

---

## 5. Upstream vecvec work required

Found while prototyping a record-slot vecvec. Written up with reprexes in
`vecvec/todo.md`; summarised here with their impact on this migration. Items 1–3 are
blocking, 4–6 are quality-of-implementation, 7 is documentation.

1. **`[<-` corrupts compressed vectors (silent data loss).** `x <- rep(vecvec(1.5), 3);
   x[2] <- 9` gives `9 NA 9` instead of `1.5 9 1.5`, including through data frame
   assignment. `[<-.vecvec::vecvec` drops the stored values being overwritten without
   checking whether other (duplicated) indices still reference them, then re-ranks all
   indices. Compression is one of the main reasons to adopt vecvec here, and `rep(dist, n)`
   is an ordinary object in distributional, so this must be fixed first.
2. **`duplicated()` / `anyDuplicated()` are wrong for record slots.** `R/predicates.R` uses
   `unlist(x@x[i], recursive = FALSE)`, which destructures an rcrd into its fields; a single
   rcrd slot is enough to return `NA`s, and `anyDuplicated()` reports `0`. `unique()`
   inherits it. Fix: `vec_c(!!!x@x[i])`. Directly affects `unique(dist)`.
3. **Argument-splitting apply.** `vecvec_apply(x, .f, ...)` passes `...` unchanged to every
   slot (confirmed: an element-aligned argument is silently recycled per slot, with a
   recycling warning). This is the one piece of infrastructure the migration needs and
   cannot get today. It could live in distributional's `dist_apply()`, but a general
   `vecvec_apply2(x, .f, ..., .align = TRUE)` would be reusable (mixtime has the same need).
   Decide where it belongs before writing it twice.
4. **Cumulative `Math` ops silently give per-slot results.** The guard in
   `Math.vecvec::vecvec` is one string, `c("cumsum, cumprod, cummax, cummin")`, so it never
   fires: `cumsum(vecvec(1:3, c(10, 20)))` returns `1 3 6 10 30`. Low impact here (cumulative
   ops on distributions are meaningless and should abort), but distributional inherits
   whatever it does, so it needs to abort rather than return numbers.
5. **`vec_proxy_equal()` builds one object per stored value** (`as.list(x)`): 0.15s for
   20,000 unique values vs ~0s atomic. Cheap when few unique values are stored (the
   compression case) but linear otherwise, and it drives `vec_match()`, `vec_in()`,
   `vec_group_id()`, dplyr joins and `group_by()`. This is exactly the per-element object
   construction the migration exists to remove, so it would reintroduce the old cost on
   grouping/joining a distribution column. A slot-wise proxy is worth pursuing upstream.
6. **`is_altrep()` costs ~54µs per call** and runs per adjacent slot pair in
   `vecvec_flatten_adj()`, i.e. on every construction, `c()` and `[<-`: 4,000 slots takes
   0.55s, `c()` of 1,000 vecvecs takes 0.18s. Constant-factor, not a scaling cliff, but
   `c()` of many distributions (one slot per family per operand) is a hot path here.
7. **`unvecvec()` failure mode.** For slots with no common ptype it throws a raw vctrs
   ptype2 error naming neither the generic nor the vecvec, from deep inside
   `mean()`/`sort()`/`as.double()`. distributional overrides these anyway (§3.6), but the
   list of inherited methods that assume a common ptype should be documented for subclass
   authors.

`vec_cast()` to a vecvec with duplicated indices also errors (the documented TODO in
`vctrs.R`), but is unreachable through `vec_c()`/`vec_slice()`/ptype-based casting, so it is
not blocking.

---

## 6. Breaking changes

### User-facing

1. **`class(dist)` changes** from `c("distribution", "vctrs_vctr", "list")` to
   `c("distributional::distribution", "vecvec::vecvec", "integer", "S7_object")`.
   `inherits(x, "distribution")` becomes `FALSE`. `is_distribution()` is updated, but any
   user or package code that tests the class string, or that registers an S3 method for
   class `"distribution"`, breaks. **This is the single largest compatibility break.**
2. **`vec_data(dist)` no longer returns a list of distribution objects** — it returns the
   integer index vector. Code reaching inside a distribution must use `x@x` (slots) or
   `as.list(x)`. Used in 17 places in this package's own tests, and by extension packages.
3. **`dimnames(dist)` changes meaning** (array dimnames, not variate names) — see §4.
   `dimnames(dist) <- c("x", "y")` is deprecated in favour of `variates<-`.
4. **`dist[[i]]`** currently returns a length-1 *distribution* (`[[.distribution` is an alias
   for `[`). vecvec's `[[` drops to the underlying slot type, i.e. a length-1 `dist_<family>`
   record. Either keep the current aliasing (override `[[`) or accept the change and document
   it. Recommendation: **keep `[[` as an alias for `[`** to limit churn.
5. **Ordering/coercion behaviour** inherited from vecvec differs: `sort()`, `xtfrm()`,
   `as.double()`, `min()`/`max()` now come from vecvec unless overridden (§3.6). Each must be
   given an explicit method or an informative error, or users will get confusing
   `unvecvec()` ptype errors.
6. **`c()` of distributions with different variate counts now succeeds** (previously an error
   from `vec_ptype2.distribution.distribution`). This is a feature, but it changes error
   behaviour that some code may rely on.
7. `format()`/`print()` output should be identical; the header line changes from
   `<distribution[n]>` to whatever `vec_ptype_full()` produces (override to keep
   `<distribution[n]>`).

### Extension API (packages that define their own families)

8. **All `<generic>.dist_<family>` methods change contract from scalar to vectorised.** Any
   package defining a custom family via `new_dist(..., class = "dist_myfamily")` must rewrite
   its methods. `new_dist()` itself keeps its signature.
9. **Parameter access.** `x[["par"]]` keeps working via the `[[.dist_default` shim, but
   `unclass(x)`, `x$par` on a plain list, `names(x)`, and `lapply(x, …)` over parameters
   change (a record is not a list). `parameters.dist_default()` needs rewriting for records.
10. **`generate()` family methods must return a list of n vectors**, not a single vector.
11. Nested-distribution families receive a `distribution` vector in the `dist` field rather
    than a bare `dist_*` object (§3.5).

### Dependencies

12. New hard dependencies: `vecvec`, `S7`. vecvec is not yet widely installed; S7 requires
    R >= 3.5.0 so the declared `R (>= 4.0.0)` floor is unaffected. `chooseOpsMethod` gains
    relevance for R >= 4.3.0 as before.

### Known downstream impact (~35 CRAN reverse dependencies)

- **ggdist** registers 6 S3 methods on class `"distribution"`
  (`distr_function.distribution`, `point_interval.distribution`, `hdi_`, `hdci_`, `Mode`,
  `distr_point_interval`) — all silently stop dispatching under change (1). This needs
  coordination with ggdist before release.
- **fabletools/fable/vital** consume distributions through the generics rather than S3 methods
  on the class, so they are lower-risk, but `fabletools::dist_types()` and any internal
  `vec_data()` use must be checked.
- posterior, tidybayes, marginaleffects, parameters, gratia, epiparameter, weird and the
  remaining revdeps are mostly generic-level consumers; a full `revdepcheck` run is required
  before release.

**Mitigation options for (1):** none are clean. Registering the S3 methods under both class
strings is possible *within* distributional, but downstream methods keyed on `"distribution"`
cannot be rescued from here. Plan on a coordinated release with ggdist and a clearly
sign-posted NEWS entry.

---

## 7. Implementation plan

Work on a long-lived `vecvec` branch; each phase should leave the test suite green.

**Phase 0 — groundwork (no behaviour change)**
- Fix the blocking vecvec issues: `[<-` on compressed vectors (§5.1) and
  `duplicated()`/`anyDuplicated()` for record slots (§5.2); decide where the aligned apply
  lives (§5.3). See `vecvec/todo.md`.
- Add a benchmark script (`_dev/bench.R`): `density()`/`cdf()`/`quantile()` over 1e5–1e6
  distributions, single-family and mixed-family, plus `rep()`-heavy cases. Record baselines.
- Audit the 17 `vec_data()` uses in tests and rewrite them to public API where possible, so
  the suite is a cleaner regression harness.

**Phase 1 — the shell**
- `R/00_classes.R`: `class_distribution`; `.onLoad()` with `S7::methods_register()` +
  `vecvec_register()`.
- Rewrite `new_dist()` to build one rcrd slot; add the `[[`/`$` field shims on `dist_default`.
- Rewrite `dist_apply()` with per-slot argument slicing (§3.3).
- Port `R/distribution.R`: the 20 `<generic>.distribution` wrappers, `format`/`print`/pillar,
  `is_distribution()`, `[[` aliasing, and the vecvec method overrides in §3.6.
- Keep the old scalar family methods working by having `dist_apply()` fall back to a
  per-element loop when a family has not yet been ported (`inherits(x, "dist_scalar_legacy")`
  or a registry of ported families). This keeps the suite green while porting.

**Phase 2 — simple families**
- Port the ~35 closed-form univariate families (`dist_normal`, `dist_poisson`, `dist_gamma`, …).
  Most bodies are unchanged; the work is verifying vectorisation and removing scalar
  assumptions (`if (is.na(x)) …`, `switch()` on a parameter, etc.).
- Port `R/default.R` fallbacks with explicit internal loops.
- Port `Ops`/`Math` methods (`dist_normal`, `dist_lognormal`, …) — check they behave under
  `Ops.vecvec::vecvec`'s unique-pair evaluation.

**Phase 3 — list-parameter and multivariate families**
- `dist_multinomial`, `dist_dirichlet`, `dist_mvnorm`, `dist_mvt`, `dist_categorical`,
  `dist_sample`, `dist_quantile`/`dist_percentile`, `dist_density`, `dist_cdf`, `dist_wrap`.
- Introduce `variates()`/`variates<-` and the `dimnames` deprecation shim (§4).
- Matrix-argument handling in `dist_apply()` (`at`, `q` as matrices of points).

**Phase 4 — nested families**
- `dist_transformed`, `dist_truncated`, `dist_inflated`, `dist_convolved`, `dist_mixture` (§3.5).
- Remove the legacy per-element fallback from `dist_apply()`.

**Phase 5 — cleanup and release prep**
- Delete `arg_listable()`, `transpose()`, `transpose_c()`, `validate_recycling()`,
  `wrap_dist()`, `enclass_dist()`, `vec_arith.*`, `vec_math.*` and the old `vec_ptype2`/`vec_cast`
  methods.
- Add the compression optimisation (dedupe operand pairs) if benchmarks justify it.
- Re-run benchmarks; document the speedups in NEWS.
- `vignette("broadcasting")` — verify every documented behaviour still holds; add a section on
  mixed-family storage.
- New `vignette("extending")` or an update to the developer docs for the vectorised method
  contract (§3.4) — extension authors need this.
- `revdepcheck`; coordinate with ggdist; write the breaking-changes NEWS section.

---

## 8. Testing strategy

- The existing 3,148 lines of tests are almost entirely behaviour-level (values of
  `density()`, `cdf()`, `quantile()`, …) and are the primary regression harness. They should
  pass unchanged apart from the 17 `vec_data()` uses and any `dimnames()` tests.
- Add tests specifically for the new structure:
  - a mixed-family vector produces the same statistics as the concatenation of the
    single-family results (this is the invariant the whole migration rests on);
  - subsetting/reordering/`rep()`/`[<-` preserve values and slot pruning;
  - NA (index-level) propagation through every generic;
  - list-valued and matrix-valued arguments (the broadcasting vignette's examples as tests);
  - `c()` across differing variate counts;
  - each ported family evaluated at n = 0, 1, and many, with mixed parameter recycling.
- Add a "vectorisation" test helper asserting that a family method is called exactly once per
  slot (e.g. via a counting `trace()`), so future changes don't silently reintroduce
  per-element loops.

---

## 9. Open questions

1. **Does the aligned-argument apply belong in vecvec or distributional?** mixtime has the
   same need; a shared implementation avoids divergence, but distributional's list-argument
   broadcasting is domain-specific.
2. **`dist_missing()`**: keep it as a family slot, or represent missing distributions as `NA`
   indices (vecvec's native missingness)? NA indices are cleaner and make `is.na()`, `[<-`,
   and `vec_init()` work for free, but `family()`/`format()` of an NA element and the
   `dist_missing()` constructor's return value change. Recommendation: keep the family in
   phase 1, evaluate switching in phase 5.
3. **Comparison operators.** Should `==` compare distributions structurally (family +
   parameters) via a slot-wise `vec_proxy_equal()`? Needed for `unique()`, `match()`, and
   grouping distribution columns — and vecvec's current proxy is O(n) object construction (§5.4).
4. **`sort()`/`order()`** on distributions: error, or order by median? Currently unsupported.
5. **Should `distribution` keep an S3 `"distribution"` class string** in the class attribute
   for back-compatibility? S7 sets the class attribute itself; forcing an extra entry is
   fragile and may break S7 dispatch, but it would rescue ggdist's methods. Test whether S7
   tolerates it before ruling it out.
6. **Naming**: `variates()` vs `dist_variates()` vs keeping `dimnames()` and forgoing vecvec's
   array support for distributions.
