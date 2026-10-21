# nocov start
.onLoad <- function(...) {
  # Register S7 methods (e.g. those inherited from vecvec::class_vecvec, plus
  # any `S7::method<-` assignments made by this package).
  S7::methods_register()

  # Register vec_ptype2()/vec_cast() so `distribution` participates in vctrs'
  # coercion system, mirroring mixtime's `class_mixtime` registration.
  # `f_cast_to` is used for casting other types *to* a distribution
  # (`vec_cast.<class>.*`), and `f_cast_from` for casting a distribution to
  # other types (`vec_cast.*.<class>`).
  vecvec::vecvec_register(
    class_distribution,
    f_ptype2 = vec_ptype2_distribution,
    f_cast_to = vec_cast_to_distribution,
    f_cast_from = vec_cast_from_distribution
  )

  # vctrs dispatches these on the first class only, which is the S7 class name
  register_s3_method(
    "vctrs", "vec_ptype_abbr", "distributional::distribution",
    vec_ptype_abbr_distribution
  )
  register_s3_method(
    "vctrs", "vec_ptype_full", "distributional::distribution",
    vec_ptype_full_distribution
  )

  if (getRversion() >= "4.3.0") {
    register_s3_method("base", "chooseOpsMethod", "dist_default")
  }
  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
