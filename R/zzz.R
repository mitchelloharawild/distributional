# nocov start
.onLoad <- function(...) {
  # Register S7 methods (e.g. those inherited from vecvec::class_vecvec, plus
  # any `S7::method<-` assignments made by this package).
  S7::methods_register()

  # Register vec_ptype2()/vec_cast() so `distribution` participates in vctrs'
  # coercion system, mirroring mixtime's `class_mixtime` registration.
  # NB: despite the parameter names, `vecvec_register()`'s `f_cast_to` is the
  # function invoked when `x` (not `to`) is our class - i.e. it casts *from*
  # a distribution to something else - and `f_cast_from` is invoked casting
  # some other type *to* a distribution. Wired up accordingly (confirmed
  # against the registered S3 method table, not just the docs).
  vecvec::vecvec_register(
    class_distribution,
    f_ptype2 = vec_ptype2_distribution,
    f_cast_to = vec_cast_from_distribution,
    f_cast_from = vec_cast_to_distribution
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
