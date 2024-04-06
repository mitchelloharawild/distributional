numderiv <- function(f) {
  function(., ...) {
    vapply(., numDeriv::jacobian, numeric(1L), func = f, ...)
  }
}

symbolic_derivative <- function(inverse, fallback_numderiv = TRUE) {
  if (!fallback_numderiv) return(Deriv::Deriv(inverse, x = 'x'))

  tryCatch(
    Deriv::Deriv(inverse, x = 'x'),
    error = function(...) {
      if (getOption('dist.verbose', FALSE)) {
        message('Cannot compute the derivative of the inverse function symbolicly.')
      }
      numderiv(inverse)
    }
  )
}

# Chain rule
chain_rule <- function(x, y, d_x = symbolic_derivative(x), d_y = symbolic_derivative(y)) {
  function(x) d_x(y(x)) * d_y(x)
}
