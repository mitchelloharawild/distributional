% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformed.R
\name{dist_transformed}
\alias{dist_transformed}
\title{Modify a distribution with a transformation}
\usage{
dist_transformed(dist, transform, inverse)
}
\arguments{
\item{dist}{A univariate distribution vector.}

\item{transform}{A function used to transform the distribution. This
transformation should be monotonic over appropriate domain.}

\item{inverse}{The inverse of the \code{transform} function.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}}

The \code{\link[=density]{density()}}, \code{\link[=mean]{mean()}}, and \code{\link[=variance]{variance()}} methods are approximate as
they are based on numerical derivatives.
}
\examples{
# Create a log normal distribution
dist <- dist_transformed(dist_normal(0, 0.5), exp, log)
density(dist, 1) # dlnorm(1, 0, 0.5)
cdf(dist, 4) # plnorm(4, 0, 0.5)
quantile(dist, 0.1) # qlnorm(0.1, 0, 0.5)
generate(dist, 10) # rlnorm(10, 0, 0.5)

}
