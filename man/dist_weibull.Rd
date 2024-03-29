% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_weibull.R
\name{dist_weibull}
\alias{dist_weibull}
\title{The Weibull distribution}
\usage{
dist_weibull(shape, scale)
}
\arguments{
\item{shape, scale}{shape and scale parameters, the latter defaulting to 1.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Generalization of the gamma distribution. Often used in survival and
time-to-event analyses.
}
\details{
We recommend reading this documentation on
\url{https://pkg.mitchelloharawild.com/distributional/}, where the math
will render nicely.

In the following, let \eqn{X} be a Weibull random variable with
success probability \code{p} = \eqn{p}.

\strong{Support}: \eqn{R^+} and zero.

\strong{Mean}: \eqn{\lambda \Gamma(1+1/k)}, where \eqn{\Gamma} is
the gamma function.

\strong{Variance}: \eqn{\lambda [ \Gamma (1 + \frac{2}{k} ) - (\Gamma(1+ \frac{1}{k}))^2 ]}

\strong{Probability density function (p.d.f)}:

\deqn{
    f(x) = \frac{k}{\lambda}(\frac{x}{\lambda})^{k-1}e^{-(x/\lambda)^k}, x \ge 0
  }

\strong{Cumulative distribution function (c.d.f)}:

\deqn{F(x) = 1 - e^{-(x/\lambda)^k}, x \ge 0}

\strong{Moment generating function (m.g.f)}:

\deqn{\sum_{n=0}^\infty \frac{t^n\lambda^n}{n!} \Gamma(1+n/k), k \ge 1}
}
\examples{
dist <- dist_weibull(shape = c(0.5, 1, 1.5, 5), scale = rep(1, 4))

dist
mean(dist)
variance(dist)
skewness(dist)
kurtosis(dist)

generate(dist, 10)

density(dist, 2)
density(dist, 2, log = TRUE)

cdf(dist, 4)

quantile(dist, 0.7)

}
\seealso{
\link[stats:Weibull]{stats::Weibull}
}
