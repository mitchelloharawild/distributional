% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_negative_binomial.R
\name{dist_negative_binomial}
\alias{dist_negative_binomial}
\title{The Negative Binomial distribution}
\usage{
dist_negative_binomial(size, prob)
}
\arguments{
\item{size}{target for number of successful trials, or dispersion
    parameter (the shape parameter of the gamma mixing distribution).
    Must be strictly positive, need not be integer.}

\item{prob}{probability of success in each trial. \code{0 < prob <= 1}.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

A generalization of the geometric distribution. It is the number
of failures in a sequence of i.i.d. Bernoulli trials before
a specified number of successes (\code{size}) occur. The probability of success in
each trial is given by \code{prob}.
}
\details{
We recommend reading this documentation on
\url{https://pkg.mitchelloharawild.com/distributional/}, where the math
will render nicely.

In the following, let \eqn{X} be a Negative Binomial random variable with
success probability \code{prob} = \eqn{p} and the number of successes \code{size} =
\eqn{r}.

\strong{Support}: \eqn{\{0, 1, 2, 3, ...\}}

\strong{Mean}: \eqn{\frac{p r}{1-p}}

\strong{Variance}: \eqn{\frac{pr}{(1-p)^2}}

\strong{Probability mass function (p.m.f)}:

\deqn{
     f(k) = {k + r - 1 \choose k} \cdot (1-p)^r p^k
  }{
     f(k) = (k+r-1)!/(k!(r-1)!) (1-p)^r p^k
  }

\strong{Cumulative distribution function (c.d.f)}:

Too nasty, omitted.

\strong{Moment generating function (m.g.f)}:

\deqn{
     \left(\frac{1-p}{1-pe^t}\right)^r, t < -\log p
  }{
     \frac{(1-p)^r}{(1-pe^t)^r}, t < -\log p
  }
}
\examples{
dist <- dist_negative_binomial(size = 10, prob = 0.5)

dist
mean(dist)
variance(dist)
skewness(dist)
kurtosis(dist)
support(dist)

generate(dist, 10)

density(dist, 2)
density(dist, 2, log = TRUE)

cdf(dist, 4)

quantile(dist, 0.7)

}
\seealso{
\link[stats:NegBinomial]{stats::NegBinomial}
}
