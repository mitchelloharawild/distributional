
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distributional

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/distributional)](https://CRAN.R-project.org/package=distributional)
[![R build
status](https://github.com/mitchelloharawild/distributional/workflows/R-CMD-check/badge.svg)](https://github.com/mitchelloharawild/distributional)
<!-- badges: end -->

The distributional package allows distributions to be used in a data
context.

## Installation

<!-- You can install the released version of distributional from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ```{r, eval = FALSE} -->

<!-- install.packages("distributional") -->

<!-- ``` -->

The development version can be installed from
[GitHub](https://github.com/mitchelloharawild/distributional) with:

``` r
# install.packages("remotes")
remotes::install_github("mitchelloharawild/distributional")
```

## Example

Distributions are created using `dist_*()` functions. Currently only the
normal distribution is supported for testing purposes.

``` r
library(distributional)
my_dist <- dist_normal(mu = 1:10, sigma = 1)
my_dist
#> <distribution[10]>
#>  [1] N(1, 1)  N(2, 1)  N(3, 1)  N(4, 1)  N(5, 1)  N(6, 1)  N(7, 1)  N(8, 1) 
#>  [9] N(9, 1)  N(10, 1)
```

The resulting object is a [vctrs](https://vctrs.r-lib.org/) vector, so
it should play nicely with the tidyverse.

``` r
library(tibble)
tibble(my_dist)
#> # A tibble: 10 x 1
#>     my_dist
#>      <dist>
#>  1  N(1, 1)
#>  2  N(2, 1)
#>  3  N(3, 1)
#>  4  N(4, 1)
#>  5  N(5, 1)
#>  6  N(6, 1)
#>  7  N(7, 1)
#>  8  N(8, 1)
#>  9  N(9, 1)
#> 10 N(10, 1)
```

The standard four distribution functions in R are usable via these
generics:

``` r
density(my_dist, 0) # dnorm(0, mean = 1:10, sd = 1)
#>  [1] 2.419707e-01 5.399097e-02 4.431848e-03 1.338302e-04 1.486720e-06
#>  [6] 6.075883e-09 9.134720e-12 5.052271e-15 1.027977e-18 7.694599e-23
cdf(my_dist, 5) # pnorm(5, mean = 1:10, sd = 1)
#>  [1] 9.999683e-01 9.986501e-01 9.772499e-01 8.413447e-01 5.000000e-01
#>  [6] 1.586553e-01 2.275013e-02 1.349898e-03 3.167124e-05 2.866516e-07
quantile(my_dist, 0.1) # qnorm(0.1, mean = 1:10, sd = 1)
#>  [1] -0.2815516  0.7184484  1.7184484  2.7184484  3.7184484  4.7184484
#>  [7]  5.7184484  6.7184484  7.7184484  8.7184484
generate(my_dist, 10) # lapply(1:10, function(mu) rnorm(10, mean = mu, sd = 1))
#> [[1]]
#>  [1]  0.6260168 -0.9305606  0.7356993  0.2667079  0.5025566 -0.9611441
#>  [7] -1.6222022  0.4953597  0.4640103  2.5252427
#> 
#> [[2]]
#>  [1]  2.3840279  2.7053469  2.9774987  2.7936730  2.0005116  2.0577472
#>  [7]  2.7347224  1.9938960 -0.2430128  1.4729865
#> 
#> [[3]]
#>  [1] 4.6893808 4.3223026 2.3880715 0.4202637 2.4209321 3.1602063 2.9938943
#>  [8] 1.2187370 2.8823051 4.2864589
#> 
#> [[4]]
#>  [1] 4.514529 3.610335 2.821020 4.313490 3.935995 4.236815 2.460896 2.338707
#>  [9] 4.242267 4.415833
#> 
#> [[5]]
#>  [1] 3.943064 3.936345 4.508295 5.729769 4.247064 4.955412 3.529347 6.919418
#>  [9] 4.958081 4.921893
#> 
#> [[6]]
#>  [1] 5.256940 4.191919 5.811072 6.351203 6.920027 3.906564 4.261139 5.922717
#>  [9] 7.292142 4.656984
#> 
#> [[7]]
#>  [1] 6.726349 7.518853 6.660905 7.254139 4.478918 7.732778 7.535511 7.216188
#>  [9] 4.660505 6.808340
#> 
#> [[8]]
#>  [1] 9.010770 7.463727 8.317105 8.249328 5.958168 8.268054 7.292959 7.597941
#>  [9] 7.975013 8.343957
#> 
#> [[9]]
#>  [1] 10.264275  9.629504  7.573913  8.691124  8.150178 10.519165  8.122288
#>  [8] 11.177584  9.973637 11.187188
#> 
#> [[10]]
#>  [1]  8.801829  9.425892 11.702573  6.895893 10.314168  9.389751 11.162907
#>  [8] 11.787623 10.671050 11.052555
```

You can also compute intervals using `hilo()`

``` r
hilo(my_dist, 0.95)
#> <hilo[10]>
#>  [1] [-0.95996398,  2.959964]95 [ 0.04003602,  3.959964]95
#>  [3] [ 1.04003602,  4.959964]95 [ 2.04003602,  5.959964]95
#>  [5] [ 3.04003602,  6.959964]95 [ 4.04003602,  7.959964]95
#>  [7] [ 5.04003602,  8.959964]95 [ 6.04003602,  9.959964]95
#>  [9] [ 7.04003602, 10.959964]95 [ 8.04003602, 11.959964]95
```

Additionally, some distributions may support other methods such as
mathematical operations and summary measures.

``` r
my_dist
#> <distribution[10]>
#>  [1] N(1, 1)  N(2, 1)  N(3, 1)  N(4, 1)  N(5, 1)  N(6, 1)  N(7, 1)  N(8, 1) 
#>  [9] N(9, 1)  N(10, 1)
my_dist*3 + 2
#> <distribution[10]>
#>  [1] N(5, 9)  N(8, 9)  N(11, 9) N(14, 9) N(17, 9) N(20, 9) N(23, 9) N(26, 9)
#>  [9] N(29, 9) N(32, 9)
mean(my_dist)
#>  [1]  1  2  3  4  5  6  7  8  9 10
```
