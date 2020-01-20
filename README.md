
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distributional

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/distributional)](https://CRAN.R-project.org/package=distributional)
[![Travis build
status](https://travis-ci.org/mitchelloharawild/distributional.svg?branch=master)](https://travis-ci.org/mitchelloharawild/distributional)
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
#>  [1]  0.6586118  1.5063896  1.6021216 -0.4349964  1.7122270  1.7124356
#>  [7] -0.6662537  0.8735667  0.3904079 -0.4629031
#> 
#> [[2]]
#>  [1] 2.5738584 3.4247179 2.2996918 3.8932889 3.3639585 3.4451997 1.7073995
#>  [8] 2.4548082 0.6415280 0.6871957
#> 
#> [[3]]
#>  [1] 2.809199 2.240730 3.736987 2.385023 4.476460 2.698719 5.260902 4.766516
#>  [9] 2.042205 2.239367
#> 
#> [[4]]
#>  [1] 2.731258 2.961121 4.240855 1.141387 2.955539 4.743368 3.493683 3.153980
#>  [9] 1.993267 3.193703
#> 
#> [[5]]
#>  [1] 6.244455 4.474137 3.833082 5.217556 5.040084 1.901594 5.836004 4.301794
#>  [9] 5.927903 5.287696
#> 
#> [[6]]
#>  [1] 5.905338 6.083361 4.138555 5.955001 6.044509 7.997498 5.165772 6.118764
#>  [9] 4.385735 6.001437
#> 
#> [[7]]
#>  [1] 6.524644 5.609954 5.054033 6.986823 7.677272 7.714541 7.190378 7.253781
#>  [9] 6.901528 5.721437
#> 
#> [[8]]
#>  [1] 8.422849 6.962393 8.710357 8.611681 6.753492 8.013936 7.927657 8.611210
#>  [9] 9.187082 6.358655
#> 
#> [[9]]
#>  [1]  7.870424  9.211297  8.584497  7.328819  9.392989 10.025127  8.171993
#>  [8]  7.934730  9.895908  8.105202
#> 
#> [[10]]
#>  [1]  8.683666 10.800799  9.320850 10.263110 10.222065  9.190568  8.804833
#>  [8] 12.986224  9.579248 10.369239
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
