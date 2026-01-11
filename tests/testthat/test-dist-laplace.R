test_that("Laplace distribution", {
  # defaults - testing with explicit parameters
  dist <- dist_laplace(0, 1)
  expect_equal(mean(dist), 0)
  expect_equal(variance(dist), 2)

  # display
  expect_s3_class(dist, "distribution")
  expect_output(print(dist), "Laplace\\(0, 1\\)")
  expect_output(print(dist_laplace(numeric(), numeric())), "<distribution\\[0\\]>")

  # error checking
  expect_error(
    dist_laplace(0, -1),
    "positive"
  )
  expect_error(
    dist_laplace(0, 0),
    "positive"
  )
  expect_silent(
    dist_laplace(mu = 0L, sigma = 1L)
  )

  set.seed(123)
  mu <- c(-0.560475646552213, -0.23017748948328, 1.55870831414912, 0.070508391424576, 
0.129287735160946, 1.71506498688328, 0.460916205989202, -1.26506123460653, 
-0.686852851893526, -0.445661970099958)
  sigma <- c(1.22408179743946, 0.359813827057364, 0.400771450594052, 0.11068271594512, 
0.555841134754075, 1.78691313680308, 0.497850478229239, 1.96661715662964, 
0.701355901563686, 0.472791407727934)
  dist <- dist_laplace(mu, sigma)

  # summary statistics
  expect_equal(mean(dist), c(-0.560475646552213, -0.23017748948328, 1.55870831414912, 0.070508391424576, 
0.129287735160946, 1.71506498688328, 0.460916205989202, -1.26506123460653, 
-0.686852851893526, -0.445661970099958), tolerance = 1e-6)
  expect_equal(median(dist), c(-0.560475646552213, -0.23017748948328, 1.55870831414912, 0.070508391424576, 
0.129287735160946, 1.71506498688328, 0.460916205989202, -1.26506123460653, 
-0.686852851893526, -0.445661970099958))
  expect_equal(variance(dist), c(2.99675249364525, 0.258931980283333, 0.321235511222522, 0.0245013272179761, 
0.617918734169395, 6.38611711695883, 0.495710197346165, 7.73516608150009, 
0.98380020131642, 0.447063430442723))
  expect_equal(skewness(dist), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  expect_equal(kurtosis(dist), c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3))
  
  # symmetry
  expect_equal(has_symmetry(dist), c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

  # density
  expect_equal(
    density(dist, 0),
    c(0.25840877941931, 0.732940031399069, 0.0255261195667388, 2.38908099437143, 
0.712858180290539, 0.107160364045725, 0.397920035658436, 0.133623419562148, 
0.267742715233142, 0.412027664803725)
  )
  
  # log density
  expect_equal(
    density(dist, 0, log = TRUE),
    c(-1.35321253156994, -0.310691392994745, -3.66805305431685, 0.87090877081269, 
-0.338472783975555, -2.23342883709496, -0.921504209314974, -2.0127297371183, 
-1.31772877737028, -0.886664784305052)
  )

  # cdf
  expect_equal(
    cdf(dist, 5),
    c(0.994676987466257, 0.999999756691372, 0.999906711705461, 1, 
0.99992177527756, 0.920458518481099, 0.999945128184019, 0.979325448598997, 
0.999849495376496, 0.999995025840115)
  )
  
  # quantile
  expect_equal(
    quantile(dist, 0.7),
    c(0.0648167011653952, -0.0463753668370739, 1.76343264038643, 
0.127047958837356, 0.413225629536493, 2.62786600460636, 0.71523098707285, 
-0.2604627988623, -0.328582285995298, -0.204148004336135)
  )

  # Test that cdf and quantile are inverses
  probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  expect_equal(
    cdf(dist[1L], quantile(dist[1L], probs)[[1L]])[[1L]], 
    probs,
    tolerance = 1e-10
  )

  # generate
  set.seed(123)
  generated <- generate(dist, 10)
  expect_length(generated, length(dist))
  expect_true(all(sapply(generated, length) == 10))

  large_sample <- generate(dist[[1]], 10000)[[1]]
  expect_equal(mean(large_sample), mu[[1]], tolerance = 0.1)
  expect_equal(var(large_sample), 2 * sigma[[1]]^2, tolerance = 0.2)
})