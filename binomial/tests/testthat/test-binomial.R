context("Test checker functions")

test_that("check_prob works with valid values", {

  expect_true(check_prob(0.5))
  expect_true(check_prob(0.75))
})

test_that("check_prob fails with invalid values", {

  expect_error(check_prob("a"))
  expect_error(check_prob(1:3))
  expect_error(check_prob(-0.5))
  expect_error(check_prob(1.5))
})

test_that("check_trials works with valid values", {

  expect_true(check_trials(5))
  expect_true(check_trials(0))
})

test_that("check_trials fails with invalid values", {

  expect_error(check_trials(1:3))
  expect_error(check_trials("a"))
  expect_error(check_trials(-0.5))
})

test_that("check_success works with valid values", {

  expect_true(check_success(5, 2))
  expect_true(check_success(5, 0:2))
})

test_that("check_success fails with invalid values", {

  expect_error(check_success(5, "a"))
  expect_error(check_success(5, -0.5))
  expect_error(check_success(5, list(1, 2)))
  expect_error(check_success(5, c(3, 6)))
  expect_error(check_success(5, c(NA, 2)))
})

context("Test summary measures")

test_that("aux_mean works with valid values", {

  expect_equal(aux_mean(10, 0.3), 3)
  expect_equal(aux_mean(5, 0.5), 2.5)
})

test_that("aux_mean errors with invalid values", {

  expect_error(aux_mean("a", 0.3))
  expect_error(aux_mean(5))
})

test_that("aux_variance works with valid values", {

  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_equal(aux_variance(5, 1), 0)
})

test_that("aux_variance errors with invalid values", {

  expect_error(aux_variance("a", 0.3))
  expect_error(aux_variance(5))
})

test_that("aux_mode works with valid values", {

  expect_equal(aux_mode(10, 0.3), 3)
  expect_equal(aux_mode(5, 0.5), c(3, 2))
})

test_that("aux_mode errors with invalid values", {

  expect_error(aux_mode("a", 0.3))
  expect_error(aux_mode(5))
})

test_that("aux_skewness works with valid values", {

  expect_equal(aux_skewness(2, 0.5), 0)
})

test_that("aux_skewness errors with invalid values", {

  expect_error(aux_skewness("a", 0.3))
  expect_error(aux_skewness(5))
})

test_that("aux_kurtosis works with valid values", {

  expect_equal(aux_kurtosis(2, 0.5), -1)
})


test_that("aux_kurtosis errors with invalid values", {

  expect_error(aux_kurtosis("a", 0.3))
  expect_error(aux_kurtosis(5))
})

context("Test binomial functions")

test_that("bin_choose works with valid values", {

  expect_equal(bin_choose(5, 2), 10)
  expect_equal(bin_choose(2, 2), 1)
})

test_that("bin_choose errors with invalid values", {

  expect_error(bin_choose(2, 5))
  expect_error(bin_choose("a", 5))
})

test_that("bin_probability works with valid values", {

  expect_equal(bin_probability(2, 5, 0.5), 0.3125)
  expect_equal(bin_probability(0:2, 5, 0.5), c(0.03125, 0.15625, 0.31250))
})

test_that("bin_probability errors with invalid values", {

  expect_error(bin_probability(c(NA, 2), 5, 0.5))
  expect_error(bin_probability(2, "a", 0.5))
  expect_error(bin_probability(2, 5, 1.5))
})

test_that("bin_distribution works with valid values", {

  expect_length(bin_distribution(trials = 5, prob = 0.5)[[1]], 5 + 1)
  expect_equal(bin_distribution(trials = 5, prob = 0.5)[[2]], bin_probability(0:5, 5, 0.5))
  expect_is(bin_distribution(trials = 5, prob = 0.5), c("bindis", "data.frame"))
})

test_that("bin_distribution errors with invalid values", {

  expect_error(bin_distribution(trials = "a", prob = 0.5))
  expect_error(bin_distribution(trials = 5, prob = 1.5))
})

test_that("bin_cumulative works with valid values", {

  expect_length(bin_cumulative(trials = 5, prob = 0.5)[[3]], 5 + 1)
  expect_equal(bin_cumulative(trials = 5, prob = 0.5)[[3]], cumsum(bin_probability(0:5, 5, 0.5)))
  expect_is(bin_cumulative(trials = 5, prob = 0.5), c("bincum", "data.frame"))
})

test_that("bin_cumulative errors with invalid values", {

  expect_error(bin_cumulative(trials = "a", prob = 0.5))
  expect_error(bin_cumulative(trials = 5, prob = 1.5))
})

