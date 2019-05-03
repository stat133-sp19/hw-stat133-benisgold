context("check checkers")

test_that("check_prob returns true if 0<=prob<=1 else errors", {
  expect_true(check_prob(0.5))
  expect_error(check_prob(1.5), "prob must be a number between 0 and 1")
  expect_error(check_prob(-0.5), "prob must be a number between 0 and 1")
})

test_that("check_trials returns true if non-negative integer else errors", {
  expect_true(check_trials(0))
  expect_true(check_trials(3))
  expect_error(check_trials(0.5), "invalid trials value")
})

test_that("check_success returns true if non-negative integer <= # trials else errors", {
  expect_true(check_success(3, 5))
  expect_true(check_success(5, 5))
  expect_error(check_success(6, 5), "success cannot be greater than trials")
})

context("check summary measures")

test_that("aux_mean is correct", {
  expect_equal(aux_mean(10, 0.5), 5)
  expect_equal(aux_mean(20, 0.2), 4)
  expect_equal(aux_mean(50, 0.9), 45)
})

test_that("aux_variance is correct", {
  expect_equal(aux_variance(10, 0.5), 2.5)
  expect_equal(aux_variance(20, 0.2), 3.2)
  expect_equal(aux_variance(50, 0.9), 4.5)
})

test_that("aux_mode is correct", {
  expect_equal(aux_mode(10, 0.5), 5)
  expect_equal(aux_mode(20, 0.2), 4)
  expect_equal(aux_mode(50, 0.9), 45)
})

test_that("aux_skewness is correct", {
  expect_equal(aux_skewness(10, 0.5), (1 - 2 * 0.5) / sqrt(10 * 0.5 * (1 - 0.5)))
  expect_equal(aux_skewness(20, 0.2), (1 - 2 * 0.2) / sqrt(20 * 0.2 * (1 - 0.2)))
  expect_equal(aux_skewness(50, 0.9), (1 - 2 * 0.9) / sqrt(50 * 0.9 * (1 - 0.9)))
})

test_that("aux_kurtosis is correct", {
  expect_equal(aux_kurtosis(10, 0.5), (1 - 6 * 0.5 * (1 - 0.5)) / (10 * 0.5 * (1 - 0.5)))
  expect_equal(aux_kurtosis(20, 0.2), (1 - 6 * 0.2 * (1 - 0.2)) / (20 * 0.2 * (1 - 0.2)))
  expect_equal(aux_kurtosis(50, 0.9), (1 - 6 * 0.9 * (1 - 0.9)) / (50 * 0.9 * (1 - 0.9)))
})

context("check binomial")

test_that("bin_choose is correct and errors if # successes > # trials", {
  expect_equal(bin_choose(5, 3), choose(5, 3))
  expect_equal(bin_choose(27, 4), choose(27, 4))
  expect_error(bin_choose(4, 5), "k cannot be greater than n")
})

test_that("bin_probability is correct and errors if invalid probability or trials", {
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
  expect_equal(bin_probability(success = 0:2, trials = 5, prob = 0.5), c(0.03125, 0.15625, 0.31250))
  expect_error(bin_probability(success = 0:2, trials = 5, prob = 2), "prob must be a number between 0 and 1")
  expect_error(bin_probability(success = 0:2, trials = -1, prob = 0.5), "invalid trials value")
})

test_that("bin_distribution returns object of correct classes and errors if invalid probability or trials", {
  expect_is(bin_distribution(trials = 5, prob = 0.5), c("bindis", "data.frame"))
  expect_error(bin_distribution(trials = 5, prob = 2), "prob must be a number between 0 and 1")
  expect_error(bin_distribution(trials = -1, prob = 0.5), "invalid trials value")
})

test_that("bin_cumulative returns object of correct classes and errors if invalid probability or trials", {
  expect_is(bin_cumulative(trials = 5, prob = 0.5), c("bincum", "data.frame"))
  expect_error(bin_cumulative(trials = 5, prob = 2), "prob must be a number between 0 and 1")
  expect_error(bin_cumulative(trials = -1, prob = 0.5), "invalid trials value")
})
