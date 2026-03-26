test_that("Returns expected shape", {
  item_probs <- matrix(c(
    .2, .8,
    .3, .7,
    .4, .6
  ), ncol = 2, byrow = TRUE)


  lik_full <- build_response_likelihood(item_probs)

  expect_true(all(dim(lik_full) == c(8, 9)))
  expect_true(all(lik_full[, 1:3] %in% c(0, 1)))
  expect_true(all(lik_full[, 9] %in% c(0, 1)))
  expect_true(all(lik_full[, 4:8] < 1))
})

test_that("Likelihood and probabilities follow the constraints", {
  item_probs <- matrix(c(
    .2, .8,
    .3, .7,
    .4, .6
  ), ncol = 2, byrow = TRUE)


  lik_full <- build_response_likelihood(item_probs)

  expect_equal(sum(lik_full[, "lik_non"]), 1)
  expect_equal(sum(lik_full[, "lik_master"]), 1)
  expect_equal(sum(lik_full[, c("prob_non", "prob_master")]), 8)
})

test_that("Passing a prior works as intended", {
  item_probs <- matrix(c(
    .2, .8,
    .3, .7,
    .4, .6
  ), ncol = 2, byrow = TRUE)


  lik_full_high <- build_response_likelihood(item_probs, prior = c(.2, .8))
  lik_full_low <- build_response_likelihood(item_probs, prior = c(.8, .2))


  # This does not consider prior so they should be the same
  expect_true(all(
    lik_full_low[, "MLE"] == lik_full_high[, "MLE"],
    lik_full_low[, "lik_master"] == lik_full_high[, "lik_master"],
    lik_full_low[, "lik_non"] == lik_full_high[, "lik_non"]
  ))

  # The prior will change the probabilities
  expect_true(all(
    lik_full_low[, "prob_master"] < lik_full_high[, "prob_master"]
  ))


  expect_error(
    build_response_likelihood(item_probs, prior = .2),
    regexp = "prior has to have two elements"
  )

  expect_error(
    build_response_likelihood(item_probs, prior = c(2, 3)),
    regexp = "Sum of priors has to be 1"
  )
})

test_that("Passing a response pattern works properly", {
  item_probs <- matrix(c(
    .2, .8,
    .3, .7,
    .4, .6
  ), ncol = 2, byrow = TRUE)

  patt <- diag(3)

  lik_full <- build_response_likelihood(item_probs)
  lik_part <- build_response_likelihood(item_probs, response_patterns = patt)


  expect_equal(lik_full[c(5, 3, 2), ], lik_part, ignore_attr = TRUE)
})
