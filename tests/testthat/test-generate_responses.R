test_that("Check that probability of response generation works", {
  qmat <- diag(3)
  sample <- lower.tri(diag(3), TRUE) * 1
  items <- matrix(rep(c(-2, 2), 3), ncol = 2, byrow = TRUE)


  responses_probs <- saltr::generate_responses(qmat, sample, items, get_probs = TRUE)

  # Creating the probs matrix
  expected_probs <- sample * 0
  expected_probs[sample == 0] <- exp(-2) / (1 + exp(-2))
  expected_probs[sample == 1] <- .5

  expect_equal(responses_probs, expected_probs)
})

test_that("Check that response generation works", {
  qmat <- diag(3)
  sample <- lower.tri(diag(3), TRUE) * 1
  items <- matrix(rep(c(-2, 2), 3), ncol = 2, byrow = TRUE)


  set.seed(314)
  responses_simulated <- saltr::generate_responses(qmat, sample, items)
  responses <- matrix(c(
    1, 0, 0,
    1, 1, 0,
    0, 1, 0
  ), ncol = 3, byrow = TRUE)

  expect_equal(responses_simulated, responses)
})
