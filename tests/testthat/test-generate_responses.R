test_that("Check that probability of response generation works", {
  qmat <- diag(3)
  examinees <- lower.tri(diag(3), TRUE) * 1
  test <- build_test_parameters(qmat, -2, 2)

  responses_probs <- generate_responses(examinees, test, get_probs = TRUE)

  # Creating the probs matrix
  expected_probs <- examinees * 0
  expected_probs[examinees == 0] <- exp(-2) / (1 + exp(-2))
  expected_probs[examinees == 1] <- .5

  expect_equal(responses_probs, expected_probs)
})

test_that("Check that response generation works", {
  qmat <- diag(3)
  examinees <- lower.tri(diag(3), TRUE) * 1

  test <- build_test_parameters(qmat, -2, 2)


  set.seed(314)
  responses <- generate_responses(examinees, test, get_probs = FALSE)

  expect_snapshot(responses)
})
