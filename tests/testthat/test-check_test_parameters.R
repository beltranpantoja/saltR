test_that("Check test parameters works with correct parameters", {
  qmatrix <- create_qmatrix(4, c(0, 0, 1))
  test <- build_test_parameters(qmatrix, 0)


  # Normal calls
  expect_no_error(
    check_test_parameters(test)
  )

  expect_no_error(
    check_test_parameters(test, qmatrix = qmatrix)
  )
})

test_that("Throws error with wrong Q-matrix argument", {
  qmatrix <- create_qmatrix(4, c(0, 0, 1))
  test <- build_test_parameters(qmatrix, 0)

  wrong_q_dimensions <- create_qmatrix(3, c(0, 1, 0))
  wrong_q_items <- qmatrix
  wrong_q_items[1, 1] <- 0

  # Wrong Q-matrix arguments
  expect_error(
    check_test_parameters(test, qmatrix = wrong_q_items)
  )

  expect_error(
    check_test_parameters(test, qmatrix = wrong_q_dimensions)
  )
})

test_that("Throws error with malformed test parameters", {
  qmatrix <- create_qmatrix(4, c(0, 0, 1))
  test <- build_test_parameters(qmatrix, 0)

  wrong_test <- test
  wrong_test[2, 1] <- NA

  # Throw error with test
  expect_error(
    check_test_parameters(wrong_test)
  )
})
