test_that("Check monotonicity works", {
  qmatrix <- create_qmatrix(3, c(0, 1))
  test <- build_test_parameters(qmatrix, -2, 2, .5, .5)

  # This works no problem
  expect_no_condition(
    check_monotonicity(test)
  )

  # Monotonicity violation

  non_monotone_test <- test
  non_monotone_test[1, 5] <- -3

  # Throws error
  expect_error(
    check_monotonicity(non_monotone_test)
  )

  # Returns false
  expect_all_false(
    check_monotonicity(non_monotone_test, action = "quiet")
  )
})
