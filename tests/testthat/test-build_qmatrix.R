test_that("Rebuilts the Q-matrix", {
  qmatrix <- create_qmatrix(4, c(0, 0, 1))
  test <- build_test_parameters(qmatrix, 0)

  expect_equal(
    build_qmatrix(test),
    qmatrix
  )
})


test_that("Rebuild Q-matrix throws error on malformed item", {
  qmatrix <- create_qmatrix(4, c(0, 0, 1))
  test <- build_test_parameters(qmatrix, 0)

  test[2, 1] <- NA

  expect_error(
    build_qmatrix(test)
  )
})
