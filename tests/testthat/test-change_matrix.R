test_that("Base arguments work", {
  qmatrix <- create_qmatrix(3, c(1, 1))
  test <- build_test_parameters(qmatrix, -2, 2, .5)

  # Simple function
  expect_snapshot(
    change_matrix(qmatrix, fun = \(x) x * 0)
  )

  # Direction argument
  expect_snapshot(
    change_matrix(qmatrix, fun = \(x) x * 0, direction = "col")
  )

  # At argument
  expect_snapshot(
    change_matrix(qmatrix, fun = \(x) x * 0, at = 2)
  )

  expect_snapshot(
    change_matrix(qmatrix, fun = \(x) x * 0, at = 2, direction = "col")
  )

  expect_snapshot(
    change_matrix(qmatrix, fun = \(x) x * 0, at = 1:4)
  )

  expect_snapshot(
    change_matrix(qmatrix, fun = \(x) x * 0, at = \(row) row[1] == 1)
  )
})


test_that("Utility functions work", {
  qmatrix <- create_qmatrix(3, c(1, 1))
  test <- build_test_parameters(qmatrix, -2, 2, .5)

  # Utility functions
  expect_snapshot(
    change_matrix(test, fun = \(x) x * 0, at = mat_is_complex(2))
  )

  expect_snapshot(
    change_matrix(qmatrix, fun = \(x) x * 0, at = mat_is_complex(1, 2))
  )

  expect_snapshot(
    change_matrix(qmatrix, fun = \(x) x * 0, at = mat_measures_attr(1))
  )

  expect_snapshot(
    change_matrix(test, fun = \(x) x * 0, at = mat_measures_attr(1))
  )
})


test_that("function validation works", {
  qmatrix <- create_qmatrix(3, c(1, 1))
  test <- build_test_parameters(qmatrix, -2, 2, .5)

  # Error at wrong size fun
  expect_error(
    change_matrix(test, fun = \(x) 0)
  )
})
