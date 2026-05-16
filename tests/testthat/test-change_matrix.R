test_that("Base arguments work", {
  qmatrix <- create_qmatrix(3, c(1, 1))

  changed_qmat <- qmatrix
  changed_qmat[1:4, 1:3] <- 0

  expect_equal(
    change_matrix(qmatrix, fun = \(x) x * 0, at = 1:4, direction = "row"),
    changed_qmat
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
