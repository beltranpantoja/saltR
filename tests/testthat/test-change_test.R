test_that("Basic arguments work", {
  qmatrix <- create_qmatrix(3, c(1, 1))
  test <- build_test_parameters(qmatrix, -2, 2, 1)

  # Basic arguments:
  new_test <- change_test(
    test,
    level = 2, attribute = 1, items = 4, fun = \(x) 0
  )

  test[4, 5] <- 0

  expect_equal(test, new_test)
})

test_that("General errors", {
  qmatrix <- create_qmatrix(3, c(1, 1))
  test <- build_test_parameters(qmatrix, -2, 2, 1)


  # Warning no matches
  expect_warning(
    change_test(test, level = 2, attribute = 1, items = 1, fun = \(x) 0)
  )

  # Error on double argument
  expect_error(
    change_test(test, fun = \(x) x, values = 0)
  )

  # Can't select levels or attributes that don't exist
  expect_error(
    change_test(test, level = 5)
  )

  expect_error(
    change_test(test, attribute = 5)
  )
})


test_that("Fun errors", {
  qmatrix <- create_qmatrix(3, c(1, 1))
  test <- build_test_parameters(qmatrix, -2, 2, 1)


  # Fun must be a function
  expect_error(
    change_test(test, fun = 0)
  )

  # Function should return single value
  expect_error(
    change_test(test, fun = \(x) rep(0, 2))
  )
})

test_that("Values errors", {
  qmatrix <- create_qmatrix(3, c(1, 1))
  test <- build_test_parameters(qmatrix, -2, 2, 1)


  # Values should be numeric
  expect_error(
    change_test(test, values = rep("a", 18))
  )


  # Error on too few values
  expect_error(
    change_test(test, values = 0)
  )

  # Warning in more values than needed
  expect_warning(
    change_test(test, values = rep(0, 100))
  )
})
