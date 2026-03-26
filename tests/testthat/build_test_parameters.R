test_that("Creates mask", {
  Q <- data.item_influence$qmatrix
  expect_snapshot(build_test_parameters(Q))
})

test_that("Factor labels are correct", {
  Q <- data.item_influence$qmatrix
  items <- build_test_parameters(Q)
  labels <- c("0", "1", "2", "3", "1-2", "1-3", "2-3", "1-2-3")

  expect_equal(colnames(items), labels)
})

test_that("Params can be passed with or without a character vector", {
  Q <- data.item_influence$qmatrix
  items_1 <- build_test_parameters(Q, 1, 2, 3, 4)
  items_2 <- build_test_parameters(Q, c(1, 2, 3, 4))
  expect_equal(items_1, items_2)
})

test_that("throws error when there are more params", {
  Q <- data.item_influence$qmatrix

  # At most we can pass 4 values for a 3-attribute Q-matrix
  expect_error(
    build_test_parameters(Q, 1:5),
    regexp = "Expected 4 parameters but got 5 instead"
  )
})
