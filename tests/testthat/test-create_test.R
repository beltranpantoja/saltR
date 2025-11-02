test_that("test creation from matrix works", {
  item_matrix <- matrix(c(
    .2, .5,
    .3, .6,
    .1, .8
  ), ncol = 2, byrow = TRUE)

  test <- saltr::create_test(item_matrix)

  nonmaster <- exp(test[, 1]) / (1 + exp(test[, 1]))
  master <- exp(rowSums(test)) / (1 + exp(rowSums(test)))

  expect_equal(item_matrix[, 1], nonmaster)
  expect_equal(item_matrix[, 2], master)
})

test_that("test creation with simple and complex items works", {
  test <- saltr::create_test(
    c(.2, .5),
    c(.2, .5, .6, .8),
    c(.2, .3)
  )

  # Cumulative sums for probabilities
  prob <- function(x) exp(x) / (1 + exp(x))

  expect_equal(prob(test[, 1]), c(.2, .2, .2))
  expect_equal(prob(rowSums(test[, 1:2])), c(.5, .5, .3))
  expect_equal(prob(sum(test[2, c(1, 3)])), .6)
  expect_equal(prob(sum(test[2, 1:4])), .8)
})
