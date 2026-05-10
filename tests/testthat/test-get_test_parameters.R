test_that("parameters complete works", {
  qmatrix <- item_influence$qmatrix
  responses <- item_influence$responses

  mod1 <- CDM::gdina(
    responses, qmatrix,
    rule = "GDINA", linkfct = "logit", progress = FALSE
  )

  mod2 <- CDM::gdina(
    responses, qmatrix,
    rule = "ACDM", linkfct = "logit", progress = FALSE
  )

  mod3 <- CDM::gdina(
    responses, qmatrix,
    rule = "DINA", linkfct = "logit", progress = FALSE
  )

  test1 <- get_test_parameters(mod1, pretty_print = FALSE)
  test2 <- get_test_parameters(mod2, pretty_print = FALSE)
  test3 <- get_test_parameters(mod3, pretty_print = FALSE)

  expect_equal(ncol(test1), ncol(test2))
  expect_equal(ncol(test1), ncol(test3))
  expect_equal(
    colnames(test1),
    c("0", "1", "2", "3", "1-2", "1-3", "2-3", "1-2-3")
  )
})

test_that("parameters incomplete work", {
  qmatrix <- item_influence$qmatrix
  responses <- item_influence$responses


  mod1 <- CDM::gdina(
    responses, qmatrix,
    rule = "GDINA", linkfct = "logit", progress = FALSE
  )

  mod2 <- CDM::gdina(
    responses, qmatrix,
    rule = "ACDM", linkfct = "logit", progress = FALSE
  )

  mod3 <- CDM::gdina(
    responses, qmatrix,
    rule = "DINA", linkfct = "logit", progress = FALSE
  )

  # None of these should have the 1-2-3 effect
  test1 <- get_test_parameters(mod1, complete = FALSE, pretty_print = FALSE)
  test2 <- get_test_parameters(mod2, complete = FALSE, pretty_print = FALSE)
  test3 <- get_test_parameters(mod3, complete = FALSE, pretty_print = FALSE)

  expect_equal(ncol(test1), 7)
  expect_equal(ncol(test2), 4)
  expect_equal(ncol(test3), 7)
  expect_equal(colnames(test1), c("0", "1", "2", "3", "1-2", "1-3", "2-3"))
  expect_equal(colnames(test2), c("0", "1", "2", "3"))
  expect_equal(colnames(test3), c("0", "1", "2", "3", "1-2", "1-3", "2-3"))
})

test_that("only implemented for linkfct logit", {
  qmatrix <- item_influence$qmatrix
  responses <- item_influence$responses


  mod <- CDM::gdina(responses, qmatrix, linkfct = "identity", progress = FALSE)

  # Fix the number of columns always has to be equal to all the possible values
  expect_error(
    get_test_parameters(mod, pretty_print = FALSE),
    regexp = "with link logit"
  )
})
