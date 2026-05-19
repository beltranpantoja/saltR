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


  test1 <- get_test_parameters(mod1)
  test2 <- get_test_parameters(mod2)

  expect_equal(ncol(test1), ncol(test2))
  expect_equal(
    colnames(test1),
    c("0", "1", "2", "3", "1-2", "1-3", "2-3", "1-2-3")
  )
})


test_that("only implemented for linkfct logit", {
  qmatrix <- item_influence$qmatrix
  responses <- item_influence$responses


  mod <- CDM::gdina(responses, qmatrix, linkfct = "identity", progress = FALSE)

  # Fix the number of columns always has to be equal to all the possible values
  expect_error(
    get_test_parameters(mod),
    regexp = "with link logit"
  )
})
