test_that("Returns expected object", {
  qmatrix <- item_influence$qmatrix
  responses <- item_influence$responses

  # This also helps check that the qmatrix names are being handled
  colnames(qmatrix) <- paste0("A", 1:3)
  model <- fit_lcdm(responses, qmatrix)

  expect_snapshot(
    get_probabilities_by_profile(model)
  )
})

test_that("Throws error if model does not use linkfct logit", {
  qmatrix <- item_influence$qmatrix
  responses <- item_influence$responses

  model <- CDM::gdina(
    responses, qmatrix,
    linkfct = "identity", progress = FALSE
  )

  expect_error(
    get_probabilities_by_profile(model),
    regexp = "link logit"
  )
})

test_that("Argument marginal_attr works properly", {
  qmatrix <- item_influence$qmatrix
  responses <- item_influence$responses
  model <- fit_lcdm(responses, qmatrix)


  expect_snapshot(
    get_probabilities_by_profile(model, marginal_attr = 1)
  )
  expect_snapshot(
    get_probabilities_by_profile(model, marginal_attr = 2)
  )
})

test_that("Argument which_items works properly", {
  qmatrix <- item_influence$qmatrix
  responses <- item_influence$responses
  model <- fit_lcdm(responses, qmatrix)

  expect_snapshot(
    get_probabilities_by_profile(model, which_items = 1)
  )
  expect_snapshot(
    get_probabilities_by_profile(model, which_items = c(1, 2))
  )
})

test_that("Arguments marginal_attr and which_items works properly in tandem", {
  qmatrix <- item_influence$qmatrix
  responses <- item_influence$responses
  model <- fit_lcdm(responses, qmatrix)

  expect_snapshot(
    get_probabilities_by_profile(
      model,
      marginal_attr = 1,
      which_items = c(2, 3)
    )
  )
})
