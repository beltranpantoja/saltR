test_that("simple LCDM item creation works", {
  probs <- c(.2, .5)
  parameters <- saltr::create_item(probs)

  logits <- c(parameters[1], sum(parameters))

  calculated_probs <- exp(logits) / (1 + exp(logits))

  expect_equal(
    calculated_probs,
    probs
  )
})

test_that("multiple attr LCDM item creation works", {
  probs <- c(.2, .5, .6, .8)
  parameters <- saltr::create_item(probs)

  logits <- c(
    parameters[1],
    sum(parameters[c(1, 2)]),
    sum(parameters[c(1, 3)]),
    sum(parameters)
  )

  calculated_probs <- exp(logits) / (1 + exp(logits))

  expect_equal(
    calculated_probs,
    probs
  )
})


test_that("two attribute DINA item creation works", {
  lcdm_probs <- c(.2, .2, .2, .8)
  dina_probs <- c(.2, .8)

  lcdm_parameters <- saltr::create_item(
    lcdm_probs
  )

  dina_parameters <- saltr::create_item(
    dina_probs,
    type = "DINA", num_attrs = 2
  )

  expect_equal(
    lcdm_parameters,
    dina_parameters
  )
})

test_that("two attribute DINO item creation works", {
  lcdm_probs <- c(.2, .8, .8, .8)
  dino_probs <- c(.2, .8)

  lcdm_parameters <- saltr::create_item(
    lcdm_probs
  )

  dino_parameters <- saltr::create_item(
    dino_probs,
    type = "DINO", num_attrs = 2
  )

  expect_equal(
    lcdm_parameters,
    dino_parameters
  )
})


test_that("two attribute RRUM item creation works", {
  lcdm_probs <- c(.2, .4, .6, .8)
  rrum_probs <- c(.2, .4, .6)

  lcdm_parameters <- saltr::create_item(lcdm_probs)
  rrum_parameters <- saltr::create_item(rrum_probs, type = "RRUM")

  expect_equal(
    lcdm_parameters,
    rrum_parameters
  )
})
