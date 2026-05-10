test_that("Returns expected shape", {
  test <- test_parameters_example


  # TODO: add  the handling of errors

  expect_snapshot(
    build_response_likelihood(test, c(0, 0, 0))
  )
})
