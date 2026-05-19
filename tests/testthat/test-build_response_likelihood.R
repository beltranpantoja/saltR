test_that("Basic call", {
  test <- test_parameters_example


  # TODO: add  the handling of errors

  expect_snapshot(
    build_response_likelihood(test, c(0, 0, 0))
  )
})

test_that("Aggregate works", {
  test <- test_parameters_example

  profiles_check <- create_patterns(3, include_filter = c(NA, 1, 1))
  profiles_list <- asplit(profiles_check, 1)

  agg_lik <- build_response_likelihood(
    test,
    profiles_check
  )[, "response_likelihood"]

  agg_profile_1 <- build_response_likelihood(
    test,
    as.vector(profiles_list[[1]])
  )[, "response_likelihood"]

  agg_profile_2 <- build_response_likelihood(
    test,
    as.vector(profiles_list[[2]])
  )[, "response_likelihood"]


  expect_all_true(
    (agg_profile_1 + agg_profile_2) / 2 ==
      agg_lik
  )
})
