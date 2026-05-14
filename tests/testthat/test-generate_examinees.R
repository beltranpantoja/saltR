test_that("sample generation returns expected object", {
  set.seed(314)

  sample <- generate_examinees(
    1000,
    3,
    base_rate = .5,
    attr_corr = .1
  )

  expect_type(sample, "double")
  expect_equal(dim(sample), c(1000, 3))
})


test_that(
  "Error and warning handling works for problematic attribute correlations",
  {
    # This correlations are not possible
    set.seed(314)
    num_examinees <- 1000

    expect_error(
      generate_examinees(
        num_examinees,
        3,
        base_rate = c(.9, .9, .1),
        attr_corr = c(.1, .2, .9),
        strict = TRUE
      ),
      regexp = "Invalid joint probability"
    )

    # Having a false binary_correlation allows free generation
    # But there could be a warning if outside the tolerance.
    set.seed(314)
    expect_warning(
      generate_examinees(
        num_examinees,
        3,
        base_rate = .9,
        attr_corr = c(.1, .2, .9),
        tolerance = 0.01,
        strict = FALSE
      ),
      regexp = "tolerance range"
    )

    # Increased tolerance avoids the warnings
    expect_no_warning(
      generate_examinees(
        num_examinees,
        3,
        base_rate = .9,
        attr_corr = c(.1, .2, .9),
        tolerance = Inf,
        strict = FALSE
      )
    )
  }
)
