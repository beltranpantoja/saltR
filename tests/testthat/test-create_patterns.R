test_that("Expected object is created", {
  # Mainly test expected dimensions
  patterns_1 <- create_patterns(1)
  patterns_2 <- create_patterns(3)

  expected_patt_1 <- matrix(
    c(
      0, 1
    ),
    ncol = 1
  )

  expected_patt_2 <- matrix(
    c(
      0,  0,  0,
      0,  0,  1,
      0,  1,  0,
      0,  1,  1,
      1,  0,  0,
      1,  0,  1,
      1,  1,  0,
      1,  1,  1
    ),
    ncol = 3,
    byrow = TRUE
  )


  expect_equal(patterns_1, expected_patt_1, ignore_attr = TRUE)
  expect_equal(patterns_2, expected_patt_2, ignore_attr = TRUE)
})


test_that("Inclusion and exclusion filters work", {
  # This should include the raising of a warning when all gets filtered
  full_pattern <- matrix(
    c(
      0,  0,  0,
      0,  0,  1,
      0,  1,  0,
      0,  1,  1,
      1,  0,  0,
      1,  0,  1,
      1,  1,  0,
      1,  1,  1
    ),
    ncol = 3,
    byrow = TRUE
  )

  pattern_1 <- create_patterns(3, include_filter = c(0, NA, NA))
  pattern_2 <- create_patterns(3, exclude_filter = c(0, NA, NA))

  pattern_3 <- create_patterns(
    3,
    include_filter = c(0, NA, NA),
    exclude_filter = c(NA, NA, 1)
  )

  expect_true(all(pattern_1[, 1] == 0))
  expect_true(all(pattern_2[, 1] != 0))
  expect_true(all(c(pattern_3[, 1] == 0, pattern_3[, 3] == 0)))

  # And an error when the filter is not properly formed (shape and content)
})

test_that("Wrong inclusion/exclusion filters throw a warning", {
  # This should include the raising of a warning when all gets filtered
  expect_warning(
    create_patterns(3, include_filter = c(2, NA, NA)),
    regexp = "no matches"
  )
})


test_that("Patterns are created with different levels", {
  # All the values should be either 0 or 2
  patterns <- create_patterns(3, levels = c(0, 2))

  expect_true(
    all(patterns %in% c(0, 2))
  )

  # Should throw a warning if no 0 is included
  expect_warning(
    create_patterns(3, levels = c(1, 2)),
    regexp = "0 not included"
  )
})

test_that("Column labels and prefix works", {
  # I should add prefix and name, so also a whole vector can be passed
  exp_labels_1 <- paste0("I", 1:3)
  exp_labels_2 <- paste0("Item", 1:3)

  labels_1 <- create_patterns(3, column_prefix = "I") |> colnames()
  labels_2 <- create_patterns(3, column_labels = exp_labels_2) |> colnames()

  expect_equal(labels_1, exp_labels_1)
  expect_equal(labels_2, exp_labels_2)
})
