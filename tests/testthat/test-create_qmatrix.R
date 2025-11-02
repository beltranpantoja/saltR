test_that("checking that qmatrix creation works", {
  single_att <- saltr::create_qmatrix(1, 3)
  simple_qmat <- create_qmatrix(3, 1)
  complex_qmat <- create_qmatrix(3, c(2, 2))

  single_att_result <- matrix(1, nrow = 3, ncol = 1)
  simple_qmat_result <- diag(3)
  complex_qmat_result <- matrix(c(
    1, 0, 0,
    1, 0, 0,
    0, 1, 0,
    0, 1, 0,
    0, 0, 1,
    0, 0, 1,
    1, 1, 0,
    1, 1, 0,
    1, 0, 1,
    1, 0, 1,
    0, 1, 1,
    0, 1, 1
  ), ncol = 3, byrow = TRUE)


  expect_setequal(single_att, single_att_result)
  expect_setequal(simple_qmat, simple_qmat_result)
  expect_setequal(complex_qmat, complex_qmat_result)
})
