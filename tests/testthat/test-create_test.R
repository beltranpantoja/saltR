test_that("test creation works", {

  test <- saltr::create_test(
    c(.2, .3, .5),
    c(.2, .4, .6),
    test_type="CRUM",
    num_attrs = 2)

  test_matrix <- matrix(c(
    saltr::create_item(.2, .3, .5, item_type="CRUM"),
    saltr::create_item(.2, .4, .6, item_type="CRUM")),
    nrow=2, ncol=4, byrow = TRUE)

  expect_equal(test,test_matrix)
})
