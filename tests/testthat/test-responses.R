test_that("Response generation works", {

  qmat <- kronecker(diag(3), matrix(1,2,1)) # 6 simple items
  sample <- matrix(rep(c(1,0),each=12), ncol=3) # 3 attr 8 respondents

  items <- matrix(rep(c(-2,2),6), ncol=2, byrow=T) # 6 equal items

  saltR::generate_responses(qmat, sample, items, get_probs = T)

  succeed(message = "Not implemented yet", info = NULL)
})


test_that(".parameter_mask works", {

  test_1 <- list(c(1)  , c(1,1))
  test_2 <- list(c(0,0)  , c(1,0,0,0))
  test_3 <- list(c(0,1)  , c(1,0,1,0) )
  test_4 <- list(c(0,1,1), c(1,0,1,1,0,0,1,0) )

  expect_equal(saltR:::.parameter_mask(test_1[[1]]), test_1[[2]])
  expect_equal(saltR:::.parameter_mask(test_2[[1]]), test_2[[2]])
  expect_equal(saltR:::.parameter_mask(test_3[[1]]), test_3[[2]])
  expect_equal(saltR:::.parameter_mask(test_4[[1]]), test_4[[2]])


})

test_that(".get_parameter_item_matrix works", {

  qmat <- kronecker(diag(3), matrix(1,2,1)) # 6 simple items
  items <- matrix(rep(c(-2,2),6), ncol=2, byrow=T) # 6 equal items

  saltR:::.get_parameter_item_matrix(qmat, items)

  succeed(message = "Not implemented yet", info = NULL)

})

