#' Attribute to parameter vector
#'
#' @param attr_vector binary vector showing presence of attribute
#' @param num_attrs number of attributes if longer than the passed vector
#' @param add_intercept if the intercept is added at the start of the result
#'
#' @returns A binary vector with all the parameter that the item should include
#' @noRd
.parameter_mask <- function(attr_vector, num_attrs = NULL, add_intercept = T) {
  if (is.null(num_attrs)) {
    num_attrs <- length(attr_vector)
  }

  # We get the vector of the effects for each attribute
  param_vector <- .parameter_attr_matrix(num_attrs) %*% attr_vector

  # We filter only for results that are 1. Values less than 1 means that not
  # all attributes were present.
  mask <- (param_vector == 1) * 1

  # The intercept is always going to be present for any profile attribute
  if (add_intercept) {
    return(c(1, mask))
  } else {
    return(mask)
  }
}


#' Parameter attribute matrix
#'
#' @param num_att Number of attributes
#'
#' @returns a matrix of parameters and attributes. The values are relative weight.
#'
#' @noRd
.parameter_attr_matrix <- function(num_att) {
  I <- diag(num_att)
  attr <- split(I, row(I))

  link_matrix <- I
  for (i in 2:num_att) {
    parameter <- combn(attr, i, FUN = function(x) Reduce(`+`, x), simplify = T)
    new_rows <- t(parameter) / i
    link_matrix <- rbind(link_matrix, new_rows)
  }
  return(link_matrix)
}
