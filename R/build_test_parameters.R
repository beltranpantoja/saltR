#' Create a matrix of items and their parameters
#'
#' This matrix already encodes the Q-matrix. It can be useful to save the
#' results for future simulations or further wrangling as a csv.
#'
#' @param qmat Q-matrix
#' @param ... values to use in the different factor levels. i.e. the first
#'  number is assumed to be the intercept, then main effects, then two-way
#'  interaction and so on.
#'
#' @returns A matrix of items and their parameters.
#' @export
#'
build_test_parameters <- function(
  qmat,
  ...
) {
  params <- c(...)

  # We create the basic mask
  mask <- .get_attr_mask_from_profile(qmat)

  # Number of attributes
  k <- ncol(qmat)

  # This counts how many combinations are for different amount of items
  factor_levels <- sapply(seq_len(k), function(num_attrs) {
    ncol(combn(k, num_attrs))
  })

  # We add the intercept
  factor_levels <- c(1, factor_levels)

  expected_params <- length(factor_levels)

  # If there are no params we just return the mask
  if (length(params) == 0) {
    return(mask)
  }

  # We shouldn't receive more params than levels of interactions we have
  if (length(params) > expected_params) {
    stop(
      sprintf(
        "Expected %d parameters but got %d instead",
        expected_params,
        length(params)
      )
    )
  }

  # If params is not the right length we pad it with 0's
  if (length(params) < expected_params) {
    params <- c(
      params,
      rep(0, expected_params - length(params))
    )
  }

  # This gives a parameter for every column
  col_multipliers <- rep(params, times = factor_levels)

  # Multiply by the mask. The t() ensures proper order
  out <- t(t(mask) * col_multipliers)

  # Replace all 0s in the original mask with NAs
  out[mask == 0] <- NA

  # Return
  out
}
