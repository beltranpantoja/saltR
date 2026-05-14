#' Check if the test parameters are compatible with a Q-matrix
#'
#' @param test item parameters matrix
#' @param qmatrix Optional. If no qmatrix is passed, then it checks that it is
#'  compatible for some qmatrix
#'
#' @returns boolean
#' @export
#'
check_test_parameters <- function(
  test,
  qmatrix = NULL,
  action = c("error", "warning", "message", "quiet")
) {
  action <- match.arg(action)

  # Standardize the input to 0/1 for matching
  test_binary <- test
  test_binary[!is.na(test)] <- 1
  test_binary[is.na(test)] <- 0


  expected_params <- if (is.null(qmatrix)) {
    num_attr <- log2(ncol(test_binary))
    build_test_parameters(create_patterns(num_attr))
  } else {
    build_test_parameters(qmatrix)
  }

  if (any(dim(test_binary) != dim(expected_params))) {
    msg <- sprintf(
      "Dimension mismatch: test is %dx%d, expected %dx%d.",
      nrow(test_binary), ncol(test_binary),
      nrow(expected_params), ncol(expected_params)
    )

    # Throw error
    saltr_emit(msg, "error", class = "dim_mismatch")
  }

  # 4. Check Values and find "Spots"
  mismatches <- which(test_binary != expected_params, arr.ind = TRUE)

  if (nrow(mismatches) > 0) {
    # Format the coordinates for the error message
    spots <- paste0("(", mismatches[, 1], ", ", mismatches[, 2], ")", collapse = ", ")
    msg <- paste("Mismatches found at [row, col] coordinates:", spots)

    saltr_emit(msg, action, class = "malformed_test")

    # Return False
    FALSE
  } else {
    # Return False
    TRUE
  }
}
