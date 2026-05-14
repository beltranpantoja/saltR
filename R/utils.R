#' Utility function to filter rows in matrices
#'
#' @param A Complete matrix to filter.
#' @param B subset of rows to match in A. Can also be a single vector.
#'
#' @returns a vector of indices in A that match B.
#'
.get_matching_rows_index <- function(A, B) {
  # If B is a numeric vector, convert to 1-row matrix
  if (is.vector(B) && is.numeric(B)) {
    B <- matrix(B, nrow = 1)
  }

  # Checking inputs
  if (!is.matrix(A) || !is.matrix(B)) {
    stop("Both A and B must be matrices, or B can be a numeric vector.")
  }

  if (ncol(A) != ncol(B)) {
    stop("Matrices must have the same number of columns.")
  }

  cols <- seq_len(ncol(B))

  # Matching logic
  idx <- vapply(seq_len(nrow(B)), function(i) {
    hits <- which(
      apply(A[, cols, drop = FALSE], 1, function(a_row) {
        all(a_row == B[i, ])
      })
    )

    if (length(hits) == 0) {
      stop(sprintf("Row %d of B was not found in A.", i))
    }

    if (length(hits) > 1) {
      stop(sprintf("Row %d of B has multiple matches in A.", i))
    }

    hits
  }, integer(1))

  idx
}


#' Construct the labels for all parameters
#'
#' This is an utility function that helps generating all the labels for
#'  possible parameters.
#'
#' @param attr_labels vector with the labels of each attribute
#' @param null_profile single character vector for the null profile (all zeroes)
#'
#' @returns a vector with parameters
#'
construct_parameters_labels <- function(
  attr_labels,
  null_profile = "0",
  collapse = "-"
) {
  col_labels <- null_profile
  for (i in seq_along(attr_labels)) {
    combs <- combn(attr_labels, i)
    # Create labels like "1", "2", "1-2", etc.
    level_labels <- apply(
      combs,
      2,
      function(idx) paste(idx, collapse = collapse)
    )
    col_labels <- c(col_labels, level_labels)
  }

  # Return
  col_labels
}


#' Get attribute matrix mask from profiles
#'
#' Returns a matrix where each row contains the parameters for each given
#'  profile
#'
#' @param profiles a matrix of the profiles for which we want the masks
#'
#' @returns A matrix
#'
.get_attr_mask_from_profile <- function(profiles) {
  n_attr <- ncol(profiles)

  # 1. Pre-generate the labels for the columns


  # 2. Run the logic to fill the data
  profile_mat <- apply(profiles, MARGIN = 1, FUN = function(attr_vec) {
    result <- c(1) # Intercept
    for (i in 1:n_attr) {
      combs <- combn(n_attr, i)
      level_bits <- apply(combs, 2, function(idx) {
        if (all(attr_vec[idx] == 1)) 1 else 0
      })
      result <- c(result, level_bits)
    }

    # Return
    result
  })

  # We transpose it back and set the names
  final_mat <- t(profile_mat)

  col_labels <- construct_parameters_labels(seq_len(n_attr))
  colnames(final_mat) <- col_labels

  # Return
  final_mat
}
