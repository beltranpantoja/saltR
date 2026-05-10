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
