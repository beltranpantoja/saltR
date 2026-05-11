#' Change Matrix
#'
#' Apply a function to the rows/columns of a matrix. This function is just a
#'  wrapper around the `apply` function with some quality of life changes. It
#'  is designed to be used mainly to modify Q-matrices and test matrices using
#'  the defined utility functions provided in the package.
#'
#'
#' @param mat Matrix to change
#' @param what function to apply
#' @param at vector of indices, logic vector or function to determine the
#'  rows/columns to be changed.
#' @param direction should the change be done for the rows or columns.
#'
#' @returns A matrix of the same dimension than `mat`.
#' @export
#'
change_matrix <- function(mat, what, at = NULL, direction = "row") {
  # Tranform direction into margin for apply
  direction <- match.arg(direction, c("row", "column"))
  margin <- if (direction == "row") 1 else 2

  # 'what' should return a vector of the same length as the original
  expected_length <- if (margin == 1) ncol(mat) else nrow(mat)
  total_elements <- dim(mat)[margin]

  # Determine Indices
  if (is.null(at)) {
    # By default all
    target_indices <- seq_len(total_elements)
  } else if (is.numeric(at)) {
    # If it's numeric then we assume those are the indices
    target_indices <- at
  } else if (is.logical(at)) {
    # If it's logic we transform it to indices
    target_indices <- which(at)
  } else if (is.function(at)) {
    # If it's a function, then we apply it to the margin
    logical_vec <- apply(mat, margin, at)
    target_indices <- which(logical_vec)
  } else {
    stop(sprintf(
      "at has to be of type numeric, logic or a single function. Got %ss",
      typeof(at)
    ))
  }

  # Validate that indices are unique and within valid range.
  if (length(target_indices) > 0) {
    # Ensure they are numeric/integer
    if (!is.numeric(target_indices)) {
      stop("Selected indices must be numeric.")
    }
    # Check for duplicates
    if (any(duplicated(target_indices))) {
      stop("Selected indices must be unique.")
    }
    # Check bounds
    if (any(target_indices < 1) || any(target_indices > total_elements)) {
      stop(sprintf(
        "Indices must be between 1 and %d (total %ss).",
        total_elements, direction
      ))
    }
  }

  # Apply the 'what' transformation validating the length of each application.
  if (length(target_indices) > 0) {
    # We create a wrapper to check the output of 'what' before assignment
    validated_what <- function(x) {
      res <- what(x)
      if (length(res) != expected_length) {
        stop(sprintf(
          "'what' returned %d elements, but the %s length is %d.",
          length(res), direction, expected_length
        ), call. = FALSE)
      }
      res
    }

    if (margin == 1) {
      # drop = FALSE is vital to keep matrix structure for apply
      results <- apply(mat[target_indices, , drop = FALSE], 1, validated_what)
      # If results is a vector (single row), or matrix (multiple rows)
      # apply with margin 1 returns a transposed result.
      mat[target_indices, ] <- if (length(target_indices) == 1) {
        results
      } else {
        t(results)
      }
    } else {
      mat[, target_indices] <- apply(
        mat[, target_indices, drop = FALSE],
        2,
        validated_what
      )
    }
  }

  # Return
  mat
}



# ========================================================================
# Utility functions for change_matrix
# ========================================================================


#' Utility functions for the `what` and `at` parameters of `change_matrix`.
#' @name change_utils
NULL


#' Check if vector is binary
#'
#' @param row a numeric vector
#'
#' @returns boolean vector
.is_binary_row <- function(row) {
  all(row %in% c(0, 1))
}


#' @rdname change_utils
#' @param low The lower range of complexity.
#' @param high The higher range of complexity. By default is the same as `low`
#'  meaning it matches the vectors that have that exact complexity.
#' @export
is_complex <- function(low, high = low) {
  function(row) {
    if (.is_binary_row(row)) {
      complexity <- sum(row > 0)
      all(complexity >= low, complexity <= high)
    } else {
      complexity <- sum(!is.na(row))
      all(complexity >= low, complexity <= high)
    }
  }
}

#' @rdname change_utils
#' @param attr The number of the attr in the Q-matrix.
#' @export
measures_attr <- function(attr) {
  function(row) {
    if (.is_binary_row(row)) {
      row[attr] == 1
    } else {
      !is.na(row[attr + 1])
    }
  }
}


#' @rdname change_utils
#' @param num The number of elements to remove.
#' @param leave_as_zero Boolean. By default is False and the values get
#'  replaced with NaN, which means the item does not measure that attribute.
#'  However, in certain circumstances you might want to keep the original
#'  Q-matrix and just make the effect be 0.
#' @export
remove_smallest <- function(num, leave_as_zero = FALSE) {
  function(row) {
    # Get non-NA indices. We ignore the intercept
    non_na_indices <- which(!is.na(row[-1]))

    # Limit the max values to remove
    if (length(non_na_indices) > num) {
      stop(
        "Number of parameters to remove is larger that number of parameters."
      )
    }

    # Find the positions of the smallest values among the non-NA set
    target_sub_indices <- order(row[non_na_indices])[1:num]

    # Map those sub-indices back to the original row positions
    final_indices <- non_na_indices[target_sub_indices]

    row[final_indices] <- if (leave_as_zero) 0 else NA

    # Return
    row
  }
}
