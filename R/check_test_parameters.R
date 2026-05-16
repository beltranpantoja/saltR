#' Check if the test parameters are compatible with a Q-matrix
#'
#' @param test item parameters matrix.
#' @param qmatrix Optional. If no Q-matrix is passed, then it checks that it is
#'  compatible for some Q-matrix.
#' @param action What to do if the test fails the check.
#'
#' @returns Boolean
#' @export
#'
check_test_parameters <- function(
  test,
  qmatrix = NULL,
  action = c("error", "warning", "message", "quiet")
) {
  action <- match.arg(action)

  # We try and build a Q-matrix from the test
  built_qmatrix <- rlang::try_fetch(
    build_qmatrix(test),

    # Catch the column error
    invalid_column_count = function(cnd) {
      new_msg <- paste("Failed to validate test parameters:", cnd$message)
      saltr_emit(new_msg, level = action, parent = cnd, class = "check_fail_cols")
    },

    # Catch the item transformation error
    parameter_match_error = function(cnd) {
      new_msg <- sprintf(
        "Validation failed at item %d. The parameters are malformed.",
        cnd$row_index
      )
      saltr_emit(new_msg, level = action, parent = cnd, class = "check_fail_row")
    }
  )

  # If there wasn't any error building a qmatrix

  # We can now check against the passed qmatrix
  if (!is.null(qmatrix)) {
    # Check dimensions first
    if (!all(dim(built_qmatrix) == dim(qmatrix))) {
      msg <- sprintf(
        "Provided Q-matrix is %dx%d, expected a Q-matrix of %dx%d.",
        nrow(qmatrix), ncol(qmatrix),
        nrow(built_qmatrix), ncol(built_qmatrix)
      )
      saltr_emit(
        msg,
        level = action,
        class = "check_fail_dimensions"
      )
    }

    # Find if there's any mismatch
    mismatches <- which(built_qmatrix != qmatrix, arr.ind = TRUE)

    if (nrow(mismatches) > 0) {
      # Extract unique row numbers
      failed_rows <- unique(mismatches[, "row"])

      # Format for the message (e.g., "1, 4, 5") and limit the length.
      rows_str <- paste(utils::head(failed_rows, 5), collapse = ", ")
      if (length(failed_rows) > 5) rows_str <- paste0(rows_str, "...")

      msg <- sprintf(
        "Test parameters don't match the provided qmatrix in item(s): %s.",
        rows_str
      )

      # Emit the error/warning
      saltr_emit(
        msg = msg,
        level = action,
        class = "qmatrix_mismatch",
        failed_rows = failed_rows
      )
    }
  }
}
