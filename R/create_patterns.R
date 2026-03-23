#' Create all possible permutations of the levels passed.
#'
#' @param num_vars number of columns
#' @param include_filter vector of length `length(num_vars)`. It only includes
#'  rows that match. NA values work like wildcards.
#' @param exclude_filter vector of length `length(num_vars)`. It excludes rows
#'  that match. NA values work like wildcards.
#' @param levels Elements to be used in the combination. By default `c(0, 1)`,
#'  thus creating a binary matrix.
#' @param column_prefix Prefix for the column names. If column_labels is passed
#'  then this is ignored.
#' @param column_labels Labels for the columns output.
#'
#' @returns a matrix of all possible permutations.
#' @export
#'
create_patterns <- function(
  num_vars,
  include_filter = NULL,
  exclude_filter = NULL,
  levels = c(0, 1),
  column_prefix = "V",
  column_labels = NULL
) {
  # 0 has to be explicitly added if you want it in the matrix
  if (!(0 %in% levels)) {
    warning("0 not included in levels argument when creating the patterns")
  }

  # First we get the complete matrix
  patterns_matrix <- .complete_patterns_matrix(num_vars, levels)

  # we run the include_filter
  if (!is.null(include_filter)) {
    patterns_matrix <- .filter_matrix(
      patterns_matrix,
      include_filter,
      include = TRUE
    )
  }
  if (!is.null(exclude_filter)) {
    patterns_matrix <- .filter_matrix(
      patterns_matrix,
      exclude_filter,
      include = FALSE
    )
  }

  # If no column labels are passed, we use the prefix to create them.
  if (is.null(column_labels)) {
    column_labels <- paste0(
      column_prefix,
      seq_len(ncol(patterns_matrix))
    )
  }

  colnames(patterns_matrix) <- column_labels

  patterns_matrix
}


.complete_patterns_matrix <- function(num_vars, levels = c(0, 1)) {
  # checking that levels is properly formed
  if (typeof(levels) != "double") {
    stop(
      "levels argument must be a double, instead got:", typeof(levels)
    )
  }
  if (length(levels) < 2) {
    stop(
      "levels must contain at least 2 elements"
    )
  }

  # Convert to a list to use the expand.grid
  levels_list <- list(levels)

  bin_matrix <- as.matrix(
    expand.grid(rep(levels_list, num_vars))
  )

  # Columns reversed as the pattern is cleaner to read from left to right
  bin_matrix[, rev(seq_len(ncol(bin_matrix))), drop = FALSE]
}



.filter_matrix <- function(mat, filter, include = TRUE) {
  # We check the filter vector is well formed
  if (typeof(filter) != "double") {
    stop(
      "filter argument must be a double, instead got:", typeof(levels)
    )
  }
  if (length(filter) != ncol(mat)) {
    stop(
      "filter must have the same number of elements than the matrix."
    )
  }

  # We find the rows for which the non-NA values coincide.
  rows_match <- apply(
    mat, 1, function(row) {
      all(row[!is.na(filter)] == filter[!is.na(filter)])
    }
  )

  # If the filter is to exclude, then we invert the matches
  if (include == FALSE) {
    rows_match <- !rows_match
  }

  # We throw a warning if the filter filtered everything
  if (all(rows_match == FALSE)) {
    warning(
      "Got no matches after filtering. The return matrix has no rows."
    )
  }

  # We return a subset of the matrix according to the filter
  mat[rows_match, ]
}
