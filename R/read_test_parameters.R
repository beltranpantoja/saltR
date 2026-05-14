#' Read test parameters file
#'
#' @param path Path of the .csv file with the test parameters.
#' @param monotonicity_check Boolean. Check the monotonicity of the test using
#'  [check_monotonicity()].
#' @param ... Extra arguments to be passed to [read.csv()].
#'
#' @returns A matrix of the test parameters.
#' @export
#'
read_test_parameters <- function(path, monotonicity_check = TRUE, ...) {
  test_csv <- read.csv(path, check.names = FALSE, ...)
  test <- test_csv

  # If first column is characters, then that's the name of the items
  if (is.character(test_csv[, 1])) {
    test <- test[, -1]
    rownames(test) <- test_csv[, 1]
  }

  if (!check_test_parameters(test)) {
    stop("The test is not properly formed.")
  }

  # Print a warning if monotonicity doesn't hold
  if (monotonicity_check) {
    check_monotonicity(test, action = "warning")
  }

  # Return
  as.matrix(test)
}
