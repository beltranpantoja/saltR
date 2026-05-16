#' Test parameters
#'
#' A `test_parameters` object is a matrix used to store item/test parameters
#' with enhanced printing and validation.
#'
#' @family test parameter objects
#' @name test_parameters
NULL



#' @rdname test_parameters
#' @family test parameter objects
#' @param x A matrix or a numeric data.frame.
#'
#'
#' @export
as_test_parameters <- function(x) {
  if (is.data.frame(x)) {
    is_num <- vapply(x, is.numeric, logical(1))

    if (!all(is_num)) {
      stop("The data.frame contains non-numeric columns, all must be numeric.")
    }
    x <- as.matrix(x)
  }

  if (!is.matrix(x)) {
    stop("Input must be a matrix or a numeric data.frame.")
  }

  # We check is properly formed
  check_test_parameters(x)

  # 3. Assign the custom class
  class(x) <- c("test_parameters", "matrix")
  x
}


#' @rdname test_parameters
#'
#' @param test_parameters matrix to print
#' @param digits number of rounding digits
#' @param prefix prefix for the columns. By default, uses the Greek letter
#'  lambda in interactive sessions.
#'
#' @family test parameter objects
#' @returns the passed test_parameters with no change
#' @export
print.test_parameters <- function(x,
                                  digits = 2,
                                  prefix = NULL,
                                  ...) {
  if (is.null(prefix)) {
    prefix <- if (interactive()) "\U03BB" else "lambda"
  }

  params <- as.data.frame(as_test_parameters(x))

  params_print <- data.frame(
    rownames(params),
    lapply(params, insight::format_value, digits = digits),
    check.names = FALSE
  )

  names(params_print) <- c("Items", paste0(prefix, colnames(params)))

  cat(insight::export_table(
    params_print,
    align = "center"
  ))

  invisible(x)
}


#' Convenience wrapper around write.csv for test parameters
#'
#' @rdname test_parameters
#'
#' @param path If null it will default to using the name of the object#'  (see examples). No need to write the extension '.csv'.
#' @param keep_item_labels Write the label of the item in the first column.
#' @param ignore_check Skip the checking of the test parameters.
#' @param safe_column_names Should the labels of the attributes be rewritten?
#' @param replace_na String to use when replacing the NA values.
#' @param ... Extra parameters for `write.csv`
#'
#' @export
#'
#' @examples
#' ## We first create a test to save
#' qmatrix <- create_qmatrix(3, c(1, 1))
#' simple_test <- build_test_parameters(qmatrix, -2, 2, 1, 1)
#' tf <- tempfile(fileext = ".csv")
#'
#' ### Standard use
#' write_test_parameters(simple_test, "some_test_parameters")
#'
#' ### This will get saved as "simple_test.csv"
#' write_test_parameters(simple_test)
#'
write_test_parameters <- function(
  test_parameters,
  path = NULL,
  keep_item_labels = TRUE,
  ignore_check = FALSE,
  safe_column_names = TRUE,
  replace_na = "",
  ...
) {
  # If path is null we try to use the test_parameters object name
  arg_quo <- rlang::enquo(test_parameters)

  # We check the test parameters before saving
  if (!ignore_check) {
    check_test_parameters(test_parameters)
  }

  # We see if we can make a path out of the test_parameters object name
  if (is.null(path)) {
    if (!rlang::quo_is_symbol(arg_quo)) {
      stop("When passing complex objects, you must pass a 'path' argument.")
    }
    path <- rlang::as_label(arg_quo)
  }

  if (!endsWith(path, ".csv")) {
    path <- paste0(path, ".csv")
  }


  # We rewrite the column names to avoid any funny business with excel.
  if (safe_column_names) {
    num_attr <- log(ncol(test_parameters), 2)
    attr_labels <- paste0("A", seq_len(num_attr))
    parameter_labels <- construct_parameters_labels(attr_labels, collapse = "_")
    colnames(test_parameters) <- parameter_labels
  }

  utils::write.csv(
    test_parameters,
    file = path,
    row.names = keep_item_labels,
    na = replace_na,
    ...
  )

  invisible(test_parameters)
}


#' Convenience wrapper around read.csv for test parameters
#'
#' @rdname test_parameters
#'
#' @param path Path of the .csv file with the test parameters.
#' @param monotonicity_check Check that the test parameters are monotone on
#'  profiles using [check_monotonicity()].
#' @param ... Extra arguments to be passed to `read.csv`.
#'
#' @returns A matrix of the test parameters.
#' @export
#'
read_test_parameters <- function(path, monotonicity_check = TRUE, ...) {
  test_csv <- utils::read.csv(path, check.names = FALSE, ...)
  test <- test_csv

  # If first column is characters, then that's the name of the items
  if (is.character(test_csv[, 1])) {
    test <- test[, -1]
    rownames(test) <- test_csv[, 1]
  }

  check_test_parameters(test)

  if (monotonicity_check) {
    check_monotonicity(test, action = "warning")
  }

  # Return
  as_test_parameters(test)
}
