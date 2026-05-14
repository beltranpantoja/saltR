write_test_parameters <- function(
  test_parameters,
  path = NULL,
  keep_item_labels = TRUE,
  ignore_check = FALSE,
  safe_column_names = TRUE,
  replace_na_with = "",
  ...
) {
  # If path is null we try to use the test_parameters object name
  arg_quo <- enquo(test_parameters)

  if (is.null(path)) {
    if (!quo_is_symbol(arg_quo)) {
      stop("When passing complex objects, you must pass a 'path' argument.")
    }
    path <- as_label(arg_quo)
  }

  if (!endsWith(path, ".csv")) {
    path <- paste0(path, ".csv")
  }


  # We check the test parameters before saving
  if (!check_test_parameters(test_parameters)) {
    msg <- "The test is not properly formed."
    if (ignore_check) {
      message(msg)
    } else {
      stop(msg)
    }
  }

  # We check the column names
  if (safe_column_names) {
    num_attr <- log(ncol(test_parameters), 2)
    attr_labels <- paste0("A", seq_len(num_attr))
    parameter_labels <- construct_parameters_labels(attr_labels, collapse = "_")
    colnames(test_parameters) <- parameter_labels
  }

  write.csv(
    test_parameters,
    file = path,
    row.names = keep_item_labels,
    na = replace_na_with,
    ...
  )
}
