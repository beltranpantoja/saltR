#' Change a test parameters
#'
#' @param test_parameters A matrix
#' @param level The level of parameters to change. 0 corresponds to the
#'  intercept, 1 to the main effects, 2 to the two-way interaction and so on.
#' @param attribute The attribute that must be present in the parameter.
#' @param items Indices of items to be changed.
#' @param values Values to replace the selected values. If there are more
#'  values than numbers to replace, then it will just use
#' @param fun A function that takes all values to be changed and returns a
#'  vector of numbers of the same length.
#'
#' @returns A matrix corresponding to the test parameters with the changes.
#' @export
#'
change_test <- function(
  test_parameters,
  level = NULL,
  attribute = NULL,
  items = NULL,
  values = NULL,
  fun = NULL
) {
  # First we check the test_parameters
  check_test_parameters(test_parameters)

  num_attr <- log(ncol(test_parameters), 2)

  # By default the indices will be all columns and all items
  level_idx <- attr_idx <- seq_len(ncol(test_parameters))
  items_idx <- seq_len(nrow(test_parameters))

  if (!is.null(items)) {
    items_idx <- items
  }


  # Getting the indices of the level
  if (!is.null(level)) {
    if (level > num_attr) {
      stop("Level cannot be larger than the number of attributes.")
    } else if (level == 0) {
      level_idx <- 1
    } else {
      level_columns <- choose(num_attr, seq(from = 0, to = num_attr))
      end_level_columns <- cumsum(level_columns)
      level_idx <- seq(
        from = end_level_columns[level] + 1,
        to = end_level_columns[level + 1]
      )
    }
  }

  # Getting the indices of the attribute
  if (!is.null(attribute)) {
    if (attribute > num_attr) {
      stop("Attribute cannot be larger than the number of attributes.")
    }

    # This is the profile that has all but the attribute we want
    profile <- rep(1, num_attr)
    profile[attribute] <- 0

    # We get the complement of the parameters that correspond to that profile
    attr_mask <- 1 - .get_attr_mask_from_profile(t(profile))
    attr_idx <- which(attr_mask == 1)
  }

  # Getting the intersection of the indices
  idx <- intersect(level_idx, attr_idx)


  # Now that we have the indices we set the values.
  target_cells_mask <- !is.na(test_parameters[items_idx, idx, drop = FALSE])
  num_to_replace <- sum(target_cells_mask)

  if (num_to_replace == 0) {
    warning("The combination of arguments returned no matches.")
    return(test_parameters)
  }

  if (!is.null(values) && !is.null(fun)) {
    stop("Provide either 'values' or 'fun', not both.")
  }

  if (!is.null(fun)) {
    if (!is.function(fun)) stop("'fun' must be a function.")
    # Apply function to the specific non-NA values
    new_values <- fun(test_parameters[, idx][target_cells_mask])
    if (length(new_values) != length(num_to_replace)) {
      stop(sprintf(
        "The function returned %d values, only %d values were expected",
        length(new_values),
        length(num_to_replace)
      ))
    }
    test_parameters[items_idx, idx][target_cells_mask] <- new_values
  } else if (!is.null(values)) {
    # check values are numbers
    if (!is.numeric(values)) {
      stop("Values should be numeric.
    ")
    }

    # Check lengths for recycling safety
    if (length(values) > num_to_replace) {
      warning(sprintf(
        "%d values provided, but only %d slots to fill. Extra values ignored.",
        length(values),
        num_to_replace
      ))
      # Only take the first values
      test_parameters[items_idx, idx][target_cells_mask] <- values[seq_along(num_to_replace)]
    } else if (length(values) < num_to_replace) {
      stop(sprintf(
        "Number of values (%d) is less than slots to fill (%d).",
        length(values),
        num_to_replace
      ))
    } else {
      test_parameters[items_idx, idx][target_cells_mask] <- values
    }
  }

  # Return full test
  test_parameters
}
