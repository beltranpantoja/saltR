#' Check that the monotonicity condition of the gdina model is fullfilled. This
#'  is useful for when you don't want to force it, but you want to know if the
#'  model was able to achieve it.
#'
#' @param object Either a a gdina object or a matrix of test_parameters
#' @param action what to do with the result. By default is a message.
#' @param tolerance tolerance for difference
#'
#' @returns a boolean and raises a warning
#' @export
#'
check_monotonicity <- function(
  object,
  action = c("error", "warning", "message", "quiet"),
  tolerance = 1e-9
) {
  action <- match.arg(action)

  # If the passed object is gdina, then we need to extract the parameters
  if (inherits(object, "gdina")) {
    test <- get_test_parameters(object)
  } else {
    test <- object
  }


  num_attr <- log(ncol(test), 2)


  full_profiles <- create_patterns(num_attr)
  probs <- generate_responses(full_profiles, test, get_probs = TRUE)

  attr_counts <- rowSums(full_profiles)

  # Setup violations storage
  violations <- c()

  # Count
  for (k in seq_len(num_attr) - 1) {
    low_idx <- which(attr_counts == k)
    high_idx <- which(attr_counts == k + 1)

    for (i in low_idx) {
      for (j in high_idx) {
        res <- .get_monotonicity_violation(i, j, full_profiles, probs, tolerance)
        violations <- c(violations, res)
      }
    }
  }


  if (length(violations) > 0) {
    header <- "Monotonicity violations found. Consider using mono.constr=TRUE."

    # Summary: Show first 5 violations to prevent console flooding
    display_viol <- head(violations, 5)
    if (length(violations) > 5) {
      display_viol <- c(display_viol, sprintf("... and %d more.", length(violations) - 5))
    }

    full_msg <- paste(c(header, display_viol), collapse = "\n")

    saltr_emit(
      msg = full_msg,
      level = action,
      class = "fail_monotonicity",
      all_violations = violations # Keep full list in the error object
    )
  }

  invisible(length(violations) == 0)
}


#' @noRd
.get_monotonicity_violation <- function(i, j, full_profiles, probs, tolerance) {
  # Logic: Profile j must be a superset of profile i
  if (!all(full_profiles[j, ] >= full_profiles[i, ])) {
    return(NULL)
  }

  # Check monotonicity: prob(higher) should be >= prob(lower) - tolerance
  violating_items <- which(probs[j, ] < (probs[i, ] - tolerance))

  if (length(violating_items) > 0) {
    p_low <- paste(full_profiles[i, ], collapse = "")
    p_high <- paste(full_profiles[j, ], collapse = "")
    items <- paste(colnames(probs)[violating_items], collapse = ", ")

    return(sprintf("Profiles %s and %s on: %s", p_low, p_high, items))
  }

  return(NULL)
}
