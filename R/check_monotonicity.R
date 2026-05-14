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
  action = c("message", "quiet", "warning", "stop"),
  tolerance = 1e-9
) {
  action <- match.arg(action)

  # If the passed object is gdina, then we need to extract the parameters
  if (class(object) == "gdina") {
    test <- get_test_parameters(object)
  } else {
    test <- object
  }


  num_attr <- log(ncol(test), 2)


  full_profiles <- create_patterns(num_attr)
  probs <- generate_responses(full_profiles, test, get_probs = TRUE)


  num_profiles <- nrow(full_profiles)

  # Setup violations storage
  violations <- c()

  for (k in seq_len(num_attr) - 1) {
    # We select profiles with 'k' and 'k+1' attributes
    num_attr_by_profile <- rowSums(full_profiles)
    low_indices <- which(num_attr_by_profile == k)
    high_indices <- which(num_attr_by_profile == k + 1)

    for (i in low_indices) {
      for (j in high_indices) {
        # We only compare profiles that are a subset og the other
        if (all(full_profiles[j, ] >= full_profiles[i, ])) {
          # Check monotonicity on the items
          if (any(probs[j, ] < (probs[i, ] - tolerance))) {
            # If we detect a violation we register the profiles
            profile_low <- full_profiles[i, ] |> paste(collapse = "")
            profile_high <- full_profiles[j, ] |> paste(collapse = "")

            # Identify which items caused the violation
            idx <- which(probs[j, ] < probs[i, ])
            item_names <- colnames(probs)[idx]
            items_str <- paste(item_names, collapse = ", ")


            msg <- sprintf(
              "Profiles %s and %s on: %s",
              profile_low, profile_high, items_str
            )

            violations <- c(violations, msg)
          }
        }
      }
    }
  }

  is_monotonic <- length(violations) == 0

  if (!is_monotonic) {
    full_msg <- paste(
      c(
        "Monotonicity violations found. Consider using mono.constr=TRUE.",
        violations
      ),
      collapse = "\n"
    )

    switch(action,
      message = message(full_msg),
      warning = warning(full_msg, call. = FALSE),
      stop    = stop(full_msg, call. = FALSE),
      quiet   = NULL # Do nothing
    )
  } else if (action != "quiet") {
    message("The monotonicity assumption holds.")
  }

  # Return the boolean invisibly to allow its use in if blocks
  invisible(is_monotonic)
}
