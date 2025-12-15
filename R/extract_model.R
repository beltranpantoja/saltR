#' Convert the parameters of a model and/or test into a tibble for easier analysis
#'
#' @param model fitted gdina model
#' @param qmat Q-matrix
#' @param test test matrix (obtained from [saltr::create_test])
#'
#' @returns a tibble with the estimation and/or real values of the items.
#' @export
#'
extract_item <- function(model = NULL, qmat = NULL, test = NULL) {
  from_model <- NULL
  from_test <- NULL

  if (!is.null(model)) {
    from_model <- .extract_parameters_from_model(model)
  }

  if (!is.null(qmat) && !is.null(test)) {
    from_test <- .extract_parameters_from_test(qmat, test)
  }

  if (!is.null(from_model) && !is.null(from_test)) {
    return(
      merge(
        from_test,
        from_model
      )
    )
  } else if (!is.null(from_model)) {
    return(from_model)
  } else if (!is.null(from_test)) {
    return(from_test)
  } else {
    stop("At least one input (model or qmat+test) must be provided.")
  }
}

# ===========================================================

#' Convert the parameters of a model into a tibble for easier analysis
#'
#' @param model fitted gdina model
#'
#' @returns a tibble with the estimation of the items.
#' @noRd
.extract_parameters_from_model <- function(model) {
  # Utility function
  type_params <- function(x) {
    ifelse(x == "0", 0L, lengths(strsplit(x, "-", fixed = TRUE)))
  }

  df <- as.data.frame(model$coef, stringsAsFactors = FALSE)

  df$type <- type_params(df$partype)
  df[df$partype.attr == "", "partype.attr"] <- "intercept"


  df <- df[, c("item", "partype.attr", "type", "est", "se")]

  rownames(df) <- NULL
  return(df[order(df$item), ])
}
# ===========================================================

#' Convert the estimated probabilities of the items in a model into a tibble
#'
#' @param model fitted gdina model
#'
#' @returns a tibble with the estimation of the items.
#' @noRd
.extract_probabilities_from_model <- function(model) {
  type_params <- function(x) {
    ifelse(x == "0", 0L, lengths(strsplit(x, "-", fixed = TRUE)))
  }

  df <- as.data.frame(model$probitem, stringsAsFactors = FALSE)
  df$type <- type_params(df$partype)

  df <- df[, c("item", "nessskill", "skillcomb", "prob")]

  rownames(df) <- NULL
  return(df)
}

# ===========================================================


#' Convert the parameters of a test into a tibble for easier analysis
#'
#' @param qmat Q-matrix
#' @param test test matrix (obtained from [saltr::create_test])
#'
#' @returns a tibble with the real values of the items.
#' @noRd
.extract_parameters_from_test <- function(qmat, test) {
  num_items <- nrow(qmat)
  num_attrs <- ncol(qmat)

  partypes <- .generate_partypes(num_attrs)


  # All items have intercepts so we add the first column of only 1s
  item_parameters_mask <- cbind(
    intercept = rep(1, num_items),
    (qmat %*% t(.parameter_attr_matrix(3)) == 1) * 1
  )

  # Applying mask to the parameter-item matrix
  parameter_matrix <- t(.get_parameter_item_matrix(qmat, test))
  parameter_matrix[item_parameters_mask != 1] <- NA


  df <- as.data.frame(parameter_matrix, stringsAsFactors = FALSE)

  df$item <- paste0("Item", seq_len(num_items))

  colnames(df) <- c("intercept", partypes$partype.attr, "item")
  df
  ## pivot_longer (manual)
  long <- stats::reshape(
    df,
    varying = names(df)[-ncol(df)],
    v.names = "real",
    timevar = "partype.attr",
    times = names(df)[-ncol(df)],
    direction = "long"
  )

  long <- long[!is.na(long$real), -4]
  rownames(long) <- NULL

  return(long[order(long$item), ])
}


#' Utility function to create the partypes for a number of attributes
#'
#' @param num_attrs number of total attributes
#'
#' @returns a dataframe with partypes types and their names
#'
#' @noRd
.generate_partypes <- function(num_attrs) {
  profiles <- .create_attr_profiles(num_attrs, FALSE)
  df <- as.data.frame(profiles)

  colnames(df) <- paste0("Attr", seq_len(num_attrs))

  df$type <- rowSums(df)

  for (j in seq_len(num_attrs)) {
    df[[j]] <- ifelse(df[[j]] == 1, colnames(df)[j], NA)
  }

  df$partype.attr <- apply(
    df[paste0("Attr", seq_len(num_attrs))],
    1,
    function(x) paste(stats::na.omit(x), collapse = "-")
  )

  df[, c("partype.attr", "type")]
}
