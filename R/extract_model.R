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
      left_join(
        from_test,
        from_model,
        by = c("item", "partype.attr", "type")
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
  .type_params <- function(x) {
    ifelse(x == "0", 0, lengths(str_split(x, "-")))
  }

  tidy_parameters <- model$coef %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate(type = .type_params(partype)) %>%
    select(item, partype.attr, type, est, se)

  return(tidy_parameters)
}
# ===========================================================

#' Convert the estimated probabilities of the items in a model into a tibble
#'
#' @param model fitted gdina model
#'
#' @returns a tibble with the estimation of the items.
#' @noRd
.extract_probabilities_from_model <- function(model) {
  model_prob <- model$probitem %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate(type = .type_params(partype)) %>%
    select(item, nessskill, skillcomb, prob)

  return(model_prob)
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
  item_parameters_mask <-
    matrix(1, num_items, 1) %>%
    cbind(
      # Binary matrix with 1s where parameters should be
      (qmat %*% t(saltr:::.parameter_attr_matrix(3)) == 1) * 1
    )

  # Applying mask to the parameter-item matrix
  parameter_matrix <-
    t(saltr:::.get_parameter_item_matrix(qmat, test)) %>%
    ifelse(item_parameters_mask == 1, ., NA)

  # Converting to tidy version
  tidy_parameters <- parameter_matrix %>%
    tibble::as_tibble(.name_repair = "universal_quiet") %>%
    dplyr::mutate(
      item = paste0("Item", 1:num_items),
      .before = dplyr::everything()
    ) %>%
    magrittr::set_names(
      c(
        "item", "intercept",
        partypes[, 1]
      )
    ) %>%
    tidyr::pivot_longer(
      cols = -item,
      names_to = "partype.attr",
      values_to = "real"
    ) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      partype.attr = stringr::str_remove(partype.attr, "intercept"),
      .before = real
    ) %>%
    dplyr::left_join(partypes, by = join_by(partype.attr)) %>%
    dplyr::mutate(type = ifelse(is.na(type), 0, type)) %>%
    dplyr::relocate(type, .before = real)

  return(tidy_parameters)
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

  partypes <- profiles %>%
    as.data.frame() %>%
    setNames(paste0("Attr", 1:num_attrs)) %>%
    mutate(type = rowSums(across(where(is.numeric)))) %>%
    mutate(
      across(
        starts_with("Attr"),
        ~ ifelse(.x == 1, cur_column(), NA)
      )
    ) %>%
    unite(
      "partype.attr",
      starts_with("Attr"),
      sep = "-",
      na.rm = T
    )
}
