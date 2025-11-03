#' Create a matrix containing all possible attribute profiles
#'
#' @description
#' This is an utility function to be used by other functions.
#'
#'
#' @param num_attrs number of total attributes
#' @param include_base_case should the profile of only 0s be included?
#'
#' @returns a matrix of profiles and attributes.
#'
#' @noRd
.create_attr_profiles <- function(num_attrs, include_base_case = TRUE) {
  # The profile of only 0s
  base_case <- matrix(0, 1, num_attrs)

  profiles <- (.parameter_attr_matrix(num_attrs) > 0) * 1


  if (include_base_case) {
    return(rbind(base_case, profiles))
  } else {
    return(profiles)
  }
}
