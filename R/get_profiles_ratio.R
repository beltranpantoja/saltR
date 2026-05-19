#' @rdname get_family
#'
#' @returns A matrix with all the profiles in the model and their estimated
#'  ratio
#' @family profiles functions
#' @export
get_profiles_ratio <- function(model) {
  # Throw error if it's not gdina model
  is_gdina_object(model)

  num_attr <- ncol(model$q.matrix)

  profiles_str <- rownames(model$attribute.patt)
  profile_ratio <- model$attribute.patt[, "class.prob"]

  profiles <- matrix(
    as.numeric(unlist(strsplit(profiles_str, split = ""))),
    ncol = num_attr,
    byrow = TRUE
  )

  colnames(profiles) <- colnames(model$q.matrix)

  # Return
  cbind(
    profiles, profile_ratio
  )
}
