#' @rdname get_family
#'
#' @param method The method used in classification.
#'
#' @returns A matrix with the MAP or MLE classifications of examinees.
#'
#' @family examinees functions
#' @export
get_examinees <- function(model, method = c("map", "mle")) {
  # Throw error if it's not gdina model
  is_gdina_object(model)

  method <- match.arg(method)

  # argument map
  col_map <- c(
    "map" = "map.est",
    "mle" = "mle.est"
  )

  # Extract column based on the mapping
  classifications <- model$pattern[, col_map[method]]

  num_attr <- ncol(model$q.matrix)

  mat <- matrix(
    as.numeric(unlist(strsplit(classifications, split = ""))),
    ncol = num_attr,
    byrow = TRUE
  )


  # Get the labels from the response data
  labels <- colnames(model$data)

  # Dafult to the pattern names
  if (!is.null(labels)) {
    labels <- mod$subj.pattern[, "pattern"]
  }

  colnames(mat) <- labels

  # Return
  mat
}
