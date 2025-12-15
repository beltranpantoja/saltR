#' Generate random simulation ids
#'
#' @param n number of ids to generate
#' @param id simulation id, to be used as a seed within this function.
#' @returns a vector of random ids to be used in simulations
#' @export
#'
generate_ids <- function(n = 1L, id = NULL) {
  seed <- .create_seed_from_id(id)
  if (!is.null(seed)) {
    withr::with_seed(
      seed,
      .generate_ids(n)
    )
  } else {
    .generate_ids(n)
  }
}


#' Internal - Generate random simulation ids
#'
#' @param n number of ids to generate
#'
#' @returns a vector of random ids to be used in simulations
#'
.generate_ids <- function(n = 1L) {
  sapply(
    1:n,
    FUN = function(x) ids::random_id(use_openssl = FALSE)
  )
}
