#' Create simulation seed from ID
#'
#' @description
#' This function is meant to be used inside the generation functions for a
#' finer replication of simulations. It works by taking an ID (any kind of R object),
#' hashing it, converting to an integer and setting that as a seed.
#'
#' @param id An arbitrary R object which will then be hashed and converted to an int.
#'
#' @returns The function returns a seed to be used.
#'
.seed_from_id <- function(id = NULL) {
  if (is.null(id)) {
    return(id)
  }
  hash <- digest::digest(id, algo = "md5")
  simul_seed <- digest::digest2int(hash)
  return(simul_seed)
}
