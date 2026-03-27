#' Utility function to fits a LCDM model using the CDM package with some
#'  convenient defaults.
#'
#' @param responses examinees responses
#' @param qmatrix qmatrix.
#' @param monotonicity Should the monotonicity constraint be forced?
#' @param rule model to use in the gdina. defaults to GDINA
#' @param verbose should it print progress? defaults to FALSE.
#' @param ... extra parameters to pass to the gdina call.
#'
#' @returns an object gdina.
#' @export
#'
fit_lcdm <- function(
  responses, qmatrix,
  monotonicity = TRUE, rule = "GDINA", verbose = FALSE, ...
) {
  # TODO: I should add a message to indicate if monotonicity was achieved or not

  CDM::gdina(
    responses, qmatrix,
    linkfct = "logit",
    mono.constr = monotonicity,
    rule = rule, progress = verbose, ...
  )
}
