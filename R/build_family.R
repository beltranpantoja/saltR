#' build_* functions
#'
#' @param test_parameters a Matrix containing the items and their parameters as
#'  created by `build_test_parameters`.
#'
#' @description
#' The `build_*` functions take as an argument a measurement object and returns
#'  something based on them. These functions can be thought of as being,
#'  partially, the  counterpart of the `get_*` functions.
#'
#'  [build_qmatrix()] and [build_test_parameters()] work as a pair. The former
#'    takes a [test_parameters] matrix and returns a Q-matrix, and the latter
#'    does the opposite.
#'
#'  * [build_profile_likelihood()] Given a `response` returns the likelihood of
#'    each possible profile.
#'  * [build_response_likelihood()] Given a `profile` returns the likelihood of
#'    each possible response pattern.
#'
#'
#' @keywords internal
#' @name build_family
NULL
