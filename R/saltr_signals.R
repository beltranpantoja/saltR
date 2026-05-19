#' Custom Signal Utility using native cli signaling
#' @noRd
saltr_emit <- function(
  msg,
  level = c("error", "warning", "message", "quiet"),
  class = NULL,
  call = rlang::caller_env(),
  .envir = parent.frame(),
  ...
) {
  level <- match.arg(level)

  if (level == "quiet") {
    return(invisible(NULL))
  }

  switch(level,
    error = cli::cli_abort(
      message = msg,
      class = class,
      call = call,
      .envir = .envir,
      ...
    ),
    warning = cli::cli_warn(
      message = msg,
      class = class,
      call = call,
      .envir = .envir,
      ...
    ),
    message = cli::cli_inform(
      message = msg,
      class = class,
      call = call,
      .envir = .envir,
      ...
    )
  )
}
# ===================================
# Error utilities
# ===================================


#' Is GDINA Model
#'
#' Utility function to check if an object has the class gdina.
#'
#' @param object Object to check
#' @param action what to do if it's not of type gdina.
#' @param ... arguments to be passed to `saltr_emit` if an error is to be
#'  thrown.
#'
#' @keywords internal
#' @returns Returns a boolean invisible.
#' @noRd
#'
is_gdina_object <- function(
  object,
  action = c("error", "warning", "message", "quiet"),
  ...
) {
  action <- match.arg(action)
  is_gdina <- inherits(object, "gdina")

  msg <- "Object is not of type gdina."
  if (!is_gdina) {
    saltr_emit(msg, level = action)
  }

  invisible(is_gdina)
}
