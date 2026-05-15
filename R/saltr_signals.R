#' Custom Signal Utility
#' @noRd
saltr_emit <- function(
  msg,
  level = c("message", "error", "warning", "quiet"),
  class = NULL,
  parent = NULL,
  call = rlang::caller_env(),
  ...
) {
  level <- match.arg(level)

  # If it's quiet we just skip it
  if (level != "quiet") {
    switch(level,
      error = rlang::abort(
        msg,
        class = class, parent = parent, call = call, ...
      ),
      warning = rlang::warn(
        msg,
        class = class, parent = parent, call = call, ...
      ),
      message = rlang::inform(
        msg,
        class = class, parent = parent, call = call, ...
      )
    )
  }
}

# ===================================
# Error utilities
# ===================================


#' Is GDINA Model
#'
#' Utility function to check if an object has the class gdina.
#'
#' @param object object to check
#' @param ... arguments to be passed to `saltr_emit` if an error is to be
#'  thrown.
#'
#' @keywords internal
#' @returns Returns a boolean invisible.
#' @noRd
#'
is_gdina_object <- function(
  object,
  ...
) {
  action <- match.arg(action)
  is_gdina <- inherits(object, "gdina")

  msg <- "Object is not of type gdina."
  if (!is_gdina && !action == "quiet") {
    saltr_emit(msg, ...)
  }

  invisible(is_gdina)
}
