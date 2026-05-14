#' Custom Signal Utility
#' @importFrom rlang abort warn inform
#' @noRd
saltr_emit <- function(
  msg,
  level = c("message", "error", "warning", "quiet"),
  class = NULL,
  ...
) {
  level <- match.arg(level)

  # If it's quiet we just skip it
  if (level != "quiet") {
    switch(level,
      error = abort(msg, class = class, ...),
      warning = warn(msg, class = class, ...),
      message = inform(msg, class = class, ...)
    )
  }
}
