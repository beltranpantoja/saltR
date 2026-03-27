#' Returns a matrix item-parameters from model
#'
#' This extracts a model's item parameters as a matrix If `pretty_print` is
#'  true, then it also prints a pretty version of the table.
#'
#' @param model a GDINA object. It has to be logit link function.
#' @param complete If the model only estimated some parameters this will not
#'  show in the table. So, for example, the ACDM model will only return main
#'  effects. If true this returns the complete table and fills the corresponding
#'  parameters with 0. This also ensures a consistent order of the parameters.
#' @param pretty_print boolean. If true it prints a pretty table. It returns a data frame silently.
#' @param digits rounding digits when printing. The return is not approximated.
#'
#' @returns matrix of item-parameters
#' @export
#'
get_test_parameters <- function(
  model,
  complete = TRUE,
  pretty_print = TRUE,
  digits = 2
) {
  # TODO: This could down the line also give the SE

  # Ensuring only logit linkfct
  if (model$linkfct != "logit") {
    stop(
      "only implemented for models with link logit, not: ", model$linkfct
    )
  }

  model.coef <- model$coef

  # We get the model's attributes names in order
  attr.labels <- colnames(model$attribute.patt.splitted)

  # Each attribute label is changed by a number. So the parameters are normalized.
  partype.attr <- model.coef[, "partype.attr"]
  for (i in seq_along(attr.labels)) {
    partype.attr <- gsub(attr.labels[i], i, partype.attr)
  }
  partype.attr[partype.attr == ""] <- "0"

  # We replace the result of the change in the coef matrix
  model.coef[, "partype.attr"] <- partype.attr


  # tapply crosses the item and partype.attr columns to obtain the est values
  item_params <- with(
    model.coef,
    tapply(
      est,
      list(item, partype.attr),
      function(x) x
    ) # The identity function allows for the return of NAs
  )

  # In some edgecases this reordering will not work as expected.
  items_order <- rownames(item_params) |>
    nchar() |>
    order()

  params_order <- colnames(item_params) |>
    nchar() |>
    order()

  item_params <- item_params[items_order, params_order]

  if (complete == TRUE) {
    mask <- build_test_parameters(model$q.matrix)

    idx <- which(!colnames(mask) %in% colnames(item_params))
    columns_to_add <- mask[, idx, drop = FALSE]

    columns_to_add[columns_to_add == 0] <- NA
    columns_to_add[columns_to_add == 1] <- 0

    item_params <- cbind(item_params, columns_to_add)
    item_params[, colnames(mask)]
  }


  if (pretty_print == TRUE) {
    pretty_print(item_params, digits = digits)
  }

  # Return
  item_params
}
