#' @rdname get_family
#'
#' @param complete If the model only estimated some parameters this will not
#'  show in the table. So, for example, the ACDM model will only return main
#'  effects. If true this returns the complete table and fills the corresponding
#'  parameters with 0. This also ensures a consistent order of the parameters.
#' @param pretty_print boolean. If true it prints a pretty table. It returns a
#'  data frame silently.
#' @param digits rounding digits when printing. The return is not approximated.
#'
#' @returns a [test_parameters] object from the parameters estimated by the
#'  model, which is just an extension of the base [base::matrix] class.
#'
#' @family test parameters functions
#' @export
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

  # TODO: add the pretty print if interactive
  model_coef <- model$coef

  # We get the model's attributes names in order
  attr_labels <- colnames(model$attribute.patt.splitted)

  # Each attribute label is changed by a number, so parameters are normalized.
  partype_attr <- model_coef[, "partype.attr"]
  for (i in seq_along(attr_labels)) {
    partype_attr <- gsub(attr_labels[i], i, partype_attr)
  }
  partype_attr[partype_attr == ""] <- "0"

  # We replace the result of the change in the coef matrix
  model_coef[, "partype.attr"] <- partype_attr


  # tapply crosses the item and partype.attr columns to obtain the est values
  item_params <- with(
    model_coef,
    tapply(
      est,
      list(item, partype_attr),
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
