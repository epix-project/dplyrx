#' Add new variables, keeping attributes
#'
#' Same as \code{mutate()}, \code{mutate2()} adds new variables, but preserving
#' the user-defined attributes (i.e. those that do not belong to the data frame
#' class) in doing so.
#'
#' @param .data a data frame.
#' @param ... Name-value pairs of expressions. Use NULL to drop a variable. See
#' the help of \code{dplyr::mutate()} for more information
#'
#' @return
#' An object of the same class as \code{.data}, with the same attributes.
#'
#' @seealso \code{\link[dplyr]{mutate}}.
#'
#' @example inst/examples/mutate2.R
#'
#' @importFrom dplyr mutate
#'
#' @export
#'
mutate2 <- function(.data, ...) {

  old_attributes <- attributes(.data)
  old_class <- class(.data)
  .data <- mutate(.data, ...)

  new_attributes <- attributes(.data)
  missing_attributes <- old_attributes[! names(old_attributes) %in%
                                         names(new_attributes)]
  if (length(missing_attributes) > 0) {
    for (i in seq_along(missing_attributes))
      attr(.data, names(missing_attributes)[i]) <- missing_attributes[[i]]
  }

  if (length(old_attributes$row.names) > 0) {
    attr(.data, "row.names") <-  old_attributes$row.names
  }

  class(.data) <- old_class
  .data
}
