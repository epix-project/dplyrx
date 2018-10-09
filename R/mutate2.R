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
#' @examples
#' ## Let's start by adding an attribute to a data frame:
#' attr(mtcars, "my_attribute_name") <- "my_attribute_value"
#'
#' ## We can see the attributes of this data frame:
#' attributes(mtcars)
#'
#' ## We can now see that mutate drops the non-class attributes:
#' mtcars %>% mutate(a_new_variable = 3) %>% attributes()
#'
#' ## Whereas mutate2 does not:
#' mtcars %>% mutate2(a_new_variable = 3) %>% attributes()
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %<>%
#'
#' @export
#'
mutate2 <- function(.data, ...) {
  old_attributes <- attributes(.data)
  .data %<>% mutate(...)
  new_attributes <- attributes(.data)
  missing_attributes <- old_attributes[! names(old_attributes) %in% names(new_attributes)]
  if (length(missing_attributes) > 0)
    for(i in seq_along(missing_attributes))
      attr(.data, names(missing_attributes)[i]) <- missing_attributes[[i]]
  .data
}