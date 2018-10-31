#' Mutate one column value by summarize value
#'
#' \code{mutate_by} mutates value of a certain variable by the summarize value
#' of the variable. If necessary, the summarize value can be calculate on
#' categorical variable groups
#'
#' The function performs a \code{\link[dplyr]{filter}}, if necessary a
#' \code{\link[dplyr]{group_by}}, followed by a \code{\link[dplyr]{summarise}}
#'  with the function(s) \code{.funs}.
#'
#' @param df the data frame on which to perform the mutate_by.
#' @param .filter a filtering function to select the rows to applied the
#' mutate_by.
#' @param .funs function called to calculate the value to applied to the filter
#' value. Inputed as a function name.
#' @param ... additional arguments passed on to function.
#' @param colgroups other variables to apply \code{group_by} used to calculate
#' the new value.
#'
#' @return A data frame with the same variables as `df`.
#'
#' @author Lucie Contamin.
#'
#' @examples
#'
#' library(dplyr)
#'
#' ## A toy data frame:
#' set.seed(30101976)
#' data <- expand.grid(letters[1:3], 1:3, 4:6)
#' data$Var4 <- sample(1:100, nrow(data), TRUE)
#' data$Var5 <- sample(1:100, nrow(data), TRUE)
#' data$Var6 <- sample(1:100, nrow(data), TRUE)
#'
#' # To mutate the value > 75 in the variable `Var4` by the mean of the value of
#' # `Var4` except the one > 75.
#' data %>% mutate_by(Var4 > 75, mean)
#'
#' # To mutate the value > 75 in the variable `Var4` by the mean of the value of
#' # `Var4` group by `Var1` and `Var2`, except the one > 75.
#' data %>% mutate_by(Var4 > 75, mean, colgroups = c("Var1", "Var2"))
#'
#' # To mutate the value > 75 in the variable `Var4` by the mean of the value of
#' # `Var4` group by `Var1` and `Var2`, except the one > 75 and by removing NA
#' # value.
#' data %>% mutate_by(Var4 > 75, mean, colgroups = c("Var1", "Var2"),
#'   na.rm = TRUE)
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter anti_join mutate bind_rows select group_by
#' summarise ungroup left_join rename matches
#' @importFrom rlang parse_expr as_quosure
#' @importFrom stringr str_extract
#' @importFrom stats na.omit
#'
#' @export
mutate_by <- function(df, .filter, .funs, ..., colgroups = NULL){

  flter <- substitute(.filter)
  flter <- rlang::as_quosure(flter, env = NULL)
  col_var <- stringr::str_extract(flter,
                                  paste(colnames(df), collapse = "|")) %>%
    na.omit() %>% unique()

  add_arg <- substitute(list(...)) %>%
    grep("list", ., invert = TRUE, value = TRUE)
  lst_arg <- lapply(seq_along(add_arg), function(x) paste0(names(add_arg)[x],
                                                          " = ", add_arg[x]))

  dff <- filter(df, !! flter)
  dft <- dff %>% anti_join(df, ., by = names(df))

  if (colgroups %>% length == 0) {

    if (length(lst_arg) > 0) {
      x <- paste0(as.character(substitute(.funs)), "(", lst_arg, ")")
    } else {
      x <-  as.character(substitute(.funs))
    }
     x <- rlang::parse_expr(x)

    res <- dff %>%
      mutate(!!col_var := df %>% select(!!col_var) %>% unlist %>% !! x) %>%
      bind_rows(dft)

  } else {

    if (length(lst_arg) > 0) {
      x <- paste0(as.character(substitute(.funs)), "(", col_var, ", ", lst_arg,
                  ")")
    } else {
      x <- paste0(as.character(substitute(.funs)), "(", col_var, ")")
    }
    x <- rlang::parse_expr(x)

    res <- dft %>%
      group_by(.dots = colgroups) %>%
      summarise(!!col_var := !! x) %>%
      ungroup() %>%
      left_join(dff, ., by = colgroups) %>%
      rename(!!col_var := paste0(col_var, ".y")) %>%
      select(-matches(paste0(col_var, ".x"))) %>%
      select(names(df)) %>%
      bind_rows(dft)
  }
 res
}
