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
#' @param ... other variables to apply \code{group_by} used to calculate the new
#' value.
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
#' data %>% mutate_by(Var4 > 75, mean, Var1, Var2)
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter anti_join mutate bind_rows select group_by
#' summarise ungroup left_join rename matches
#' @importFrom rlang parse_expr as_quosure
#' @importFrom stringr str_extract
#' @importFrom stats na.omit
#'
#' @export
mutate_by <- function(df, .filter, .funs, ...){

  col_sel <-  as.character(substitute(list(...))) %>%
    grep("list", ., invert = TRUE, value = TRUE)

  flter <- substitute(.filter)
  flter <- rlang::as_quosure(flter)
  col_var <- stringr::str_extract(flter, colnames(df)) %>%
    na.omit %>% unique

  x <- paste0(as.character(substitute(.funs)), "(", col_var, ")")
  x <- rlang::parse_expr(x)

  dff <- filter(df, !! flter)
  dft <- dff %>% anti_join(df, . , by = names(df))
  if (col_sel %>% length == 0) {
    res <- dff %>%
      mutate(!!col_var := df %>% select(!!col_var) %>% unlist %>% .funs) %>%
      bind_rows(dft)
  } else {
    res <- dft %>%
      group_by(.dots = col_sel) %>%
      summarise(!!col_var := !! x) %>%
      ungroup %>%
      left_join(dff, ., by = col_sel) %>%
      rename(!!col_var := paste0(col_var, ".y")) %>%
      select(-matches(paste0(col_var, ".x"))) %>%
      select(names(df)) %>%
      bind_rows(dft)
  }
  res
}
