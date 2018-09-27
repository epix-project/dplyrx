#' Aggregating observations on some variables
#'
#' \code{aggregate_by} aggregates some variables of a data frame according to
#' the values of a categorical variable of the same data frame and using a given
#' function for the aggregation.
#'
#' The function splits the data frame \code{df} in two data frames, based on the
#' \code{what} argument. On the data frame that contains the values of the
#' categorical variable to aggregate (df1), the function performs a
#' \code{\link[dplyr]{group_by}} followed by a \code{\link[dplyr]{summarise_all}}
#' with the function \code{fun} and then binds it by row with the other data
#' frame (df2).
#'
#' @param df the data frame on which to perform the aggregation.
#' @param what an expression on the values of the categorical variable used for
#' aggregation.
#' @param ... the variables (unquoted names) on which to perform the aggregation,
#' using the funtion \code{fun}.
#' @param fun the function used to perform the aggregation. By default \code{sum}.
#' @param new_name the new value of the categorical variable, after aggregation.
#' By default, it concatenates the values of the categorical variables used in
#' \code{what}, separating them by " & ".
#'
#' @return A data frame with the same variables as `df` but for which some of
#' the observation have been aggregated (i.e. less rows than in `df`).
#'
#' @author Marc Choisy and Lucie Contamin.
#'
#' @examples
#' ## A toy data frame:
#' set.seed(30101976)
#' data <- expand.grid(letters[1:3], 1:3, 4:6)
#' data$Var4 <- sample(1:100, nrow(data), TRUE)
#' data$Var5 <- sample(1:100, nrow(data), TRUE)
#' data$Var6 <- sample(1:100, nrow(data), TRUE)
#'
#' ## Aggregating the values "a" and "b" of the categorical variable Var1,
#' ## summing the values of variables Var4, Var5, Var6 (i.e. all the variables
#' ## that are not in the arguments of the function call):
#' aggregate_by(data, Var1 %in% c("a", "b"), Var2, Var3)
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter anti_join mutate_if bind_rows select group_by summarise_all mutate
#'
#' @export
#'
aggregate_by <- function(df, col_name, ..., .funs) {

  res <- try(eval(col_name), silent = TRUE)
  if (inherits(res, "try-error")) {
    col_name <- deparse(substitute(col_name))
  }

  res <- try(eval(...), silent = TRUE)
  if (inherits(res, "try-error")) {
    col_sel <-  as.character(substitute(list(...))) %>%
      grep("list", ., invert = T, value = T)
  } else {
    res1 <- try(eval(substitute(...)), silent = TRUE)
    if(inherits(res1, "try-error")) {
      col_sel <- list(...) %>% unlist
    } else {
      col_sel <- eval(substitute(list(...))) %>% unlist %>% as.vector()
    }
  }

  group_var <-  c(col_name, sel)

  funs <- as.character(substitute(.funs)) %>%
    grep("list", ., invert = T, value = T) %>%
    unlist

  if (funs %>% is.element(names(data)) %>% any()) {
    print("funs contains one element which is element names(df)")

    group_var <-  c(col_name, sel)
    x <- enquo(.funs)
    df %<>% group_by(.dots = group_var) %>%
      summarise(!!! x)

  } else if (grepl(paste(names(df), collapse = "|"), funs) %>% any == FALSE) {
    print("funs is not math of names(df)")

    df %<>% group_by(.dots = group_var) %>%
      summarise_all(funs)

  } else {
    print("funs contains names(df) with other element")

    df <- lapply(funs, function(x) {
      group_var <-  c(col_name, sel)

      if(grepl("\\,", x)){

        x <- as.character(x)
        sel <- strsplit(x, ", |,") %>%  unlist %>% strsplit("\\(") %>% unlist
        msel <- sel[1]
        x <- paste0(sel, ")", sep = "") %>% gsub("))", ")", .) %>%
          paste(msel, ., sep = "(") %>% .[-1] %>% unlist

        df <- lapply(x, function(z){

          z <- rlang::parse_expr(z)
          df %<>% group_by(.dots = group_var) %>%
            summarise(!! z)
          }) %>%
          purrr::reduce(left_join, by = group_var)

        } else {

          x <- rlang::parse_expr(x)
          df %<>% group_by(.dots = group_var) %>%
            summarise(!! x)
        }
        }) %>%
          purrr::reduce(left_join, by = group_var)
  }
  df
}
