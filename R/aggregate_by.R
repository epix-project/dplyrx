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
#' @param col_name categorical variable used for aggregation.
#' @param ... the variables on which to perform the aggregation, using the
#' funtion \code{fun}.
#' @param expr an expression apply on col_name, to filter the values used for
#' aggregation. In the expression, use a \code{.} to signify the position of
#' \code{col_name} instead of naming it (see \code{examples}). If NULL, all the
#' value are used for aggragation.
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
#' aggregate_by(data, Var1, Var2, Var3, . %in% c("a", "b"))
#'
#' ## other way of writing it:
#' sel <- c("Var2", "Var3")
#' aggregate_by(data, Var1, Var2, Var3, expr = ". %in% c('a', 'b')")
#' aggregate_by(data, "Var1", sel, expr = "grepl("a|b", .)")
#' aggregate_by(data, Var1, "Var2", "Var3", expr = ". %in% c('a', 'b')")
#' aggregate_by(data, "Var1", sel, expr = grepl("a|b", .))
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter anti_join mutate_if bind_rows select group_by summarise_all mutate
#'
#' @export
#'
aggregate_by <- function(df, col_name, ..., expr = NULL, fun = sum, new_name = NULL) {

  res <- try(eval(col_name), silent = TRUE)
  if (inherits(res, "try-error")) {
    what_var <- deparse(substitute(col_name))
  } else {
    what_var <- col_name
  }

  res <- try(eval(...), silent = TRUE)
  if (inherits(res, "try-error")) {
    sel <-  as.character(substitute(list(...))) %>%
      grep("list", ., invert = T, value = T)
  } else {
    sel <- eval(substitute(list(...))) %>% unlist %>% as.vector()
  }

  df %<>% mutate_if(is.factor, as.character)

  res <- try(eval(expr), silent = TRUE)
   if (inherits(res, "try-error")) {
     expr <- deparse(substitute(expr)) %>% as.character %>%
       gsub("\\.", what_var, .) %>%
       paste0("filter(df, ", ., ")")
     df2 <- eval(parse(text = expr))
   } else if (is.null(expr) == FALSE) {
    expr <- expr %>% as.character %>% gsub("\\.", what_var, .) %>%
      paste0("filter(df, ", ., ")")
    df2 <- eval(parse(text = expr))
  } else {
   df2 <- df
  }

  if (is.null(new_name)) {
    new_name <- select(df2, !!what_var) %>%
      unlist() %>%
      unique() %>%
      paste(collapse = " & ")
  }

  out <- anti_join(df, df2, names(df))

  df2 %>%
    select(-!!what_var) %>%
    group_by(.dots = sel) %>%
    summarise_all(fun) %>%
    mutate(!!what_var := new_name) %>%
    bind_rows(out, .)
}

