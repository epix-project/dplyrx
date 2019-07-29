#' Aggregating observations on some variables
#'
#' \code{aggregate_by} aggregates some variables of a data frame according to
#' the values of a categorical variable of the same data frame and using a given
#' function for the aggregation.
#'
#' On a data frame that contains the values of the categorical variable to
#' aggregate (\code{col_name}, \code{...}), the function performs a
#' \code{\link[dplyr]{group_by}} followed by a \code{\link[dplyr]{summarise}}
#' with the function(s) \code{.funs}.\cr
#' The \code{.funs} arguments can be inputed with or without quotation as one
#' function, for example: \code{.funs = sum} or \code{.funs = "sum"} or a list
#' of multiple function, for example: \code{.funs = list(sum, mean)} or
#' \code{.funs = list("sum", "mean")}. See
#' \code{vignette("aggregating_observations")} for usage. In case of multiple
#' functions or function for a specific column (example: \code{sum(COLNAME)})
#' inputed, the function will be the name of the variable in the result.
#'
#' @param df the data frame on which to perform the aggregation.
#' @param col_name the variable on which to perform the aggregation.
#' @param ... others variables passed on to \code{group_by}.
#' @param .funs the name of a function given as a name, literal character
#' string or a list of names or character strings. See \code{Details} for
#' more information on the specific usage. By default \code{sum}.
#'
#' @return A data frame with the same variables as \code{df} but for which some
#' of the observation have been aggregated (i.e. less rows than in \code{df}).
#'
#' @author Marc Choisy and Lucie Contamin.
#'
#' @example inst/examples/aggregate_by.R
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter anti_join mutate_if bind_rows select group_by
#' summarise_all mutate enquo summarise left_join ungroup
#' @importFrom purrr reduce
#' @importFrom rlang parse_expr
#' @importFrom stringr str_detect
#'
#' @export
aggregate_by <- function(df, col_name, ..., .funs = sum) {

  res <- try(eval(col_name), silent = TRUE)
  if (inherits(res, "try-error")) {
    col_name <- deparse(substitute(col_name))
  }

  res <- try(eval(...), silent = TRUE)
  if (inherits(res, "try-error")) {
    col_sel <-  as.character(substitute(list(...))) %>%
      grep("list", ., invert = TRUE, value = TRUE)
  } else {
    res1 <- try(eval(substitute(...)), silent = TRUE)
    if (inherits(res1, "try-error")) {
      col_sel <- list(...) %>% unlist()
    } else {
      col_sel <- eval(substitute(list(...))) %>% unlist() %>% as.vector() #nocov
    }
  }

  group_var <-  c(col_name, col_sel)

  funcs <- as.character(substitute(.funs)) %>%
    grep("list", ., invert = TRUE, value = TRUE) %>%
    unlist()

  test <- funcs %>% as.character() %>% unlist() %>%
    strsplit("\\, |\\(|\\)") %>% unlist()
  sel <- test %>% stringr::str_detect(paste(names(df), collapse = "|"))
  func_res <- try(lapply(test[!sel], function(x) match.fun(x)), silent = TRUE)

  if (inherits(func_res, "try-error")) {
    funcs <- unlist(.funs)
  }

  if (funcs %>% is.element(names(df)) %>% any()) {

    x <- enquo(.funs)
    df %<>% group_by(.dots = group_var) %>%
      summarise(!!! x)

  } else if (grepl(paste(names(df), collapse = "|"), funcs) %>% any == FALSE) {

    df %<>% group_by(.dots = group_var) %>%
      summarise_all(funcs)

  } else {

    df <- lapply(funcs, function(x) {

      if (grepl("\\,", x)){

        x <- as.character(x)
        sel <- strsplit(x, ", |,") %>% unlist() %>% strsplit("\\(") %>% unlist()
        msel <- sel[1]
        x <- paste0(sel, ")", sep = "") %>% gsub("))", ")", .) %>%
          paste(msel, ., sep = "(") %>% .[-1] %>% unlist()

        df <- lapply(x, function(z) {

          z <- rlang::parse_expr(z)
          df %<>% group_by(.dots = group_var) %>%
            summarise(!! z)
        }) %>%
          reduce(left_join, by = group_var)

        } else {

        x <- rlang::parse_expr(x)
        df %<>% group_by(.dots = group_var) %>%
          summarise(!! x)
      }
    }) %>%
      reduce(left_join, by = group_var)
  }
  df %>% ungroup()
}
