#' Apply agregating function
#'
#'@param df the data frame on which to perform the aggregation.
#' @param .funcs function(s) to apply.
#' @param col names/position of the column where the \code{.funcs} be apply.
#' @param group_var names of the column grouped to aggregate.
#' @noRd
apply_fct <- function(df, .funcs, col, group_var) {

   if (length(.funcs) > 1) {
    df <- lapply(.funcs, function(x) {
      group_cal <- df[col]
      df <- aggregate(group_cal,
                      by = as.list(df[group_var]),
                      FUN = x)
      names(df)[which(names(df) %in% names(group_cal))] <-
        paste0(names(df)[which(names(df) %in% names(group_cal))], "_", x)
      df
    })
    df <- Reduce(merge, df)
  } else {
    df <- aggregate(df[col],
                    by = as.list(df[group_var]),
                    FUN = .funcs)
    names(df)
  }
  df <- df[do.call(order, df[, group_var, drop = TRUE]), ]
  df
}

#-------------------------------------------------------------------------------
#' Aggregating observations on some variables
#'
#' \code{aggregate_by} aggregates some variables of a data frame according to
#' the values of a categorical variable of the same data frame and using a given
#' function for the aggregation.
#'
#' The function does not support factors.
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
#' @importFrom stats aggregate
#'
#' @export
aggregate_by <- function(df, col_name, ..., .funs = sum) {

  res <- try(eval(col_name), silent = TRUE)
  if (inherits(res, "try-error")) {
    col_name <- deparse(substitute(col_name))
  }

  res <- try(eval(...), silent = TRUE)
  if (inherits(res, "try-error")) {
    col_sel <-  as.character(substitute(list(...)))
    col_sel <-  grep("list", col_sel, invert = TRUE, value = TRUE)
  } else {
    res1 <- try(eval(substitute(...)), silent = TRUE)
    if (inherits(res1, "try-error")) {
      col_sel <- unlist(list(...))
    } else { # nocov start
      col_sel <- eval(substitute(list(...)))
      col_sel <- as.vector(unlist(col_sel))
    } #nocov end
  }

  group_var <-  c(col_name, col_sel)

  funcs <- as.character(substitute(.funs))
  funcs <- unlist(grep("list", funcs, invert = TRUE, value = TRUE))

  test <- as.character(funcs)
  test <- unlist(test)
  test <- unlist(strsplit(test, "\\, |\\(|\\)"))

  sel <- grepl(paste(names(df), collapse = "|"), test)
  func_res <- try(lapply(test[!sel], function(x) match.fun(x)), silent = TRUE)

  if (inherits(func_res, "try-error")) {
    funcs <- unlist(.funs)
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)

  if (any(is.element(funcs, names(df)))) {

   sel <- names(df)[names(df) %in% funcs]
   funcs <- funcs[!funcs %in% names(df)]
   df <- apply_fct(df, funcs, sel, group_var)

  } else if (any(grepl(paste(names(df), collapse = "|"), funcs)) == FALSE) {

    sel <- -which(colnames(df) %in% group_var)
    df <- apply_fct(df, funcs, sel, group_var)

  } else {

    df <- lapply(funcs, function(x) {

     if (grepl("\\,", x)){
       x <- as.character(x)
       sel <- unlist(strsplit(x, ", |,"))
       sel <- unlist(strsplit(sel, "\\("))
       sel <- gsub("[[:punct:]]", "", sel)

       fct_sel <- sel[1]
       col_sel <- names(df)[names(df) %in% sel]
       df <- apply_fct(df, fct_sel, col_sel, group_var)

      } else {

          x <- unlist(strsplit(x, "[[:punct:]]"))
          sel <- names(df)[names(df) %in% x]
          fun_s <- x[!x %in% names(df)]
          df <- apply_fct(df, fun_s, sel, group_var)
      }
    })
   df <-  Reduce(merge, df)
  }
  df <- df[do.call(order, df[, group_var, drop = TRUE]), ]
  df
}
