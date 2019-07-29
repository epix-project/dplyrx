#' Mutate one column value by summarize value
#'
#' \code{mutate_by} mutates value of a certain variable by the summarize value
#' of the variable. If necessary, the summarize value can be calculate on
#' categorical variable groups
#'
#' The function does not support factors.
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
#' @return A data frame with the same variables as \code{df}.
#'
#' @author Lucie Contamin.
#'
#' @example inst/examples/mutate_by.R
#'
#' @importFrom rlang parse_expr
#' @importFrom stats na.omit aggregate
#'
#' @export
mutate_by <- function(df, .filter, .funs, ..., colgroups = NULL){

  flter <- substitute(.filter)
  col_var <- strsplit(as.character(flter),
                      paste0("[^", paste(colnames(df), collapse = "|"), "]"))
  col_var <- grep(paste(colnames(df), collapse = "|"), unlist(col_var),
                  value = TRUE)
  col_var <- unique(unlist(col_var))

  add_arg <- substitute(list(...))
  add_arg <- grep("list", add_arg, invert = TRUE, value = TRUE)
  lst_arg <- lapply(seq_along(add_arg), function(x) paste0(names(add_arg)[x],
                                                          " = ", add_arg[x]))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  dff <- subset(as.data.frame(df), eval(flter))
  dft <- df[!rownames(df) %in% rownames(dff), ]

  if (length(colgroups) == 0) {

    if (length(lst_arg) > 0) {
      x <- paste0(as.character(substitute(.funs)), '(df[, "', col_var,
                  '", drop = TRUE], ', lst_arg, ")")
    } else {
      x <-  paste0(as.character(substitute(.funs)), '(df[, "', col_var,
                   '", drop = TRUE])')
    }
    x <- rlang::parse_expr(x)

     res <- dff
     res[, col_var] <- eval(x)
     res <- rbind(res, dft)
     res <- res[do.call(order, res[, names(res), drop = TRUE]), ]

  } else {

    if (length(lst_arg) > 0) {
      x <- paste0("function(x) ", as.character(substitute(.funs)),
                  "(x, ", lst_arg, ")")
    } else {
      x <-  paste0("function(x) ", as.character(substitute(.funs)), "(x)")
    }
    x_func <- rlang::parse_expr(x)

    res <- dft
    res <- aggregate(res[, col_var],
                     by = as.list(res[, colgroups, drop = FALSE]),
                     FUN = eval(x_func))
    res <- merge(dff, res, by = colgroups)
    res <- res[-which(colnames(res) %in% col_var)]
    colnames(res)[length(res)] <- col_var
    res <- res[, colnames(df)]
    res <- rbind(res, dft)
    res <- res[do.call(order, res[, names(res), drop = TRUE]), ]
  }
 res
}
