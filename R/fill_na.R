#' Fill NA value in a data frame
#'
#' Replace \code{NA} value in columns containing the \code{keyword} in their
#' name by the maximum value in the same column and in the rows with the same
#' value in the column non containg the \code{keyword} in their name.
#'
#' The function works in multiple step:
#' \enumerate{
#'      \item the data frame is split by the value in the columns non containing
#'      the \code{keywords}.
#'      For example: \cr
#'      \tabular{rrrrr}{
#'      "value1" \tab "value2" \tab "key" \cr
#'      1 \tab NA \tab "a" \cr
#'      NA \tab 2 \tab "a" \cr
#'      3 \tab NA \tab "b" \cr
#'      NA \tab NA \tab  b" \cr
#'      }
#'      will be split in two data frame: \cr
#'      \tabular{rrr}{
#'      "value1" \tab "value2" \tab "key" \cr
#'      1 \tab NA \tab "a" \cr
#'      NA \tab 2 \tab "a" \cr
#'      } &
#'      \tabular{rrr}{
#'      "value1" \tab "value2" \tab "key" \cr
#'      3 \tab NA \tab "b" \cr
#'      NA \tab NA \tab  b" \cr
#'      }
#'      \item for each group, replace the \code{NA} in the columns containing
#'      the \code{keywords} by the maximum value of the columns. If only
#'      \code{NA} are in the columns then the value will be \code{NA}.
#'      For example: \cr
#'      \tabular{rrr}{
#'      "value1" \tab "value2" \tab "key" \cr
#'      1 \tab 2 \tab "a" \cr
#'      1 \tab 2 \tab "a" \cr
#'      } &
#'      \tabular{rrr}{
#'      "value1" \tab "value2" \tab "key" \cr
#'      3 \tab NA \tab "b" \cr
#'      3 \tab NA \tab  b" \cr
#'      }
#'      \item all the data frame are merged back together and the duplicated
#'      row are removed
#'      For example: \cr
#'      \tabular{rrr}{
#'      "value1" \tab "value2" \tab "key" \cr
#'      1 \tab 2 \tab "a" \cr
#'      3 \tab NA \tab "b" \cr
#'      }
#'}
#'
#' @param df data frame outputted by reshape (long format)
#' @param keyword string character, value contained in the name of the columns
#' with NA.
#'
#' @examples
#' df <- data.frame("value_1" = c(1, 2, NA, NA, NA, NA, NA, NA),
#'                  "value_2" = c(NA, NA, 3, 4, NA ,NA, NA, NA),
#'                  "value_3" = c(NA, NA, NA, NA, 5, 6, NA, NA),
#'                  "key1" = rep(letters[1:2], 4),
#'                  "key2"= rep(letters[5:6], each = 4))
#' fill_na(df, "value")
#' @export
fill_na <- function(df, keyword = "value") {

  lst <- split(df, as.list(df[, grep(keyword, names(df), invert = TRUE),
                              drop = TRUE]), drop = TRUE)
  lst_t <- lapply(lst, function(y){
    tab <- lapply(grep(keyword, names(y), value = TRUE), function(x){
      y[, x] <- ifelse(all(is.na(y[, x])), y[, x] , max(y[, x], na.rm = TRUE))
      y[, x, drop = FALSE]
    })
    tab <- Reduce(cbind, tab)
    y[, grep(keyword, names(y))] <- tab
    y
  })
  dff <- Reduce(rbind, lst_t)
  dff <- unique(dff)
}
