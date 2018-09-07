#' dplyrx: an extension of dplyr
#'
#' dplyrx provides a collection of functions to work on data frames that
#' complement those of the dplyr package.
#'
#' To learn more about dplyrx, start with the vignettes:
#' `browseVignettes(package = "dplyrx")`
#'
"_PACKAGE"

## quiets concerns of R CMD check for the values that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c(".", ":="))
