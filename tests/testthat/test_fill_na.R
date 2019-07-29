
test_that("`fill_na` returns the correct output", {

  df <- data.frame("value_1" = c(1, 2, NA, NA, NA, NA, NA, NA),
                   "value_2" = c(NA, NA, 3, 4, NA ,NA, NA, NA),
                   "value_3" = c(NA, NA, NA, NA, 5, 6, NA, NA),
                   "key1" = rep(letters[1:2], 4),
                   "key2"= rep(letters[5:6], each = 4),
                   stringsAsFactors = FALSE)
 dff <- fill_na(df, "value")
 dfe <- data.frame("value_1" = c(1, 2, NA, NA),
                   "value_2" = c(3, 4, NA, NA),
                   "value_3" = c(NA, NA, 5, 6),
                   "key1" = rep(letters[1:2], 2),
                   "key2"= rep(letters[5:6], each = 2),
                   stringsAsFactors = FALSE)

 testthat::expect_equivalent(dff, dfe)
})
