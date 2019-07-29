# create a simple data frame
set.seed(30101976)
data <- expand.grid(letters[1:3], 1:3, 4:6)
data$Var4 <- sample(1:100, nrow(data), TRUE)
data$Var5 <- sample(1:100, nrow(data), TRUE)
data$Var6 <- sample(1:100, nrow(data), TRUE)
data2 <- transform(data, Var1 =  ifelse(data$Var1 == "a", "b",
                                        as.character(data$Var1)))

# expected results
sum6 <- aggregate(Var6 ~ Var1  + Var2 + Var3, data = data2, FUN = sum)
sum6 <- sum6[order(sum6$Var1, sum6$Var2, sum6$Var3), ]
sum6 <- as.data.frame(sum6, stingsAsFactors = FALSE)
mean4 <- aggregate(Var4 ~ Var1  + Var2 + Var3, data = data2, FUN = mean)
mean4 <- mean4[order(mean4$Var1, mean4$Var2, mean4$Var3), ]
mean4 <- as.data.frame(mean4, stingsAsFactors = FALSE)
mean5 <- aggregate(Var5 ~ Var1  + Var2 + Var3, data = data2, FUN = mean)
mean5 <- mean5[order(mean5$Var1, mean5$Var2, mean5$Var3), ]
mean5 <- as.data.frame(mean5, stingsAsFactors = FALSE)
expect <- list(mean4, mean5, sum6)
expect <- Reduce(merge, expect)

sum_all <- aggregate(. ~ Var1  + Var2 + Var3, data = data2, FUN = sum)
sum_all <- sum_all[order(sum_all$Var1, sum_all$Var2, sum_all$Var3), ]
sum_all <- as.data.frame(sum_all, stingsAsFactors = FALSE)

mean_all <- aggregate(. ~ Var1  + Var2 + Var3, data = data2, FUN = mean)
mean_all <- mean_all[order(mean_all$Var1, mean_all$Var2, mean_all$Var3), ]
mean_all <- as.data.frame(mean_all, stingsAsFactors = FALSE)

expect_all <- list(sum_all, mean_all)
expect_all <- Reduce(
  function(x, y) merge(x = x, y = y, by = c("Var1", "Var2", "Var3")),
  expect_all)
colnames(expect_all) <- gsub(".x", "_sum", colnames(expect_all))
colnames(expect_all) <- gsub(".y", "_mean", colnames(expect_all))

context("`aggregate_by`")

test_that("`aggregate_by` returns the correct output with columns name
          imputed", {

  # Test with one function
  test1 <- aggregate_by(data2, Var1, Var2, Var3, .funs = sum(Var6))
  testthat::expect_equal(test1, sum6)

  # Test with two operations
  test2 <- aggregate_by(data2, Var1, Var2, Var3,
                        .funs = list(sum(Var6), mean(Var4)))
  testthat::expect_equal(test2, Reduce(merge, list(sum6, mean4)))

  # Test with two operations and on one two columns
  test3 <- aggregate_by(data2, Var1, Var2, Var3,
                        .funs = list(mean(Var4, Var5), sum(Var6)))
  testthat::expect_equal(test3, expect)
})

test_that("`aggregate_by` returns the correct output without columns name
          imputed in `funs`", {

   # Test with one sum function
   test4 <- aggregate_by(data2, Var1, Var2, Var3, .funs = sum)
   testthat::expect_equal(test4, sum_all)

   # Test with one mean function
   test5 <- aggregate_by(data2, Var1, Var2, Var3, .funs = mean)
   testthat::expect_equal(test5, mean_all)

   # Test with both mean and sum function
   test6 <- aggregate_by(data2, Var1, Var2, Var3, .funs = list(sum, mean))
   testthat::expect_equal(test6, expect_all)

})

test_that("`aggregate_by` returns the correct output with quotation or not for
columns name imputed", {

   # Test without quotation
   test7 <- aggregate_by(data2, Var1, Var2, Var3)
   testthat::expect_equal(test7, sum_all)

   # Test with quotation
   test8 <- aggregate_by(data2, "Var1", "Var2", "Var3")
   testthat::expect_equal(test8, sum_all)

   # Test with a vector
   sel <- c("Var2", "Var3")
   test8 <- aggregate_by(data2, Var1, sel)
   testthat::expect_equal(test8, sum_all)

   # Test with a vector
   sel <- c("Var1", "Var2", "Var3")
   test9 <- aggregate_by(data2, sel)
   testthat::expect_equal(test9, sum_all)

})

test_that("`aggregate_by` behaviours during programming", {

  # Test inside a function
  do_aggregate <- function(df, group_var){
    df <- aggregate_by(df, Var1, group_var)
  }

  group_var <- c("Var2", "Var3")
  test10 <- do_aggregate(data2, group_var)
  testthat::expect_equal(test10, sum_all)

  # Test2 inside a function
  do_aggregate2 <- function(df, col_name, group_var, functs){
    aggregate_by(df, col_name, group_var, .funs = functs)
  }

  sel <- c("Var2", "Var3")
  funs1 <- list("sum", "mean")
  test11 <- do_aggregate2(data2, "Var1", sel, funs1)
  testthat::expect_equal(test11, expect_all)

  funs2 <- list("mean(Var4, Var5)", "sum(Var6)")
  test12 <- do_aggregate2(data2, "Var1", sel, funs2)
  testthat::expect_equal(test12, expect)
})
