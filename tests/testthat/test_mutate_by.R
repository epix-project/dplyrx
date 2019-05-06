# test examples
set.seed(30101976)
data <- expand.grid(letters[1:3], 1:3, 4:6)
data$Var4 <- sample(1:100, nrow(data), TRUE)
data$Var5 <- sample(1:100, nrow(data), TRUE)
data$Var6 <- sample(1:100, nrow(data), TRUE)
data <- transform(data, Var1 =  as.character(data$Var1))

context("`mutate_by`")

test_that("`mutate_by` returns the correct output", {

  expected <- data
  expected[which(expected$Var4 > 75), "Var4"] <-  mean(data$Var4)

  test1 <- mutate_by(data, Var4 > 75, mean)

  testthat::expect_equal(expected[order(expected$Var1, expected$Var2),],
                         test1[order(test1$Var1, test1$Var2),])

  # test arguments `...`
  data$Var4 <- c(sample(1:100, nrow(data) - 2, TRUE), NA, NA)
  expected2 <- data
  expected2[which(expected2$Var4 > 75), "Var4"] <- mean(data$Var4, na.rm = TRUE)

  test2 <- mutate_by(data, Var4 > 75, mean, na.rm = TRUE)
  testthat::expect_equal(expected2[order(expected2$Var1, expected2$Var2),],
                         test2[order(test2$Var1, test2$Var2),])

  # test all arguments
  expected3 <- data
  expected3[which(expected3$Var4 == 94), "Var4"] <-
    mean(expected3[which(expected3$Var2 == 3 & expected3$Var1 == "c" &
                         expected3$Var4 != 94), "Var4"])

  test3 <- mutate_by(data, Var4 == 94, mean, na.rm = TRUE,
                     colgroups = c("Var1", "Var2"))
  testthat::expect_equivalent(expected3[order(expected3$Var1, expected3$Var2),],
                              test3)
})
