library(dplyr)    # for 'recode', 'select', 'summarise'
library(magrittr) # for ' %>% '

context("test `gaggregate_by` output")

test_that("`aggregate_by` returns the correct output with columns name
          imputed", {

  # create a simple data frame
  set.seed(30101976)
  data <- expand.grid(letters[1:3], 1:3, 4:6)
  data$Var4 <- sample(1:100, nrow(data), TRUE)
  data$Var5 <- sample(1:100, nrow(data), TRUE)
  data$Var6 <- sample(1:100, nrow(data), TRUE)

  # expected results
  sum6 <- data %>%
    mutate(Var1 = recode(Var1, a = "b")) %>% group_by(Var1, Var2, Var3) %>%
    summarise(sum(Var6))
  mean4 <- data %>% mutate(Var1 = recode(Var1, a = "b")) %>%
    group_by(Var1, Var2, Var3) %>% summarise(mean(Var4))
  mean5 <- data %>% mutate(Var1 = recode(Var1, a = "b")) %>%
    group_by(Var1, Var2, Var3) %>% summarise(mean(Var5))
  expect <- list(mean4,mean5, sum6) %>% purrr::reduce(left_join)

  # Test with one function
  test1 <- data %>%
    mutate(Var1 = recode(Var1, a = "b")) %>%
    aggregate_by(Var1, Var2, Var3, .funs = sum(Var6))

  expect_equal(test1, expect %>% select(-`mean(Var5)`, -`mean(Var4)`))

  # Test with two operations
  test2 <- data %>%
    mutate(Var1 = recode(Var1, a = "b")) %>%
    aggregate_by(Var1, Var2, Var3, .funs = list(sum(Var6), mean(Var4)))

  expect_equal(test2, expect %>% select(-`mean(Var5)`))

  # Test with two operations and on one two columns
  test3 <- data %>%
    mutate(Var1 = recode(Var1, a = "b")) %>%
    aggregate_by(Var1, Var2, Var3, .funs = list(mean(Var4, Var5), sum(Var6)))

  expect_equal(expect,test3)

})


test_that("`aggregate_by` returns the correct output with columns name
          imputed", {

  # create a simple data frame
  set.seed(30101976)
  data <- expand.grid(letters[1:3], 1:3, 4:6)
  data$Var4 <- sample(1:100, nrow(data), TRUE)
  data$Var5 <- sample(1:100, nrow(data), TRUE)
  data$Var6 <- sample(1:100, nrow(data), TRUE)

   # expected results
   sum_all <- data %>%
     mutate(Var1 = recode(Var1, a = "b")) %>% group_by(Var1, Var2, Var3) %>%
     summarise_all(sum)
   mean_all <- data %>%
     mutate(Var1 = recode(Var1, a = "b")) %>% group_by(Var1, Var2, Var3) %>%
     summarise_all(mean)

   # Test with one sum function
   test4 <- data %>%
     mutate(Var1 = recode(Var1, a = "b")) %>%
     aggregate_by(Var1, Var2, Var3, .funs = sum)
   expect_equal(test4, sum_all)

   # Test with one mean function
   test5 <- data %>%
     mutate(Var1 = recode(Var1, a = "b")) %>%
     aggregate_by(Var1, Var2, Var3, .funs = mean)
   expect_equal(test5, mean_all)

   # Test with both mean and sum function
   colnames(sum_all) %<>% gsub("Var4", "Var4_sum", .) %>%
     gsub("Var5", "Var5_sum", .) %>%
     gsub("Var6", "Var6_sum", .)
   colnames(mean_all) %<>% gsub("Var4", "Var4_mean", .) %>%
     gsub("Var5", "Var5_mean", .) %>%
     gsub("Var6", "Var6_mean", .)
   expect <- list(sum_all, mean_all) %>%
     purrr::reduce(left_join, by = c("Var1", "Var2", "Var3"))

   test6 <- data %>%
     mutate(Var1 = recode(Var1, a = "b")) %>%
     aggregate_by(Var1, Var2, Var3, .funs = list(sum, mean))
   expect_equal(test6, expect)

})
