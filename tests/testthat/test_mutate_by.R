library(magrittr) # for ' %>% '
library(dplyr)

context("`mutate_by`")

test_that("`mutate_by` returns the correct output", {

  set.seed(30101976)
  data <- expand.grid(letters[1:3], 1:3, 4:6)
  data$Var4 <- sample(1:100, nrow(data), TRUE)
  data$Var5 <- sample(1:100, nrow(data), TRUE)
  data$Var6 <- sample(1:100, nrow(data), TRUE)

  expected <- data
  expected[which(expected$Var4 > 75), ] %<>%
    anti_join(expected, .) %>%
    group_by(Var1, Var2) %>%
    summarise(Var4 = mean(Var4)) %>%
    ungroup %>%
    left_join(data[which(expected$Var4 > 75), ], ., by = c("Var1", "Var2")) %>%
    rename(Var4 = Var4.y) %>%
    select(-Var4.x) %>%
    select(names(expected))
  attr(expected, "out.attrs") <- NULL

  expected2 <- data
  expected2[which(expected2$Var4 > 75), "Var4"] <-  mean(data$Var4)
  attr(expected2, "out.attrs") <- NULL

  testthat::expect_equal(data %>% mutate_by(Var4 > 75, mean, Var1, Var2) %>%
                           arrange(Var1, Var2, Var3),
                         expected %>% arrange(Var1, Var2, Var3))

  testthat::expect_equal(expected2 %>% arrange(Var1, Var2, Var3),
                         data %>% mutate_by(Var4 > 75, mean) %>%
                           arrange(Var1, Var2, Var3))

})