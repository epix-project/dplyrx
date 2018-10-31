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

  testthat::expect_equal(data %>%
                           mutate_by(Var4 > 75, mean,
                                     colgroups = c("Var1", "Var2")) %>%
                           arrange(Var1, Var2, Var3),
                         expected %>% arrange(Var1, Var2, Var3))

  testthat::expect_equal(expected2 %>% arrange(Var1, Var2, Var3),
                         data %>% mutate_by(Var4 > 75, mean) %>%
                           arrange(Var1, Var2, Var3))

  df <- dplyr::starwars %>% select(name, height, mass)
  dfexp <- df
  dfexp[which(dfexp$height > 220), "height"] <- mean(dfexp$height, na.rm = TRUE)
  attr(dfexp, "out.attrs") <- NULL

  testthat::expect_equal(df %>% mutate_by(height > 220, mean, na.rm = TRUE) %>%
                           arrange(name, height, mass),
                         dfexp %>% arrange(name, height, mass))

  dfexp2 <- dplyr::starwars %>% select(name, height, mass, species)
  dfexp2[which(dfexp2$height > 230), ] %<>%
    anti_join(dfexp2, .) %>%
    group_by(species) %>%
    summarise(height = mean(height, na.rm = TRUE)) %>%
    ungroup %>%
    left_join(dfexp2[which(dfexp2$height > 220), ], . , by = c("species")) %>%
    rename(height = height.y) %>%
    select(-height.x) %>%
    select(names(dfexp2))
  attr(dfexp2, "out.attrs") <- NULL

  testthat::expect_equal(dfexp2 %>% mutate_by(height > 230, mean, na.rm = TRUE,
                                              colgroups = "species") %>%
                           arrange(name, height, mass),
                         dfexp2 %>% arrange(name, height, mass))

})
