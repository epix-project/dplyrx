library(dplyr)

## A toy data frame:
set.seed(30101976)
data <- expand.grid(letters[1:3], 1:3, 4:6)
data$Var4 <- sample(1:100, nrow(data), TRUE)
data$Var5 <- sample(1:100, nrow(data), TRUE)
data$Var6 <- sample(1:100, nrow(data), TRUE)

# To mutate the value > 75 in the variable `Var4` by the mean of the value of
# `Var4` except the one > 75.
data %>% mutate_by(Var4 > 75, mean)

# To mutate the value > 75 in the variable `Var4` by the mean of the value of
# `Var4` group by `Var1` and `Var2`, except the one > 75.
data %>% mutate_by(Var4 > 75, mean, colgroups = c("Var1", "Var2"))

# To mutate the value > 75 in the variable `Var4` by the mean of the value of
# `Var4` group by `Var1` and `Var2`, except the one > 75 and by removing NA
# value.
data %>% mutate_by(Var4 > 75, mean, colgroups = c("Var1", "Var2"),
  na.rm = TRUE)
