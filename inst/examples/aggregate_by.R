library(dplyr)

## A toy data frame:
set.seed(30101976)
data <- expand.grid(letters[1:3], 1:3, 4:6)
data$Var4 <- sample(1:100, nrow(data), TRUE)
data$Var5 <- sample(1:100, nrow(data), TRUE)
data$Var6 <- sample(1:100, nrow(data), TRUE)

## Aggregating the values "a" and "b" of the categorical variable Var1,
## summing the values of variables Var4, Var5, Var6 (i.e. all the variables
## that are not in the arguments of the function call):
data %>%
  mutate(Var1 = recode(Var1, a = "b")) %>%
  aggregate_by(Var1, Var2, Var3)

## To calculate the mean value:
data %>%
  mutate(Var1 = recode(Var1, a = "b")) %>%
  aggregate_by(Var1, Var2, Var3, .funs = mean)

## or to apply it to all the columns:
data %>%
  mutate(Var1 = recode(Var1, a = "b")) %>%
  aggregate_by(Var1, Var2, Var3, .funs = list(sum, mean))

## To calculate the mean and the sum:
data %>%
  mutate(Var1 = recode(Var1, a = "b")) %>%
  aggregate_by(Var1, Var2, Var3, .funs = list(sum(Var6), mean(Var4)))
