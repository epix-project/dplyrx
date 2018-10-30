library(magrittr) # for ' %>% '
library(dplyr)

context("`mutate2`")

test_that("`mutate2` returns the correct output", {

  attr(mtcars, "my_attribute_name") <- "my_attribute_value"

  expect_equivalent(mtcars %>% mutate2(gear = gear), mtcars)

  expect_s3_class(mtcars %>% mutate2(gear = gear), class(mtcars))

})

