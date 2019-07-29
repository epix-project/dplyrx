library(dplyr)

context("`mutate2`")

test_that("`mutate2` returns the correct output", {

  attr(mtcars, "my_attribute_name") <- "my_attribute_value"

  expect_equivalent(mutate2(mtcars, gear = gear), mtcars)

  expect_s3_class(mutate2(mtcars, gear = gear), class(mtcars))

})
