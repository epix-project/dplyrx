library(dplyr)

# Let's start by adding an attribute to a data frame:
attr(mtcars, "my_attribute_name") <- "my_attribute_value"

# We can see the attributes of this data frame:
attributes(mtcars)

# We can now see that mutate drops the non-class attributes:
mutate(mtcars, a_new_variable = 3) %>% attributes()

# Whereas mutate2 does not:
mutate2(mtcars, a_new_variable = 3) %>% attributes()
