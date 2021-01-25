## Week 2 Notes

## One of the first things you will have in any script or .rmd file is a section to load
## all the libraries that you use in that script.

# you can install a library by using the install.packages() function, for example:
install.packages("tidyverse")

# with this installed, you can then load the package using the library() function


library(tidyverse)



# Distributions




# Working in R



## Reading in data

prior_survey <- read_csv("ENGE_5714_2021_pre_survey.csv")



# Exploring the data





## Plotting data

ggplot(data = prior_survey, mapping = aes(x = `I know what a type I error is`)) +
  geom_bar() +
  coord_flip()