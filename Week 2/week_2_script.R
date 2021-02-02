## Week 2 Notes ----


x <- seq(1:10)
y_is_a_very_long_name <- 2* x + 3



plot(x, y)


# Getting your R environment set up ----

## One of the first things you will have in any script or .rmd file is a section to load
## all the libraries that you use in that script.

# you can install a library by using the install.packages() function, for example:
install.packages("tidyverse")

install.packages("janitor")

install.packages("psych")

# with this installed, you can then load the package using the library() function


library(tidyverse)
library(janitor)
library(psych)





## Reading in data ----

### A good first step is to check which directory you are working in with the getwd() function
getwd()

### you can also check which files are in that directory with list.files()
list.files()

### If you notice that the file you are looking for is not there, then you can use setwd()
### to change your working directory
setwd("./Week 2/")

### After that, make sure you have switched to the correct working directory

getwd()
list.files()


### Assuming you have directed yourself to the correct place, you can now read in 
### the file(s) that you want to be working with

prior_survey <- read_csv("ENGE_5714_2021_pre_survey.csv")

prior_survey <- read_csv("C:/desktop/my super sweet course folder/Week 2 is the best/ENGE_5714_survey.csv")



# Exploring the data ----

## Take a look at the csv

prior_survey

view(prior_survey)



prior_survey <- prior_survey %>% clean_names() # from janitor package

prior_survey$`I have heard the term "non-parametric statistics" before`


table(prior_survey$i_have_taken_a_quantitative_research_methods_course_before)

#using describe() from the psych package
describe(prior_survey)


## Plotting data

ggplot(data = prior_survey, mapping = aes(x = `I know what a type I error is`)) +
  geom_bar() +
  coord_flip()



prior_survey %>% 
  gather(key = "survey_item", value = "survey_response") %>%
  


prior_survey %>% 
  gather(key = "survey_item", value = "survey_response") %>% 
  group_by(survey_item, survey_response) %>% 
  summarize(n = n()) %>% 
  ggplot(mapping = aes(x = survey_response, y = survey_item, fill = n)) +
  geom_tile()


## This plot is okay for giving a general sense of what is going on in these plots
## but there are a bunch of other ways to go about doing this

### First, maybe we want to rename the response categories to a numerical scale

prior_survey <- prior_survey %>% 
  gather(key = "survey_item", value = "survey_response") %>% 
  mutate(survey_response_num = case_when(survey_response == "Strongly disagree" ~ 0,
                                         survey_response == "Somewhat disagree" ~ 1,
                                         survey_response == "Neither agree nor disagree" ~ 2,
                                         survey_response == "Somewhat agree" ~ 3,
                                         survey_response == "Strongly agree" ~ 4,
                                         )) 


### Then we plot the same data but with the numerical scale along the x-axis
prior_survey %>% 
  group_by(survey_item, survey_response_num) %>% 
  summarize(n = n()) %>% 
  ggplot(mapping = aes(x = survey_response_num, y = survey_item, fill = n)) +
  geom_tile()
  





