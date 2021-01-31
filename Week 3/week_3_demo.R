## Week 3 Demo


## NOTE: In future weeks we will try to transition to .rmd (R markdown) files instead of .R (script) files
## but I am using the .R this week to try and reduce cognitive load 
# (i.e., give you one less new thing to think about)

library(tidyverse)



### Reading and Writing for RDS ----

# The .RDS format is helpful when working with very large datasets (on the order of hundreds of MB). If your
# dataset is under 300 MB in size, I wouldn't worry too much about saving in .RDS format versus .csv
#save(mydf,file="FRL.RData")
#saveRDS(mydf,file="FRL.RDS")






# This shows me reading in with a .csv and then saving in two different formats
mydata <- read_csv(file="/Users/akatz4/Downloads/Free Reduced Lunch.csv")
#mydf <- read_csv("Free Reduced Lunch.csv")


#this demonstrates reading in the .RDS file. We have also done load("FRL.RData")                                
mydata <- readRDS("FRL.RDS")
mydata <- read_rds("FRL.RDS")



### Reading CSV ---- 

getwd()
file_path <- "YOUR PATH HERE"

setwd(file_path)

# since I have my files organized a little differently, I use this:
setwd("./Week 3/")
list.files()


setwd("./data/")

# use this to make sure you have the file(s) that you want in the working directory
list.files()

mydata <- read_csv("Free Reduced Lunch by Schools and Grade Structures 2008-2017_final.csv")




# check the structure of the data
str(mydata)
str(mydata$total_2017)



### Mutating Variables ----

# note that almost all of the data reads in as a "character" data type which are just strings, 
# This can create issues.  
# We know that many of the columns are actually storing  numbers or "numeric" values as R refers to them.
# We need to fix this.
# Let's tell R that these columns (at least the two we are going to use) are numeric.
# We are going to see two interchangeable ways to do this.

# First, we use the $ operator which lets me specify a specific column within my data frame
# in combination with the as.numeric() function

mydata$total_2017<-as.numeric(mydata$total_2017)
mydata$totalFRL_1718<-as.numeric(mydata$totalFRL_1718)


# Second, alternatively, we can do this for a whole set of variables at once.
# We just need to specify a matching criteria.

newdf <- mydata %>% 
  mutate_at(vars(starts_with("total")), as.numeric)

newdf <- newdf %>% 
  mutate_at(vars(starts_with("totalFRL")), as.numeric)

# Check whether the old and new variables are stored differently 
# (old as a character, new as a numeric variable)
str(mydata$total_2008)
str(newdf$total_2008)



### Filtering, Selecting, Grouping, and Summarizing ----


# A basic operation we do a lot is to filter the data so that we are working with a subset of all that we have
# We can do this with the filter() function, part of the dplyr package (in the tidyverse collection of packages).


# Let's say we want to look at the schools with div_num values less than 50
newdf %>% filter(div_num < 50)

# Or, if we want to look at schools where the highest grade in 2008 was grade five, we can try:
newdf %>% filter(higrade_2008 == "5") # this returns a subsetted dataframe with 878 rows

## Note that we had to set it equal to the character value "5" rather than the numeric value 5. Why?

# If we wanted to filter on numeric values instead, we would want to do something like this:
newdf %>% 
  mutate(higrade_2008 = as.numeric(higrade_2008)) %>% 
  filter(higrade_2008 == 5) # again, this returns a subsetted dataframe with 878 rows


# Let's shift gears to a different combination of operations...

# Let's go ahead and try using tidyverse to narrow to what we want.
# Imagine we want to see the county level aggregate numbers for FRL in the 2017-2018 school year.

# We will start out with our entire data frame and then use pipes (the %>% operator) to work from there.
# The final result will be stored in our new data frame that we are creating, called county_level_aggregate.
 
# First, select will pick columns
# Next, group_by and summarize work together to get us our aggregate totals.

county_level_aggregate <- newdf %>% 
  select(div_name,total_2017,totalFRL_1718) %>%
  group_by(div_name) %>%
  summarize(totalstudents = sum(total_2017), 
            totalFRL = sum(totalFRL_1718))
# now, we can compute percentages if we like and we can specify a new column by referring to
# one that doesn't exist yet but will after we run this code.
# We will do this two interchangeable ways.

# First, the old school way:

county_level_aggregate$percent_FRL <- county_level_aggregate$totalFRL/county_level_aggregate$totalstudents*100


# Second, the tidyverse way:
county_level_aggregate <- county_level_aggregate %>% 
  mutate(percent_frl = totalFRL / totalstudents * 100)



# just for fun, let's see how this could have been incorporated into our summarize call

county_level_percents <- newdf %>% 
  select(div_name, total_2017, totalFRL_1718) %>%
  group_by(div_name) %>%
  summarize(percentFRL=sum(totalFRL_1718)/sum(total_2017) * 100)





### Plotting ---- 


# Finally, let's plot something


# Something is going to look weird with this plot
newdf %>% 
  ggplot(aes(totalFRL_0809, totalFT_2008)) +
  geom_point() +
  labs(title = "FRL 2008", x = "totalFRL_0809")


# Let's see if we can fix it
newdf %>% 
  filter(totalFRL_0809 > 0) %>% 
  ggplot(aes(totalFRL_0809, totalFT_2008)) +
  geom_point() +
  labs(title = "FRL 2008", x = "totalFRL_0809") +
  xlim(0, 1000) +
  ylim(0, 1000)

typeof(newdf$totalFT_2008)








