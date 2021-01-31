## Week 3 Notes


library(tidyverse)


# this shows me reading in with a .csv and then saving in two different formats
mydata <- read_csv(file="/Users/akatz4/Downloads/Free Reduced Lunch.csv")
#mydf <- read_csv("Free Reduced Lunch.csv")

# the .RDS format is helpful when working with very large datasets (on the order of hundreds of MB). If your
# dataset is under 300 MB in size, I wouldn't worry too much about saving in .RDS format versus .csv
#save(mydf,file="FRL.RData")
#saveRDS(mydf,file="FRL.RDS")

getwd()
file_path <- "YOUR PATH HERE"

setwd(file_path)

# use this to make sure you have the file(s) that you want in the working directory
list.files()


#this demonstrates reading in the .RDS file. We have also done load("FRL.RData")                                
mydata <- readRDS("FRL.RDS")
mydata <- read_rds("FRL.RDS")

# check the structure of the data
str(mydata)
str(mydata$total_2017)

# note that all this data reads in as "factors" which are categorical variables.
# but, we know that many of them are numbers or "numeric" values as R will call them.
# so, lets tell R that these columns (at least the two I'm going to use) are numbers
# I'm going to use the $ operator which lets me specify a specific column within my data frame

mydata$total_2017<-as.numeric(mydata$total_2017)
mydata$totalFRL_1718<-as.numeric(mydata$totalFRL_1718)


#alternatively, you can do this for a whole set of variables at once depending on a matching criteria

newdf <- mydata %>% 
  mutate_at(vars(starts_with("total_")), as.numeric)

newdf <- newdf %>% 
  mutate_at(vars(starts_with("totalFRL")), as.numeric)

#check whether the old and new variables are stored differently (old as a factor, new as a numeric variable)
str(mydata$total_2008)
str(newdf$total_2008)



# let's go ahead and try using tidyverse to narrow to what I want
# I will start out with my entire data frame and then use pipes to operate from there
# then, the final result will be stored in my new data frame I'm creating called
# county.level.aggregate
# select will pick columns
# group_by and summarize work together to get me my aggregate totals

county.level.aggregate <- newdf %>% 
  select(div_name,total_2017,totalFRL_1718) %>%
  group_by(div_name) %>%
  summarize(totalstudents=sum(total_2017), totalFRL=sum(totalFRL_1718))
# now, I can compute percentages if i like and I can specify a new column by referring to
# one that doesn't exist yet but will after I run this code

county_level_aggregate$percent_FRL <- county_level_aggregate$totalFRL/county_level_aggregate$totalstudents*100



# just for fun, let me show you how this could have been incorporated into my summarize call

county_level_percents <- newdf %>% 
  select(div_name,total_2017,totalFRL_1718) %>%
  group_by(div_name) %>%
  summarize(percentFRL=sum(totalFRL_1718)/sum(total_2017))

myGraph <- ggplot(newdf, aes(school_name, totalFT_2008)) + ggtitle("FRL 2008") 
myGraph + geom_point()












