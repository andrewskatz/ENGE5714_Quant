## Week 5 Demo

require(tidyverse)
require(psych)
require(kableExtra)
library(broom)


### Data generation demo - one set sample size ##########


#store the sample size that we want to use
samp_size <- 100

# uniformly sample X values (values for our predictor variable) from 0 to 20 
x <- round(runif(n = samp_size, min = 0, max = 30), digits = 1) # this gives samp_size number of random numbers



# store the noise values for our different test models
sd_min <- 2 # low noise
sd_med <- 6 # medium noise
sd_max <- 12 # high noise


# generate the outcome variable values under different amounts of noise (the rnorm() function is what is generating noise here)
y_noise_sd_none <- 3 + 2*x # this is the true relationship without any noise
y_noise_sd_min <- 3 + 2*x + round(x = rnorm(n = samp_size, mean = 0, sd = sd_min), digits = 1)
y_noise_sd_med <- 3 + 2*x + round(x = rnorm(n = samp_size, mean = 0, sd = sd_med), digits = 1)
y_noise_sd_max <- 3 + 2*x + round(x = rnorm(n = samp_size, mean = 0, sd = sd_max), digits = 1)



# typical step 1: visualize! Let's plot each of these x values vs y
plot(x, y_noise_sd_none)
plot(x, y_noise_sd_min)
plot(x, y_noise_sd_med)
plot(x, y_noise_sd_max)


# let's put all of these vectors together into a data frame to make it easier to analyze later on
# note, this is not a vital step for conducting the simple regression
demo_df <- tibble("x" = x, 
                  "y_noise_sd_none"=y_noise_sd_none, 
                  "y_noise_sd_min" = y_noise_sd_min,
                  "y_noise_sd_med" = y_noise_sd_med,
                  "y_noise_sd_max" = y_noise_sd_max)

#check out what demo_df looks like
head(demo_df, n = 10)

# order by increasing x value
demo_df <- demo_df %>% 
  arrange(desc(x))



# check out what the arrange() function did
head(demo_df)

# let's make this a long df so that we can plot multiple standard deviation values together
demo_df_long <- demo_df %>% 
  pivot_longer(cols = starts_with("y_noise"),
               names_to = "y_col",
               values_to = "y_val"
  )

# again, check on what this did
head(demo_df_long)

# let's add in a column to note whether the value is from the min, med, max, or zero sd (noise) model
demo_df_long <- demo_df_long %>% 
  mutate(sd_val = case_when(str_detect(y_col, "sd_none") ~ 0,
                            str_detect(y_col, "sd_min") ~ sd_min,
                            str_detect(y_col, "sd_med") ~ sd_med,
                            str_detect(y_col, "sd_max") ~ sd_max))

# use facet_grid to separate the plots out by 
demo_df_long %>% 
  ggplot(aes(x = x, y = y_val)) +
  geom_point() +
  facet_grid(.~y_col)

# you can also automatically add in a line with the geom_smooth() function
demo_df_long %>% 
  ggplot(aes(x = x, y = y_val)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(.~y_col)



# now we can create a linear model for the data with minimum noise with the following command:
fit_demo_min <- lm(y_noise_sd_min ~ x)

# and we can look at the summary of the model with:
summary(fit_demo_min)

# we can also look at model results with the glance() function from the broom package
broom::glance(fit_demo_min)


# we can create models for the med and max sd values as well and take a look at those with the summary() function once again
fit_demo_med <- lm(y_noise_sd_med ~ x)
summary(fit_demo_med)

fit_demo_max <- lm(y_noise_sd_max ~ x)
summary(fit_demo_max)

# notice the increase in the standard error of the coefficient estimates as the noise in y values went up



# from a programming perspective, this was not very efficient because I just copied, pasted, and corrected these values.
# there is a better way to do this using lists (see below)



# let's do some fancy stuff to make multiple models at once rather than having to write new lines for each model
# some of these ideas are taken from the R4DS book chapter 25
test_nest <- demo_df_long %>% nest(data = -sd_val)


linear_model <- function(df) {
  lm(y_val ~ x, data = df)
}


models <- map(test_nest$data, linear_model)

summary(models[[2]])
summary(models[[3]])
summary(models[[4]])


# we can also store the models as new columns in the nested dataframe
test_nest <- test_nest %>% 
  mutate(model = map(data, linear_model))


# finally, we can unnest the models to make it easier to compare them with each other in a data frame
test_nest <- test_nest %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)


test_nest






### data generation demo - one set sample size; 
## The change from the past demo is that we are now sampling from integer values rather than continuous for the predictor #######



#store the sample size that we want to use
samp_size <- 200


# instead of sampling uniformly from 0 to 20, this is to sample integers from 40 to 100 uniformly.
# we take "samp_size" number of samples. Replace = TRUE means we can get the same x value multiple times
x <- sample(x = c(60:100), size = samp_size, replace = TRUE)

# as before, store the noise values for our different test models
sd_min <- 2
sd_med <- 6
sd_max <- 12

y_noise_sd_none <- 3 + 2*x
y_noise_sd_min <- 3 + 2*x + round(x = rnorm(n = samp_size, mean = 0, sd = sd_min), digits = 1)
y_noise_sd_med <- 3 + 2*x + round(x = rnorm(n = samp_size, mean = 0, sd = sd_med), digits = 1)
y_noise_sd_max <- 3 + 2*x + round(x = rnorm(n = samp_size, mean = 0, sd = sd_max), digits = 1)



# typical step 1: visualize! Let's plot each of these x values vs y
plot(x, y_noise_sd_none)
plot(x, y_noise_sd_min)
plot(x, y_noise_sd_med)
plot(x, y_noise_sd_max)

# let's put all of these vectors together into a data frame to make it easier to analyze later on
# note, this is not a vital step for conducting the simple regression
demo_df <- tibble("x" = x, 
                  "y_noise_sd_none"=y_noise_sd_none, 
                  "y_noise_sd_min" = y_noise_sd_min,
                  "y_noise_sd_med" = y_noise_sd_med,
                  "y_noise_sd_max" = y_noise_sd_max)

# order by increasing x value
demo_df <- demo_df %>% 
  arrange(x)

# let's make this a long df so that we can plot multiple standard deviation values together
demo_df_long <- demo_df %>% 
  pivot_longer(cols = starts_with("y_noise"),
               names_to = "y_col",
               values_to = "y_val"
  )

demo_df_long <- demo_df_long %>% 
  mutate(sd_val = case_when(str_detect(y_col, "sd_none") ~ 0,
                            str_detect(y_col, "sd_min") ~ sd_min,
                            str_detect(y_col, "sd_med") ~ sd_med,
                            str_detect(y_col, "sd_max") ~ sd_max))

demo_df_long %>% 
  ggplot(aes(x = x, y = y_val)) +
  geom_point() +
  facet_grid(.~y_col)

demo_df_long %>% 
  ggplot(aes(x = x, y = y_val)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(.~y_col)


fit_demo_min <- lm(y_noise_sd_min ~ x)
summary(fit_demo_min)

# can also look at model results with the glance() function from the broom package
glance(fit_demo_min)


# we can create models for the med and max sd values as well and take a look at those with the summary() function once again
fit_demo_med <- lm(y_noise_sd_med ~ x)
summary(fit_demo_med)

fit_demo_max <- lm(y_noise_sd_max ~ x)
summary(fit_demo_max)

# notice the increase in the standard error of the coefficient estimates as the noise in y values went up



# from a programming perspective, this was not very efficient because I just copied, pasted, and corrected these values.
# there is a better way to do this using lists (see below)



# let's do some fancy stuff to make multiple models at once rather than having to write new lines for each model
# some of these ideas are taken from the R4DS book chapter 25
test_nest <- demo_df_long %>% nest(data = -sd_val)


linear_model <- function(df) {
  lm(y_val ~ x, data = df)
}


models <- map(test_nest$data, linear_model)

summary(models[[2]])
summary(models[[3]])
summary(models[[4]])


# we can also store the models as new columns in the nested dataframe
test_nest <- test_nest %>% 
  mutate(model = map(data, linear_model))


# finally, we can unnest the models to make it easier to compare them with each other in a data frame
test_nest <- test_nest %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

















### data generation with three different sample sizes #######


# let's run the same demo but now have three different sample sizes - 10, 50, and 500

# first, store the sample sizes we want to use
samp_sizes <- c(10, 50, 500)

#next, create a bookkeping column for ourselves to keep track of which sample size the future values will come from
samp_size_col <- rep(x = c(10,50, 500), times = samp_sizes)

# calculate the total number of values we will need from the three samples combined
tot_samp_size <- sum(samp_sizes)

# sample uniformly from 0 to 20
x <- round(x = runif(n = tot_samp_size, min = 0, max = 20), digits = 1)

# store the standard deviations for the min, med, and max models
sd_min <- 2
sd_med <- 6
sd_max <- 12

# calculate the y values for the different scenarios where there is no noise up to max noise
y_noise_sd_none <- 3 + 2*x
y_noise_sd_min <- 3 + 2*x + round(x = rnorm(n = tot_samp_size, mean = 0, sd = sd_min), digits = 1)
y_noise_sd_med <- 3 + 2*x + round(x = rnorm(n = tot_samp_size, mean = 0, sd = sd_med), digits = 1)
y_noise_sd_max <- 3 + 2*x + round(x = rnorm(n = tot_samp_size, mean = 0, sd = sd_max), digits = 1)



# typical step 1: visualize! Let's plot each of these x values vs y
plot(x, y_noise_sd_none)
plot(x, y_noise_sd_min)
plot(x, y_noise_sd_med)
plot(x, y_noise_sd_max)

# can we calculate the correlations between x and these different y values? (pro tip: yes)


# let's put all of these vectors together into a data frame to make it easier to analyze later on
# note, this is not a vital step for conducting the simple regression
demo_df <- tibble("n" = samp_size_col,
                  "x" = x, 
                  "y_noise_sd_none"=y_noise_sd_none, 
                  "y_noise_sd_min" = y_noise_sd_min,
                  "y_noise_sd_med" = y_noise_sd_med,
                  "y_noise_sd_max" = y_noise_sd_max)

# order by increasing x value
demo_df <- demo_df %>% 
  arrange(n, x)

# let's make this a long df so that we can plot multiple standard deviation values together
demo_df_long <- demo_df %>% 
  pivot_longer(cols = starts_with("y_noise"),
               names_to = "y_col",
               values_to = "y_val"
  )

demo_df_long <- demo_df_long %>% 
  mutate(sd_val = case_when(str_detect(y_col, "sd_none") ~ 0,
                            str_detect(y_col, "sd_min") ~ sd_min,
                            str_detect(y_col, "sd_med") ~ sd_med,
                            str_detect(y_col, "sd_max") ~ sd_max))

demo_df_long %>% 
  ggplot(aes(x = x, y = y_val)) +
  geom_point() +
  facet_grid(n~y_col)

demo_df_long %>% 
  ggplot(aes(x = x, y = y_val)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_grid(n~y_col)


fit_demo_min <- lm(y_noise_sd_min ~ x)
summary(fit_demo_min)

# can also look at model results with the glance() function from the broom package
glance(fit_demo_min)


# we can create models for the med and max sd values as well and take a look at those with the summary() function once again
fit_demo_med <- lm(y_noise_sd_med ~ x)
summary(fit_demo_med)

fit_demo_max <- lm(y_noise_sd_max ~ x)
summary(fit_demo_max)

# notice the increase in the standard error of the coefficient estimates as the noise in y values went up



# from a programming perspective, this was not very efficient because I just copied, pasted, and corrected these values.
# there is a better way to do this using lists (see below)



# let's do some fancy stuff to make multiple models at once rather than having to write new lines for each model
# some of these ideas are taken from the R4DS book chapter 25
test_nest <- demo_df_long %>% nest(data = -c(sd_val, n))


linear_model <- function(df) {
  lm(y_val ~ x, data = df)
}


models <- map(test_nest$data, linear_model)

summary(models[[2]])
summary(models[[3]])
summary(models[[4]])


# we can also store the models as new columns in the nested dataframe
test_nest <- test_nest %>% 
  mutate(model = map(data, linear_model))


# finally, we can unnest the models to make it easier to compare them with each other in a data frame
test_nest <- test_nest %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

# and look at the different models by just calling the data frame
test_nest





### Teacher salary linear regression demo  ########


file_path_prin <- "./data/principalSalaries.csv"
file_path_tea <- "./data/teacherSalaries.csv"

principal_salaries <- read_csv(file_path_prin)
teacher_salaries <- read_csv(file_path_tea)

public_schools_principal_salaries <- principal_salaries %>% 
  filter(str_detect(div_name,"Public")) %>%
  select(div_num,Av14_16P)

# convert the div_num column to numeric
public_schools_principal_salaries$div_num <- as.numeric(public_schools_principal_salaries$div_num)


public_schools_teacher_salaries <- teacher_salaries %>% 
  filter(str_detect(div_name,"Public")) %>% 
  select(div_num,Av14_16T)

public_schools_teacher_salaries$div_num <- as.numeric(public_schools_steacher_salaries$div_num)


combined_salaries <- inner_join(public_schools_teacher_salaries, 
                                public_chools_principal_salaries,
                                by="div_num")

# this section helps make tables for formatting in r or printing to csv file
# to load into excel etc
making_a_table <- describe(combined_salaries$Av14_16P) %>%
  select(mean,sd,skew,kurtosis)


kable(making_a_table) %>% 
  kable_styling("striped", full_width = F) 



# let's talk about running a linear model (simple regression) with the teacher 
# salary data

fit1 <- lm(Av14_16P ~ Av14_16T, data=combined_salaries)

summary(fit1)




combined_salaries %>% 
  ggplot(aes(x = Av14_16T,y = Av14_16P)) +
  geom_point() +
  geom_smooth(method = "lm")


combined_salaries %>% 
  ggplot(aes(x=div_num,y=Av14_16P)) +
  geom_point() +
  geom_point(aes(x=div_num,y=Av14_16T),colour='blue')






