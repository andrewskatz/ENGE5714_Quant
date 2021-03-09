### Week 8 - Comparing means (t-test) demos 


library(tidyverse)
library(broom)




getwd()
wd_path <- wd_path <- "G:/My Drive/AK Faculty/Teaching/ENGE 5714 - Quantitative Analysis Spring 2020/ENGE 5714 Quantitative Analysis - Shared materials/Week 8 - Comparing two means (t-tests)"
setwd(wd_path)
list.files()





### Demo 1 - comparing salary data for chemical engineering and civil engineering #####


# First, let's generate some data so that we know the underlying data-generating process that we use statistical tests to characterize

# we'll have a sample of 100 participants
N <- 100

student_id <- seq(N)

group_size <- N/2

chem_eng_df <- tibble(id = seq(group_size),
                      salary = round(rnorm(n = group_size, mean = 75000, sd = 3000), 2),
                      discipline = "chemical")
chem_eng_df %>% 
  ggplot(aes(x= salary)) +
  geom_histogram() +
  theme_bw()


env_eng_df <- tibble(id = seq(group_size),
                      salary = round(rnorm(n = group_size, mean = 65000, sd = 4000), 2),
                      discipline = "environmental")
env_eng_df %>% 
  ggplot(aes(x= salary)) +
  geom_histogram() +
  theme_bw()


# combine the two disciplinary dataframes together
salary_comb_long <- bind_rows(chem_eng_df, env_eng_df)



salary_comb_long %>% 
  ggplot(aes(x = salary, fill = discipline)) +
  geom_histogram(alpha = 0.2, position = "identity")


# independent samples t-test using the long format approach (NB: the long format is a good practice - it keeps the grouping variable in one column and the continuous outcome variable in a separate column)
salary_test <- t.test(salary ~ discipline, data = salary_comb_long, paired = FALSE)
salary_test

# use the tidy() function from the broom package to generate a tibble with the output of the t.test()
tidy(salary_test)


# remember that a t-test is similar to a linear regression model with a binary predictor variable and continuous outcome variable
salary_lm <- lm(salary ~ discipline, data = salary_comb_long)
summary(salary_lm)

# calculate the effect size of discipline on salary
(t <- salary_test$statistic[[1]]) # wrapping this in parentheses prints out the value of t at the same time that it stores the value of t
(df <- salary_test$parameter[[1]])
(r <- sqrt(t^2/(t^2 + df)))
round(r,3)
r^2

# we now have the results of an independent samples t-test and the effect size. 
# Think about what would happen if we changed the sample size 
# or if we changed the underlying distributions that we used to generate the salary data


# we can also run the t-test using a wide format approach
salary_comb_wide <- salary_comb_long %>% 
  pivot_wider(names_from = discipline, values_from = salary)

salary_test_2 <- t.test(salary_comb_wide$chemical, salary_comb_wide$environmental, paired = FALSE)
salary_test_2






### Demo 2 - student SAT scores #####

# Let's try a second example. This time we'll simulate data we might have where a dependent samples t-test would be appropriate. 
# We are going to simulate the scenario where we have student SAT scores on both the verbal section and math section

samp_size <- 1000

student_id <- seq(samp_size)
sat_verbal <- round(rnorm(n = samp_size, mean = 620, sd = 55), 0)
sat_math <- round(rnorm(n = samp_size, mean = 700, sd = 40), 0)

sat_verbal_capped <- modify(.x = sat_verbal, .f = ~ min(800, .x))
sat_math_capped <- modify(.x = sat_math, .f = ~min(800, .x))

sat_df <- tibble(id = student_id,
                 verbal = sat_verbal_capped,
                 math = sat_math_capped)

sat_df %>% ggplot(aes(x = verbal)) +
  geom_histogram() +
  labs(title = "Simulated SAT Verbal Histogram",
       x = "Verbal Score",
       y = "Count")


sat_df %>% ggplot(aes(x = math)) +
  geom_histogram() +
  labs(title = "Simulated SAT Math Histogram",
       x = "Math Score",
       y = "Count")


# alternatively, we could plot these together by either making a long data set or 
# just overlaying histograms and modifying transparency (the alpha parameter)
sat_df %>% ggplot() +
  geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
  labs(title = "Simulated SAT Scores Histogram",
       x = "SAT Score",
       y = "Count") +
  geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2)


# looking at the histogram, we can see that there appears to be a systematic difference in the scores
# Running a t-test will help us see if that initial observation is statistically correct. 
# Since we will be comparing verbal and math scores for each student, this will be a dependent samples (or paired samples) t-test

sat_t_test <- t.test(sat_df$verbal, sat_df$math, paired = TRUE) # don't forget to add: paired = TRUE
# now look at the results
sat_t_test




# we see there was a statistically significant difference, but now let's try changing the distributions and see what happens

# first, we'll change make the means very close together and see how sensitive the test is to that
samp_size <- 1000

student_id_2 <- seq(samp_size)
sat_verbal_2 <- round(rnorm(n = samp_size, mean = 690, sd = 55), 0)
sat_math_2 <- round(rnorm(n = samp_size, mean = 700, sd = 40), 0)

# let's cap the simulated scores at 800
sat_verbal_2_capped <- modify(.x = sat_verbal_2, .f = ~ min(800, .x))
sat_math_2_capped <- modify(.x = sat_math_2, .f = ~min(800, .x))


sat_df_2 <- tibble(id = student_id,
                 verbal = sat_verbal_2_capped,
                 math = sat_math_2_capped)

sat_df_2 %>% ggplot(aes(x = verbal)) +
  geom_histogram() +
  labs(title = "Simulated SAT Verbal Histogram",
       x = "Verbal Score",
       y = "Count")


sat_df_2 %>% ggplot(aes(x = math)) +
  geom_histogram() +
  labs(title = "Simulated SAT Math Histogram",
       x = "Math Score",
       y = "Count")


# alternatively, we could plot these together by either making a long data set or 
# just overlaying histograms and modifying transparency (the alpha parameter)
sat_df_2 %>% ggplot() +
  geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
  labs(title = "Simulated SAT Scores Histogram",
       x = "SAT Score",
       y = "Count") +
  geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2)


# looking at the histogram, we can see that there appears to be a systematic difference in the scores
# Running a t-test will help us see if that initial observation is statistically correct. 
# Since we will be comparing verbal and math scores for each student, this will be a dependent samples (or paired samples) t-test

sat_t_test_2 <- t.test(sat_df_2$verbal, sat_df_2$math, paired = TRUE) # don't forget to add: paired = TRUE
# now look at the results
sat_t_test_2






# let's try a third test - this time we'll change the sample size to be 25 students rather than 1000
samp_size <- 25

student_id_3 <- seq(samp_size)
sat_verbal_3 <- round(rnorm(n = samp_size, mean = 690, sd = 55), 0)
sat_math_3 <- round(rnorm(n = samp_size, mean = 700, sd = 40), 0)

# cap the scores at 800
sat_verbal_3_capped <- modify(.x = sat_verbal_3, .f = ~ min(800, .x))
sat_math_3_capped <- modify(.x = sat_math_3, .f = ~min(800, .x))



sat_df_3 <- tibble(id = student_id_3,
                 verbal = sat_verbal_3_capped,
                 math = sat_math_3_capped)

sat_df_3 %>% ggplot(aes(x = verbal)) +
  geom_histogram() +
  labs(title = "Simulated SAT Verbal Histogram",
       x = "Verbal Score",
       y = "Count")


sat_df_3 %>% ggplot(aes(x = math)) +
  geom_histogram() +
  labs(title = "Simulated SAT Math Histogram",
       x = "Math Score",
       y = "Count")


# alternatively, we could plot these together by either making a long data set or 
# just overlaying histograms and modifying transparency (the alpha parameter)
sat_df_3 %>% ggplot() +
  geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
  labs(title = "Simulated SAT Scores Histogram",
       x = "SAT Score",
       y = "Count") +
  geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2)


# looking at the histogram, we can see that there appears to be a systematic difference in the scores
# Running a t-test will help us see if that initial observation is statistically correct. 
# Since we will be comparing verbal and math scores for each student, this will be a dependent samples (or paired samples) t-test

sat_t_test_3 <- t.test(sat_df_3$verbal, sat_df_3$math, paired = TRUE) # don't forget to add: paired = TRUE
# now look at the results
sat_t_test_3



# now we see that the statistically significant difference is gone. How about if we go back to having a large difference in the means

samp_size <- 25

student_id_4 <- seq(samp_size)
sat_verbal_4 <- round(rnorm(n = samp_size, mean = 620, sd = 55), 0)
sat_math_4 <- round(rnorm(n = samp_size, mean = 700, sd = 40), 0)

#cap the scores at 800
sat_verbal_4_capped <- modify(.x = sat_verbal_4, .f = ~ min(800, .x))
sat_math_4_capped <- modify(.x = sat_math_4, .f = ~min(800, .x))


sat_df_4 <- tibble(id = student_id_4,
                   verbal = sat_verbal_4_capped,
                   math = sat_math_4_capped)

# visualize the distributions of scores
sat_df_4 %>% ggplot(aes(x = verbal)) +
  geom_histogram() +
  labs(title = "Simulated SAT Verbal Histogram",
       x = "Verbal Score",
       y = "Count")


sat_df_4 %>% ggplot(aes(x = math)) +
  geom_histogram() +
  labs(title = "Simulated SAT Math Histogram",
       x = "Math Score",
       y = "Count")


# alternatively, we could plot these together by either making a long data set or 
# just overlaying histograms and modifying transparency (the alpha parameter)
sat_df_4 %>% ggplot() +
  geom_histogram(aes(x = math, fill = "red"), alpha = 0.2) +
  labs(title = "Simulated SAT Scores Histogram",
       x = "SAT Score",
       y = "Count") +
  geom_histogram(aes(x = verbal, fill = "blue"), alpha = 0.2) +
  scale_fill_manual(name = "Category", values = c("blue", "red"), labels = c("verbal", "math"))


# looking at the histogram, we can see that there appears to be a systematic difference in the scores
# Running a t-test will help us see if that initial observation is statistically correct. 
# Since we will be comparing verbal and math scores for each student, this will be a dependent samples (or paired samples) t-test

sat_t_test_4 <- t.test(sat_df_4$math, sat_df_4$verbal, paired = TRUE) # don't forget to add: paired = TRUE
# now look at the results
sat_t_test_4

# We should see that the t-test will pick up differences between means if there is a sufficiently large difference.










