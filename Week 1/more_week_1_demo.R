## More demo code for week 1

x <- 3
y <- c(1, 2, 3)
z <- "this is a string"

a <- x + y




# installing packages


install.packages("")


# loading packages

library(tidyverse)



# basic plotting
x <- seq(1, 50) # create a sequence of numbers from 1 to 50
y <- 2*x + 3
plot(x, y)


# slightly fancier plotting
df <- tibble(x = x, y = y)

df %>% ggplot(aes(x = x, y = y)) + 
  geom_point() 


disciplines <- sample(c("chemical", "civil"), replace = TRUE, size = length(x))


df$disciplines <- disciplines  

# now maybe we want to add some color to the plot
df %>% ggplot(aes(x = x, y = y, color = disciplines)) +
  geom_point()



# or maybe we want to separate the plots by discipline

df %>% ggplot(aes(x = x, y = y)) +
  geom_point() + 
  facet_grid(. ~ disciplines)
  
