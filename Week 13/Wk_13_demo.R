## Week 13 Demo Script - Clustering

library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(reshape2)




## k-means clustering ----

# create cluster assignments (the 'true' data)
cluster_num <- 3
n <- 100
true_cluster <- sample(seq(cluster_num), size = n, replace = TRUE)
table(true_cluster)

# create a vector of student IDs
student_id <- map_chr(.x = seq(n), .f = ~ paste("student", .x))
student_id


  
# create the simulated data based on the 'true' data (i.e., true data + noise)




## method 1 - using for loops

## create placeholder vectors for the x and y coordinates
x <- rep(0, n)
y <- rep(0, n)

for (i in 1:n) {
  if (true_cluster[i] == 1) {
    x[i] <- rnorm(n = 1, mean = 3, sd = 1)
  } else {
    x[i] <- rnorm(n = 1, mean = 10, sd = 1)
  }
}

for (i in 1:n) {
  if (true_cluster[i] == 1) {
    y[i] <- rnorm(n = 1, mean = 3, sd = 1)
  } else {
    y[i] <- rnorm(n = 1, mean = 10, sd = 1)
  }
}


### method 2 - using purrr
cluster_num <- 3
n <- 100
true_cluster <- sample(seq(cluster_num), size = n, replace = TRUE)
table(true_cluster)


dim1 <- map_dbl(.x = true_cluster, .f = ~rnorm(n = 1, mean = .x, sd = .3))
dim2 <- map_dbl(.x = true_cluster, .f = ~rnorm(n = 1, mean = .x, sd = .3))


# for distance metrics, we'll use euclidean - mahalanobis (?)

plot(dim1, dim2)


data_df <- tibble(student_id, true_cluster, dim1, dim2)

data_df$dim1 <- scale(data_df$dim1)
data_df$dim2 <- scale(data_df$dim2)

# just pull out the scores that we will use from clustering 
score_df <- data_df %>% select(dim1, dim2)

k3 <- kmeans(score_df, centers = 3, nstart = 25)
str(k3)

#store the clusters in the dataframe
data_df$cluster <- k3$cluster

#visualization method one
fviz_cluster(k3, data = score_df)

#visualization method two
data_df %>% 
  ggplot(aes(x = dim1, y = dim2, color = factor(cluster))) +
  geom_point() +
  labs(title = "Sample k-means clustering for two clusters") +
  theme(plot.title = element_text(hjust = 0.5))





# method 1 to check for number of clusters

# first, create a function to calculate the within-cluster sum of squares
wss <- function(k) {
  kmeans(score_df, k, nstart = 10)$tot.withinss
}

# store values of k to test
max_k <- 15
k.values <- 1:max_k

#extract wss for clusters
wss_values <- map_dbl(k.values, wss)


plot(k.values, wss_values)



# method 2 to check for number of clusters using elbow plot
fviz_nbclust(score_df, kmeans, method = "wss")






# cluster determination using silhouette instead of elbow method

# method 1 - use a custom function
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(score_df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(score_df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")



# method 2 - use the fviz_nbclust() function with method = "silhouette"
fviz_nbclust(score_df, kmeans, method = "silhouette")










# extracting results and summarizing
data_df %>% 
  group_by(cluster) %>% 
  summarize(mean_dim1 = mean(dim1),
            mean_dim2 = mean(dim2))













### Hierarchical clustering ----

## we will do two demos for hierarchical clustering. The first will use the same data as for k-means.
## The second fake data will use new data that we generate.

### Hierarchical clustering demo 1 - use same data from k-means demo


# Step 1: start by calculating a distance matrix (how far away each observation is from every other observation)

# method 1 for distance calculation
distance <- get_dist(score_df)
fviz_dist(distance)


# method 2 for distance calculation
distance <- dist(score_df, method = "euclidean")


# with a distance matrix, we can now run the hierarchical clustering algorithm using hclust()
hc_res <- hclust(distance, method = "complete")
plot(hc_res, cex = 0.6, hang = -1)




# Demo 2 for hierarchical clustering

## we'll start by creating a fake data set of n students.
## The scenario for these data is that we administered a survey of q likert-scale items 
## and now want to see if there are clusters we can identify using hierarchical clustering instead of k-means

n <- 150
q_count <- 15 # number of questions in survey
q_scale <- 7 # size of likert-scale questions
cluster_num <- 4
true_cluster <- sample(x = seq(cluster_num), size = n, replace = TRUE)

# check to see if the distribution of the true clusters seems reasonable
table(true_cluster)


# generate the fake survey responses based on the true scores - first assign probabilities
probs <- case_when(true_cluster == 1 ~ 0.1,
                   true_cluster == 2 ~ 0.35,
                   true_cluster == 3 ~ 0.65,
                   true_cluster == 4 ~ 0.9)

# check to make sure probs vector contains a vector of probabilities
probs

# use several lines to create the actual responses
student_responses <- purrr::map(.x = probs, .f = ~ rbinom(n = q_count, size = q_scale, prob = .x))
# tibble(student_responses) -- This line is not good - returns a dataframe of lists, which is not what we want

# now we need to calculate the distance between the 
data_df <- melt(student_responses)
data_df$question <- rep(paste0("q", seq(1:q_count)), times = n)

# check to make sure we understand what is going on with rep()
rep(paste0("q", seq(1:q_count)), times = n)


# now we can pivot to make the dataframe that we would expect to have if we survey n students with q_count questions
data_df <- data_df %>% pivot_wider(names_from = question, values_from = value)
data_df <- data_df %>% rename(student_id = L1)
data_df$true_cl <- true_cluster

data_df[1,2:(q_count+1)]

score_df <- data_df %>% select(-c(student_id, true_cl))
# check the dataframe to make sure it looks like we would expect

# calculate the distance using one of two methods - either the subset or from making a new dataframe with select
distance <- dist(data_df[,2:(q_count+1)], method = "euclidean")



# use the dataframe with only the scores to calculate the distance matrix
distance <- dist(score_df, method = "euclidean")

# Manhattan distance metric is another alternative for method = ""

# with a distance matrix, we can now run the hierarchical clustering algorithm using hclust()
hc1 <- hclust(distance, method = "complete")
plot(hc1, cex = 0.6, hang = -1)


# draw the dendrogram with a border
plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 4, border = 2:5)


sub_grp <- cutree(hc1, k = 4)


table(sub_grp)
#check to see if these grouping counts match
table(true_cluster)

data_df <- data_df %>% mutate(hc_result = sub_grp)



# visualize the clutering after dimensionality reduction
fviz_cluster(list(data = data_df, cluster = sub_grp))


# create elbow plot
fviz_nbclust(score_df, FUN = hcut, method = "wss")


# create silhouette plot
fviz_nbclust(score_df, FUN = hcut, method = "silhouette")

