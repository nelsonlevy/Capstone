##########################################################
# Create edx set, validation set (provided by the course)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

###########################################################
#Create RMSE function and test for optimal lambda
###########################################################

#Create RMSE function
RMSE <- function (true_ratings, predicted_ratings){ 
  sqrt(mean((true_ratings - predicted_ratings)^2)) 
} 

#Testing the best lambda (tuning parameter)
possible_lambdas <- seq(0, 5, 0.1) 

#Creates a function to test each possible lambda
rmses <- sapply(possible_lambdas, function (l){
  
  #Calculate mean of ratings in the training set
  mean_rating<- mean(edx$rating)
  
  #Bias of users
  b_users <- edx %>% 
    left_join(b_movies, by= "movieId") %>% 
    group_by(userId) %>% 
    summarize(b_users = sum(rating - b_movies - mean_rating)/(n()+l))  
  
  #Bias of movies
  b_movies <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_movies = sum(rating - mean_rating)/(n()+l))
  
  #predict ratings with each lambda
  predicted_ratings <- edx %>% 
    left_join(b_movies, by = "movieId") %>% 
    left_join(b_users, by = "userId") %>% 
    mutate(pred = mean_rating+ b_movies + b_users) %>% 
    .$pred
  
  #returns RMSE for each lambda
  return(RMSE(predicted_ratings, edx$rating)) 
}) 

##########################################################
# With the optimal lambda, predict the ratings using mean 
# and biases as details in the report
##########################################################


# Uses the minimal RMSE as lambda
lambda <- possible_lambdas[which.min(rmses)] 

pred_y <- sapply(lambda, function (l){
  
  #Calculate mean rating
  mean_rating<- mean(edx$rating)
  
  #User bias with optimal lambda
  b_users <- edx %>%
    left_join(b_movies, by="movieId") %>% 
    group_by(userId) %>% 
    summarize(b_users = sum(rating - b_movies - mean_rating)/(n()+l))
  
  #Movie bias with optimal lambda
  b_movies <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_movies = sum(rating - mean_rating)/(n()+l))
  
  #Predict ratings on test set (validation)
  predicted_ratings <- validation %>% 
    left_join(b_movies, by = "movieId" ) %>% 
    left_join(b_users, by = "userId") %>% 
    mutate(pred = mean_rating + b_movies + b_users) %>% 
    .$pred
  
  return(predicted_ratings) 
})

#Calculate the final RMSE
RMSE_final <- RMSE(validation$rating, pred_y)

##########################################################
# Export the predicted ratings 
##########################################################

write.csv(validation %>% 
            select(userId, movieId) %>% 
            mutate(rating = pred_y), "predictions.csv", na = "", row.names=FALSE)
