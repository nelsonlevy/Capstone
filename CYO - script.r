if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(writexl)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(readxl)
library(writexl)
library(dslabs)



#Read initial dataset as provided
data <- as.data.frame(read_xlsx(file.choose(), sheet=1, progress = readxl_progress()))

#alternative source for data file
#data <- tempfile()
#download.file("https://github.com/nelsonlevy/Capstone/blob/main/Fraud_base.xlsx", data)


#define the models for training
models <- c("gamLoess", "multinom", "lda", "glm") 

#define set.seed
set.seed(1, sample.kind = "Rounding") 

#divides in train_set and test_set
sample_size  <- floor(0.9 * nrow(data))
train_index <- sample(seq_len(nrow(data)), size=sample_size)  
train_set <- data[train_index,]  #90% of observations
test_set <- data[-train_index,]  #10% of observations

#Fitting the models
fits <- lapply(models, function(model){ 
  print(model)
  train(Status ~ ., method = model, data = train_set)
}) 
names(fits) <- models

#Register the predictions
pred <- sapply(fits, function(object) 
  predict(object, newdata = test_set))
dim(pred)

#Accuracy analysis 
acc <- colMeans(pred == test_set$Status)
acc
mean(acc)

#compute the votes
votes <- rowMeans(pred == "Fraud")

#if there are at least two votes for "Fraud", compute as "Fraud"; else, compute as "Not_Fraud"
y_hat <- ifelse(votes > 0.49, "Fraud", "Not_Fraud")
mean(y_hat == test_set$Status)

# organize the data in a matrix
table(predicted = y_hat, actual = test_set$Status)

# ensembled accuracy
test_set %>%
  mutate(y_hat = y_hat) %>%
  summarize(accuracy = mean(y_hat == Status))

#Testing in the rest of the dataset
data_all <- as.data.frame(read_xlsx(file.choose(), sheet=1, progress = readxl_progress()))

#alternative source for data_all file
#data <- tempfile()
#download.file("https://github.com/nelsonlevy/Capstone/blob/main/distinct.xlsx", data)


#Predict to observations in "data_all" object
pred2 <- sapply(fits, function(object) 
  predict(object, newdata = data_all))
dim(pred2)

#Export data
head(pred)
write_xlsx(as.data.frame(test_set), file.choose())
write_xlsx(as.data.frame(pred), file.choose())
