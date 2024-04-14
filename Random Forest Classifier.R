library(tidyverse)
library(readxl)
library(randomForest)
library(caret)
#importing data set

set.seed(73)
setwd("C:/Users/danie/Documents/data projects/Bank Churn Rate/Test and Submission Data")
bank_churn <- read_excel("train.xlsx")
#turning gender, geography, has credit card, is active member and exited to categorical variables
categorize_vars <- function(df){
  df$Gender <- factor(df$Gender)
  df$Geography <- factor(df$Geography)
  df$HasCrCard <- factor(df$HasCrCard, levels = c(0, 1), labels = c("no", "yes"))
  df$IsActiveMember <- factor(df$IsActiveMember, levels = c(0, 1), labels = c("no", "yes"))
  return(df)
}
bank_churn <- categorize_vars(bank_churn)
bank_churn$Exited <- factor(bank_churn$Exited, levels = c(0, 1), labels = c("no", "yes"))
#printing the summary
summary(bank_churn)
#separating data into training and test data
train_data <- bank_churn %>% sample_frac(0.7)
test_data <- anti_join(bank_churn, train_data)
y_test <- test_data$Exited
x_test <- test_data %>% select(-Exited)
#normalizing test data
model_test_transform <- preProcess(train_data)
x_test <- predict(model_test_transform, x_test)
#normalizing training data
train_data <- train_data %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))
#run the Random Forest Model
randFr <- randomForest(Exited ~ ., data = train_data, importance = TRUE)
randFr
# confusion matrix on test data
confusionMatrix(y_test, predict(randFr, newdata = x_test))

#competition prediction data
pred_data <- read.csv("test.csv")

#set aside id
pred_id <- pred_data$id

#remove unnecessary columns like id and customerId
pred_data <- pred_data %>% select(-c(CustomerId, Surname, id))

#made HasCreditCard, Geography etc into categorical variables
pred_data <- categorize_vars(pred_data)

#normalized numeric data
pred_data <- predict(model_test_transform, pred_data)

#predicting the probabilities of exiting using the prediction data
final_pred <- as.data.frame(predict(randFr, newdata = pred_data, type = "prob")) 

#exporting the dataframe made from the predictions to a csv file
final_pred <- data.frame(id = pred_id, Exited = final_pred$yes)
write.csv(final_pred, file = "random_forest_prediction.csv", row.names = FALSE)
