library(pROC)
library(tidyverse)
library(readxl)
library(caret)
library(glmnet)
library(glm2)
set.seed(302)
setwd("C:/Users/danie/Documents/data projects/Bank Churn Rate/Test and Submission Data")

#importing data set
bank_churn <- read_excel("train.xlsx")

#turning gender, geography, has credit card, is active member and exited to categorical variables
bank_churn$Exited <- factor(bank_churn$Exited, levels = c(0, 1), labels = c("no", "yes"))
categorize_vars <- function(df){
  df$Gender <- factor(df$Gender)
  df$Geography <- factor(df$Geography)
  df$HasCrCard <- factor(df$HasCrCard, levels = c(0, 1), labels = c("no", "yes"))
  df$IsActiveMember <- factor(df$IsActiveMember, levels = c(0, 1), labels = c("no", "yes"))
  return(df)
}
bank_churn <- categorize_vars(bank_churn)
#printing the summary
summary(bank_churn)

#separating data into training and test data
train_data <- bank_churn %>% sample_frac(0.7)
test_data <- anti_join(bank_churn, train_data)

#scaling test data with respect to training data
model_test_transform <- preProcess(train_data)
test_data <- predict(model_test_transform, test_data)
#scaling training data
train_data <- train_data %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))

#seperating explanatory and response variables 
x_train_data <- select(train_data, - Exited)
y_train_data <- train_data$Exited
x_train_encoded <- model.matrix(~., x_train_data) #one-hot encoding to make the x for glmnet
y_test_data <- test_data$Exited
x_test_data <- test_data %>% select(-Exited)

#run logistic regressions and find the threshold that gives max Area Under the Curve and minimal
# classification error
log_auc <- cv.glmnet(x_train_encoded, y_train_data, family = "binomial", type.measure = "auc")
log_misclassify <- cv.glmnet(x_train_encoded, y_train_data, family = "binomial", type.measure = "class")

# Plotting values for AUC vs log of lambda 
plot(log_auc)

#plotting ROC curve 
log_lm2 <- glm2(Exited ~., data = train_data, family = "binomial")
pred <- predict(log_lm2, x_test_data, type = "response")
ggroc(roc(y_test_data, pred))

#creating results for Kaggle prediction

#Read in data
pred_data <- read.csv("test.csv")

#set aside id
pred_id <- pred_data$id

#remove unnecessary columns like id and customerId
pred_data <- pred_data %>% select(-c(CustomerId, Surname, id))

#made HasCreditCard, Geography etc into categorical variables
pred_data <- categorize_vars(pred_data)

#normalized numeric data
pred_data <- predict(model_test_transform, pred_data)

#predictions (type argument is "class" for returning the prediction of Exited or not)
final_pred<- predict(log_misclassify, newx = model.matrix(~., pred_data), s = "lambda.min", type = "response")
final_pred <- as.vector(final_pred)
final_pred <- data.frame(id = pred_id, Exited = final_pred)
write.csv(final_pred, "log_predictions.csv", row.names = FALSE)
