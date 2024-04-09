library(tidyverse)
library(readxl)
library(randomForest)
library(caret)
#importing data set

set.seed(73)
setwd("C:/Users/danie/Documents/data projects/Bank Churn Rate/Test and Submission Data")
bank_churn <- read_excel("train.xlsx")
#turning gender, geography, has credit card, is active member and exited to categorical variables
bank_churn$Gender <- factor(bank_churn$Gender)
bank_churn$Geography <- factor(bank_churn$Geography)
bank_churn$HasCrCard <- factor(bank_churn$HasCrCard, levels = c(0, 1), labels = c("no", "yes"))
bank_churn$IsActiveMember <- factor(bank_churn$IsActiveMember, levels = c(0, 1), labels = c("no", "yes"))
bank_churn$Exited <- factor(bank_churn$Exited, levels = c(0, 1), labels = c("no", "yes"))
#printing the summary
summary(bank_churn)
#separating data into training and test data
train <- bank_churn %>% sample_frac(0.7)
test <- anti_join(bank_churn, train)
y_test <- test$Exited
x_test <- test %>% select(-Exited)
#normalizing test data
x_test$CreditScore = (x_test$CreditScore - mean(train$CreditScore))/sd(train$CreditScore)
x_test$Age = (x_test$Age - mean(train$Age))/sd(train$Age)
x_test$Tenure = (x_test$Tenure - mean(train$Tenure))/sd(train$Tenure)
x_test$Balance = (x_test$Balance - mean(train$Balance))/sd(train$Balance)
x_test$NumOfProducts = (x_test$NumOfProducts - mean(train$NumOfProducts))/sd(train$NumOfProducts)
x_test$EstimatedSalary = (x_test$EstimatedSalary - mean(train$EstimatedSalary))/sd(train$EstimatedSalary)
#normalizing training data
train <- train %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))
#run the Random Forest Model
randFr <- randomForest(Exited ~ ., data = train, importance = TRUE)
randFr
# confusion matrix on test data
confusionMatrix(y_test, predict(randFr, newdata = x_test))


