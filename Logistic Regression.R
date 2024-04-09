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
bank_churn$Gender <- factor(bank_churn$Gender)
bank_churn$Geography <- factor(bank_churn$Geography)
bank_churn$HasCrCard <- factor(bank_churn$HasCrCard, levels = c(0, 1), labels = c("no", "yes"))
bank_churn$IsActiveMember <- factor(bank_churn$IsActiveMember, levels = c(0, 1), labels = c("no", "yes"))
bank_churn$Exited <- factor(bank_churn$Exited, levels = c(0, 1), labels = c("no", "yes"))

#printing the summary
summary(bank_churn)

#separating data into training and test data
train_data <- bank_churn %>% sample_frac(0.7)
test_data <- anti_join(bank_churn, train_data)

#seperating explanatory and response variables 
x_train_data <- select(train_data, - Exited)
y_train_data <- train_data$Exited
x_train_encoded <- model.matrix(~., x_train_data) #to make the x for glmnet
y_test_data <- test_data$Exited
x_test_data <- test_data %>% select(-Exited)

#scaling test data with respect to training data
test_transform <- preProcess(train_data)
test_data <- predict(test_transform, test_data)
#scaling training data
train_data <- train_data %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))

#run logistic regressions and find the threshold that gives max Area Uder the Curve and minimal
# classification error
log_auc <- cv.glmnet(x_train_encoded, y_train_data, family = "binomial", type.measure = "auc")
log_misclassify <- cv.glmnet(x_train_encoded, y_train_data, family = "binomial", type.measure = "class")

# Plotting values for AUC vs log of lambda 
plot(log_auc)

#plotting ROC curve 
log_lm2 <- glm2(Exited ~., data = train_data, family = "binomial")
pred <- predict(log_lm2, x_test_data, type = "response")
ggroc(roc(y_test_data, pred))
