getwd()
setwd("D:/Bank/Code")
# Import libraries
library(mice)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(moments)
library(car)
library(fmsb)
library(corrplot)
library(psych)
library(DMwR)
library(caret)
library(ROCR)
library(randomForest)
library(e1071)
library(data.table)
library(mlr)
library(xgboost)
library(parallel)
library(parallelMap)
library(VIM)

# Import dataset
train = read.csv("bank-additional-full-csv-r-processed-train.csv", na.strings = "")
test = read.csv("bank-additional-full-csv-r-processed-test.csv", na.strings = "")

# MODELLING
# Logistic Regression
# Model Execution
logit_model <- glm(y ~., family=binomial(link="logit"), data=train)
# Summary Statistics
summary(logit_model)
anova(logit_model, test="Chisq")
# Prediction
pred_log <- predict(logit_model, newdata=test, type='response')
# Create prediction and performance parameter for tuning
pred <- prediction(pred_log, test$y)
perf <- ROCR::performance(pred, measure="tpr", x.measure="fpr")
plot(perf)
# Choose the optimum cutoff value 
# 1-Based on Cost function
cost.perf = ROCR::performance(pred, "cost", cost.fp=1, cost.fn=3.5) #Type II error has given more cost because it is not affordable in our case
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
# 2-Based on ROC Curve
plot(perf)
# Optimum cutoff value
pred_log_res <- ifelse(pred_log>0.7611971, 2, 1)
# Confusion Matrix
table(pred_log_res, test$y)
# F1-score = 0.9295, Test Accuracy = 0.8726
# ROC - Curve
pred_res <- prediction(pred_log_res, test$y)
perf_res <- ROCR::performance(pred_res, measure="tpr", x.measure="fpr")
plot(perf_res)
# AUC - Metric
auc <- ROCR::performance(pred_res, measure="auc")
auc <- auc@y.values[[1]]
auc # 0.6162
# Hosmer-Lemeshow Goodness of Fit (GOF) Test
library(ResourceSelection)
hoslem.test(logit_model$y, fitted(logit_model))