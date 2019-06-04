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
# Random Forest
# Model Execution
rf_model<-randomForest(y ~., data=train, importance=TRUE, ntree=1000)
# Summary Statistics
summary(rf_model)
# Variable Importance
varImpPlot(rf_model) # Better generated using h2o
# Make levels equal
levels(test$default) <- levels(train$default)
# Prediction
pred_rf <- predict(rf_model, test, type="prob")
pred_rf <- pred_rf[,2]
# Create prediction and performance parameter for tuning
pred <- prediction(pred_rf, test$y)
perf <- ROCR::performance(pred, measure="tpr", x.measure="fpr")
plot(perf)
# Choosing the optimum cutoff value 
# 1-Based on Cost function
cost.perf = ROCR::performance(pred, "cost", cost.fp=1, cost.fn=2.7) # Type II error has given more cost because it is not affordable in our case
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
# 2-Based on ROC Curve
plot(perf)
# Optimum Cutoff Value
pred_rf_res <- ifelse(pred_rf>0.403, 2, 1)
# Confusion Matrix
table(pred_rf_res, test$y)
# F1-score = 0.9306, Test Accuracy = 0.8771
# ROC - Curve
pred_res <- prediction(pred_rf_res, test$y)
perf_res <- ROCR::performance(pred_res, measure="tpr", x.measure="fpr")
plot(perf_res)
# AUC - Metric
auc <- ROCR::performance(pred_res, measure="auc")
auc <- auc@y.values[[1]]
auc # 0.6989