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
library(h2o)

# Import dataset
train = read.csv("bank-additional-full-csv-r-processed-train.csv", na.strings = "")
# Launch H2O cluster
localH2O <- h2o.init(nthreads = -1)
# Check status
h2o.init()
# Data to h2o cluster
train.h2o <- as.h2o(train)
# Check column index number
colnames(train.h2o)
# Dependent variable
y.dep <- 18
# Independent variable
x.indep <- c(1:17)
# Random Forest
rf_model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame=train.h2o, ntrees=1000)
# Summary
summary(rf_model)
# Variable Importance
h2o.varimp_plot(rf_model)