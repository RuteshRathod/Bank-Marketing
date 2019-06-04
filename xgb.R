getwd() # Checking the working directory
setwd("D:/Bank/Code") # Setting the working directory

# Import libraries
library(car)
library(caret)
library(corrplot)
library(data.table)
library(DMwR)
library(e1071)
library(fmsb)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(mice)
library(mlr)
library(moments)
library(parallel)
library(parallelMap)
library(psych)
library(randomForest)
library(ROCR)
library(VIM)
library(xgboost)

# Import dataset
data = read.csv("bank-additional-full-csv-r.csv", na.strings = "", stringsAsFactors=F)

# DATA PRE-PROCESSING AND FEATURE ENGINEERING
# Feature binning - Age variable
data$age <- cut(data$age, c(0,19,35,60,100), labels = c("Teens","Young Adults", "Adults", "Senior Citizens"))
# Remove features
data <- subset(data, select = -c(euribor3m)) # To remove severe multi-colinearity
data <- subset(data, select = -c(duration)) # To make the model realistic
data <- subset(data, select = -c(pdays)) # Constant value
# Split the training data into train and validation set
set.seed(123)
i <- createDataPartition(data$y, p = 3/4, list = FALSE)
train <- data[i,]
test <- data[-i,]
# Convert data frame to data table
setDT(train) 
setDT(test)
# Set all missing value as "Missing" 
train[is.na(train)] <- "Missing" 
test[is.na(test)] <- "Missing"
# Using one hot encoding 
labels <- train$y
ts_label <- test$y
new_tr <- model.matrix(~.+0,data = train[,-c("y"),with=F]) 
new_ts <- model.matrix(~.+0,data = test[,-c("y"),with=F])
# Convert factor to numeric 
labels <- as.numeric(as.factor(labels))-1
ts_label <- as.numeric(as.factor(ts_label))-1
# Prepare matrix 
dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
# Default parameters
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, 
			min_child_weight=1, subsample=1, colsample_bytree=1)
# Find best iteration using cross-validation
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 200, nfold = 5, showsd = T, stratified = T, 
			print.every.n = 10, early.stop.round = 20, maximize = F)
# Best Iteration = 4
# First default - model training
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 4, watchlist = list(val=dtest,train=dtrain), 
			print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")
# Convert data table into data frame
setDF(train) 
setDF(test)
# Create tasks
traintask <- makeClassifTask(data = train, target = "y")
testtask <- makeClassifTask(data = test, target = "y")
# Do one hot encoding
traintask <- createDummyFeatures(obj = traintask) 
testtask <- createDummyFeatures(obj = testtask)
# Create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.1)
# Set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")),
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1),
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
# Set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
# Search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)
# Set parallel backend
parallelStartSocket(cpus = detectCores())
# Parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, 
			par.set = params, control = ctrl, show.info = T)
mytune$y
mytune$x
# Set the Best Optimum Parameters obtained from the tuning
# Result
# best_params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1, gamma=0,
#                    max_depth=4, min_child_weight=3.08, subsample=0.83, colsample_bytree=0.555)