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
data = read.csv("bank-additional-csv-r.csv", na.strings = "")

# Description
names(data)

# DATA CLEANING
# Check for missing values
sum(is.na(data))
sapply(data, FUN=function(x) sum(is.na(x))) # Lots of missing values

# Impute the missing values
# Method - MICE (Multivariate Imputation by Chained Equation) - 'mice' package
init = mice(data, maxit = 0)
predM = init$predictorMatrix # Predictor Matrix
meth = init$method # Set methods for each variable
imp = mice(data, m=5, predictorMatrix = predM, method = meth)
data = complete(imp) # Combine the imputed data with the original data

# FEATURE ENGINEERING
# Feature Binning - Age Variable
data$age <- cut(data$age, c(0,19,35,60,100), labels = c("Teens","Young Adults", "Adults", "Senior Citizens"))
# Feature Selection
#1
# Check the predictor variables that are highly correlated with each other
# Metrics - Correlation factor with VIF
# Correlation factor
bank_cor <- subset(data, select=-c(y))
for(i in 1:ncol(bank_cor)){bank_cor[,i]<- as.integer(bank_cor[,i])} #Change the variables into int
correlationMatrix <- cor(bank_cor) # Correlation matrix
# Find attributes that are highly correlated (>0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose = TRUE)
print(highlyCorrelated) # Result -> 'euribor3m' and 'emp.var.rate'
# VIF factor
# Define a custom VIF Function
vif_func<-function(in_frame, thresh=10, trace=T, ...)
{
  library(fmsb)
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  # Get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names)
  {
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  if(vif_max < thresh)
  {
    if(trace==T)
    {
      # Print output of each iteration
      prmatrix(vif_init, collab=c('var', 'vif'), rowlab=rep('', nrow(vif_init)), quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh, ', max VIF ', round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else
  {
    in_dat<-in_frame
    # Backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh)
    {
      vif_vals<-NULL
      var_names <- names(in_dat)
      for(val in var_names)
      {
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      vif_max<-as.numeric(vif_vals[max_row,2])
      if(vif_max<thresh) break
      if(trace==T)
      {
        # Print output of each iteration
        prmatrix(vif_vals,collab=c('var', 'vif'), rowlab=rep('', nrow(vif_vals)), quote=F)
        cat('\n')
        cat('removed: ', vif_vals[max_row,1], vif_max, '\n\n')
        flush.console()
      }
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
    }
    return(names(in_dat))
  }
}
# Check the variables that have VIF value greater than 10
vif_func(in_frame=bank_cor, thresh=10, trace=T) # Result - 'euribor3m'
# From the above 2 metrics - it is decided to remove the 'euribor3m' variable
data <- subset(data, select = -c(euribor3m))
#2
# Note found in Dataset Description
# Note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.
data <- subset(data, select = -c(duration))
#3
# Remove pdays because it has constant value
data <- subset(data, select = -c(pdays))
# PRE-PROCESSING
# Split the training data into train and validation set
set.seed(123)
i <- createDataPartition(data$y, p=3/4, list=FALSE)
new_data_pre <- data[i,]
test <- data[-i,]
# Deal with Imbalanced Data
# SMOTE Technique
new_data <-SMOTE(y~., new_data_pre, perc.over=400, perc.under=150, k=5)
data_smote <- SMOTE(y~., data, perc.over=400, perc.under=150, k=5)
# Check the proportion of classes before and after SMOTE
prop.table(table(new_data_pre$y)) # No-89.03%, Yes-10.97%
prop.table(table(new_data$y)) # No-54.54%, Yes-45.45%

# MODELLING
# Support Vector Machines (SVM)
# Model Execution
svm_model <- svm(y~., data=new_data, kernel="radial",  probability=TRUE)
# Tuning 
svm_tune <- tune.svm(y~., data = new_data, gamma = 10^(-2:2), cost= 1)
# Model with tuned parameters
svm_model <- svm(y~., data=new_data, kernel="radial",  probability=TRUE, cost= 1, gamma=0.02)
# Summary
summary(svm_model)
#Prediction
pred_svm<- predict(svm_model, test, probability = TRUE)
names(pred_svm) <- c("probabilities")
pred_svm_prob <- attr(pred_svm, "probabilities")
pred_svm_prob <- pred_svm_prob[,2]
# Create prediction and performance parameter for tuning
pred <- prediction(pred_svm_prob, test$y)
perf <- ROCR::performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
# Choose the optimum cutoff value 
#1-Based on Cost function
cost.perf = ROCR::performance(pred, "cost", cost.fp = 1 , cost.fn = 4) #Type II error has given more cost because it is not affordable in our case
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
#2-Based on ROC Curve
plot(perf)
# Optimum Cutoff Value
pred_svm_res <- ifelse(pred_svm_prob > 0.5155,2,1)
# Confusion Matrix
table(pred_svm_res, test$y)
# F1-score = 0.9206, Test Accuracy = 0.8610
# ROC - Curve
pred_res <- prediction(pred_svm_res, test$y)
perf_res <- ROCR::performance(pred_res, measure = "tpr", x.measure = "fpr")
plot(perf_res)
# AUC - Metric
auc <- ROCR::performance(pred_res, measure = "auc")
auc <- auc@y.values[[1]]
auc # 0.7182