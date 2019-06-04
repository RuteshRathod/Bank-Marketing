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
data = read.csv("bank-additional-full-csv-r.csv", na.strings = "")

# Description
names(data)

# Missing Values Plot
aggr_plot <- aggr(data, col=c('slateblue','red'), numbers=TRUE, prop=FALSE,
                  sortVars=TRUE, labels=names(train), cex.axis=0.7, gap=1,
                  varheight = FALSE,combined = FALSE,cex.numbers =0.5, 
                  ylab=c("Histogram of missing data","Pattern"))

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

# EXPLORATORY DATA ANALYSIS
theme_set(theme_fivethirtyeight())
barfill <- "#4271AE"
barlines <- "#1F3552"
# Age variable
p1 <- ggplot(data, aes(x = age)) +
  geom_histogram(aes(fill = ..count..), binwidth = 1,
                 colour = barlines, fill = barfill)+
  scale_x_continuous(name = "Age",
                     breaks = seq(15,100,10),
                     limits = c(15,100)) +
  xlab("Age")+ylab("Count")+
  ggtitle("Frequency histogram of Age") +
  theme(plot.title = element_text(hjust = 0.5))
p1
p2 <- ggplot(data, aes(x = y, y =age)) +
  geom_boxplot(fill = "coral2", colour="firebrick4")+
  scale_y_continuous(name = "Age") +
  scale_x_discrete(name="Subscribed") +
  ggtitle("Boxplot of Age by Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p2
# Marital variable
p3 <- ggplot() + geom_bar(aes(y = (..count..), x = marital, fill = y), data = data,
                          stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Marital Status vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p3
# Job variable
p4 <- ggplot() + geom_bar(aes(y = (..count..), x = job, fill = y), data = data,
                          stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Job vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1))
p4
# Education variable
p5 <- ggplot() + geom_bar(aes(y = (..count..), x = education, fill = y), data = data,
                          stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Education vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1))
p5
# Contact variable
p6 <- ggplot() + geom_bar(aes(y = (..count..), x = contact, fill = y), data = data,
                          stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Contact vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p6
# Default variable
p7 <- ggplot() + geom_bar(aes(y = (..count..), x = default, fill = y), data = data,
                          stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Default vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
 p7
# Housing variable
p8 <- ggplot() + geom_bar(aes(y = (..count..), x = housing, fill = y), data = data,
                          stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Housing vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p8
# Loan Variable
p9 <- ggplot() + geom_bar(aes(y = (..count..), x = loan, fill = y), data = data,
                          stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Loan vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p9
# Campaign
p11 <- ggplot() + geom_histogram(aes(y = (..count..), x = campaign, fill = y ), data = data,
                          binwidth=5, stat="count") +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  scale_x_continuous(breaks = seq(1,15,1),limits=c(0,15))+
  ggtitle("No. of Contacts vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p11
# Month Vairable
p12 <- ggplot() + geom_bar(aes(y = (..count..), x = month, fill = y), data = data,
                           stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Month vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p12
# Days of Week Vairable
p13 <- ggplot() + geom_bar(aes(y = (..count..), x = day_of_week, fill = y), data = data,
                           stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Day vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p13
# Duration Vairable
p14 <- ggplot() + geom_bar(aes(y = (..count..), x = duration, fill = y), data = data,
                           stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Duration vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p14
# ln_duration <- log(train$duration)
ln_duration <- log(1+(data$duration))
p24 <- ggplot() + geom_histogram(aes(x = ln_duration, fill = y), data = data,
                                 binwidth = 0.1 ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Log Duration vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p24
# pdays Vairable
p16 <- ggplot() + geom_histogram(aes( x = pdays, fill = y), data = data,
                                 binwidth = 50 ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("PCampaign Days vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p16
table(data$pdays)
# Previous
p17 <- ggplot() + geom_histogram(aes( x = previous, fill = y), data = data,
                                 binwidth = 1 ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("PContact vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p17
# POutcome
p18 <- ggplot() + geom_bar(aes(y = (..count..), x = poutcome, fill = y), data = data,
                           stat="count" ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("POutcome vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p18
# Employment variation Rate
p19 <- ggplot() + geom_histogram(aes(x = emp.var.rate, fill = y), data = data,
                                 binwidth = 1 ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Emp.var.rate vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p19
# Consumer Price Index
p20 <- ggplot() + geom_histogram(aes( x = cons.price.idx, fill = y), data = data,
                                 binwidth = 1 ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Cons.Price.Index vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p20
# Consumer Confidence Index
p21 <- ggplot() + geom_histogram(aes( x = cons.conf.idx, fill = y), data = data,
                                 binwidth =5 ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Cons.Conf.Index vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p21
# Euro Interbank Offered Rates
p22 <- ggplot() + geom_histogram(aes( x = euribor3m, fill = y), data = data,
                                 binwidth =0.01 ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("Euro.Interbank.Rate vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p22
# No. of Employees
p23 <- ggplot() + geom_histogram(aes( x = nr.employed, fill = y), data = data,
                                 binwidth =100 ) +
  labs(fill="Subscription") +
  scale_y_continuous(name = "Count")+
  ggtitle("No. of Employees vs Subscription") +
  theme(plot.title = element_text(hjust = 0.5))
p23

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
# Note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). 
#Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. 
#Thus, this input should only be included for benchmark purposes and should be discarded if the intention is 
#to have a realistic predictive model.
data <- subset(data, select = -c(duration))
#3
# Remove pdays because it has constant value
data <- subset(data, select = -c(pdays))
# PRE-PROCESSING
# Split the training data into train and validation set
set.seed(123)
i <- createDataPartition(data$y, p = 3/4, list = FALSE)
new_data_pre <- data[i,]
test <- data[-i,]
# Deal with Imbalanced Data
# SMOTE Technique
new_data <-SMOTE(y~., new_data_pre, perc.over = 400, perc.under = 150, k=5)
data_smote <- SMOTE(y~., data, perc.over = 400, perc.under = 150, k=5)
# Check the proportion of classes before and after SMOTE
prop.table(table(new_data_pre$y)) # No-88.73%, Yes-11.26%
prop.table(table(new_data$y)) # No-54.54%, Yes-45.45%
write.csv(new_data, file="bank-additional-full-csv-r-processed-train.csv", row.names=FALSE)
write.csv(test, file="bank-additional-full-csv-r-processed-test.csv", row.names=FALSE)