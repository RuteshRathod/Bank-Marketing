# Improving Prediction of Potential Clients for Bank Term Deposits using Machine Learning Approaches

## Overview
The data is related with direct marketing campaigns of a **Portuguese banking institution**. The marketing campaigns were based on phone calls to offer **term deposit subscriptions** to their customers.
<br><br>
In this project, we use various machine learning approaches to predict **whether or not the client will subscribe to a term deposit**. Moreover, we also identify the **factors influencing the decision of the clients**.

## Data
**Source:** https://archive.ics.uci.edu/ml/datasets/Bank+Marketing

## Data Cleaning
The originally semi-colon separated dataset is read properly using “;” as separator and then converted into comma separated dataset. The string “unknown” is replaced with empty cell in MS Excel.

## Feature Engineering
**Exploratory Data Analysis** is done on the dataset by checking for missing values in features and generating numerous plots between different pairs of variables. **Feature Selection** is done to select the best variables for prediction. Synthetic Minority Oversampling Technique (**SMOTE**) is used to overcome class imbalance.

## Result
|          | Logistic  Regression | Random Forest | Support Vector Machine | XGBoost |
|:--------:|:--------------------:|:-------------:|:----------------------:|:-------:|
| **AUC**      |        0.6162        |     0.6989    |         0.7182         |  **0.7368** |
| **F1 Score** |        0.9295        |     0.9306    |         0.9206         |  0.9291 |
| **Accuracy** |        0.8726        |     0.8771    |         0.8610         |  0.8351 |

**XGBoost** gives better performance than all other methods as is shown with AUC.