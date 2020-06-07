---
title: "Persons' Lifting Activity Modelling for Good Health Sustainability and Control"
author: "NIYONZIMA Felix"
date: "June 07, 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(caret)
library(rpart)
library(randomForest)
```

**1. Introduction** \
\
This study seeks to establish a classification model which will be used to predict the manner in which a given  person carries out a lifting activity. The data have been taken from 6 persons who were asked to lift a barbell to as to study how different they perform this lifting activity.
The dependent variable is the "classe" variable in the [training data set](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv) and the predictors are all remaining variables in the same data. We will final use the model to make a prediction on 20 new observations availabe in the [test data set](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv).

```{r loadData, echo=FALSE, message=FALSE}
Testing = read.csv("E:/Coursera/Prac. Machine Learning/Project/pml-testing.csv", header = TRUE)
Training = read.csv("E:/Coursera/Prac. Machine Learning/Project/pml-training.csv", header = TRUE)

```

**2. Exploratory Data Analysis**\
\
Here below are some basic information about the training and the test data sets. 

```{r dataSplit}
# some colunms and their first few observations in the training set.
head(Training[1:5, 18:22])

# dimension of the training and test data sets
dim(Training)
dim(Testing)
```

The test set has 20 observations and 160 variables while the training set has 19622 observations and the same number of variables as the test data. It is seen from the training set that in both the training and test sets, some variables  have almost all observations with missing values and other with empties, thus the data need to be cleaned.\
\
This problem is a multi-class since the dependent variable is a factor variable of five levels as shown below.

```{r dimension}
levels(Training$classe)
```


**3. Data Cleaning**

```{r Cleaning}
# remove all columns with NA (missing values) and emplty values in both test and train data sets
maxNAperC = 20
maxNAcount = nrow(Training)/100*maxNAperC
NAcol = which(colSums(is.na(Training) | Training =="") > maxNAcount)
TrainingCleaned = Training[,-NAcol]
TestingCleaned = Testing[,-NAcol]

# remove from the train data; the "cvtd_timestamp" variable since it contains time observations and they are not needed, the "X" variable since it looks like having no other meaning than data rows numbering and the "user_name" variable since it plays a role only in data collection.

TrainingCleaned2 = TrainingCleaned %>% dplyr::select(-c("cvtd_timestamp", "X", "user_name"))

# remove from the test data, the "cvtd_timestamp", the "X", the "user_name" and the "problem_id" variables since the later is not part of the predictor variables. 
TestingCleaned2 = TestingCleaned %>% dplyr::select(-c("cvtd_timestamp", "X", "user_name", "problem_id"))

# show some basic characteristics of the predictors: this is a part of exploratory data analysis
summary(TrainingCleaned2)[1:5, 4:7]
```
We see that some predictors are almost normally and others non-normally distributed.\
\
**4. Model Building** \
\
It is hard to precise which model will be good for this classification problem; therefore, we will train three different models and proceed to choosing the most accurate one. The first is a linear model, the Linear Discriminant Analysis (LDA); the second is a non linear and non parametric model, the Classification and Regression Trees (CART); the last is the advanced model, the Random Forest (RF). For each model we will use the "Accuracy" as a model performance metric and we will use the same metric for selecting a good model. 5-fold crossvalidation will be applied to estimate the accuracy and to reduce the likelihood of the overfitting.We expect a good result since the we have seen that some predictors are almost normally and others non-normally distributed.


```{r modelling}
# setting a crossvalidation
control <- trainControl(method="cv", number=5)
# train LDA model
set.seed(123)
fit_lda <- train(classe~., data = TrainingCleaned2, method = "lda", metric = "Accuracy", trControl=control, tuneLength = 8)

# train CART model
set.seed(123)
fit_cart <- train(classe~., data = TrainingCleaned2, method = "rpart", metric = "Accuracy", trControl=control, tuneLength = 8)

# train RF model
set.seed(123)
fit_rf <- train(classe~., data = TrainingCleaned2, method = "rf", metric = "Accuracy", trControl=control, tuneLength = 8)

```

**5. Selecting the best model**\
\
We need to compare the above three models one against another and select the most accurate.

```{r modelsAccuracy}
results <- resamples(list(lda = fit_lda, cart = fit_cart, rf = fit_rf))
summary(results)

```
We see that the RF model has the largest and excellent accuracy (99%), and it is therefore the best model to use for this data. Even though such accurracy is obtained with the same data as used to train the model, we expect a very good outer sample accuracy, almost not less than 95%, implying the out of sample error less than or equals 5%, since the model was trained with a minimization of overfitting. \
Here below is a summary of the model.
 
```{r modelSummary}
print(fit_rf)

```
 
 
**6. Prediction**\
\
In this section we apply the model on unseen data so as to get a good insight about its perfomance. 
```{r prediction}
pred = predict(fit_rf, newdata = TestingCleaned2)
pred


```

**7. Conclusion**\
\
We have tried to build a model classification to identify the various ways people perform a lifting exercise. We found that the random forest model is the most appropriate with the accuracy of 99% and 95% confidence interval for the considered data.  
