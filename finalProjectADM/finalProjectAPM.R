######  Final Project APM

library(dplyr)
library(caret)
set.seed(975)

###### Load Data
data <- read.csv("HGUAnalysisPRJ/HDdata.csv")

###### explore data
str(data)
summary(data)

### Removing NA's
#str(data[!is.na(data$age),])
data_noNAs <- data[!is.na(data$age),]
head(data_noNAs)
summary(data_noNAs)

### setting categorical columns as factors and explored applying log10 to continuous columns.
categorical_columns <- c('sex', 'cp', 'restecg', 'fbs', 'exang', 'num')
# continuous_columns <- c('age','trestbps','chol','thalach')
data_noNAs[,categorical_columns] <- lapply(data_noNAs[,categorical_columns],factor)
# data_noNAs[,continuous_columns] <- lapply(data_noNAs[,continuous_columns],log10)
str(data_noNAs)
summary(data_noNAs)


######  Choose an appropriate model type
###  Considering the response is categorical, I will use the logistic regression model type.

###### Divide your data into a training and test set.
split_pct = 0.7

trainingRows <- createDataPartition(data_noNAs$num, p = split_pct,list= FALSE)
train_data <- data_noNAs[trainingRows,]
test_data <- data_noNAs[-trainingRows,]

### explore the resulting data sets
str(train_data)
summary(train_data)
str(test_data)
summary(test_data)

###### In your data are there:
######  a. NAs?
###           Yes in the age feature, I will remove those observations with missing age since they are only 10 out of 303 observations.  (completed before splitting into training/test sets)
sum(is.na(data$age))

######  b. categorical values?
###        Yes. They are: sex,cp,restecg,fbs,exang,num
head(select(data,sex,cp,restecg,fbs,exang,num))
######  c. binary values?
###        Yes. They are: sex,num,fbs,exang
head(select(data,sex,num,fbs,exang))
######  d. numeric (real number) values?
###        Yes. They are: age,trestbps,chol,thalach,oldpeak
head(select(data,age,trestbps,chol,thalach,oldpeak))

###### Perform any needed preprocessing of your data, but no need to enforce a linear relationship between your numeric features (e.g. with a spline).
### categorical features were converted to factors earlier.


###### Fit a linear or logistic regression model on your training set.
# features_used <- names(data_noNAs)[1:10]
# model <- glm(num ~.-age-chol-fbs-restecg,family=binomial,data=train_data)
model <- glm(num ~.,family=binomial,data=train_data)

summary(model)
###### Use your test set to assess the accuracy of your model.

###### a. Choose a prediction threshold.
pred_thres <- .5  ## Selected .5 after manually comparing different values and looking at the resulting confusion matrix.

train_predict <- predict(model,type = "response")
predicted_num <- factor(train_predict >= pred_thres)
#head(predicted_num)
###### b. Construct a confusion matrix.
###### c. Calculate the accuracy, PPV, NPV, Sensitivity and Specificity.
confusionMatrix(predicted_num,reference = factor(train_data$num == 1))

###  Test prediction performance
test_predict <- predict(model,newdata = select(test_data,-num),type = "response")
test_predicted_num <- factor(test_predict >= pred_thres)
head(test_predicted_num)
###### b. Construct a confusion matrix.
###### c. Calculate the accuracy, PPV, NPV, Sensitivity and Specificity.
confusionMatrix(test_predicted_num,reference = factor(test_data$num == 1))

### When comparing the training/test confusion matrix, the test performance metrics are close to the training performance metrics.  For example, accuracy-train : .8204 and accuracy-test: .7931.  Also, the PPV-train : 0.8136 and the PPV-test : 0.7843.

### If we remove some of the features and create a new model, we can get minimal improvement on performance metrics while keeping the model simpler.

####################
### exploring roc
# library(pROC)
# rocCurve <-  roc(response = predicted_num,predictor = train_data$num,levels = rev(levels( predicted_num)))
# auc(rocCurve)
# ci(rocCurve)
# plot(rocCurve, legacy.axes = TRUE)


