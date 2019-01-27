#install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
# install.packages("caret")
# install.packages("klaR")
# install.packages("MASS")
# install.packages("pROC")
library(caret)
library(MASS)
library(klaR)
library(pROC)
set.seed(975)
### train/test data
simulatedTrain <- quadBoundaryFunc(500)
simulatedTest <- quadBoundaryFunc(1000)
head(simulatedTrain)

### Random Forest
#install.packages("randomForest")
library(randomForest)
rfModel <- randomForest(class ~ X1 + X2,data = simulatedTrain,ntree = 2000)
library(MASS) ## for the qda() function
### QDA model
qdaModel <- qda(class ~ X1 + X2, data = simulatedTrain)

### Calculate prediction on training set
qdaTrainPred <- predict(qdaModel, simulatedTrain)
names(qdaTrainPred)

head(qdaTrainPred$class)


head(qdaTrainPred$posterior)

### Predict using test set
qdaTestPred <- predict(qdaModel, simulatedTest)
simulatedTrain$QDAprob <- qdaTrainPred$posterior[,"Class1"]
simulatedTest$QDAprob <- qdaTestPred$posterior[,"Class1"]

###  Predict using random forest model and test set
rfTestPred <- predict(rfModel, simulatedTest, type = "prob")
head(rfTestPred)

simulatedTest$RFprob <- rfTestPred[,"Class1"]
simulatedTest$RFclass <- predict(rfModel, simulatedTest)

# Class 1 will be used as the event of interest
### calculate metrics
#install.packages("caret")
library(caret)
sensitivity(data = factor(simulatedTest$RFclass), reference = simulatedTest$class,positive = "Class1")

specificity(data = factor(simulatedTest$RFclass),reference = factor(simulatedTest$class),negative = "Class2")

### Predictive Values
posPredValue(data = simulatedTest$RFclass,reference = simulatedTest$class,positive = "Class1")

negPredValue(data = simulatedTest$RFclass,reference = simulatedTest$class,positive = "Class2")

# Change the prevalence manually
posPredValue(data = simulatedTest$RFclass,reference = simulatedTest$class,positive = "Class1",prevalence = .9)

### Confusion Matrix
confusionMatrix(data = simulatedTest$RFclass,reference = simulatedTest$class,positive = "Class1")

### Receiver Operationg Characteristics Curves
library(pROC)
rocCurve <-  roc(response = simulatedTest$class,predictor = simulatedTest$RFprob,levels = rev(levels(simulatedTest$class)))
### ^ This function assumes that the second class is the event of interest, so we reverse the labels.

### area under the curve
auc(rocCurve)

### Calculate confidence interval
ci(rocCurve)


# plot roc

plot(rocCurve, legacy.axes = TRUE)

## Lift
labs <- c(RFprob = "Random Forest",QDAprob = "Quadratic Discriminant Analysis")
liftCurve <- lift(class ~ RFprob + QDAprob, data = simulatedTest,labels = labs)
liftCurve

### plot
xyplot(liftCurve,auto.key = list(columns = 2,lines = TRUE,points = FALSE))


### calibrate probabilities
calCurve <- calibration(class ~ RFprob + QDAprob, data = simulatedTest)
calCurve

#plot
xyplot(calCurve, auto.key = list(columns = 2))

sigmoidalCal <- glm(relevel(class, ref = "Class2") ~ QDAprob,data = simulatedTrain,family = binomial)
coef(summary(sigmoidalCal))

### predict with corrected probabilities
sigmoidProbs <- predict(sigmoidalCal,newdata = simulatedTest[,"QDAprob", drop = FALSE],type = "response")
simulatedTest$QDAsigmoid <- sigmoidProbs

### using the Bayesian approach for calibration
BayesCal <- NaiveBayes(class ~ QDAprob, data = simulatedTrain,usekernel = TRUE)

BayesProbs <- predict(BayesCal,newdata = simulatedTest[, "QDAprob", drop = FALSE])
simulatedTest$QDABayes <- BayesProbs$posterior[, "Class1"]
## The probability values before and after calibration
head(simulatedTest[, c(5:6, 8, 9)])

### plot to evaluate probabilities
calCurve2 <- calibration(class ~ QDAprob + QDABayes + QDAsigmoid,data = simulatedTest)
xyplot(calCurve2)

