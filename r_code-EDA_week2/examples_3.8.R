### Examples 3.8

## find functions for creating a confusion matrix within the currently loaded packages
apropos("confusion")

#find function in any package
RSiteSearch("confusion", restrict = "functions")

# install library and load it
install.packages('AppliedPredictiveModeling')
library(AppliedPredictiveModeling)

# load dasta
data(segmentationOriginal)
# set train data
segData <- subset(segmentationOriginal, Case == "Train")

# get columns in their own cells
cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
# Now remove the columns
segData <- segData[, -(1:3)]

# remove several “status” columns which were binary versions of the predictors 
statusColNum <- grep("Status", names(segData))
statusColNum

segData <- segData[, -statusColNum]

# analyzing skewness
install.packages('e1071')
library(e1071)
# For one predictor:
skewness(segData$AngleCh1)

# check skewness on all columns
skewValues <- apply(segData, 2, skewness)
head(skewValues)

# using caret library for transformations
install.packages('caret')
library(caret)
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans

# before transformation
head(segData$AreaCh1)
# after transformation
predict(Ch1AreaTrans, head(segData$AreaCh1))
# check 1st column manually
(819^(-.9) - 1)/(-.9)

# set PCA object
pcaObject <- prcomp(segData, center = TRUE, scale. = TRUE)

# Calculate the cumulative percentage of variance which each component accounts for.
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3]

head(pcaObject$x[, 1:5])

# using rotation:  which stores the variable loadings, where rows correspond to predictor variables and columns are associated with the components
head(pcaObject$rotation[, 1:3])

# transforming using the parameters set on method
trans <- preProcess(segData, method = c("BoxCox", "center", "scale", "pca"))
trans

# Apply the transformations:
transformed <- predict(trans, segData)
# These values are different than the previous PCA components since they were transformed prior to PCA
head(transformed[, 1:5])

# the column numbers of any predictors that fulfill the conditions and help us find problematic predictors.
nearZeroVar(segData)

#calculate the correlations between predictor variables
correlations <- cor(segData)
dim(correlations)

correlations[1:4, 1:4]

# look at the correlation plot
install.packages('corrplot')
library(corrplot)
corrplot(correlations, order = "hclust")

## get the carsubset data
data(cars)
type <- c("convertible", "coupe", "hatchback", "sedan", "wagon")
cars$Type <- factor(apply(cars[, 14:18], 1, function(x) type[which(x == 1)]))
 
carSubset <- cars[sample(1:nrow(cars), 20), c(1, 2, 19)]

head(carSubset)

# check type levels
levels(carSubset$Type)

simpleMod <- dummyVars(~Mileage + Type, data = carSubset, levelsOnly = TRUE)
simpleMod

#generate the dummy variable
predict(simpleMod, head(carSubset))

# add more predictors to reflect interactions
withInteraction <- dummyVars(~Mileage + Type + Mileage:Type, data = carSubset,levelsOnly = TRUE)
withInteraction

predict(withInteraction, head(carSubset))
