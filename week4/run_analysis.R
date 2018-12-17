library(dplyr)
library(tidyverse)
### set working directory.  This might need to be updated based on your computer system.
setwd('~/Downloads/UCI HAR Dataset/r_code')

##### 1. Merges the training and the test sets to create one data set.

###   Load X,y train/test data
X_train <- read.table("../train/X_train.txt")
y_train <- read.table("../train/y_train.txt")

X_test <- read.table("../test/X_test.txt")
y_test <- read.table("../test/y_test.txt")

features_names <- read.table("../features.txt")
str(features_names)
features_names$V2
colnames(X_train) <- features_names$V2
colnames(X_test) <- features_names$V2

###  Look data loaded
str(X_train)
str(y_train)

str(X_test)
str(y_test)

dim(X_train)
dim(y_train)

dim(X_test)
dim(y_test)

### add category column to each subset so that we can use it to know what type of data we have on each observation after combining them.
# X_train$data_type <- "train"
# X_test$data_type <- "test"

### Combine Training and Test data. I'm combining X and y independently, if they need to be used together will combine at that moment.
library(data.table)
X_TrainTest <- rbindlist(list(X_train,X_test))
y_TrainTest <- rbindlist(list(y_train,y_test))
dim(X_TrainTest)
dim(y_TrainTest)

##### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

### select columns (features) that include only the ones that have mean, Mean, std, or Std in them.

### getting the indexes of those features that meet our requirements.
index_sel_feats <- as.array(grep("[Mm]ean|[Ss]td",colnames(X_TrainTest)))

### checking dimensions
dim(index_sel_feats)

### creating new variable with only those features that meet requirements.
X_TrainTest_selected_feats <- X_TrainTest[,index_sel_feats, with =FALSE]

### looking at the new variable with the data we want.
str(X_TrainTest_selected_feats)
dim(X_TrainTest_selected_feats)

###### 3. Uses descriptive activity names to name the activities in the data set

### load activity labels and change columns labels
activity_labels <- read.table("../activity_labels.txt")
colnames(activity_labels) <- c("activity_id","activity_name")
activity_labels

###### 4. Appropriately labels the data set with descriptive variable names.
###  create a merged variable that includes the activity labels on y set.
Y_wActivity_TrainTest <- merge(y_TrainTest,activity_labels,by.x = "V1",by.y = "activity_id")
colnames(Y_wActivity_TrainTest) <- c("activity_id","activity_name")
Y_wActivity_TrainTest

###### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### combining X and y:

cbind(list())
X_y_TrainTest <- cbind(X_TrainTest_selected_feats,Y_wActivity_TrainTest)
X_y_TrainTest
dim(X_y_TrainTest)

###  load subject data

sub_train <- read.table("../train/subject_train.txt")
sub_test <- read.table("../test/subject_test.txt")
dim(sub_train)
dim(sub_test)

### add category (train/test) on subject data

# sub_train$data_type <- "train"
# sub_test$data_type <- "test"

### Combine Training and Test subject(sub) data.

sub_TrainTest <- rbindlist(list(sub_train,sub_test))
# colnames(sub_TrainTest) <- c("subject_id","data_type")
colnames(sub_TrainTest) <- c("subject_id")
dim(sub_TrainTest)
sub_TrainTest

#### combine subject data to the other data frame

X_y_sub_TrainTest <- cbind(X_y_TrainTest,sub_TrainTest)
str(X_y_sub_TrainTest$activity_name)

X_y_sub_TrainTest$activity_name <- factor(X_y_sub_TrainTest$activity_name,levels=X_y_sub_TrainTest$activity_id,labels=X_y_sub_TrainTest$activity_name)
#### reshape data using melt

X_y_sub_TrainTest.melted <- melt(X_y_sub_TrainTest,id = c("subject_id","activity_name"))

X_y_sub_TrainTest.melted

#### calculate the means.
X_y_sub_TrainTest.mean <- dcast(X_y_sub_TrainTest.melted,subject_id+activity_name ~ variable,mean)
head(X_y_sub_TrainTest.mean)
write.table(X_y_sub_TrainTest.mean,"5_dataset.txt",row.name=FALSE)
