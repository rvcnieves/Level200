# CodeBook

Most of the description of what was completed for this project is included in the r_code.  However, here is a brief description of the variables.

. X_train
. y_train

These two are the train data sets (X->features, y->activity)

. X_test
. y_test

These two are the test data sets


features_names <- includes names for each column(feature) 

. X_TrainTest 
. y_TrainTest

Combined Trained and Test data sets

index_sel_feats <- Index with the selected features as they match with the requirements.


X_TrainTest_selected_feats <- X train and test with only those features we selected 

activity_labels <- id and labels for activities

Y_wActivity_TrainTest <- combined (train/test) Y with activity name

X_y_TrainTest <- X and y combined

. sub_train
. sub_test

These are the subjects train/test sets.

sub_TrainTest <- subject train/test data combined

X_y_sub_TrainTest <- X,y,subject combined

X_y_sub_TrainTest.melted <- melted dataset. (refer to: https://www.statmethods.net/management/reshape.html)

X_y_sub_TrainTest.mean <- calculated mean for each subject/activity
