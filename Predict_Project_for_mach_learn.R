#  I could not find the detailed instructions for this assignment when I went to complete it
#  I stumbleed upon them earlier in the course and completed assignment from memory.
# Loading Libraries
library(caret)
library(pgmm)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)


# I downloaded the CSV files into a directory I set as my working directory
# Next I load the testing data set and training data sets into two corresponding data frames
# I use the na.strings argument to help prepare the data for cleansing by setting the indicated values as NA
setwd("C:/Users/Tom/Desktop/Tom_2016/Coursera/Machine_Learning_John_Hop/Predict_Project")
df_pml_train <- read.csv("pml-training.csv",stringsAsFactors=FALSE,na.strings=c("NA", "#DIV/0!", ""))
df_pml_test <- read.csv("pml-testing.csv",stringsAsFactors=FALSE,na.strings=c("NA", "#DIV/0!", ""))

# Data cleansing
# 1. I remove columns not relevant to the analysis e.g. time series data by subsetting
# df_pml_train and storing it into a new data frame df_temp
# 2. I then remove the columns that have NA data by identifying the valid columns and creating a new data frame df_train for further processing (partitioning)
df_temp <- df_pml_train[,-c(1:7)]
valid_cols <- sapply(1:ncol(df_temp),function(x) (0==sum(is.na(df_temp[,x])) ))
df_train <- df_temp[, valid_cols]
df_train$classe = factor(df_train$classe)

# perfrom same cleansing on test
df_temp <- df_pml_test[,-c(1:7)]
valid_cols <- sapply(1:ncol(df_temp),function(x) (0==sum(is.na(df_temp[,x])) ))
df_test <- df_temp[, valid_cols]

dim(df_train)
dim(df_test)

# df_test is reserved to determine model error and validate the model.
# I create model building training and test sets labelled mod_train and mod_test from df_train
# 60% of the sample is used for training with 40% reserved for testing
set.seed(123)
inTrain=createDataPartition(y=df_train$classe, p=0.6, list=FALSE)
mod_train <-df_train[inTrain,]
mod_test <- df_train[-inTrain,]

# I use the classification tree as the first model to evaluate for prediction of exercise perfromance class
# I predict the classe of the mod_test data set
# I do not expect this to be highly accurate, but good to validate assumption

rpart_modFit <- train(classe ~ ., method = "rpart", data = mod_train)
fancyRpartPlot(rpart_modFit$finalModel)

pred_rpart <- predict(rpart_modFit, newdata = mod_test)
con_mat <- confusionMatrix(pred_rpart, mod_test$classe)
con_mat

# The accuracy of 0.493 is very poor since .5 is equal to no purity where 0 equals perfect purity.
# Next I will try the Random Forest model since it is often very accurate with the tradeoff
# of interpretability and potential overfitting.

set.seed(123)
#fit the model based on our training dat
rf_modfit <- randomForest(classe~., data=mod_train, method='class')
# using the model fit, run the prediction on the test data
pred_rf <- predict(rf_modfit, newdata=mod_test, type='class')
con_mat_rf <- confusionMatrix(pred_rf, mod_test$classe)
con_mat_rf
# I choose the random forest model due to its .9929 or 99% accuracy
# Unilateral Dumbbell Biceps Curl : performance class by training data
qplot(roll_belt,pitch_belt, colour=classe, data=mod_train)
qplot(roll_belt,yaw_belt, colour=classe, data=mod_train)
qplot(pitch_belt,yaw_belt, colour=classe, data=mod_train)

# run the model on the test data
final_test_pred <- predict(rf_modfit,df_test,type='class')
# print results
final_test_pred
# create files
for (i in 1:length(final_test_pred)){
  filename =  paste0("problem_id",i,".txt")
  write.table(final_test_pred[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}


