

library(caret)
library(AppliedPredictiveModeling)
library(lubridate)

df_train <- read.csv(file="C:/Users/Marketelligent/Downloads/pml-training.csv", header=TRUE, sep=",")
df_test <- read.csv(file="C:/Users/Marketelligent/Downloads/pml-testing.csv", header=TRUE, sep=",")

############# preprocessing  ###################


# 1) Removing new_window = yes - this is done because test set has only "no" values
df_train1 <- df_train[df_train$new_window == "no",]
df_test1  <- df_test

# 2) Creating an hour field - exercise pattern could vary by hour of day
df_train1$hour <- as.numeric(substr(df_train1$cvtd_timestamp, 12,13))
df_test1$hour <- as.numeric(substr(df_test1$cvtd_timestamp, 12,13))

# 3) Creating dummy variables for each of the Users

df_train1$adelmo   <- 0
df_train1$carlitos <- 0
df_train1$charles  <- 0
df_train1$eurico   <- 0
df_train1$jeremy   <- 0
df_train1$pedro    <- 0

df_train1$adelmo[df_train1$user_name == "adelmo"]     <- 1
df_train1$carlitos[df_train1$user_name == "carlitos"] <- 1
df_train1$charles[df_train1$user_name == "charles"]   <- 1
df_train1$eurico[df_train1$user_name == "eurico"]     <- 1
df_train1$jeremy[df_train1$user_name == "jeremy"]     <- 1
df_train1$pedro[df_train1$user_name == "pedro"]       <- 1


df_test1$adelmo   <- 0
df_test1$carlitos <- 0
df_test1$charles  <- 0
df_test1$eurico   <- 0
df_test1$jeremy   <- 0
df_test1$pedro    <- 0

df_test1$adelmo[df_test1$user_name == "adelmo"]     <- 1
df_test1$carlitos[df_test1$user_name == "carlitos"] <- 1
df_test1$charles[df_test1$user_name == "charles"]   <- 1
df_test1$eurico[df_test1$user_name == "eurico"]     <- 1
df_test1$jeremy[df_test1$user_name == "jeremy"]     <- 1
df_test1$pedro[df_test1$user_name == "pedro"]       <- 1

# 4) Removing first 5 columns - these are ID, name or date columns
df_train1 <- df_train1[,6:167]
df_test1 <- df_test1[,6:167]


# 5) Removing columns that only have NA values
non_NA <- apply(!is.na(df_train1),2,sum)>0
df_train1  <- df_train1[,non_NA]
df_test1<-df_test1[,non_NA]

# 6) Removing columns that only have blank values

df_test1  <- df_test1[, colSums(df_train1 != "") != 0]
df_train1 <- df_train1[, colSums(df_train1 != "") != 0]


#################### Splitting into training, testing and validation data sets #########################

# This is the original testing data set - ignore for now
test_id      <- df_test1$problem_id
test_feature <- df_test1[,names(df_test1)!="problem_id"]

# This splits the df_train1 data set into training and validation
set.seed(500)
df_train2 <- df_train1[sample(nrow(df_train1)),]

train_rows      <- createDataPartition(y=df_train2$classe,p=0.8,list=FALSE)
training_data   <- df_train2[train_rows,]

validation_data <- df_train2[-train_rows,]

# Factorize the classe column
training_data$classe <- factor(training_data$classe)


################### Building a random forest on the training data set ######################

# Training the model on the train data set, and using k-fold cross validation to measure accuracy
rf_model<-train(classe~.,data=as.data.frame(training_data),method="rf",metric="Accuracy",
                trControl=trainControl(method="cv",number=4), ntree=20,
                tuneLength=10)

# Final validation on the validation data set - check if accuracy of validation set is same as the accuracy of the k-folds
result_validation <- predict(rf_model,validation_data)
table(result_validation,validation_data$classe)

# Display the final model
rf_model$finalModel

################## Run the model on the test data set provided for the project #####################

# Predict the test values and create a new data set with the ID column
result_test <- predict(rf_model,test_feature)
output      <- data.frame(test_id,result_test)

# Write to csv output file
write.csv(output, file = "C:/Users/Marketelligent/Downloads/pml-output.csv")

