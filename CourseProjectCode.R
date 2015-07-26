library(dplyr)
library(caret)

library(parallel)  
library(doParallel)  
theCluster <- makeCluster(detectCores())
registerDoParallel(theCluster)


setwd("C:/Users/julie/Desktop/Data Science Coursera/PML/CourseProject")
training <- read.csv("pml-training.csv", na.strings=c("NA","","#DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings=c("NA","","#DIV/0!"))

glimpse(training)



## remove columns that have lots of NAs
na_count <- sapply(training, function(y) sum(length(which(is.na(y)))))
training2 <- training[,na_count<0.9]

## remove other columns that don't seem to matter
dropvars <- c("X","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","num_window")
training2 <- select(training2, -one_of(dropvars))

## Partition the data set
set.seed(333)
trainIndex <- createDataPartition(training2$classe, p=0.6, list=FALSE)
trainSet <- training2[trainIndex,]
testSet <- training2[-trainIndex,]

## Tree
library(rpart)
treeModel <- rpart(classe ~ ., data=trainSet, method="class")
library(rpart.plot)
prp(treeModel)

## Random Forest
tCtrl <- trainControl(method="cv", number=5, allowParallel=TRUE)
model.1 <- train(classe~., data=trainSet, method="rf", trControl = tCtrl)
model.1
model.1$finalModel

## Check the Variance Importance against tree
varImp(model.1)

## Get the prediction on the test set, 40% partition of training set
validate <- predict(model.1,testSet)
confusionMatrix(validate,testSet$classe)

## Make the prediction vector
submission <- predict(model.1, testing)

