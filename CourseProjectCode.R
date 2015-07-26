setwd("C:/Users/julie/Desktop/Data Science Coursera/PML/CourseProject")
training <- read.csv("pml-training.csv", na.strings=c("NA","","#DIV/0!"))
testing <- read.csv("pml-testing.csv", na.strings=c("NA","","#DIV/0!"))

str(training)
names(training)


## remove columns that have lots of NAs
na_count <- sapply(training, function(y) sum(length(which(is.na(y)))))
training2 <- training[,na_count<0.9]

## remove other columns that don't seem to matter
dropvars <- c("X","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp")
training2 <- select(training2, -one_of(dropvars))

## Partition the data set
set.seed(333)
trainIndex <- createDataPartition(training2, p=0.6, list=FALSE)
trainSet <- training2[trainIndex,]
testSet <- training2[-trainIndex,]
trControl <- trainControl(method="cv", number=3)

model.1 <- train(classe, data=trainSet, method="rf", trControl = trControl)

