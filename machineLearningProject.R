#load the libraries required
#set seed for reproducibility

library(Hmisc)
library(caret)
library(randomForest)
library(foreach)
library(doParallel)
set.seed(23)

#download the files to get started

trainUrl <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainFile <- "./pml-training.csv"
testFile <- "./pml-testing.csv"


if (!file.exists(trainFile)) {
  download.file(trainUrl, destfile=trainFile, method="curl")
}
if (!file.exists(testFile)) {
  download.file(testUrl, destfile=testFile, method="curl")
}

#read the files. Standardise blanks or div/0 values to NA. Remove NA from the columns. 

trainRaw <- read.csv(trainFile, na.strings=c("NA","#DIV/0!", ""))
testRaw <- read.csv(testFile , na.strings=c("NA", "#DIV/0!", ""))
trainDS <- trainRaw[,colSums(is.na(trainRaw))==0]
testDS <- testRaw[,colSums(is.na(testRaw))==0]

#check the dimensions of the files
dim(trainDS)
dim(testDS)

#remove the featuresuser_name raw_timestamp_part_1 raw_timestamp_part_2 cvtd_timestamp new_window num_window since they are not related to calculations. 
trainDS <- trainDS[,-c(1:7)]
testDS <- testDS[,-c(1:7)]

#create stratified random sample of data into training and test sets
trainPart <- createDataPartition(trainDS$classe, p = 0.6, list = FALSE)
trainData <- trainDS[trainPart, ]
testData <- trainDS[-trainPart,]

#use random forest and save time with doparallel

registerDoParallel()
classe <- trainData$classe
variables <- trainData[-ncol(trainData)]

rf <- foreach(ntree=rep(250, 4), .combine=randomForest::combine, .packages='randomForest') %dopar% {
  randomForest(variables, classe, ntree=ntree) 
}

trainPredictions <- predict(model)
confusionMatrix(trainPredictions,trainData$classe)

#to do 20 predictions for submission
featureSet <- colnames(trainData)
PredictForest = predict(model, newdata = testData)

#to output txt files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(PredictForest)