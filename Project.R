# describing how you built your model
# how you used cross validation
# what you think the expected out of sample error is, 
# and why you made the choices you did
# Inserir gráficos?

# Data loading
setwd("C:/Users/Ewerton/Dropbox/Curso_Data_Science/8_MachineLearning/Project")
trainingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(trainingURL, destfile = "./pml-training.csv")
download.file(testingURL, destfile = "./pml-testing.csv")
training <- read.csv("./pml-training.csv", na.strings=c("NA","#DIV/0!",""))
test_dataset <- read.csv("./pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

# Data cleaning
# Remove id and control variables
training <- training[,-c(1:7)]

# Variables with many NA, created by the researchers, were removed.
library(dplyr)
training <- select(training, -starts_with("amplitude"))
training <- select(training, -starts_with("avg"))
training <- select(training, -starts_with("kurtosis"))
training <- select(training, -starts_with("max"))
training <- select(training, -starts_with("min"))
training <- select(training, -starts_with("skewness"))
training <- select(training, -starts_with("stddev"))
training <- select(training, -starts_with("var"))

# Partitioning
library("caret")
trainIndex <- createDataPartition(y = training$classe, p=0.7, list=FALSE)
training <- training[trainIndex,]
testing <- training[-trainIndex,]
dim(training);dim(testing)


# Decision tree model
dtModel <- train(y = training$classe, x = training[,1:52], method = "rpart")
dtPredict <- predict(dtModel, testing)
confusionMatrix(dtPredict, testing$classe)
library(rattle)
fancyRpartPlot(dtModel$finalModel)


# Random forest model
library(randomForest)
rfModel<- randomForest(classe ~ ., data = training, type = "class", do.trace = TRUE, verboseIter = TRUE)
rfPredict <- predict(rfModel, testing)
confusionMatrix(rfPredict, testing$classe)
plot(rfModel)
importance(rfModel)
varImpPlot(rfModel)




# # Bagging model
# bagModel <- train(y = training$classe, x = training[,1:52], method = "treebag")
# bagPredict <- predict(bagModel, testing)
# confusionMatrix(bagPredict, testing$classe)

# Boosting
# boostingModel<- train(y = training$classe, x = training[,1:52], method = "gbm")
# boostingPredict <- predict(boostingModel, testing)
# confusionMatrix(boostingPredict, testing$classe)

# Teste de previsão
test_dataset <- test_dataset[,-c(1:7)]
test_dataset <- select(test_dataset, -starts_with("amplitude"))
test_dataset <- select(test_dataset, -starts_with("avg"))
test_dataset <- select(test_dataset, -starts_with("kurtosis"))
test_dataset <- select(test_dataset, -starts_with("max"))
test_dataset <- select(test_dataset, -starts_with("min"))
test_dataset <- select(test_dataset, -starts_with("skewness"))
test_dataset <- select(test_dataset, -starts_with("stddev"))
test_dataset <- select(test_dataset, -starts_with("var"))
predict(rfModel, test_dataset[1,])
predict(rfModel, test_dataset[2,])
predict(rfModel, test_dataset[3,])
predict(rfModel, test_dataset[4,])
predict(rfModel, test_dataset[5,])
predict(rfModel, test_dataset[6,])
predict(rfModel, test_dataset[7,])
predict(rfModel, test_dataset[8,])
predict(rfModel, test_dataset[9,])
predict(rfModel, test_dataset[10,])
predict(rfModel, test_dataset[11,])
predict(rfModel, test_dataset[12,])
predict(rfModel, test_dataset[13,])
predict(rfModel, test_dataset[14,])
predict(rfModel, test_dataset[15,])
predict(rfModel, test_dataset[16,])
predict(rfModel, test_dataset[17,])
predict(rfModel, test_dataset[18,])
predict(rfModel, test_dataset[19,])
predict(rfModel, test_dataset[20,])
