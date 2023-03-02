## Initializing Project
setwd("C:/Users/brend/OneDrive/Desktop/datasciencecoursera/08 Practical Machine Learning/Project/Machine Learning Project")
library(caret)

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "traindata.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile = "testdata.csv")

totaldf <- read.csv("traindata.csv",na.strings = c("#DIV/0!","NA"))
validationdf <- read.csv("testdata.csv")
totaldf$classe <- as.factor(totaldf$classe)

inTrain <- createDataPartition(totaldf$classe, p = 0.7, list = FALSE)
traindf <- totaldf[inTrain,]
testdf <- totaldf[-inTrain,]

preobj <- preProcess(traindf,method = "medianImpute")
traindf <- predict(preobj,traindf)
testdf <- predict(preobj,testdf)
validdf <- predict(preobj,validationdf)

traindf <- traindf[,!as.logical(sapply(traindf,function(x) any(is.na(x))))]
traindf$user_name <- as.factor(traindf$user_name)
traindf <- traindf[,-1]

testdf <- testdf[,names(testdf)%in%names(traindf)]
validdf <- validdf[,names(validdf)%in%names(traindf)]

preddf <- traindf[,c(2,grep("avg",names(traindf)),153)]
tpreddf <- testdf[,c(2,grep("avg",names(traindf)),153)]

fit1 <- train(classe~.,method = "gbm", data = preddf, verbose = FALSE)
fit2 <- train(classe~.,method = "lda2", data = df)

pred <- predict(fit1,newdata=tpreddf)
confusionMatrix(tpreddf$classe,pred)

pred2 <- predict(fit2,newdata=tdf2)
confusionMatrix(tdf$classe,pred2)
