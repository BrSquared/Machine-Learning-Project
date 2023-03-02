MachineLearningProject
================
Brendan Brogan
2023-2-20

## ExecutiveSummary

The purpose of this study was to use data from fitness tracking devices
such as fitbit to determine if 5 sets of excercises were done correctly.
Tracking the motion of various parts of the body, it can be easy for
these devices to track when the person is exercising, but the quality of
the exercize is more difficult to track. A sample of 6 individuals were
asked to do barbell lifts both correctly and incorrectly, and several
prediction algorithms were trained on this data in an attempt to predict
if the lifts were done correctly.

## Exploring/Cleaning the Data

The data came from the source:
<http://groupware.les.inf.puc-rio.br/har>. The data was downloaded and
stored in the project directory.

``` r
setwd("C:/Users/brend/OneDrive/Desktop/datasciencecoursera/08 Practical Machine Learning/Project/Machine Learning Project")
totaldf <- read.csv("traindata.csv",na.strings = c("#DIV/0!","NA"))
validationdf <- read.csv("testdata.csv")
totaldf$classe <- as.factor(totaldf$classe)

library(caret)
```

    ## Warning: package 'caret' was built under R version 4.2.2

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 4.2.1

    ## Loading required package: lattice

``` r
set.seed(1001)
inTrain <- createDataPartition(totaldf$classe, p = 0.7, list = FALSE)
traindf <- totaldf[inTrain,]
testdf <- totaldf[-inTrain,]
```

19622 observations were read in the training set, and only 20 in the
testing set. Because of the small test set, cross validation was used in
the training set to test the models, and only the best performing model
was tested on the final testing set, referred to as the validation set.
There were 160 variables in total, including the “classe” variable,
which determined what type of lift the test subjects performed.

The data was partitioned with 30% making up the new testing set and 70%
making up the training set.

``` r
preobj <- preProcess(traindf,method = "medianImpute")
traindf <- predict(preobj,traindf)
testdf <- predict(preobj,testdf)

traindf <- traindf[,!as.logical(sapply(traindf,function(x) any(is.na(x))))]
traindf$user_name <- as.factor(traindf$user_name)
traindf <- traindf[,-1]
testdf <- testdf[,-1]

testdf <- testdf[,names(testdf)%in%names(traindf)]
```

There were several missing values in the data set. These were dealt with
using the median impute method from the caret package. This method
replaces the missing values with the median value of the variable. There
were six columns which this method did not work for and still had
missing values. These columns were removed from the analysis.

## Building the Model

Several models and methods were built on the training dataset. As an
exploratory first model, only the variables that represented the average
of the measurements were used. This reduced the number of variables from
153 to 14, which significantly sped up the process of building the
models The idea was to use this to identify if certain models performed
better, and then rebuild them on the full data set.

``` r
preddf <- traindf[,c(2,grep("avg",names(traindf)),153)]
tpreddf <- testdf[,c(2,grep("avg",names(traindf)),153)]

fit1 <- train(classe~.,method = "lda2", data = preddf)
pred1 <- predict(fit1,newdata=tpreddf)

confusionMatrix(tpreddf$classe,pred1)$overall[1]
```

    ##  Accuracy 
    ## 0.2880204

The accuracy was 28.7%. Given that there are 5 options for the predicted
variable, classe, the minimum accuracy is 20%. That makes this model not
useful. The next method tested was a decision tree, using the rpart
method.

``` r
fit2 <- train(classe~.,method = "rpart", data = preddf)
pred2 <- predict(fit2,newdata=tpreddf)

confusionMatrix(tpreddf$classe,pred2)$overall[1]
```

    ##  Accuracy 
    ## 0.9920136

The accuracy of this method was far higher, almost 99%. This makes
sense, since decision trees are far better suited for non linear
outcomes such as this problem. With an accuracy of almost 99%, it seemed
suitable to test against the validation set. Out of sample error was a
concern, since several models were tested against the same training set,
this could be the result of overfitting. To counter this potential
problem, the testing and training set were resampled before testing
against the validation set.

``` r
set.seed(2002)
inTrain <- createDataPartition(totaldf$classe, p = 0.7, list = FALSE)
traindf <- totaldf[inTrain,]
testdf <- totaldf[-inTrain,]

preobj <- preProcess(traindf,method = "medianImpute")
traindf <- predict(preobj,traindf)
testdf <- predict(preobj,testdf)

traindf <- traindf[,!as.logical(sapply(traindf,function(x) any(is.na(x))))]
traindf$user_name <- as.factor(traindf$user_name)
traindf <- traindf[,-1]

testdf <- testdf[,names(testdf)%in%names(traindf)]

preddf <- traindf[,c(2,grep("avg",names(traindf)),153)]
tpreddf <- testdf[,c(2,grep("avg",names(traindf)),153)]

fit2r <- train(classe~.,method = "rpart", data = preddf)
pred2r <- predict(fit2r,newdata=tpreddf)

confusionMatrix(tpreddf$classe,pred2r)$overall[1]
```

    ##  Accuracy 
    ## 0.9909941

The accuracy was compatible, so it seemed like the model was predicting
the data well. It was time to format the validation data set in the same
way of the training set, and then test the model.

``` testing
validdf <- predict(preobj,validationdf)
validdf <- validdf[,names(validdf)%in%names(traindf)]

predv <- predict(fit3, newdata = validdf)
```

The validation set did not come with the classe variable, as it was put
directly onto coursera for a quiz exercize, but it returned with results
of 100% accuracy. This is in line with what was expected with a 99%
accuracy on a 20 record set.
