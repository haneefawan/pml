# Practical Machine Learning

### Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 


### Data 

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.

The training data for this project are available here: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


```r
suppressMessages(library(caret))
suppressMessages(library(dplyr))
suppressMessages(library(randomForest))

# Getting data
training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(training, "./train.csv", method = "curl")
download.file(testing, "./test.csv", method = "curl")

train <- read.csv("./train.csv", na.strings = c("#DIV/0!", "", "NA"))
test <- read.csv("./test.csv", na.strings = c("#DIV/0!", "", "NA"))  

set.seed(3431)

# Splitting data set into 70% and 30% for training and cross-validation respectively.
inTrain <- createDataPartition(y = train$classe, p = 0.7, list = FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]

# Removing near zero-variance variables
nsv <- nearZeroVar(training)
training <- training[, -nsv]

# removing uninformative covariates
training <- select(training, -(X:num_window))

# removing covariates with NAs (>50%)
training <- training[,colSums(is.na(training))/nrow(training) < 0.50]

# Training model
modFit <- randomForest(classe~., data = training, method = "class")

# Cross validation
predict <- predict(modFit, testing, type = "class")
confusionMatrix(predict, testing$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    6    0    0    0
##          B    0 1133    5    0    0
##          C    0    0 1020   14    1
##          D    0    0    1  950    2
##          E    0    0    0    0 1079
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9951          
##                  95% CI : (0.9929, 0.9967)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9938          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9947   0.9942   0.9855   0.9972
## Specificity            0.9986   0.9989   0.9969   0.9994   1.0000
## Pos Pred Value         0.9964   0.9956   0.9855   0.9969   1.0000
## Neg Pred Value         1.0000   0.9987   0.9988   0.9972   0.9994
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2845   0.1925   0.1733   0.1614   0.1833
## Detection Prevalence   0.2855   0.1934   0.1759   0.1619   0.1833
## Balanced Accuracy      0.9993   0.9968   0.9955   0.9924   0.9986
```

```r
final <- predict(modFit, test, type = "class")
final
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
### Conclusion
Accuracy of Random Forest model: 99.51%.

Expected out-of-sample error:  0.49%

Thus, there will be very few miscalculations when categorizing the test set.

### Writing files for submission


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(final)
```
