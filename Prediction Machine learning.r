Prediction Assignment Writeup

I have tried to generate correct answers for each of the 20 test data cases of this assignment using of caret and randomForest, this allowed me to generate the correct answers for each of the 20 test data cases provided in this assignment.

options(warn=-1)
suppressWarnings(library(Hmisc))
suppressWarnings(library(caret))
suppressWarnings(library(randomForest))
suppressWarnings(library(foreach))
suppressWarnings(library(doParallel))
set.seed(1024)

if (!file.exists("pmlTraining.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                  destfile = "pmlTraining.csv")
}
if (!file.exists("pmlTesting.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                  destfile = "pmlTesting.csv")
}

training_data <- read.csv("pmlTraining.csv", na.strings=c("#DIV/0!") )
evaluation_data <- read.csv("pmlTesting.csv", na.strings=c("#DIV/0!") )

for(i in c(8:ncol(training_data)-1)) {training_data[,i] = as.numeric(as.character(training_data[,i]))}

for(i in c(8:ncol(evaluation_data)-1)) {evaluation_data[,i] = as.numeric(as.character(evaluation_data[,i]))}

I have casted all data in 8 columns as numeric values. Displaying out our feature set.

feature_set <- colnames(training_data[colSums(is.na(training_data)) == 0])[-(1:7)]
model_data <- training_data[feature_set]
feature_set

We can split our dataset in 2 models data: training and testing.

idx <- createDataPartition(y=model_data$classe, p=0.75, list=FALSE )
training <- model_data[idx,]
testing <- model_data[-idx,]

Using parallel processing to build the model, we build 5 random forests with 150 trees each.

registerDoParallel()
x <- training[-ncol(training)]
y <- training$classe

rf <- foreach(ntree=rep(150, 6), .combine=randomForest::combine, .packages='randomForest') %dopar% {
randomForest(x, y, ntree=ntree) 
}

Conclusions and Test Data Submit

This model is accurate as we can see in the consusion matrix. This test data was around 99% accurate and all of test cases are nearly to be correct.

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


x <- evaluation_data
x <- x[feature_set[feature_set!='classe']]
answers <- predict(rf, newdata=x)

answers

pml_write_files(answers)
