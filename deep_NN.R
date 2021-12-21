#LOAD PACKAGES
library(VGAM)
library(MASS)
library(nnet)
library(fdm2id)
library(randomForest)
library(caret)
library(e1071)
library(caTools)
library(class)
library(randomForest)
library(CRAN)
library(neuralnet)
#OBTAINING AND SPLITTING DATA
data_in <- read.csv("/Users/kevin/Desktop/sensors-19-05524-s001_new/data.txt", header=FALSE)
set.seed(222)
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))
train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_test = test[,2:175]
y_in_test = test[,535]
x_in_train = train[,2:175]
y_in_train = train[,535]
train_subset=train[c(2:175,535)]
test_subset = test[c(2:175,535)]

nn=neuralnet(as.factor(V535)~.,data=data.frame(train_subset), hidden=5,learningrate = 1e-5, act.fct = "logistic",
             linear.output = TRUE)

Predict=compute(nn,x_in_test)
pr.nn_2 <- max.col(Predict$net.result)
mean(pr.nn_2 == y_in_test )


#Deep neural network 
x_in_test = test[,200:239]
y_in_test = test[,535]
x_in_train = train[,200:239]
y_in_train = train[,535]
train_subset=train[c(200:239,535)]
test_subset = test[c(200:239,535)]

nn=neuralnet(as.factor(V535)~.,data=data.frame(train_subset), hidden=5,learningrate = 1e-5, act.fct = "logistic",
             linear.output = TRUE)

Predict=compute(nn,x_in_test)
pr.nn_2 <- max.col(Predict$net.result)
mean(pr.nn_2 == y_in_test )



#Deep neural network 
#Splitting up the data for each of the differnt runs EDA ARM
x_in_test = test[,330:340]
y_in_test = test[,535]
x_in_train = train[,330:340]
y_in_train = train[,535]
train_subset=train[c(330:340,535)]
test_subset = test[c(330:340,535)]

nn=neuralnet(as.factor(V535)~.,data=data.frame(train_subset), hidden=5,learningrate = 1e-5, act.fct = "logistic",
             linear.output = TRUE)

Predict=compute(nn,x_in_test)
pr.nn_2 <- max.col(Predict$net.result)
mean(pr.nn_2 == y_in_test )




#Deep neural network 
#Splitting up the data for each of the differnt runs EDA HAND
x_in_test = test[,440:445]
y_in_test = test[,535]
x_in_train = train[,440:445]
y_in_train = train[,535]
train_subset=train[c(440:445,535)]
test_subset = test[c(440:445,535)]

nn=neuralnet(as.factor(V535)~.,data=data.frame(train_subset), hidden=5,learningrate = 1e-5, act.fct = "logistic",
             linear.output = TRUE)

Predict=compute(nn,x_in_test)
pr.nn_2 <- max.col(Predict$net.result)
mean(pr.nn_2 == y_in_test )


#Deep neural network 
#Splitting up the data for each of the differnt runs ECG+TEB+EDA
x_in_test = test[c(2:21,200:230,400:430)]
y_in_test = test[,535]
x_in_train = train[c(2:21,200:230,400:430)]
y_in_train = train[,535]
train_subset=train[c(2:21,200:230,400:430,535)]
test_subset = test[c(2:21,200:230,400:430,535)]

nn=neuralnet(as.factor(V535)~.,data=data.frame(train_subset), hidden=5,learningrate = 1e-5, act.fct = "logistic",
             linear.output = TRUE)

Predict=compute(nn,x_in_test)
pr.nn_2 <- max.col(Predict$net.result)
mean(pr.nn_2 == y_in_test )



#Deep neural network 
#Splitting up the data for each of the differnt runs ECG+TEB
x_in_test = test[c(2:31,200:229)]
y_in_test = test[,535]
x_in_train = train[c(2:31,200:229)]
y_in_train = train[,535]
train_subset=train[c(2:31,200:229,535)]
test_subset = test[c(2:31,200:229,535)]

nn=neuralnet(as.factor(V535)~.,data=data.frame(train_subset), hidden=5,learningrate = 1e-5, act.fct = "logistic",
             linear.output = TRUE)

Predict=compute(nn,x_in_test)
pr.nn_2 <- max.col(Predict$net.result)
mean(pr.nn_2 == y_in_test )


#Deep neural network 
#Splitting up the data for each of the differnt runs ECG+EDA
x_in_test = test[c(2:11,400:409)]
y_in_test = test[,535]
x_in_train = train[c(2:11,400:409)]
y_in_train = train[,535]
train_subset=train[c(2:11,400:409,535)]
test_subset = test[c(2:11,400:409,535)]

nn=neuralnet(as.factor(V535)~.,data=data.frame(train_subset), hidden=5,learningrate = 1e-5, act.fct = "logistic",
             linear.output = TRUE)

Predict=compute(nn,x_in_test)
pr.nn_2 <- max.col(Predict$net.result)
mean(pr.nn_2 == y_in_test )




#Deep neural network 
#Splitting up the data for each of the differnt runs TEB+EDA
x_in_test = test[c(200:250,400:430)]
y_in_test = test[,535]
x_in_train = train[c(200:250,400:430)]
y_in_train = train[,535]
train_subset=train[c(200:250,400:430,535)]
test_subset = test[c(200:250,400:430,535)]

nn=neuralnet(as.factor(V535)~.,data=data.frame(train_subset), hidden=5,learningrate = 1e-5, act.fct = "logistic",
             linear.output = TRUE)

Predict=compute(nn,x_in_test)
pr.nn_2 <- max.col(Predict$net.result)
mean(pr.nn_2 == y_in_test )


#Deep neural network 
#Splitting up the data for each of the differnt runs EDA
x_in_test = test[c(330:339,440:449)]
y_in_test = test[,535]
x_in_train = train[c(330:339,440:449)]
y_in_train = train[,535]
train_subset=train[c(330:339,440:449,535)]
test_subset = test[c(330:339,440:449,535)]


nn=neuralnet(as.factor(V535)~.,data=data.frame(train_subset), hidden=5,learningrate = 1e-5, act.fct = "logistic",
             linear.output = TRUE)

Predict=compute(nn,x_in_test)
pr.nn_2 <- max.col(Predict$net.result)
mean(pr.nn_2 == y_in_test )