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
library(vglm)
library(arm) 
#OBTAINING AND SPLITTING DATA
data_in <- read.csv("/Users/kevin/Desktop/sensors-19-05524-s001_new/data.txt", header=FALSE)
set.seed(222)
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))
train <- data_in[ind==1,]
test <- data_in[ind==2,]



#Splitting up the data for each of the differnt runs ECG
train_subset<- train[c(2:175,535)]
x_in_test <- test[c(2:175)]
test_subset = test[c(2:175,535)]
fit<-multinom(V535~.,data=train_subset)
probabilities <- predict(fit,x_in_test)
table(probabilities, test_subset$V535)

#Splitting up the data for each of the differnt runs TEB
train_subset<- train[c(200:239,535)]
x_in_test <- test[c(200:239)]
test_subset = test[c(200:239,535)]
fit<-multinom(V535~.,data=train_subset)
probabilities <- predict(fit,x_in_test)
table(probabilities, test_subset$V535)


#Splitting up the data for each of the differnt runs EDA ARM
train_subset<- train[c(330:340,535)]
x_in_test <- test[c(330:340)]
test_subset = test[c(330:340,535)]
fit<-multinom(V535~.,data=train_subset)
probabilities <- predict(fit,x_in_test)
table(probabilities, test_subset$V535)

#Splitting up the data for each of the differnt runs EDA HAND
train_subset<- train[c(440:445,535)]
x_in_test <- test[c(440:445)]
test_subset = test[c(440:445,535)]
fit<-multinom(V535~.,data=train_subset)
probabilities <- predict(fit,x_in_test)
table(probabilities, test_subset$V535)



#Splitting up the data for each of the differnt runs ECG+TEB+EDA
train_subset<- train[c(2:21,200:209,400:409,535)]
x_in_test <- test[c(2:21,200:209,400:409)]
test_subset = test[c(2:21,200:209,400:409,535)]
fit<-multinom(V535~.,data=train_subset)
probabilities <- predict(fit,x_in_test)
table(probabilities, test_subset$V535)

#Splitting up the data for each of the differnt runs ECG+TEB
train_subset<- train[c(2:31,200:229,535)]
x_in_test <- test[c(2:31,200:229)]
test_subset = test[c(2:31,200:229,535)]
fit<-multinom(V535~.,data=train_subset)
probabilities <- predict(fit,x_in_test)
table(probabilities, test_subset$V535)

#Splitting up the data for each of the differnt runs ECG+EDA
train_subset<- train[c(2:31,400:409,535)]
x_in_test <- test[c(2:31,400:409)]
test_subset = test[c(2:31,400:409,535)]
fit<-multinom(V535~.,data=train_subset)
probabilities <- predict(fit,x_in_test)
table(probabilities, test_subset$V535)

#Splitting up the data for each of the differnt runs TEB+EDA
train_subset<- train[c(200:229,400:409,535)]
x_in_test <- test[c(200:229,400:409)]
test_subset = test[c(200:229,400:409,535)]
fit<-multinom(V535~.,data=train_subset)
probabilities <- predict(fit,x_in_test)
table(probabilities, test_subset$V535)


#Splitting up the data for each of the differnt runs EDA
train_subset<- train[c(330:339,440:449,535)]
x_in_test <- test[c(330:339,440:449)]
test_subset = test[c(330:339,440:449,535)]
fit<-multinom(V535~.,data=train_subset)
probabilities <- predict(fit,x_in_test)
table(probabilities, test_subset$V535)

