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

#OBTAINING AND SPLITTING DATA
data_in <- read.csv("/Users/kevin/Desktop/sensors-19-05524-s001_new/data.txt", header=FALSE)
data_in2 = data_in[-c(1,101,114,115,243,255,268,269) ]
set.seed(222)

ind <- sample(2, nrow(data_in2), replace = TRUE, prob = c(0.7, 0.3))
train <- data_in[ind==1,]
test <- data_in[ind==2,]



#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
#Splitting up the data for each of the differnt runs ECG
x_in_test = test[,2:175]
y_in_test = test[,535]
x_in_train = train[,2:175]
y_in_train = train[,535]
test_subset = test[c(2:175,535)]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 15)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))

#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
#Splitting up the data for each of the differnt runs TEB
x_in_test = test[,200:239]
y_in_test = test[,535]
x_in_train = train[,200:239]
y_in_train = train[,535]
test_subset = test[c(200:239,535)]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 15)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))

#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
#Splitting up the data for each of the differnt runs EDA ARM
x_in_test = test[,330:340]
y_in_test = test[,535]
x_in_train = train[,330:340]
y_in_train = train[,535]
test_subset = test[c(330:340,535)]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 15)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))


#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
#Splitting up the data for each of the differnt runs EDA HAND
x_in_test = test[,440:445]
y_in_test = test[,535]
x_in_train = train[,440:445]
y_in_train = train[,535]
test_subset = test[c(440:445,535)]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 15)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))

#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
#Splitting up the data for each of the differnt runs ECG+TEB+EDA
x_in_test = test[,2:21,200:209,400:409]
y_in_test = test[,535]
x_in_train = train[,2:21,200:209,400:409]
y_in_train = train[,535]
test_subset = test[c(2:21,200:209,400:409,535)]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 15)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))

#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
#Splitting up the data for each of the differnt runs ECG+TEB
x_in_test = test[,2:31,200:229]
y_in_test = test[,535]
x_in_train = train[,2:31,200:229]
y_in_train = train[,535]
test_subset = test[c(2:31,200:229,535)]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 15)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))


#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
#Splitting up the data for each of the differnt runs ECG+EDA
x_in_test = test[,2:31,400:409]
y_in_test = test[,535]
x_in_train = train[,2:31,400:409]
y_in_train = train[,535]
test_subset = test[c(2:31,400:409,535)]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 15)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))

#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
#Splitting up the data for each of the differnt runs TEB+EDA
x_in_test = test[,200:229,400:409]
y_in_test = test[,535]
x_in_train = train[,200:229,400:409]
y_in_train = train[,535]
test_subset = test[c(200:229,400:409,535)]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 15)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))


#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
#Splitting up the data for each of the differnt runs EDA
x_in_test = test[,330:339,440:449]
y_in_test = test[,535]
x_in_train = train[,330:339,440:449]
y_in_train = train[,535]
test_subset = test[c(330:339,440:449,535)]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 15)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))
