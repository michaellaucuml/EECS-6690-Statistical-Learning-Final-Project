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



#RANDOM FOREST
#Splitting up the data for each of the differnt runs ECG
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_train = train[,2:175]
y_in_train = train[,535]
test_subset = test[c(2:175,535)]
x_in_test = test[,2:175]
y_in_test= test[,535]

fit2<- randomForest(x=x_in_train,y=as.factor(y_in_train), 
                    importance = TRUE,
                    proximity = TRUE)

predictions2 <- predict(fit2,x_in_test)
table(predictions2, test_subset$V535)


#RANDOM FOREST
#Splitting up the data for each of the differnt runs TEB
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_train = train[,200:239]
y_in_train = train[,535]
test_subset = test[c(200:239,535)]
x_in_test = test[,200:239]
y_in_test= test[,535]

fit2<- randomForest(x=x_in_train,y=as.factor(y_in_train), 
                    importance = TRUE,
                    proximity = TRUE)

predictions2 <- predict(fit2,x_in_test)
table(predictions2, test_subset$V535)

#RANDOM FOREST
#Splitting up the data for each of the differnt runs EDA Arm
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_train = train[,330:340]
y_in_train = train[,535]
test_subset = test[c(330:340,535)]
x_in_test = test[,330:340]
y_in_test= test[,535]

fit2<- randomForest(x=x_in_train,y=as.factor(y_in_train), 
                    importance = TRUE,
                    proximity = TRUE)

predictions2 <- predict(fit2,x_in_test)
table(predictions2, test_subset$V535)


#RANDOM FOREST
#Splitting up the data for each of the differnt runs EDA Hand
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_train = train[,440:445]
y_in_train = train[,535]
test_subset = test[c(440:445,535)]
x_in_test = test[,440:445]
y_in_test= test[,535]

fit2<- randomForest(x=x_in_train,y=as.factor(y_in_train), 
                    importance = TRUE,
                    proximity = TRUE)

predictions2 <- predict(fit2,x_in_test)
table(predictions2, test_subset$V535)


#RANDOM FOREST
#Splitting up the data for each of the differnt runs ECG+TEB+EDA
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_train = train[c(2:21,200:209,400:409)]
y_in_train = train[,535]
test_subset = test[c(2:21,200:209,400:409,535)]
x_in_test = test[c(2:21,200:209,400:409)]
y_in_test= test[,535]

fit2<- randomForest(x=x_in_train,y=as.factor(y_in_train), 
                    importance = TRUE,
                    proximity = TRUE)

predictions2 <- predict(fit2,x_in_test)
table(predictions2, test_subset$V535)


#RANDOM FOREST
#Splitting up the data for each of the differnt runs ECG+TEB
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_train = train[c(2:31,200:229)]
y_in_train = train[,535]
test_subset = test[c(2:31,200:229,535)]
x_in_test = test[c(2:31,200:229)]
y_in_test= test[,535]

fit2<- randomForest(x=x_in_train,y=as.factor(y_in_train), 
                    importance = TRUE,
                    proximity = TRUE)

predictions2 <- predict(fit2,x_in_test)
table(predictions2, test_subset$V535)


#RANDOM FOREST
#Splitting up the data for each of the differnt runsECG+EDA
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_train = train[c(2:31,400:409)]
y_in_train = train[,535]
test_subset = test[c(2:31,400:409,535)]
x_in_test = test[c(2:31,400:409)]
y_in_test= test[,535]

fit2<- randomForest(x=x_in_train,y=as.factor(y_in_train), 
                    importance = TRUE,
                    proximity = TRUE)

predictions2 <- predict(fit2,x_in_test)
table(predictions2, test_subset$V535)

#RANDOM FOREST
#Splitting up the data for each of the differnt runs TEB+EDA
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_train = train[c(200:229,400:409)]
y_in_train = train[,535]
test_subset = test[c(200:229,400:409,535)]
x_in_test = test[c(200:229,400:409)]
y_in_test= test[,535]

fit2<- randomForest(x=x_in_train,y=as.factor(y_in_train), 
                    importance = TRUE,
                    proximity = TRUE)

predictions2 <- predict(fit2,x_in_test)
table(predictions2, test_subset$V535)

#RANDOM FOREST
#Splitting up the data for each of the differnt runs EDA
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]

x_in_train = train[c(330:339,440:449)]
y_in_train = train[,535]
test_subset = test[c(330:339,440:449,535)]
x_in_test = test[c(330:339,440:449)]
y_in_test= test[,535]

fit2<- randomForest(x=x_in_train,y=as.factor(y_in_train), 
                    importance = TRUE,
                    proximity = TRUE)

predictions2 <- predict(fit2,x_in_test)
table(predictions2, test_subset$V535)