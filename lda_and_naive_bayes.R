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

#OBTAINING AND SPLITTING DATA
data_in <- read.csv("/Users/kevin/Desktop/sensors-19-05524-s001_new/data.txt", header=FALSE)
data_in2 = data_in[-c(101,114,115,243,255,268,269) ]
set.seed(222)

ind <- sample(2, nrow(data_in2), replace = TRUE, prob = c(0.7, 0.3))
train <- data_in2[ind==1,]
test <- data_in2[ind==2,]



# LDA CLASSIFICATION
fit <- lda(V535~., data=train)
summary(fit)
predictions <- predict(fit, test[,1:527])$class

table(predictions, test$V535)






#NAIVES BAYES
fit3 <- naiveBayes(V535~., data=train)
summary(fit3)
predictions3=predict(fit3,newdata = test,type = "class")
table(predictions3, test$V535)


