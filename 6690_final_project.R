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


#RANDOM FOREST
ind <- sample(2, nrow(data_in), replace = TRUE, prob = c(0.7, 0.3))

train <- data_in[ind==1,]
test <- data_in[ind==2,]


x_in = train[,1:534]
y_in = train[,535]

fit2<- randomForest(x=x_in,y=as.factor(y_in), 
                        importance = TRUE,
                        proximity = TRUE)

predictions2 <- predict(fit2,test[,1:534])
table(predictions2, test$V535)



#NAIVES BAYES
fit3 <- naiveBayes(V535~., data=train)
summary(fit3)
predictions3=predict(fit3,newdata = test,type = "class")
table(predictions3, test$V535)


#KNN
#CAN TRY OUT DIFFERNT VALUES OF K
x_in_test = test[,1:534]
y_in_test = test[,535]
x_in_train = train[,1:534]
y_in_train = train[,535]

fit4 <- knn(train = x_in_train,
            test = x_in_test,
            cl = train$V535,
            k = 3)

misClassError <- mean(fit4 != test$V535)
print(paste('Accuracy =', 1-misClassError))



#MLP
#CAN ADJUST DIFFERENT SETTING FOR THE NEURAL NETWORK
nn.1  <- nnet(as.factor(V535)~., data = train,size=10,decay=1e-3,maxit=50,MaxNWts=150000)
predictions5 <- predict(nn.1, newdata=test, type = "class")
comparison <- data.frame(actual = as.factor(test$V535), predicted = predictions5)
table(comparison)

