train <- read.csv("C:/Users/Aditi/Desktop/Machine Learing/Project/bank/bank_imputed.csv")

head(train)

lapply(train, class)


#Partitioning data
library(caret)
set.seed(1234567)
train2<-createDataPartition(train$y,p=0.7,list=FALSE)
training<-train[train2,]
testing<-train[-train2,]

#NAIVE BAYES
library(e1071)
model <- naiveBayes(y~., data=training)
#test_prediction <- predict(model, testing, type="raw") #predict is a generic function for predictions from the results of various model fitting functions. The function invokes particular methods which depend on the class of the first argument.
 
test_prediction <- predict(model, testing)


library(ROCR)
predvec <- ifelse(test_prediction=="yes", 1, 0)
realvec <- ifelse(testing$y=="yes", 1, 0)

pr <-prediction(predvec, realvec)
plot(prf <- performance(pr, measure = "tpr", x.measure = "fpr"))


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]   
auc
precision_recall <- performance(pr, "prec", "rec")
plot(precision_recall)

#mat <- table(test_prediction, testing$y)

#accuracy <-((mat[2,2]+mat[1,1])/(mat[1,1]+mat[1,2]+mat[2,1]+mat[2,2]) ) * 100
#accuracy 

