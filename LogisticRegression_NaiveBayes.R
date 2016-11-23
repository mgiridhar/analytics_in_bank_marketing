
train <- read.csv("C:/Users/Aditi/Desktop/Machine Learing/Project/bank/bank_imputed.csv")

# Train:
print("Train")
train
#Code here
print("Test")
test
#Code here

head(train)
head(test)


lapply(train, class)



x <- seq(0,1, length=100)
x <- x[2:(length(x)-1)]
logit <- function (t) {
  log( t / (1-t) )
}



plot(x~logit(x), type="l")

inv_logit <- function(x){
  exp(x)/(1+exp(x))
}

y <- seq(-100,100, length=200)

plot(y~inv_logit(y), type="l")

#Partitioning data
library(caret)
set.seed(1234567)
train2<-createDataPartition(train$y,p=0.8,list=FALSE)
training<-train[train2,]
testing<-train[-train2,]

#building model

model <- glm(y~., family=binomial(link="logit"), data=training) #glm is used to fit generalized linear models, specified by giving a symbolic description of the linear predictor and a description of the error distribution.

summary(model)

test_prediction <- predict(model, testing, type="response") #predict is a generic function for predictions from the results of various model fitting functions. The function invokes particular methods which depend on the class of the first argument.

head(test_prediction)

class(test_prediction)

#install.packages('ROCR')
library(ROCR)

#transform the input data into a standardized format.
pr <- prediction(test_prediction, testing$y)
#All kinds of predictor evaluations are performed using this function.
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
prf


plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]   
auc


#Precision and Recall
precision_recall <- performance(pr, "prec", "rec")
plot(precision_recall)

#bank_full accuracy is 90.86863
#bank-additional-full accuracy 93.74682


#NAIVE BAYES
library(e1071)
model <- naiveBayes(y~., data=training)
test_prediction <- predict(model, testing, type="raw") #predict is a generic function for predictions from the results of various model fitting functions. The function invokes particular methods which depend on the class of the first argument.

pred <- predict(model, testing)

mat <- table(pred, testing$y)

accuracy <-((mat[2,2]+mat[1,1])/(mat[1,1]+mat[1,2]+mat[2,1]+mat[2,2]) ) *100
accuracy 

#Naive Bayes Accuracy - 86.37854