
train <- read.csv("C:/Users/Aditi/Desktop/Machine Learing/Project/bank/bank_imputed.csv")

head(train)

lapply(train, class)

#Partitioning data
library(caret)
set.seed(1234567)
train2<-createDataPartition(train$y,p=0.7,list=FALSE)
training<-train[train2,]
testing<-train[-train2,]

#building model

model <- glm(y~., family=binomial(link="logit"), data=training) #glm is used to fit generalized linear models, specified by giving a symbolic description of the linear predictor and a description of the error distribution.

summary(model)

#predict is a generic function for predictions from the results of various model fitting functions. 
#The function invokes particular methods which depend on the class of the first argument.
test_prediction <- predict(model, testing, type="response") 

head(test_prediction)

class(test_prediction)

#install.packages('ROCR')
library(ROCR)

#transform the input data into a standardized format.
pr <- prediction(test_prediction, testing$y)
#All kinds of predictor evaluations are performed using this function.
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]   
auc


#Precision and Recall
precision_recall <- performance(pr, "prec", "rec")
plot(precision_recall)

#bank_full accuracy is 90.86863
#bank-additional-full accuracy 93.74682
