train <- read.csv("C:/Users/Aditi/Desktop/Machine Learing/Project/bank/bank_imputed.csv")
#bank_full_addition <- read.csv("C:/Users/Aditi/Desktop/Machine Learing/Project/bank-additional/bank-additional-full.csv",sep=";")

#head(bank_full) ## Displays first 6 rows for each variable

#str(bank_full) ## Describes each variables

library(caret)
set.seed(1234567)
train2<-createDataPartition(train$y,p=0.7,list=FALSE)
training<-train[train2,]
testing<-train[-train2,]


#install.packages("e1071")
library(e1071)

#?svm

## TUNING to perform cross-validation to know about optimum cost parameter for SVM modelling. Tune() performs only ten-fold cross validation here.
train.tune<-tune(svm,y~.,data=training,kernel="polynomial",ranges=list(cost=c(0.01,0.1,1,5,10,100)))

## Diffferent kernels such as Polynomial, Radial Basis and Sigmoid can also be used for tuning.
summary(train.tune)
bestmodal.tune<-train.tune$best.model
summary(bestmodal.tune)

#Linear Kernel
train.svm<-svm(y~.,train,kernel="linear",cost=0.01)

#polynomial kernel
#train.svm<-svm(y~.,train,kernel="polynomial",cost=0.01,scale=TRUE,degree=3,gamma=1)

summary(train.svm)
plot(train.svm,train)  ## Not possible for large data-sets
## Testing data using the trained model
test.svm<-predict(train.svm,test)
## Misclassification Table
mat <-table(predict=test.svm,truth=test$y)

(mat[2,2]+mat[1,1])/(mat[1,1]+mat[1,2]+mat[2,1]+mat[2,2]) 
