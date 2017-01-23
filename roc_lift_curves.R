#install.packages("randomForest")
library(rminer)
library(randomForest)
#setwd('D:/ms_cs/machine_learning/project/bank-additional/bank_prepocess')
#bank = read.csv('bank.csv')
#without removing unknowns
#bank = read.table("bank-full.csv", sep=";", header = TRUE)
bank = read.csv("C:/Users/Aditi/Desktop/Machine Learing/Project/bank/bank_imputed.csv")
bank$age <- as.numeric(bank$age)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)
bank$balance <- as.numeric(bank$balance)
bank$month = factor(bank$month, levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"), ordered = TRUE)


#MNB3=mining(y~.,bank,method=c("holdout",2/3), model="decisiontrees", Runs=20)

MNB3=mining(y~.,bank,method=c("holdout",2/3), model="naivebayes", Runs=20)
savemining(MNB3, "mining_nb.output",ascii=TRUE)
MNB3=loadmining("mining_nb.output") # read MNB3 from file

MDT3=mining(y~.,bank,method=c("holdout",2/3), model="dt",Runs=20)
savemining(MDT3, "mining_dt.output",ascii=TRUE)
MDT3=loadmining("mining_dt.output")

MLR3=mining(y~.,bank,method=c("holdout",2/3), model="logistic",Runs=20)
savemining(MLR3, "mining_lr.output",ascii=TRUE)
MLR3=loadmining("mining_lr.output")

MRF3=mining(y~.,bank,method=c("holdout",2/3), model="randomforest",Runs=20)
savemining(MRF3, "mining_rf.output",ascii=TRUE)
MRF3=loadmining("mining_rf.output")

MSVM3=mining(y~.,bank,method=c("holdout",2/3), model="svm",Runs=20)
savemining(MSVM3, "mining_svm.output",ascii=TRUE)
MSVM3=loadmining("mining_svm.output")

L=vector("list",5); L[[1]]=MNB3; L[[2]]=MDT3; L[[3]]=MSVM3; L[[4]]=MLR3; L[[5]]=MRF3;
mgraph(L,graph="ROC",TC=2,
       leg=list(pos=c(0.65,0.55), leg=c("NB","DT","SVM","LR","RandomForest")), 
       baseline=TRUE, 
       Grid=15, main="ROC curves",
       col = c("green","red","Purple","blue","brown")) # ROC graph

L=vector("list",5); L[[1]]=MNB3; L[[2]]=MDT3; L[[3]]=MSVM3; L[[4]]=MLR3; L[[5]]=MRF3;
mgraph(L,graph="LIFT",
       TC=2,
       leg=list(pos=c(0.65,0.4),
                leg=c("NB","DT","SVM","LR","RandomForest")), 
       baseline=TRUE,
       col = c("green","red","Purple","blue","brown"),
       Grid=15) # Accumulative Lift graph


print(mmetric(MNB3,metric="CONF",TC=2)) # confusion matrix
print(mmetric(MNB3,metric="AUC",TC=2)) # area under the ROC curve
print(mmetric(MNB3,metric="ACC",TC=2)) # accuracy
print(mmetric(MNB3,metric="TPR",TC=2)) # true positive rate
print(mmetric(MNB3,metric="ALIFT",TC=2)) # area under Lift curve


data <- read.csv('C:/Users/Aditi/Desktop/Machine Learing/Project/bank.csv')
head(data,10)



sapply(data, function(x) sum(is.na(x)))

dat <- data.frame(x=c('NB','LR','SVM','DT','Random Forest'), 
                  y=c(89.47019,90.86863,91.79294,90.58, 91.80))

barplot(dat$y, xlab = "Models",ylab = "Accuracy",names.arg=dat$x, ylim=c(0,100),
        col = c("light green","pink","grey"," light blue","light yellow"))

?MICE
