## RANDOM FORESTS ##
setwd('D:/ms_cs/machine_learning/project/bank-additional/')
library(randomForest)
?randomForest

bank = read.csv('project_bank_train.csv')
size <- nrow(bank) * 0.8
#DO WE NEED TO SAMPLE FOR RANDOM FORESTS???
validation_index <- sample(1:nrow(bank), size = size)
validation <- bank[-validation_index,]
train <- bank[validation_index,]
str(train)

set.seed(50)
#mtry - number of features to be selected for decision making at each level, default value for classification sqrt(num. of features)
rf = randomForest(y~., data = train, importance=TRUE, ntree=50)
rf = randomForest(y~., data = train, importance=TRUE, ntree=500)
rf = randomForest(y~., data = train, importance=TRUE, ntree=1500)

#feature selection
rf = randomForest(y~duration+euribor3m+age+job, data = train, importance=TRUE, ntree=1500)
#accuracy after feature selection
# 0.9037388 for ntree=50
# 0.9071377 for ntree=1500
par(mfrow=c(1,2))
varImpPlot(rf)

predictions = predict(rf, validation)
confusion.matrix = prop.table(table(predictions, validation$y))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]
# accuracy # ntree=50
#[1] 0.9115076
# accuracy # ntree=500
#[1] 0.9134499
# accuracy # ntree=1500
#[1] 0.9141782

str(rf)
rf$confusion
rf$importance
plot(rf)
rf$confusion

######################
bank1 = read.table("bank-full.csv", sep=";", header = TRUE)
size <- nrow(bank) * 0.8
validation_index <- sample(1:nrow(bank1), size = size)
validation1 <- bank1[-validation_index,]
train1 <- bank1[validation_index,]

str(train1)
set.seed(51)
fit = randomForest(y~., data = train1, importance=TRUE, ntree=500)
fit1 = randomForest(y~., data = train1, importance=TRUE, ntree=50)
fit2 = randomForest(y~., data = train1, importance=TRUE, ntree=1500)
varImpPlot(fit)
fit$confusion
predictions = predict(fit, validation1)
confusion.matrix = prop.table(table(predictions, validation1$y))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]
# accuracy # for ntree = 50
#[1] 0.9071105
# accuracy # for ntree = 500
#[1] 0.9049833
# accuracy # for ntree = 1500
#[1] 0.9041677

#########################################################
##Data cleaning and preparation##
bank = read.csv('project_bank_train.csv')
bank_cpy = data.frame(bank)
## Data cleaning and preparation 
bank$age <- as.numeric(bank$age)
bank$duration <- as.numeric(bank$duration)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)
bank$emp.var.rate <- as.numeric(bank$emp.var.rate)
bank$cons.price.idx <- as.numeric(bank$cons.price.idx)
bank$cons.conf.idx <- as.numeric(bank$cons.conf.idx)
bank$euribor3m <- as.numeric(bank$euribor3m)
bank$nr.employed <- as.numeric(bank$nr.employed)
##
size <- nrow(bank) * 0.8
#DO WE NEED TO SAMPLE FOR RANDOM FORESTS???
set.seed(80)
validation_index <- sample(1:nrow(bank), size = size)
validation <- bank[-validation_index,]
train <- bank[validation_index,]
str(train)

set.seed(50)
#mtry - number of features to be selected for decision making at each level, default value for classification sqrt(num. of features)
rf = randomForest(y~., data = train, importance=TRUE, ntree=50)
rf = randomForest(y~., data = train, importance=TRUE, ntree=500)
rf = randomForest(y~., data = train, importance=TRUE, ntree=1000)
rf = randomForest(y~., data = train, importance=TRUE, ntree=1500)

#sample feature selection
rf = randomForest(y~duration+euribor3m+age+job, data = train, importance=TRUE, ntree=1500)

par(mfrow=c(1,2))
varImpPlot(rf)

predictions = predict(rf, validation)
confusion.matrix = prop.table(table(predictions, validation$y))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]
#[1] 0.9121146 #for ntree=50
