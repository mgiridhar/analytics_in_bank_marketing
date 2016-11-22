do kfold cross validation and check


# https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
library(VIM)
mice_plot <- aggr(bank.mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(bank.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

library(mice)
imputed_data <- mice(bank.mis, m=1, maxit = 5, method = 'polyreg', seed = 500)

#random forests

#accuracy ntree=50
#[1] 0.9161783
#ntree = 500
#[1] 0.9161783
#ntree = 1500
#[1] 0.9150724

##after cleaning and preparation
#accuracy ntree=50
#[1] 0.9171735
#ntree = 500
#[1] 0.9185005
#ntree = 1500
#[1] 0.9189428

##Data cleaning and preparation##
bank = read.csv('bank_imputed.csv')
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
#[1] 0.9171735 ntree=50

#feature selection
rf = randomForest(y~duration+poutcome+month+balance, data = train, importance=TRUE, ntree=50)
#[1] 0.9025766 ntree=50