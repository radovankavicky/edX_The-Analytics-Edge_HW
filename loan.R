#Problem 1
loans = read.csv("loans.csv")
str(loans)
summary(loans)
table(loans$not.fully.paid)
loans = read.csv("loans_imputed.csv")

#Problem 2
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
LoansLog = glm(not.fully.paid ~.,data=train,family=binomial)
summary(LoansLog)

predicted.risk=predict(LoansLog,type="response",newdata=test)
test$predicted.risk=predicted.risk
table(test$not.fully.paid, predicted.risk > 0.5)

library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Problem 3
LoansLog2 = glm(not.fully.paid ~int.rate,data=train,family=binomial)
summary(LoansLog2)
predicted.risk=predict(LoansLog2,type="response",newdata=test)
summary(predicted.risk)
table(test$not.fully.paid, predicted.risk > 0.5)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Problem 5
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
test[which.max(test$profit),]

#Problem 6
highInterest=subset(test, "int.rate" >= 0.15 )
str(highInterest)
summary(highInterest)
mean(highInterest$profit)
table(highInterest$int.rate>=0.15,highInterest$not.fully.paid)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
