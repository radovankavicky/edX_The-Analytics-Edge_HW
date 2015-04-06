#Problem 1
parole = read.csv("parole.csv")
str(parole)
summary(parole)
table(parole$violator)

#Problem 2
parole$state=as.factor(parole$state)
parole$crime=as.factor(parole$crime)
summary(parole)

#Problem 3
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#Problem 4
ParoleLog = glm(violator ~.,data=train,family=binomial)
summary(ParoleLog)

#Problem 5
predictTest=predict(ParoleLog,type="response",newdata=test)
summary(predictTest)
table(test$violator, predictTest > 0.5)
library(ROCR)
ROCRpred = prediction(predictTest, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
