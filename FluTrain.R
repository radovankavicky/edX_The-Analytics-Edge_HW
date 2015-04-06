#Problem 1
FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
summary(FluTrain)
which.max(FluTrain$ILI)
FluTrain[303,]
which.max(FluTrain$Queries)
FluTrain[303,]
hist(FluTrain$ILI)
FluTrain$LogILI =log(FluTrain$ILI)
str(FluTrain)

#Problem 2
plot(FluTrain$Queries,FluTrain$LogILI)
FluTrend1 =lm (log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)
cor(FluTrain$Queries,FluTrain$LogILI)

#Problem 3
FluTest = read.csv("FluTest.csv")
PredTest1 = predict(FluTrend1, newdata=FluTest)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
EstimatedILI = PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
ObservedILI = FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),]
(2.293422-2.187378)/2.293422
SSE = sum((FluTest$ILI-PredTest1)^2)
RMSE=(SSE/nrow(FluTest))^(0.5)

#Problem 4
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)
plot(FluTrain$ILILag2,FluTrain$ILI)
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2),data=FluTrain)
summary(FluTrend2)

#Probelm 5
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((FluTest$ILI-PredTest2)^2)
RMSE=(SSE/nrow(FluTest))^(0.5)
RMSE