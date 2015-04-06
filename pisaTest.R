#Problem 1
pisaTrain = read.csv("pisa2009train.csv")
pisaTest  = read.csv("pisa2009test.csv")
str(pisaTrain)

tapply(pisaTrain$readingScore,pisaTrain$male,mean)
summary(pisaTrain)
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)

#Problem 2
summary(pisaTrain)

#Problem 3
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~.,data = pisaTrain)
summary(lmScore)
SSE = sum(lmScore$residuals^2)
RMSE=(SSE/2414)^(0.5)
RMSE

#Probelm 4
predTest = predict(lmScore, newdata = pisaTest)
Range = predTest[which.max(predTest)]-predTest[which.min(predTest)]
SSE = sum((pisaTest$readingScore - predTest)^2)
SSE
N = nrow(pisaTest)
RMSE=(SSE/N)^(0.5)
RMSE
mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
SST
R=1-SSE/SST
R