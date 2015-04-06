#Problem 1

ClimateChange = read.csv("climate_change.csv")
str(ClimateChange)

TrainingSet =subset(ClimateChange, Year <= 2006)
str(TrainingSet)
summary(TrainingSet)

TestingSet =subset(ClimateChange, Year > 2006)
str(TestingSet)
summary(TestingSet)

Model1 =lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data=TrainingSet)
summary(Model1)

#Problem 2
cor(TrainingSet)

#Problem 3
Model2 =lm(Temp ~ MEI + TSI + Aerosols + N2O ,data=TrainingSet)
summary(Model2)

#Probelm 4
StepModel = step(Model1)
Model3 =lm(Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data=TrainingSet)
summary(Model3)

#Probelm 5
predictTest = predict(Model3, newdata=TestingSet)
predictTest
# Compute R-squared
SSE = sum((TestingSet$Temp - predictTest)^2)
SST = sum((TestingSet$Temp - mean(TrainingSet$Temp))^2)
R = 1 - SSE/SST
R


