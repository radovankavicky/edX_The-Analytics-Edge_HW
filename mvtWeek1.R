#Problem 1

mvt=read.csv("mvtWeek1.csv")
str(mvt)
summary(mvt)
mvt$ID[which.max(mvt$ID)]
mvt$Beat[which.min(mvt$Beat)]
table(mvt$Arrest)
LocationAlley=subset(mvt,mvt$LocationDescription == "ALLEY")
nrow(LocationAlley)

#Problem 2
mvt$Date
DateConvert=as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month,mvt$Arrest)

#Problem 3
hist(mvt$Date, breaks=100)
table(mvt$Year)
#Reference:
http://msenux.redwoods.edu/math/R/boxplot.php
boxplot(mvt$Date ~ mvt$Arrest)
table(mvt$Year,mvt$Arrest)

#Problem 4
sort(table(mvt$LocationDescription))
Location1 = subset(mvt,mvt$LocationDescription == "STREET")
Location2 = subset(mvt,mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)")
Location3 = subset(mvt,mvt$LocationDescription == "ALLEY")
Location4 = subset(mvt,mvt$LocationDescription == "GAS STATION")
Location5 = subset(mvt,mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")
Top5=rbind(Location1,Location2,Location3,Location4,Location5)
nrow(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription,Top5$Arrest)
table(Location4$Weekday)
table(Location5$Weekday)              



