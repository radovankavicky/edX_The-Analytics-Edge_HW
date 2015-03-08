#Problem 1
CPS= read.csv("CPSData.csv")
summary(CPS)
str(CPS)

sort(table(CPS$Region)) 
IntervieweebyState =sort(table(CPS$State)) 
IntervieweebyCitizenship =sort(table(CPS$Citizenship)) 
IntervieweebyHispanic =sort(table(CPS$Hispanic)) 

#Problem 2
is.na(CPS$Married) 
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))

table(CPS$State,is.na(CPS$MetroAreaCode))
nrow(table(CPS$State,is.na(CPS$MetroAreaCode)))
table(CPS$Region,is.na(CPS$MetroAreaCode))
tapply(is.na(CPS$MetroAreaCode),CPS$State,mean)
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))

#Problem 3
MetroAreaMap =read.csv("MetroAreaCodes.csv")
CountryMap =read.csv("CountryCodes.csv")
str(MetroAreaMap)
str(CountryMap)
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
str(CPS)
sort(table(CPS$MetroArea))
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))
tapply(CPS$Race == "Asian",CPS$MetroArea,mean)
sort(tapply(CPS$Race == "Asian",CPS$MetroArea,mean))
head(sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE)))

#Probelm 4
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)
summary(CPS)
table(CPS$Country,CPS$State =="North America")
tapply(CPS$Country =="United States",CPS$MetroArea,mean,na.rm=TRUE)
sort(tapply(CPS$Country =="United States",CPS$MetroArea,sum,na.rm=TRUE))
sort(tapply(CPS$Country =="Brazil",CPS$MetroArea,sum,na.rm=TRUE))





