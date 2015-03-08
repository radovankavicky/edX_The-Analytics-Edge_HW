#Problem 1

IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
Boeing = read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

Stock=rbind(IBM,GE,CocaCola,ProcterGamble,Boeing)
Stock$Date[which.min(Stock$Date)]
Stock$Date[which.max(Stock$Date)]

mean(IBM$StockPrice)
GE$StockPrice[which.min(GE$StockPrice)]
CocaCola$StockPrice[which.max(CocaCola$StockPrice)]

median(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)


#Problem 2
plot(CocaCola$Date,CocaCola$StockPrice,type="l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue",lty=2)
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-07-01")), lwd=2)

#Problem 3
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="yellow")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="black")
abline(v=as.Date(c("1997-10-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

#Problem 4
tapply(IBM$StockPrice,months(IBM$Date),mean)
mean(IBM$StockPrice)
tapply(GE$StockPrice,months(GE$Date),mean)
tapply(CocaCola$StockPrice,months(CocaCola$Date),mean)