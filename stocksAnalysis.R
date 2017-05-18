#install.packages("quantmod")
#remotes::install_github("joshuaulrich/quantmod", ref="157_yahoo_502")
library(quantmod)
getSymbols("AAPL")
chartSeries(AAPL,subset="last 9 months")
addSMA(10,col="blue")
addSMA(20,col="red")


getSymbols("AAPL")
chartSeries(AAPL['2007-03-30/2013-12-31'],theme="white")
fastMA<-runMean(AAPL[,4],n=20)
slowMA<-runMean(AAPL[,4],n=60)
addTA(fastMA,on=1,col="blue")
addTA(slowMA,on=1,col="red")
position<-Lag(ifelse(fastMA>=slowMA, 1,0))
return<-ROC(Cl(AAPL))*position
return <- return['2007-03-30/2013-12-31']
eq <- exp(cumsum(return))
addTA(eq)
plot(eq)


sell <- function(x) {
 diff(x) < 0
}
buy <- function(x) {
 diff(x) > 0
}
chartSeries(AAPL['2007-03-30/2013-12-31'],theme="white")
addTA(fastMA ,on=1,col="blue")
addTA(slowMA,on=1,col="red")
addTA(fastMA-slowMA)
addTA(AAPL[buy(position),"AAPL.Low"] - 30, pch = 2, type = "p", col = "red", on = 1)
addTA(AAPL[sell(position),"AAPL.High"] + 30, pch = 6, type = "p", col = "green", on = 1)
