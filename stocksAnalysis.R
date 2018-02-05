#install.packages("quantmod")
#remotes::install_github("joshuaulrich/quantmod", ref="157_yahoo_502")
library(quantmod)
library(BatchGetSymbols)

Sys.setenv(TZ="Singapore")

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

##TA
if(weekdays(Sys.Date())=="Monday"){
    ytd=Sys.Date()-3
    ytd2=Sys.Date()-4
}else if(weedays(Sys.Date())=="Tuesday"){
    ytd=Sys.Date()-1
    ytd2=Sys.Date()-3
 else{
    ytd=Sys.Date()-1
    ytd2=Sys.Date()-2
}
allsgx<-BatchGetSymbols(paste0(sgx$Symbol,".SI"),first.date = ytd2,last.date = ytd)
ows<-which(sapply(unique(allsgx$df.tickers$ticker),function(x){
    y<-(allsgx$df.tickers[allsgx$df.tickers$ticker==x,])
    y$price.open[2]>y$price.open[1] && y$price.close[2]>y$price.close[1] && y$price.open[1]!=y$price.close[1]
    }))

ham<-which(sapply(unique(allsgx$df.tickers$ticker),function(x){
    y<-(allsgx$df.tickers[allsgx$df.tickers$ticker==x,])
    (abs(y$price.high[2]-y$price.close[2])/y$price.close[2])<0.01 && (abs(y$price.open[2]-y$price.close[2])/y$price.close[2])<0.01 && (abs(y$price.close[2]-y$price.low[2])/y$price.close[2])>0.02 && y$price.close[2]>0.2
    }))

