# rebalancing analysis for 60/40 US stock/bond portfolio
#install.packages('tseries')

# load packages
library(quantmod)
library(tseries)
library(PerformanceAnalytics)
library(TTR)
library(quantstrat)
library(IBrokers)
library(xts)
library(blotter)
library(quantstrat)
library(blotter)
library(quantmod)
library(dplyr)

# download prices
VTI <-get.hist.quote(instrument="VTI",start="2003-12-31",quote="AdjClose",compression="d")
BND <-get.hist.quote(instrument="BND",start="2003-12-31",quote="AdjClose",compression="d")

# choose asset weights
w = c(0.6,0.4) # 60% / 40%

# merge price histories into one dataset
# calculate 1-day % returns
# and label columns
portfolio.prices <-as.xts(merge(VTI,BND))
#head(portfolio.prices)
portfolio.returns <-na.omit(ROC(portfolio.prices,1,"discrete"))
#head(portfolio.returns)
colnames(portfolio.returns) <- c("VTI","BND")

# calculate portfolio total returns
# rebalanced portfolio
portfolio.rebal <-Return.portfolio(portfolio.returns,
                                   rebalance_on="years",
                                   weights=w,wealth.index=TRUE,verbose=TRUE)
#head(portfolio.rebal)

# buy and hold portfolio/no rebalancing
portfolio.bh <-Return.portfolio(portfolio.returns,
                                weights=w,wealth.index=TRUE,verbose=TRUE)

# merge portfolio returns into one dataset
# label columns
portfolios.2 <-cbind(portfolio.rebal$returns,portfolio.bh$returns)
colnames(portfolios.2) <-c("rebalanced","buy and hold")

dev.off()
chart.CumReturns(portfolios.2,
                 wealth.index=TRUE,
                 legend.loc="bottomright",
                 main="Growth of $1 investment",
                 ylab="$")

# Compare return/risk
table.AnnualizedReturns(portfolios.2)

table.Drawdowns((portfolio.bh$returns)) # buy and hold drawdowns

table.Drawdowns((portfolio.rebal$returns))

# Compare portfolio return distribution vs. normal distribution
qqnorm(portfolio.rebal$returns,main="Rebalanced Portfolio")
qqline(portfolio.rebal$returns,col="red")

# Compare rolling 1-year returns
chart.RollingPerformance(portfolios.2,width=252,
                         legend.loc="bottomright",
                         main="Rolling 1yr % returns")










#Donchain VTI



#####
# Make connection with IB and verify:
#tws <- twsConnect(port=7497)  # 7497 is fixed by IBtws


# A TWS Equity object can be instantiated using either
# twsEquity(.) or twsSTK(.):

# Defaults to exch = SMART (IB's Smart routing: https://www.interactivebrokers.com/en/index.php?f=1685&ns=T)
#VTI <- twsEquity("AAPL")   # Apple stock
#qqq <- twsSTK("QQQ")  
#####

currency("USD")
initDate <-'2006-12-31'
startDate <-'2007-01-01'
endDate <-'2018-12-31'
initEq <- 1e6
tradeSize <- initEq/2
symbols <- "VTI"

stock(symbols,currency="USD",multiplier=1)

Sys.setenv(TZ = "UTC")

getSymbols(symbols, 
           from = startDate,
           to = endDate,
           index.class = c("POSIXct", "POSIXt"),
           adjust = T, 
           src = 'yahoo'
)

for(symbol in symbols)
{
  stock(symbol, currency="USD",multiplier=1)
  x <- get(symbol)
  x <- to.daily (x,indexAt='endof',drop.time=FALSE)
  indexFormat(x) <-'%Y-%m-%d'
  colnames(x) <- gsub("x",symbol,colnames(x))
  assign(symbol,x)
}

osFixedDollar <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/ClosePrice,-2)
  return(orderqty)
}

#preliminary charting
myTheme<-chart_theme()
myTheme$col$dn.col<-'red'
myTheme$col$dn.border <-'red'
myTheme$col$up.border <-'green'

par(mfrow=c(1,2))
for(symbol in symbols)
  {plot(chart_Series(get(symbol),name=symbol,theme=myTheme))}
par(mfrow=c(1,1))

chart_Series(VTI, theme = myTheme, name = symbols) #can you chart both. YES!

##### Donchian Turtle System for VTI
#new strategy object for adding the indicators for another Donchian channel
rm.strat(strat.Donchian.VTI)
strat.Donchian.VTI <- "Donchian VTI Port"
rm.strat(strat.Donchian.VTI)
#giving name strat.Donchian.VTI to portoflio and account.
initPortf(strat.Donchian.VTI, symbols = symbols, initDate = initDate) #portfolio startes at inital date
initAcct(strat.Donchian.VTI, portfolios = strat.Donchian.VTI, initDate = initDate, initEq=initEq) #account start and end with inital equity
initOrders(portfolio = strat.Donchian.VTI, initDate = initDate) #orders that go into said portfolio from inital date


for(symbol in symbols)
{
  pos <- round((initEq/first(getPrice(get("VTI")))),-2)
  addPosLimit("Donchian VTI Port", "VTI", initDate, maxpos = pos, minpos = 0)
}

#posval <- initEq/length(VTI)
#for(symbol in symbols){
#  pos <- round((posval/first(getPrice(get(VTI)))),-2)
#addPosLimit('multi.faber',symbol,initDate, maxpos=pos,minpos=-pos)
#}

strategy(strat.Donchian.VTI, store = TRUE)

strat <- getStrategy(strat.Donchian.VTI) #retrieve strategy object
class(strat) #check class
summary(getStrategy(strat.Donchian.VTI))

add.indicator(strategy = strat.Donchian.VTI, 
              name = "DonchianChannel",
              arguments = list(HL = quote(HLC(mktdata)[,2:3]),
                               n = 10),
              label = "DCH10")

add.indicator(strategy = strat.Donchian.VTI, 
              name = "DonchianChannel",
              arguments = list(HL = quote(HLC(mktdata)[,2:3]),
                               n = 20),
              label = "DCH20")

add.indicator(strategy = strat.Donchian.VTI,
              name = "EMA",
              arguments = list(x = quote(Cl(mktdata)), n=20),
              label = "EMA20")

add.indicator(strategy = strat.Donchian.VTI,
              name = "EMA",
              arguments = list(x = quote(Cl(mktdata)), n=180),
              label = "EMA180")


#shut off from previous run

add.signal(strat.Donchian.VTI, name = "sigComparison",
          arguments = list(columns = c("Close", "high.DCH20"), relationship = "gte"),
         label = "Close.gte.high.DCH20")

add.signal(strat.Donchian.VTI, name = "sigComparison",
          arguments = list(columns = c("Close", "low.DCH20"), relationship = "lte"),
         label = "Close.lte.low.DCH20")

add.signal(strat.Donchian.VTI, name = "sigComparison",
           arguments = list(columns = c("Close", "high.DCH10"), relationship = "gte"),
           label = "Close.gte.high.DCH10")

add.signal(strat.Donchian.VTI, name = "sigComparison",
           arguments = list(columns = c("Close", "low.DCH10"), relationship = "lte"),
           label = "Close.lte.low.DCH10")

add.signal(strat.Donchian.VTI, name = "sigComparison",
          arguments = list(columns = c("EMA20", "EMA180"), relationship = "gte"),
         label = "EMA20.gte.EMA180")

add.signal(strat.Donchian.VTI, name = "sigComparison",
          arguments = list(columns = c("EMA20", "EMA180"), relationship = "lte"),
         label = "EMA20.lte.EMA180")

#####

#add.signal(strat.Donchian.VTI, name="sigFormula", 
#                    arguments = list(columns=c("",""),
 #                                      formula = " && ",
  #                                     label="trigger", 
   #                                    cross=FALSE),
    #                  label="EMAFilterLong" ) #Close.gt.high.DCH20"

#sigFormula(label = "EMAFilterLong" , formula='Close.gte.high.DCH20 && EMA20.gte.EMA180' ,cross = FALSE)

#sigFormula(label = "EMAFilterShort" ,formula='Close.lte.low.DCH20 && EMA20.lte.EMA180' ,cross = TRUE)

#add.signal(strat.Donchian.VTI, name="sigFormula", 
 #          arguments = list(columns=c('Close', "EMA.EMA20","EMA.EMA180","high.DCH20"),
  #                          formula = "(EMA.EMA20 > EMA.EMA180) & (Close > high.DCH20)",
   #                         label="trigger", 
    #                        cross=TRUE),
     #      label="EMAFilterLong"  #Close.gt.high.DCH20"
#)

#add.signal(strat.Donchian.VTI, name="sigFormula", 
 #          arguments = list(columns=c('Close', "EMA.EMA20","EMA.EMA180","high.DCH20"),
  #                          formula = "(EMA.EMA180 > EMA.EMA20) & (low.DCH20 > Close)",
   #                         label="trigger", 
    #                        cross=TRUE),
     #      label="EMAFilterShort"     #Close.lt.low.DCH20
#)

#####
#Will work but will throw an error when it goes to BND Data or, vice-versa, with VTI data in mktdata.

add.signal(strat.Donchian.VTI, name="sigFormula", 
           arguments = list(columns=c("VTI.Close", "EMA.EMA20","EMA.EMA180","high.DCH20"),
                            formula = "(EMA.EMA20 > EMA.EMA180) & (VTI.Close > high.DCH20)",
                            label="trigger", 
                            cross=FALSE),
           label="EMAFilterLongVTI"  #Close.gt.high.DCH20"
)

add.signal(strat.Donchian.VTI, name="sigFormula", 
           arguments = list(columns=c("VTI.Close", "EMA.EMA20","EMA.EMA180","high.DCH20"),
                            formula = "(EMA.EMA180 > EMA.EMA20) & (low.DCH20 > VTI.Close)",
                            label="trigger", 
                            cross=FALSE),
           label="EMAFilterShortVTI"     #Close.lt.low.DCH20
)

#add.signal(strat.Donchian.VTI, name="sigFormula", 
#                             arguments = list(columns=c("BND.Close", "EMA.EMA20","EMA.EMA180","high.DCH20"),
#                                              formula = "(EMA.EMA20 > EMA.EMA180) & (BND.Close > high.DCH20)",
#                                              label="trigger", 
#                                              cross=FALSE),
#                             label="EMAFilterLongBND"  #Close.gt.high.DCH20"
#)

#add.signal(strat.Donchian.VTI, name="sigFormula", 
#                             arguments = list(columns=c("BND.Close", "EMA.EMA20","EMA.EMA180","high.DCH20"),
#                                              formula = "(EMA.EMA180 > EMA.EMA20) & (low.DCH20 > BND.Close)",
#                                              label="trigger", 
#                                              cross=FALSE),
#                             label="EMAFilterShortBND"     #Close.lt.low.DCH20
#)


add.rule(strat.Donchian.VTI, name = 'ruleSignal',
         arguments = list(sigcol = "EMAFilterLongVTI", #changed for the addition of the signal
                          sigval=TRUE,
                          orderqty=100,
                          ordertype='market', 
                          orderside='long',
                          osFUN='osMaxPos'), #long or short/null
         type='enter', storefun = FALSE)

#add.rule(strat.Donchian.VTI, name = 'ruleSignal',
#         arguments = list(sigcol = "EMAFilterLongBND", #changed for the addition of the signal
#                          sigval=TRUE,
#                          orderqty=100,
#                          ordertype='market', 
#                          orderside='long',
#                          osFUN='osFixedDollar'), #long or short/null
#         type='enter', storefun = FALSE)

add.rule(strat.Donchian.VTI, name = 'ruleSignal',
         arguments = list(sigcol = "Close.lte.low.DCH10",
                          sigval=TRUE,
                          orderqty='all',
                          ordertype='market',
                          orderside='long'),
         type='exit')

add.rule(strat.Donchian.VTI, name = 'ruleSignal',
         arguments = list(sigcol = "EMAFilterShortVTI", #changed for the addition of the signal 
                          sigval=TRUE,
                          orderqty= 100,
                          ordertype='market',
                          orderside='short',
                          osFUN='osMaxPos'),
         type='enter', storefun = FALSE)

add.rule(strat.Donchian.VTI, name = 'ruleSignal',
         arguments = list(sigcol = "EMAFilterShortVTI", #changed for the addition of the signal 
                          sigval=TRUE,
                          orderqty= -100,
                          ordertype='market',
                          orderside='short',
                          osFUN='osFixedDollar'),
         type='enter', storefun = FALSE)

add.rule(strat.Donchian.VTI, name = 'ruleSignal',
         arguments = list(sigcol = "Close.gte.high.DCH10",
                          sigval=TRUE,
                          orderqty='all',
                          ordertype='market',
                          orderside='short'),
         type='exit')

#no issues
head(mktdata) #market data check
tail(mktdata)

summary(getStrategy(strat.Donchian.VTI))
applyStrategy(strategy = strat.Donchian.VTI, portfolios = strat.Donchian.VTI)

updatePortf("Donchian VTI Port")
updateAcct("Donchian VTI Port")
updateEndEq("Donchian VTI Port")

myTheme2<-chart_theme()
myTheme2$col$dn.col<-'grey'
myTheme2$col$dn.border <-'grey'
myTheme2$col$up.border <-'grey'

chart.Posn("Donchian VTI Port","VTI",theme=myTheme2)
chart.Posn("Donchian VTI Port","VTI",theme=myTheme2, Dates = "2018-01-01::2018-12-31")
chart.Posn("Donchian VTI Port","VTI",theme=myTheme2, Dates = "2010-01-01::2011-12-31")#adding Donchian to chart - HELP, would make this cleaner

pts <- perTradeStats("Donchian VTI Port")






#BND
currency("USD")
initDate <-'2008-12-31'
startDate <-'2009-01-01'
endDate <-'2018-12-31'
initEq <- 1e6
tradeSize <- initEq/2
symbols <- "BND"

stock(symbols,currency="USD",multiplier=1)

Sys.setenv(TZ = "UTC")

getSymbols(symbols, 
           from = startDate,
           to = endDate,
           index.class = c("POSIXct", "POSIXt"),
           adjust = T, 
           src = 'yahoo'
)

for(symbol in symbols)
{
  stock(symbol, currency="USD",multiplier=1)
  x <- get(symbol)
  x <- to.daily (x,indexAt='endof',drop.time=FALSE)
  indexFormat(x) <-'%Y-%m-%d'
  colnames(x) <- gsub("x",symbol,colnames(x))
  assign(symbol,x)
}

osFixedDollar <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
  orderqty <- round(tradeSize/ClosePrice,-2)
  return(orderqty)
}

#preliminary charting
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <-'lightgray'
myTheme$col$up.border <-'lightgray'

par(mfrow=c(1,1))
for(symbol in symbols)
{plot(chart_Series(get(symbol),name=symbol,theme=myTheme))}
par(mfrow=c(1,1))

chart_Series(BND, theme = myTheme, name = symbols) #can you chart both. YES!












##### Donchian Turtle System for BND
#new strategy object for adding the indicators for another Donchian channel
rm.strat(strat.Donchian.BND)
strat.Donchian.BND <- "Donchian BND Port"
rm.strat(strat.Donchian.BND)
#giving name strat.Donchian.BND to portoflio and account.
initPortf(strat.Donchian.BND, symbols = symbols, initDate = initDate) #portfolio startes at inital date
initAcct(strat.Donchian.BND, portfolios = strat.Donchian.BND, initDate = initDate, initEq=initEq) #account start and end with inital equity
initOrders(portfolio = strat.Donchian.BND, initDate = initDate) #orders that go into said portfolio from inital date

#posval <- initEq/length(BND)
#for(symbol in symbols){
#  pos <- round((posval/first(getPrice(get(BND)))),-2)
#addPosLimit('multi.faber',symbol,initDate, maxpos=pos,minpos=-pos)
#}

strategy(strat.Donchian.BND, store = TRUE)

strat <- getStrategy(strat.Donchian.BND) #retrieve strategy object
class(strat) #check class
summary(getStrategy(strat.Donchian.BND))

add.indicator(strategy = strat.Donchian.BND, 
              name = "DonchianChannel",
              arguments = list(HL = quote(HLC(mktdata)[,2:3]),
                               n = 10),
              label = "DCH10")

add.indicator(strategy = strat.Donchian.BND, 
              name = "DonchianChannel",
              arguments = list(HL = quote(HLC(mktdata)[,2:3]),
                               n = 20),
              label = "DCH20")

add.indicator(strategy = strat.Donchian.BND,
              name = "EMA",
              arguments = list(x = quote(Cl(mktdata)), n=20),
              label = "EMA20")

add.indicator(strategy = strat.Donchian.BND,
              name = "EMA",
              arguments = list(x = quote(Cl(mktdata)), n=360),
              label = "EMA360")


#shut off from previous run

add.signal(strat.Donchian.BND, name = "sigComparison",
           arguments = list(columns = c("Close", "high.DCH20"), relationship = "gte"),
           label = "Close.gte.high.DCH20")

add.signal(strat.Donchian.BND, name = "sigComparison",
           arguments = list(columns = c("Close", "low.DCH20"), relationship = "lte"),
           label = "Close.lte.low.DCH20")

add.signal(strat.Donchian.BND, name = "sigComparison",
           arguments = list(columns = c("Close", "high.DCH10"), relationship = "gte"),
           label = "Close.gte.high.DCH10")

add.signal(strat.Donchian.BND, name = "sigComparison",
           arguments = list(columns = c("Close", "low.DCH10"), relationship = "lte"),
           label = "Close.lte.low.DCH10")

add.signal(strat.Donchian.BND, name = "sigComparison",
           arguments = list(columns = c("EMA20", "EMA360"), relationship = "gte"),
           label = "EMA20.gte.EMA360")

add.signal(strat.Donchian.BND, name = "sigComparison",
           arguments = list(columns = c("EMA20", "EMA360"), relationship = "lte"),
           label = "EMA20.lte.EMA360")

#####

#add.signal(strat.Donchian.BND, name="sigFormula", 
#                    arguments = list(columns=c("",""),
#                                      formula = " && ",
#                                     label="trigger", 
#                                    cross=FALSE),
#                  label="EMAFilterLong" ) #Close.gt.high.DCH20"

#sigFormula(label = "EMAFilterLong" , formula='Close.gte.high.DCH20 && EMA20.gte.EMA360' ,cross = FALSE)

#sigFormula(label = "EMAFilterShort" ,formula='Close.lte.low.DCH20 && EMA20.lte.EMA360' ,cross = TRUE)

#add.signal(strat.Donchian.BND, name="sigFormula", 
#          arguments = list(columns=c('Close', "EMA.EMA20","EMA.EMA360","high.DCH20"),
#                          formula = "(EMA.EMA20 > EMA.EMA360) & (Close > high.DCH20)",
#                         label="trigger", 
#                        cross=TRUE),
#      label="EMAFilterLong"  #Close.gt.high.DCH20"
#)

#add.signal(strat.Donchian.BND, name="sigFormula", 
#          arguments = list(columns=c('Close', "EMA.EMA20","EMA.EMA360","high.DCH20"),
#                          formula = "(EMA.EMA360 > EMA.EMA20) & (low.DCH20 > Close)",
#                         label="trigger", 
#                        cross=TRUE),
#      label="EMAFilterShort"     #Close.lt.low.DCH20
#)

#####
#Will work but will throw an error when it goes to BND Data or, vice-versa, with BND data in mktdata.

add.signal(strat.Donchian.BND, name="sigFormula", 
           arguments = list(columns=c("BND.Close", "EMA.EMA20","EMA.EMA360","high.DCH20"),
                            formula = "(EMA.EMA20 > EMA.EMA360) & (BND.Close > high.DCH20)",
                            label="trigger", 
                            cross=FALSE),
           label="EMAFilterLongBND"  #Close.gt.high.DCH20"
)

add.signal(strat.Donchian.BND, name="sigFormula", 
           arguments = list(columns=c("BND.Close", "EMA.EMA20","EMA.EMA360","high.DCH20"),
                            formula = "(EMA.EMA360 > EMA.EMA20) & (low.DCH20 > BND.Close)",
                            label="trigger", 
                            cross=FALSE),
           label="EMAFilterShortBND"     #Close.lt.low.DCH20
)

#add.signal(strat.Donchian.BND, name="sigFormula", 
#                             arguments = list(columns=c("BND.Close", "EMA.EMA20","EMA.EMA360","high.DCH20"),
#                                              formula = "(EMA.EMA20 > EMA.EMA360) & (BND.Close > high.DCH20)",
#                                              label="trigger", 
#                                              cross=FALSE),
#                             label="EMAFilterLongBND"  #Close.gt.high.DCH20"
#)

#add.signal(strat.Donchian.BND, name="sigFormula", 
#                             arguments = list(columns=c("BND.Close", "EMA.EMA20","EMA.EMA360","high.DCH20"),
#                                              formula = "(EMA.EMA360 > EMA.EMA20) & (low.DCH20 > BND.Close)",
#                                              label="trigger", 
#                                              cross=FALSE),
#                             label="EMAFilterShortBND"     #Close.lt.low.DCH20
#)


add.rule(strat.Donchian.BND, name = 'ruleSignal',
         arguments = list(sigcol = "EMAFilterLongBND", #changed for the addition of the signal
                          sigval=TRUE,
                          orderqty=270,
                          ordertype='market', 
                          orderside='long',
                          osFUN='osMaxPos'), #long or short/null
         type='enter', storefun = FALSE)

#add.rule(strat.Donchian.BND, name = 'ruleSignal',
#         arguments = list(sigcol = "EMAFilterLongBND", #changed for the addition of the signal
#                          sigval=TRUE,
#                          orderqty=100,
#                          ordertype='market', 
#                          orderside='long',
#                          osFUN='osFixedDollar'), #long or short/null
#         type='enter', storefun = FALSE)

add.rule(strat.Donchian.BND, name = 'ruleSignal',
         arguments = list(sigcol = "Close.lte.low.DCH10",
                          sigval=TRUE,
                          orderqty='all',
                          ordertype='market',
                          orderside='long'),
         type='exit')

add.rule(strat.Donchian.BND, name = 'ruleSignal',
         arguments = list(sigcol = "EMAFilterShortBND", #changed for the addition of the signal 
                          sigval=TRUE,
                          orderqty=-270,
                          ordertype='market',
                          orderside='short',
                          osFUN='osMaxPos'),
         type='enter', storefun = FALSE)

#add.rule(strat.Donchian.BND, name = 'ruleSignal',
#         arguments = list(sigcol = "EMAFilterShortBND", #changed for the addition of the signal 
#                          sigval=TRUE,
#                          orderqty=-100,
#                          ordertype='market',
#                          orderside='short',
#                          osFUN='osFixedDollar'),
#         type='enter', storefun = FALSE)

add.rule(strat.Donchian.BND, name = 'ruleSignal',
         arguments = list(sigcol = "Close.gte.high.DCH10",
                          sigval=TRUE,
                          orderqty='all',
                          ordertype='market',
                          orderside='short'),
         type='exit')

addPosLimit(portfolio = strat.Donchian.BND,
                symbol = "BND",
                timestamp = initDate,
                maxpos = orderqty)

osFixedDollar <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...) {
    if(!exists("trade_size")) stop("You must set trade_size")
    ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
    orderqty <- round(trade_size/ClosePrice,-2)
    return(orderqty)
}

trade_size <- initEq
orderqty <- 270 #data snooping

#no issues
head(mktdata) #market data check
tail(mktdata)

summary(getStrategy(strat.Donchian.BND))
applyStrategy(strategy = strat.Donchian.BND, portfolios = strat.Donchian.BND)

updatePortf("Donchian BND Port")
updateAcct("Donchian BND Port")
updateEndEq("Donchian BND Port")

myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <-'lightgray'
myTheme$col$up.border <-'lightgray'

chart.Posn("Donchian BND Port","BND",theme=myTheme)
chart.Posn("Donchian BND Port","BND",theme=myTheme, Dates = "2012-01-01::2013-01-01") #adding Donchian to chart - HELP, would make this cleaner
chart.Posn("Donchian BND Port","BND",theme=myTheme, Dates = "2008-01-01::2009-01-01") 

chart.Posn("Donchian BND Port","BND",theme=myTheme, Dates = "2010-01-01::2011-01-01")#

#maxdd and profit to max
tstats <- tradeStats(strat.Donchian.BND)
t(tstats)

library(quantstrat)
library(blotter)
library(quantmod)
library(TTR)
library(dplyr)

tab.trades <- tstats %>% 
  mutate(Trades = Num.Trades, 
         Win.Percent = Percent.Positive, 
         Loss.Percent = Percent.Negative, 
         WL.Ratio = Percent.Positive/Percent.Negative) %>% 
  select(Trades, Win.Percent, Loss.Percent, WL.Ratio)

t(tab.trades)

tab.profit <- tstats %>% 
  select(Net.Trading.PL, Gross.Profits, Gross.Losses, Profit.Factor)
t(tab.profit)

tab.wins <- tstats %>% 
  select(Avg.Trade.PL, Avg.Win.Trade, Avg.Losing.Trade, Avg.WinLoss.Ratio)

t(tab.wins)

pts <- perTradeStats("Donchian BND Port")

rets <- PortfReturns(Account = strat.Donchian.BND)
rownames(rets) <- NULL
charts.PerformanceSummary(rets, colorset = bluefocus)

for(symbol in symbols) {
  pts <- perTradeStats(strat.Donchian.BND, Symbol = "BND")
}
pts


tab.perf <- table.Arbitrary(rets,
                            metrics=c(
                              "Return.cumulative",
                              "Return.annualized",
                              "SharpeRatio.annualized",
                              "CalmarRatio"),
                            metricsNames=c(
                              "Cumulative Return",
                              "Annualized Return",
                              "Annualized Sharpe Ratio",
                              "Calmar Ratio"))
tab.perf

#annualized returns, sharpe ratio
tab.risk <- table.Arbitrary(rets,
                            metrics=c(
                              "StdDev.annualized",
                              "maxDrawdown",
                              "VaR",
                              "ES"),
                            metricsNames=c(
                              "Annualized StdDev",
                              "Max DrawDown",
                              "Value-at-Risk",
                              "Conditional VaR"))
tab.risk

chart.CumReturns(rets,
                 wealth.index=TRUE,
                 legend.loc="bottomright",
                 main="Growth of $1 investment",
                 ylab="$")

retsVTI <- PortfReturns(Account = strat.Donchian.VTI)
rownames(retsVTI) <- NULL

retsBND <- PortfReturns(Account = strat.Donchian.BND)
rownames(retsBND) <- NULL

returnsVTIBND <- cbind(retsBND, retsVTI, portfolio.rebal$returns,portfolio.bh$returns)

dev.off()
charts.PerformanceSummary(returnsVTIBND, geometric = FALSE, wealth.index = TRUE, main = "BND vs VTI vs 60/40 BH vs 60/40 Rebal")



#chart 2/same as above.... better look at % growth.
chart.RollingPerformance(returnsVTIBND,width=252,
                         legend.loc="bottomright",
                         main="Rolling 1yr % returns")





