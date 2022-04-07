#########################################################
#  Assignment 2
#  Areeb Akhter
##########################################################

## load libraries
library(fpp2)

## import data
retail<- read.csv("Hidden")

# combine data series
df = retail
df = ts(df, start=2010, frequency=12)
y = df[,"MRTSSM4453USN"]
View(y)

## classical decomposition analysis 
y %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of US Retail Sales")

## STL decomposition analysis 
y %>% stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot() + xlab("Year") +
  ggtitle("STL decomposition of US Retail Sales of Beer, Wine, and Liquor")

##########################################################
# set training data, test data, out of sample
train = window(y,start=c(2010, 1),end=c(2018, 12), frequency = 12)
train
plot(train)

test =  window(y,start=c(2019, 1),end=c(2020, 7), frequency = 12)
both = window(y,start=c(2010, 1))
h=length(test)
#########################################################
#Forecast

## seasonal naive forecasting
forecast1 = snaive(train, h=h)
summary(forecast1)
autoplot(forecast1)

## forecast against data
autoplot(y) +  
  autolayer(forecast1, series="Seasonal naive", PI=FALSE) +
  xlab("Year") + ylab("Millions of Dollars") +
  ggtitle("Forecasts for US Retail Sales") +
  guides(colour=guide_legend(title="Forecast"))


# holt winter's  method
forecast2 <- hw(train, seasonal="additive", h=h) 
summary(forecast2)
autoplot(forecast2) 

# STL  method using naive
y.stln <- stl(train, t.window=h, s.window="periodic", robust=TRUE)
summary(y.stln)
forecast3 <- forecast(y.stln, method="naive",h=h)
summary(forecast3)
autoplot(forecast3)

# STL  method using ets
y.stle <- stl(train, t.window=h, s.window="periodic", robust=TRUE)
summary(y.stle)
forecast4 <- forecast(y.stle, method="ets",h=h)
summary(forecast4)
autoplot(forecast4)

# ETS  method
y.ets <- ets(train, model="ZZZ") 
summary(y.ets)
forecast5 <- forecast(y.ets, h=h)
summary(forecast5)
autoplot(forecast5)

# trend plus seasonal method: TSLM
tps <- tslm(train ~ trend + season)
summary(tps) 
forecast6 = forecast(tps,h=h)
autoplot(forecast6) 

##########################################################
# accuracy measures
##########################################################

a1 = accuracy(forecast1, test)
a2 = accuracy(forecast2, test)
a3 = accuracy(forecast3, test)
a4 = accuracy(forecast4, test)
a5 = accuracy(forecast5, test)
a6 = accuracy(forecast6, test)

#Combining forecast summary statistics into a table with row names
a.table<-rbind(a1, a2, a3, a4, a5,a6)
row.names(a.table) <- c("seasonalnaive training", "seasonalnaive test", "holtwinters training", "holtwinters test", "stlnaive training", "stlnaive test", "stlets training", "stlets test", "ets training", "ets test", "tslm training", "tslm test")

View(a.table)

# order the table according to MASE
a.table<-as.data.frame(a.table)
a.table<-a.table[order(a.table$MASE),]
View(a.table)

## based on MASE to choose model, forecast 15 periods into the future
y.f <- ets(train, model="ZZZ") 
y.f = forecast(y.f,h=13)
autoplot(y.f) 

## seasonal naive forecasting
forecast1 <- snaive(train, h=h)
autoplot(forecast1)
## forecast against data
autoplot(y) +  
  autolayer(forecast1, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Millions of Dollars") +
  ggtitle("Forecasts for US Retail Sales") +
  guides(colour=guide_legend(title="Forecast"))

# holt winter's  method
forecast2 <- hw(train, seasonal="additive", h=h) 
summary(forecast2)
autoplot(forecast2) 

# STL  method using naive
y.stln <- stl(train, t.window=h, s.window="periodic", robust=TRUE)
summary(y.stln)
forecast3 <- forecast(y.stln, method="naive",h=h)
summary(forecast3)
autoplot(forecast3) 

# STL  method using ets
y.stle <- stl(train, t.window=h, s.window="periodic", robust=TRUE)
summary(y.stle)
forecast4 <- forecast(y.stle, method="ets",h=h)
summary(forecast4)
autoplot(forecast4)

# ETS  method
y.ets <- ets(train, model="ZZZ") 
summary(y.ets)
forecast5 <- forecast(y.ets, h=h)
summary(forecast5)
autoplot(forecast5) 

# trend plus seasonal method: TSLM
tps <- tslm(train ~ trend + season)
summary(tps) 
forecast6 = forecast(tps,h=h)
autoplot(forecast6) 

##########################################################
# accuracy measures
##########################################################
a1 = accuracy(forecast1, test)
a2 = accuracy(forecast2, test)
a3 = accuracy(forecast3, test)
a4 = accuracy(forecast4, test)
a5 = accuracy(forecast5, test)
a6 = accuracy(forecast6, test)
#Combining forecast summary statistics into a table with row names
a.table<-rbind(a1, a2, a3, a4, a5, a6)
row.names(a.table) <- c("seasonalnaive training", "seasonalnaive test", "holtwinters training", "holtwinters test", "stlnaive training", "stlnaive test", "stlets training", "stlets test", "ets training", "ets test", "tslm training", "tslm test")
View(a.table)
# order the table according to MASE
a.table<-as.data.frame(a.table)
a.table<-a.table[order(a.table$MASE),]
View(a.table)

## based on MASE to choose model, forecast 15 periods into the future
y.f <- ets(train, model="ZZZ") 
y.f = forecast(y.f,h=13)
autoplot(y.f) 

## residuals of TSLM
checkresiduals(y.f)
