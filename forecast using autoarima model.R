##clear workspace
rm(list=ls())

## Set working directory

setwd("C:\\Users\\rishi\\Downloads")
## Load helpful functions


source('C:\\Users\\rishi\\Downloads\\helpfulFunctions.R')
## Loading data
library(gdata)  

data1 <- read.csv(file.choose(), header = T)

myseries <- ts(data1$Revenues, start=c(2010,1), end=c(2015,10), frequency=12)
plot(myseries)

'''
##checking for stationarity with unit root tes

library(forecast)
library(tseries)

adf.test(myseries)

# plot(diff(diff(diff(log(myseries),12), xlab="Year",
# ylab="Annual change in monthly log myseries sales")))

adf.test(diff(log(myseries)))

adf.test(log(myseries))

plot(myseries)



ns <- nsdiffs(myseries)
if(ns > 0) {
  xstar <- diff(myseries,lag=frequency(myseries),differences=ns)
} else {
  xstar <- myseries
}
nd <- ndiffs(xstar)
if(nd > 0) {
  xstar <- diff(xstar,differences=nd)
} 



acf()

ndiffs(myseries)
nsdiffs(myseries)
pacf(myseries)
# autoARIMA

fit <- auto.arima(myseries,seasonal=FALSE)
fit
forecast(myseries,10)
'''


# ARIMA model 

d <- 0:2
p<- 0:6
q <- 0:6

Datamodels <- expand.grid(d=d, p=p,q=q)


germodelAIC <- function(myseries, p,d,q){tsdata <- arima(myseries,order= c(p,d,q)) 
                return(tsdata$aic)
               }

germodelAICsafe <- function(myseries,p,d,q){result = tryCatch({germodelAIC(myseries,p,d,q) },
  
                   error =function(e){Inf})
                }


Datamodels$aic  <- mapply(function(x,y,z)
                         germodelAICsafe(myseries,x,y,z), Datamodels$p, Datamodels$d, Datamodels$q) 



subset(Datamodels, aic == min(aic))

Datamodels <- arima(myseries, order = c(1,2,1))
summary(Datamodels)


forecast(Datamodels,12)

plot(forecast(Datamodels,12))







