##clear workspace
rm(list=ls())

library(forecast)
library(doBy)
library(DescTools)
library(forecast)
library(caret)
library(fracdiff)

data <- read.csv('C:/Practicum/RepByMonthVersion7Sample.csv', stringsAsFactors = F, na.strings = 'NULL')






# Read in the data.

data <- read.csv('RepByMonthVersion7Sample.csv', stringsAsFactors = F, na.strings = 'NULL')

str(data$Month.End.Date)


colnames(data)

grep('Month.End.Date|u5|tenure', names(data), ignore.case=F)
names(data)[grep('Month.End.Date|u5|tenure', names(data), ignore.case=F)]

#  Convert the character dates to date format in R

data$monthEndDate <- as.Date(data[,c(101)], '%m/%d/%Y')
data$u5Date <- as.Date(data[,c(12)], '%m/%d/%Y')
data$year<-format(data$monthEndDate, '%Y')

grep('monthEndDate|u5Date', names(data), ignore.case=F)
names(data)[grep('monthEndDate|u5Date', names(data), ignore.case=F)]

names(data)

str(data$monthEndDate)
str(data$u5Date)

#  Keep records where current date > u5 date or u5 date is blank.

data$present <- ifelse(data[,12]=='', 1, 0)

data1 <- data[data$present==1,]

data$month <- month(data$monthEndDate)









names(data1)
str(data1$present)

grep('tenure', names(data1), ignore.case=T)
names(data1)[grep('tenure', names(data1), ignore.case=T)]

# sort data by Masked.Rep.Id and reverse Tenure.With.HDVest.in.Months

data1 <- data1[order(data1[,1], -data1[,18]),]


# calculate total revenue
str(data1$Advisory.Amount) 

for (j in c('Advisory.Amount', 'Insurance.Amount', 'Securities.Amount')){
  data1[,j] <- as.numeric(as.character(data1[,j])) 
  
}

data1$totalRevenue <- apply(data1[ , c('Securities.Amount', 'Advisory.Amount', 'Insurance.Amount')], 1, sum, na.rm=T)
names(data1)

#  create the year variable.

data1$year <- format(data1$monthEndDate, '%Y')


# Drop missing Masked.Rep.Ids

sum(is.na(data1$Masked.Rep.Id))

data2 <- data1[!is.na(data1$Masked.Rep.Id),]


# Keep Masked.Rep.Ids that have at least 3 years of data.

library(doBy)
checkN  <- summaryBy(.~Masked.Rep.Id, data=data2[,c('Masked.Rep.Id', 'totalRevenue')], FUN=c(n=length))

checkN$keep <- checkN[,2] >= 36

table(checkN$keep)

data3 <- merge(data2, checkN, by='Masked.Rep.Id', all.x=T, all.y= F)

names(data3)

table(data3$keep)

data3 <- data3[data3$keep,]

table(data3$keep)


######################################################################################
#  Loop to automate model creation
#  This can be run faster by using a foreach loop with parallel processing.
#  This loop does the following:
#  (1) drops predictors with no variation across time (i.e. sd=0)
#  (2) drops correlated predictors
#  (3) centers and scales predictors
#  (4) drops near zero variance predictors
#  (5) lags predictors (i.e. predict this period's total revenue based on 
#      last period's predictors)
#  (6) drops predictors that have missing values for some time periods.
#  (7) creates training data from start of series to Dec 2015.
#  (8) runs arima models using auto.arima which determines optimal p,d,q, P,D,Q 
#  (9) determines which regressors to add by running backward stepwise linear regression 
#      of residuals from auto.arima on predictors.
#  (10) tries to run Arima with optmial p,d,q, P,D,Q with added predictors from last step.
#  (11) if Arima in step 10 errors out, go back to auto.arima result without regressors.
#  (12) forecast 2016 based on final model
#  (13) Calculate MAE and MAPE for final model in predicting 2016 
#       (annual forecasted and actual were the sum of 12 months)
#  (14) Append various results to R object named 'results' and save results.
######################################################################################


# Determine unique Masked.Rep.Id values.
repIds <- unique(data3$Masked.Rep.Id)


# Determine which predictors have no variation across time.
checkSd <- summaryBy(.~Masked.Rep.Id, data=data3, FUN = sd, na.rm=T)

write.csv(data3, "data3.csv")

results <- NULL

for (i in repIds[19]){
  sdData <- checkSd[checkSd$Masked.Rep.Id==i,]
  varNames <- names(sdData)[sdData>0 & !is.na(sdData)]
  varNames <- gsub('\\.sd', '', varNames)
  
  tempData <- data3[data3$Masked.Rep.Id==i,varNames]
  
  diffData <- tempData[,-grep('Masked.Rep.Id|totalRevenue', names(tempData))]
  
  corMat <- cor(diffData, use='pairwise.complete.obs')
  corMat[is.na(corMat)] <- 0
  
  badColumns <- findCorrelation(corMat)
  badNames <- colnames(corMat)[badColumns]
  if (length(badColumns)>0) {diffData <- diffData[,!(names(diffData) %in% badNames)] }
  
  preProcOut <- preProcess(diffData, method=c('center', 'scale', 'nzv'), use='pairwise.complete.obs')
  zDiffData <- predict(preProcOut, diffData )
  
  for (j in names(zDiffData)){
    zDiffData$temp <- lag(zDiffData[,j])
    names(zDiffData)[ncol(zDiffData)] <- paste0(j,'_lag', sep='')
  }
  zDiffData <- zDiffData[,grep('_lag', names(zDiffData))]
  zDiffData <- data.frame(zDiffData, tempData$totalRevenue)
  
  colsWithMissing <- (1:ncol(zDiffData))[apply(zDiffData, 2, function(x){!sum(is.na(x))>0})]
  zDiffData <- zDiffData[, colsWithMissing]
  
  # Create the time series data for model training (up to start to Dec. 2015)
  
  #startDate <- min(data3[data3$Masked.Rep.Id==i,'monthEndDate'])
  #endDate <- max(data3[data3$Masked.Rep.Id==i,'monthEndDate'])
  
  #startdate<-data3 %>% group_by(Masked.Rep.Id) %>% top_n(1,monthEndDate )
  #endDate<-data3 %>% group_by(Masked.Rep.Id) %>% top_n(-1,monthEndDate )
  
  
  #startDate <- datamax1<-HD_5years %>% group_by(Masked.Rep.Id) %>% top_n(1,monthEndDate )
  #endDate <- datamax2<-HD_5years %>% group_by(Masked.Rep.Id) %>% top_n(-1,monthEndDate )
  
  
  tsData <- ts(zDiffData[,'tempData.totalRevenue'], 
               start=c(2010,01),#c(Year(startDate),Month(startDate))# ,
               #end=c(Year(endDate),Month(endDate)))#
               end=  c(2015,10), frequency=12)
  
  
  startDate
  
  arimaOut <- auto.arima(tsData)
  
  p <- arimaOut$arma[1]
  d <- arimaOut$arma[6]
  q <- arimaOut$arma[2]
  P <- arimaOut$arma[3]
  D <- arimaOut$arma[7]
  Q <- arimaOut$arma[4]
  
  
  tsData <- ts(zDiffData, 
               start= start=c(2010,01),
               end=c(2015,10), frequency=12  , frequency=12)
  varSelect <- step(lm(residuals~., data=data.frame(tsData[,-grep('totalRevenue',colnames(tsData))], residuals=residuals(arimaOut))))
  varSelect <-   lm(tempData.totalRevenue~ ., data=tsData)  
  
  ArimaOut <- tryCatch( 
    Arima(y=tsData[,'tempData.totalRevenue'],
          order    = c(p,d,q),
          seasonal = c(P,D,Q),
          xreg     = tsData[,names(coefficients(varSelect))[-1]]),
    error=function(e) paste('error'))
  
  tsData2 <- ts(zDiffData, 
                start=c(2016,1),
                end=  c(2016,12), frequency=12)
  
  
  if (ArimaOut=='error'){
    ArimaOut <- withRestarts(arimaOut)
    
    fcast <- forecast(ArimaOut, h=12)
    regressors=''
  } else {
    fcast <- forecast(ArimaOut, h=12, xreg=tsData2[,names(coefficients(varSelect))[-1]])
    regressors=paste0(names(coefficients(varSelect))[-1], collapse=', ')
  }
  
  mae <- DescTools::MAE(x=sum(fcast$mean), ref= sum(tsData2[,'tempData.totalRevenue']))
  mape <- MAPE(x=sum(fcast$mean), ref= sum(tsData2[,'tempData.totalRevenue']))
  
  results <- rbind(results,
                   data.frame(Masked.Rep.Id=i,
                              lower95=sum(fcast$lower[,'95%']), 
                              fcast = sum(fcast$mean),
                              upper95=sum(fcast$upper[,'95%']),
                              mae=mae,
                              mape=mape,
                              regressors=regressors))
}

 save(results, file='results.RData')


# NOTE:
# list named arma, found in the Arima output, has the following definition:	
# A compact form of the specification, as a vector giving the number 
# of AR, MA, seasonal AR and seasonal MA coefficients, plus the period 
# and the number of non-seasonal and seasonal differences.

results

plot(results)

library(xlsx)
write.csv(datamax, "C:/Practicum/datamax.csv")

max