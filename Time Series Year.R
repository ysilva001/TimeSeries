###################
# Title : Census - Median and Average Sale Price of Houses Sold (Years)
# Data Source: https://www.census.gov/construction/nrs/historical_data/index.html
# Author : Yesenia Silva
# MSDS664x70_Stat Infer Predictive Analytics
###################

library(RColorBrewer) #Colors
library(readr)  ##Read the file
library(dplyr) ##Data Manipulation
library(tidyr) ##Data Manipulation used in conjunction with dplyr
library(ggplot2) ##Visuals
library(corrplot)  ##graphical display of a correlation matrix, confidence interval
library(RColorBrewer) #Colors
library(stats) #creates a time-series object
library(graphics) #plots a time series
library(forecast) #fits a simple moving-average time series model
library(TTR) #used to smooth time series data using a simple moving average

houseprices <- read_csv("C:/Users/ysilva/Desktop/Statistical Inference and Predictive Analytics/pricereg_cust.csv")
str(houseprices)
summary(houseprices)

#Remove year from dataframe
housepricesnoyear <-  houseprices[ -c(1) ]
summary(housepricesnoyear)

#Create time series objects
tusmedian <- ts(housepricesnoyear[,1], start=1963, end=2016, frequency=1)
tnemedian <- ts(housepricesnoyear[,2], start=1963, end=2016, frequency=1)
tmwmedian <- ts(housepricesnoyear[,3], start=1963, end=2016, frequency=1)
tsmedian <- ts(housepricesnoyear[,4], start=1963, end=2016, frequency=1)
twmedian <- ts(housepricesnoyear[,5], start=1963, end=2016, frequency=1)
tusaverage <- ts(housepricesnoyear[,6], start=1963, end=2016, frequency=1)
tneaverage <- ts(housepricesnoyear[,7], start=1963, end=2016, frequency=1)
tmwaverage <- ts(housepricesnoyear[,8], start=1963, end=2016, frequency=1)
tsaverage <- ts(housepricesnoyear[,9], start=1963, end=2016, frequency=1)
twaverage <- ts(housepricesnoyear[,10], start=1963, end=2016, frequency=1)

#plot median
plot(cbind(tusmedian,tnemedian,tmwmedian, tsmedian, twmedian))
#plot average
plot(cbind(tusaverage,tneaverage,tmwaverage,tsaverage,twaverage))

##Interested in subsetting to 2000- 2015 for US average/Median and the west since I live on the west
tusmedian.subset <- window(tusmedian, start=2000, end=2015, frequency=1)
tusaverage.subset <- window(tusmedian, start=2000, end=2015, frequency=1)
twmedian.subset <- window(tusmedian, start=2000, end=2015, frequency=1)
twaverage.subset <- window(tusmedian, start=2000, end=2015, frequency=1)
plot(cbind(tusmedian.subset,twmedian.subset,tusaverage.subset,twaverage.subset))

#Smoothing with simple moving averages
##focus on median sales prices in the west for 1963-2016
opar <- par(no.readonly=TRUE) ##is used to get all the graphical parameters (as a named list).
par(mfrow=c(2,2))
ylim <- c(min(twmedian), max(twmedian))
plot(twmedian, main="Raw time series")
plot(ma(twmedian, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(twmedian, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(twmedian, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

#Smoothing with simple moving averages
##focus on median sales prices in the west for 2000-2015
opar <- par(no.readonly=TRUE) ##is used to get all the graphical parameters (as a named list).
par(mfrow=c(2,2))
ylim <- c(min(twmedian.subset), max(twmedian.subset))
plot(twmedian.subset, main="Raw time series")
plot(ma(twmedian.subset, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(twmedian.subset, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(twmedian.subset, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

##Seasonal decomposition - captures cyclical effects due to the time of year for the west medians subset
#Using stl() decompose a time series into trend, seasonal, and irregular components by loess smoothing
##Additive Model
fit <- stl(twmedian2, s.window="periodic") ##periodic  forces seasonal effects to be identical across years

##ERROR - retry ts with data$column as suggested by stackflow
twmedian2 <- ts(housepricesnoyear$WestMedian, start=1963, end=2016, frequency=1)
##Type of object 
str(twmedian2) 
class(twmedian2)
##Multiplicative Models


plot(tusmedian)
start(thouse)
end(thouse)
frequency(thouse)
summary(thouse)

#Graphics
ts(1:10, frequency = 4, start = c(1959, 2))

by(houseprices$Period, mean)

data("AirPassengers")
summary(thouse)
