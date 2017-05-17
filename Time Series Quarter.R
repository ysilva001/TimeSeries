
###################
# Title : Census - Median and Average Sale Price of Houses Sold
# Data Source: https://www.census.gov/construction/nrs/historical_data/index.html
# Author : Yesenia Silva
# MSDS664x70_Stat Infer Predictive Analytics
###################

##############Packages#############
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

houseprices <- read_csv("C:/Users/ysilva/Desktop/Statistical Inference and Predictive Analytics/pricereg_custq.csv")
str(houseprices)
summary(houseprices)

#Remove year from dataframe
housepricesnoyear <-  houseprices[ -c(1) ]
summary(housepricesnoyear)

#Create time series of west median vector
ts.west.median <- ts(houseprices$West, start=1963, end=2016, frequency=4)
ts.west.median
mean(ts.west.median)
frequency(ts.west.median)

#plot time series
plot.ts(ts.west.median)

#subset from 1975 to 2016
ts.west.median.subset <- window(ts.west.median, start=1975, end=2016, frequency=4)
plot.ts(ts.west.median.subset)

##fit a simple moving-average model 
opar <- par(no.readonly=TRUE) ##is used to get all the graphical parameters (as a named list).
par(mfrow=c(2,2))
ylim <- c(min(ts.west.median.subset), max(ts.west.median.subset))
plot(ts.west.median.subset, main="Raw time series")
plot(ma(ts.west.median.subset, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(ts.west.median.subset, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(ts.west.median.subset, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

##plot the years by quarters
monthplot(ts.west.median.subset)	##stats	Plots the seasonal components of a time series

#transform ts by calculating the natural log
log.ts.west.median.subset <- log(ts.west.median.subset)
plot.ts(log.ts.west.median.subset)

##to estimate the trend, seasonal and irregular components of this time series lets use the decompose() function
ts.west.median.subset.components <- decompose(ts.west.median.subset)
ts.west.median.subset.components$seasonal
log.ts.west.median.subset.components <- decompose(log.ts.west.median.subset)
plot(ts.west.median.subset.components)
plot(log.ts.west.median.subset.components)


##subest for 2005-2015
ts.west.median.subset2 <- window(ts.west.median, start=2005, end=2015, frequency=4)
plot.ts(ts.west.median.subset)


seasonplot(ts.west.median.subset2,ylab="Median West House Prices", xlab="Year", 
           main="Seasonal plot: median house price",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

#estimate the autocorrelation function
hacf <- Acf(ts.west.median.subset)
plot(hacf, type='l')

##fit a simple exponential smoothing predictive model using the "HoltWinters()
ts.west.median.subset.forecasts <- HoltWinters(ts.west.median.subset, beta=FALSE, gamma=FALSE)
ts.west.median.subset.forecasts
ts.west.median.subset.forecasts$fitted
plot(ts.west.median.subset.forecasts)
ts.west.median.subset.forecasts$SSE #measure the accuracy of the forecasts, sum of squared errors for the in-sample forecast errors
ts.west.median.subset.forecasts2 <- forecast.HoltWinters(ts.west.median.subset.forecasts, h=10)
ts.west.median.subset.forecasts2$residuals
plot.forecast(ts.west.median.subset.forecasts2)
acf(ts.west.median.subset.forecasts2$residuals) #Getting error due to 1975 q1 being NA  - remove 1975 and rerun forecast. 
ts.west.median.subset3 <- window(ts.west.median, start=1976, end=2015, frequency=4)
ts.west.median.subset3.forecasts <- HoltWinters(ts.west.median.subset3, beta=FALSE, gamma=FALSE)
ts.west.median.subset3.forecasts2 <- forecast.HoltWinters(ts.west.median.subset.forecasts, h=20)
plot.forecast(ts.west.median.subset3.forecasts2)
acf(ts.west.median.subset3.forecasts2$residuals, lag.max = 20) ##Still getting error try box.test next
Box.test(ts.west.median.subset3.forecasts2$residuals, lag=20, type="Ljung-Box")
plot(ts.west.median.subset.forecasts2$residuals)
plot.ts(ts.west.median.subset.forecasts2$residuals)  # make a time plot
plotForecastErrors(ts.west.median.subset.forecasts2$residuals) # make a histogram


##########First Part of Paper with only year values################

# #Smoothing with simple moving averages
# ##focus on median sales prices in the west for 1963-2016
# opar <- par(no.readonly=TRUE) ##is used to get all the graphical parameters (as a named list).
# par(mfrow=c(2,2))
# ylim <- c(min(twmedian), max(twmedian))
# plot(twmedian, main="Raw time series")
# plot(ma(twmedian, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
# plot(ma(twmedian, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
# plot(ma(twmedian, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
# par(opar)
# 
# #Smoothing with simple moving averages
# ##focus on median sales prices in the west for 2000-2015
# opar <- par(no.readonly=TRUE) ##is used to get all the graphical parameters (as a named list).
# par(mfrow=c(2,2))
# ylim <- c(min(twmedian.subset), max(twmedian.subset))
# plot(twmedian.subset, main="Raw time series")
# plot(ma(twmedian.subset, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
# plot(ma(twmedian.subset, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
# plot(ma(twmedian.subset, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
# par(opar)
# 
# ##Seasonal decomposition - captures cyclical effects due to the time of year for the west medians subset
# #Using stl() decompose a time series into trend, seasonal, and irregular components by loess smoothing
# ##Additive Model
# fit <- stl(twmedian2, s.window="periodic") ##periodic  forces seasonal effects to be identical across years
# 
# ##ERROR - retry ts with data$column as suggested by stackflow
# twmedian2 <- ts(housepricesnoyear$WestMedian, start=1963, end=2016, frequency=1)
# ##Type of object 
# str(twmedian2) 
# class(twmedian2)
# ##Multiplicative Models
# 
# 
# plot(tusmedian)
# start(thouse)
# end(thouse)
# frequency(thouse)
# summary(thouse)
# 
# #Graphics
# ts(1:10, frequency = 4, start = c(1959, 2))
# 
# by(houseprices$Period, mean)
# 
# data("AirPassengers")
# summary(thouse)
