# R-resource-learning
# Missing dates
setwd("D:/R")
library(zoo)
library(dplyr)
library(data.table)
library(forecast)
#install.packages("data.table")
data <- read.csv("D:/R/test_date.csv" ,stringsAsFactors=FALSE)
data$date <- as.Date(data$date, format = "%m/%d/%Y")
alldates1 <- data.table(date=seq.Date(min(data$date), max(data$date), 
                                     by="day"))
dt1 <- merge(data, alldates1, by="date", all=TRUE)
dt1$year_mon <- as.yearmon(dt1$date, format="%m%Y")
dt1$year_mon <- format(dt1$year_mon,"%b%Y")
#dt$net[which(is.na(dt$net))] <- 0 # assigning "0" value to NA's
dt2<-summarise(group_by(dt1, year_mon), net=sum(net, na.rm=T))

dt3 <-summarise(group_by(dt1,date, year_mon), net=sum(net, na.rm=T))

sum(dt$net, na.rm=T)

dt3$net[which(dt3$net == 0)] <- NA # assigning "0" value to NA's
data.sale <- ts(dt3$net, start=c(5, 01), frequency=1)

#cleaning the missing outliners & fill the missing values
data.sale1 <- round(tsclean(data.sale, replace.missing = TRUE, lambda = NULL))

fitarima <- auto.arima(data.sale)
plot(forecast(fitarima, 40))
predict <- data.frame(forecast(fitarima, 40))
predict1 <- data.frame(forecast(fitarima, 40))
