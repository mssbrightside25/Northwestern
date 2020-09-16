library(forecast)
library(stats)

setwd("C:/Users/crmo/Desktop/Class/Predict413/Discussion 1")
data = read.csv("MANU.csv")

#only keep Adj Close data
keep<-c("Adj.Close")
data<-data[keep]

#datatimeseries = DTS
DTS<-ts(data)

#forecast data using Holtwinters approach
forecast<-HoltWinters(datatimeseries, beta=FALSE, gamma = FALSE)
plot(forecast)

#plot future forecast
future<-forecast(forecast, h=20)
plot(future)

