library(fpp2)
library(ggplot2)
library(readr)

inventory <- read_csv("BusinessInv.csv")
head(inventory,20)

inventory_ts = ts(inventory[,2], frequency = 4,start = c(1955,4))

autoplot(inventory_ts, geom = "line", ylab = "Business Inventory", xlab = "Year", 
         main = "Quarterly change in business inventories (billions) 1955-1969")

ets1 <- ets(inventory_ts,model="AAA")
ets1.pred <- forecast(ets1,h=12)

autoplot(ets1.pred,ylab = "Business Inventory", xlab = "Year", 
         main = "Forecast for AAA")

accuracy(ets1)

arima1 <- auto.arima(inventory_ts)
arima1.pred <- forecast(arima1,h=12)

autoplot(arima1.pred,ylab = "Business Inventory", xlab = "Year", 
         main = "Auto ARIMA Forecast")

accuracy(arima1)

inventory_ts.train <- subset(inventory_ts,end = length(inventory_ts)-6)
inventory_ts.test <- subset(inventory_ts,start = length(inventory_ts)-5)

ets2 <- ets(inventory_ts.train,model="AAA")
ets2.pred <- forecast(ets2,h=6)

arima2 <- auto.arima(inventory_ts.train)
arima2.pred <- forecast(arima2,h=6)

accuracy(ets2.pred,inventory_ts.test)
accuracy(arima2.pred,inventory_ts.test)

autoplot(inventory_ts) +
  autolayer(ets2.pred,series="ETS Forecast") +
  autolayer(arima2.pred,series="Auto Arima Forecast") +
  xlab("Year") +
  ylab("Business Inventory") +
  ggtitle("Quarterly change in business inventories (billions) 1955-1969")


