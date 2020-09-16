library(TTR)
library(ggplot2)
library(fpp)
library(forecast)
library(gridExtra)
library(RColorBrewer)
library(dplyr)

# NVDA <- read.csv(file = "NVDA.csv", sep = "\t", header=T)
# 
# str(NVDA)
# 
# NVDA <- na.omit(NVDA)
# 
# NVDA$VOL2 <- NVDA$Volume/1000
# 
# #Volumne Lag
# NVDA[2:nrow(NVDA),"VOLt_1"] <- NVDA[1:(nrow(NVDA)-1),]$VOL2
# head(NVDA)
# names(NVDA)
# NVDA_TS <- ts(NVDA$Adj.Close)
# head(NVDA_TS)
# 
# #lag for volume
# #Adj.Close  Volume
# autoplot(NVDA_TS)
# str(NVDA)
# 1261-1201
# 
# NVDA_train <- ts(NVDA[2:1201,])
# NVDA_test <- NVDA[1202:1261,]
# head(NVDA)
# names(NVDA)
# 
# fitAMZN2 <- auto.arima(AMZN2TS[2:1201], xreg=AMZN2$VOLt_1[2:1201],d=2)
# 
# summary(fitAMZN2)
# 
# AMZN2fc<- forecast(fitAMZN2, h=60, xreg = AMZN2$VOLt_1[1202:1261])
# 
# autoplot(AMZN2fc)
# 
# autoplot(AMZN2TS) +
#   autolayer(AMZN2fc, series="T-1", PI=FALSE)+
#   ylab("Daily Close") +
#   guides(colour=guide_legend(title="Model"))+
#   xlim(1000,1300)+
#   ylim(900,2200)


#Read in file
BTC <- read.csv("BTC.csv")

#Change format of Date & order by Date
BTC$Date <- as.Date(BTC$Date, format="%m/%d/%Y")
BTC <- BTC[order(BTC$Date),]

#Add -1 lagged Volume column
numrow <- nrow(BTC) #1827
BTC$lagvol <- 0
BTC[2:1827,]$lagvol <- BTC[1:1826,]$Volume

#Remove unnecessary columns
BTC <- select(BTC, -Open, -High, - Low, -Close, -Volume) #Retains Date, Adj.Close, lagvol columns

#Define ts for Adj. Close Column (Coln #2)
BTC.ts <- ts(BTC[,2], start=1)

#Visualize
autoplot(BTC.ts, facets=TRUE) + ggtitle("Daily Adjusted Closing Stock Prices for BTC") +
  xlab("Number of Days (Starting at 8/05/13 [Day 1] - 8/05/18 [Day 1827])")

train <- window(BTC.ts, end=1370)
test <- window(BTC.ts, start=1371)

ndiffs(train) #1 difference required to make data stationary
nsdiffs(train) #not seasonal

train.model <- auto.arima(train[2:1370], xreg=BTC$lagvol[2:1370], seasonal=FALSE)
summary(train.model)
checkresiduals(train.model)

forecast <- forecast(train.model, h=457, xreg=BTC$lagvol[1371:1827])
model.acc <- accuracy(forecast, test)
model.acc

autoplot(BTC.ts, facets=TRUE, series="Data") + 
  autolayer(forecast, series="test", PI=FALSE) +
  ggtitle("Daily Adjusted Closing Stock Prices for BTC") +
  xlab("Number of Days (Starting at 8/05/13 [Day 1] - 8/05/18 [Day 1827])")