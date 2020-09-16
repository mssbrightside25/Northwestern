library(fma)
library(forecast)
library(fpp)
library(fpp2)

autoplot(hsales)
ggseasonplot(hsales)
ggsubseriesplot(hsales)
gglagplot(hsales)
ggAcf(hsales)

autoplot(usdeaths)
ggseasonplot(usdeaths)
ggsubseriesplot(usdeaths)
gglagplot(usdeaths)
ggAcf(usdeaths)

autoplot(bricksq)
ggseasonplot(bricksq)
ggsubseriesplot(bricksq)
gglagplot(bricksq)
ggAcf(bricksq)

autoplot(sunspotarea)
ggseasonplot(sunspotarea)
ggsubseriesplot(sunspotarea)
gglagplot(sunspotarea)
ggAcf(sunspotarea)

autoplot(gasoline)
ggseasonplot(gasoline)
ggsubseriesplot(gasoline)
gglagplot(gasoline)
ggAcf(gasoline)

train1 <- window(visnights[, "QLDMetro"], end = c(2015, 4))
fc1 <- snaive(train1, h=11) 
fc1.1 <- window(visnights, start=1998)
accuracy(fc1.1, train1)

train2 <- window(visnights[, "QLDMetro"], end = c(2014, 4))
fc2 <- snaive(train2, h=11)
fc2.1 <- window(visnights, start=1998)
accuracy(fc2.1, train2)

train3 <- window(visnights[, "QLDMetro"], end = c(2013, 4))
fc3 <- snaive(train3, h=11)
fc3.1 <- window(visnights, start=1998)
accuracy(fc3.1, train3)

autoplot(dowjones)+autolayer(rwf(dowjones, drift=TRUE, h=40), series="Drift", PI=FALSE)

autoplot(dowjones) +
  autolayer(meanf(dowjones, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(dowjones, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(dowjones, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("DowJones (daily ending in year 2000)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(ibmclose, main="Closing Price of IBM Stock")
  legend("topright", lty=1, col=c("red", "blue"), legend=c("30 day rolling average","60 day rolling average"))

ibmclose.training <- window(ibmclose, start=1, end=300)
ibmclose.test <- window(ibmclose, start=301)
  
ibmclose.prediction_horizon <- length(ibmclose) - 300;
  
# Calc mean, naive, seasonal naive, drift predictions
ibmclose.fit.mean <- meanf(ibmclose.training, h=ibmclose.prediction_horizon)
ibmclose.fit.naive <- naive(ibmclose.training, h=ibmclose.prediction_horizon)
ibmclose.fit.seasonalNaive <- snaive(ibmclose.training, h=ibmclose.prediction_horizon)
ibmclose.fit.drift <- rwf(ibmclose.training, drift=TRUE, h=ibmclose.prediction_horizon)
  
# Plot data with predictions
autoplot(ibmclose.training, main="Closing Price of IBM Stock")
autoplot(ibmclose.fit.mean, plot.conf=FALSE, main="Closing Price of IBM Stock")
lines(ibmclose.fit.naive$mean, col=2)
lines(ibmclose.fit.seasonalNaive$mean, col=3)
lines(ibmclose.fit.drift$mean, col=1)
lines(ibmclose)
legend("topright", lty=1, col=c(4,2,3,1), legend=c("Mean method","Naive method","Seasonal naive method", "Drift"))
  
# Look at error values from our predictions
accuracy(ibmclose.fit.mean, ibmclose.test)
accuracy(ibmclose.fit.naive, ibmclose.test)
accuracy(ibmclose.fit.seasonalNaive, ibmclose.test)
accuracy(ibmclose.fit.drift, ibmclose.test)


autoplot(hsales, main="Sales of new one-family houses in the USA")
ggseasonplot(hsales, col=1:20, pch=19, year.labels=TRUE, year.labels.left=TRUE)
ggsubseriesplot(hsales,ylab="$ million",xlab="Month",xaxt="n", main="Sales of new one-family houses in the USA")
axis(1,at=1:12,labels=month.abb,cex=0.8)
hsales.training <- window(hsales, start=c(1973, 1), end=c(1993, 12)) 
hsales.test <- window(hsales, start=c(1994, 1), end=c(1995, 11))
hsales.prediction_horizon <- 23;
hsales.fit.mean <- meanf(hsales.training, h=hsales.prediction_horizon)
hsales.fit.naive <- naive(hsales.training, h=hsales.prediction_horizon)
hsales.fit.seasonalNaive <- snaive(hsales.training, h=hsales.prediction_horizon)
hsales.fit.drift <- rwf(hsales.training, drift=TRUE, h=hsales.prediction_horizon)
plot(hsales.training, main="Sales of new one-family houses in the USA w/ Forecasts")
plot(hsales.fit.mean, plot.conf=FALSE, main="Sales of new one-family houses in the USA")
lines(hsales.fit.naive$mean, col=2)
lines(hsales.fit.seasonalNaive$mean, col=3)
lines(hsales.fit.drift$mean, col=1)
lines(hsales)
legend("topright", lty=1, col=c(4,2,3,1), legend=c("Mean method","Naive method","Seasonal naive method", "Drift"))
accuracy(hsales.fit.mean, hsales.test)
accuracy(hsales.fit.naive, hsales.test)
accuracy(hsales.fit.seasonalNaive, hsales.test)
accuracy(hsales.fit.drift, hsales.test)


gas1 <- window(gasoline, start=1991)
autoplot(gas1) + xlab("Year") + ylab("million barrels per day")
fit.gas <- tslm(gas1 ~ trend + season)
summary(fit.gas)
fourier.gas <- tslm(gas1 ~ trend + fourier(gasoline, K=2))
summary(fourier.gas)
checkresiduals(fit.gas)
fc <- forecast(fit.gas, newdata=data.frame(fourier(gasoline,K=5,h=12)))

bricksq %>%
  stl(t.window=13, s.window="periodic", robust=FALSE) %>%
  autoplot()
seasonal(bricksq)
seasadj(bricksq)
fit %>% seasadj() %>% naive() %>%
autoplot()
fcast <- stlf(bricksq, method='naive')
bricksq %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()


