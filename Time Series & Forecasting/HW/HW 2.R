library(fpp)

#load data set "wmurder"
data(wmurders)

#7a: By studying appropriate graphs of the series in R, find an appropriate ARIMA(p,d,q) model for these data
#and 7d: Fit the model using R and examine the residuals. Is the model satisfactory?

  #plots a ts along with its acf, and either its pacf, lagged scatterplot, or spectrum
  tsdisplay(wmurders)
  
  #After reviewing the plots, it's not showing as stationary, so we take the first difference.
  diff_1.wmurders <- diff(wmurders)
  tsdisplay(diff_1.wmurders)
  #The ACF and PACF still show spikes at lag 2, but look better than the previous.
  
  #ADF test (augmented dickey-fuller) - checks that 'x' has a unit root; ADF test p-value <0.05 indicates a stationary series
  adf.test(diff_1.wmurders)
  
  #KPSS test - checks whether 'x' is level or trend stationary; KPSS test p-value <0.05 indicates a non-stationary series
  kpss.test(diff_1.wmurders)
  
  #Both first difference with ADF and KPSS results show as stationary and non-stationary; therefore, more tests need to be completed.
  
  #Take second difference
  diff_2.wmurders <- diff(diff(wmurders))
  
  tsdisplay(diff(wmurders,2))
  adf.test(diff(wmurders,2))
  kpss.test(diff(wmurders,2))
  
  #Now both p-values are <0.05, meaning it's stationary. The ACF and PACF plots show a large spike at 1 which means either p or q need to be 1.
  
  arima_fit<-auto.arima(wmurders)
  summary(arima_fit)
  
  #The arima model chose 1,2,1 - which is what was needed for p and/or q
  
  #Now review the residuals
  tsdisplay(residuals(arima_fit))

  #There are no lags for ACF or PACF.
  
#7b: Should you include a constant in the model? Explain.
  #Constants bring a drift into models, so we wouldn't want a constant for this data since d=2.

#7c: Write this model in terms of the backshift operator.
  # y^t^=phi x y^(t-2)^ + theta x (e-2)

#7e:Forecast three times ahead. Check your forecasts by hand to make sure you know how they have been calculated.
  forecast(arima_fit,h=3)
  
#7f: Create a plot of the series with forecasts and prediction intervals for the next three periods shown.
  plot(forecast(arima_fit,h=3))

#7g: Does auto.arima give the same model you have chosen? If not, which model do you think is better?
  # yes
  

#9a: For the usgdp series: if necessary, find a suitable Box-Cox transformation for the data
  lam <- BoxCox.lambda(usgdp)
  usgdp.box <- BoxCox(usgdp, lambda = lam)
  tsdisplay(usgdp.box)

#9b: fit a suitable ARIMA model to the transformed data using auto.arima()
  usgdp.fit <- auto.arima(usgdp, trace = TRUE, ic ="aic", lambda = lam)
  usgdp.fit

#9c: try some other plausible models by experimenting with the orders chosen
  usgdp.fit2 <- Arima(usgdp, order = c(0,1,1), lambda = lam)
  usgdp.fit3 <- Arima(usgdp, order = c(0,1,3), lambda = lam)
  
  accuracy(usgdp.fit)
  accuracy(usgdp.fit2)
  accuracy(usgdp.fit3)

#9d: Choose what you think is the best model and check the residual diagnostics
  #usgdp.fit had the lowest RMSE/MAE and was considered the best arima model (0,1,2) with drift.
  plot(usgdp.fit$residuals)

#9e: produce forecasts of your fitted model. Do the forecasts look reasonable?
  forecast_1 <- forecast(usgdp.fit, h=10)
  plot(forecast_1)
  # yes this forecast looks reasonable
  
#9f: compare the results with what you would obtain using ets() (with no transformation)
  #fit first
  usgdp.fit4 <- ets(usgdp); usgdp.fit4
  
  #then forecast
  forecast_2 <- forecast(usgdp.fit4, h=10)
  plot(forecast_2)
  

#10a: Consider austourists, the quarterly number of international tourists to Australia for the period 1999-2010. Describe the time plot
  data("austourists")
  tsdisplay(austourists)
  
  #This ts looks multiplicative in seasonality and a growth trend.

#10b: What can you learn from the ACF graph?
  #There's autocorrelation in the lagged obs. ACF has 5 lags (1,4,8,12,16) which is 31.25% - well over the 5% threshold. 
  
#10c: What can you learn from the PACF graph?
  #PACF has 3 lags (1,4,5) which is 18.75% - also well over the 5% threshold. Seasonal differencing is needed.
  
#10d: Produce plots of the seasonally differenced data (equation...). What model do these graphs suggest?
  tourists.decom <- decompose(austourists, "multiplicative")
  austourists.adj <- austourists / tourists.decom$seasonal
  austourists.adj.diff <- diff(austourists.adj)
  tsdisplay(austourists.adj.diff)
  
  #Lags at 1 suggest that p or q need to be 1.

#10e: Does auto.arima() give the same model that you chose? If not, which model do you think is better?
  tourist.fit <- auto.arima(austourists)
  tourist.fit

#10f: Write the model in terms of the backshift operator, then without using the backshift operator.
  # (1???B4)Yt=BY(t???1)+et
  

#18a: install rdatamarket; pick a ts and import
library(rdatamarket)

runnerData <- ts(rdatamarket::dmseries("https://datamarket.com/data/set/248g/mens-olympic-running-records-by-year#!ds=248g!2ltf&display=table")[,1], start=1896, frequency=12)

#18b: Plot graphs of the data, and try to identify an appropriate ARIMA model
  tsdisplay(runnerData)
  
  runnerData.fit <- auto.arima(runnerData)
  runnerData.fit

#18c: Do residual diagnostic checking of your ARIMA model. Are the residuals white noise?  
  tsdisplay(residuals(runnerData.fit))
  
#18d: Use your chosen ARIMA model to forecast the next four years.
  forecast(runnerData.fit,h=48)
  plot(forecast(runnerData.fit,h=48))

#18e: Now try to identify an appropriate ETS model.
  #fit first
  runnerData.fit2 <- ets(runnerData); runnerData.fit2
  
  #find accuracy
  accuracy(runnerData.fit2)
  
  #then forecast
  forecast.runnerData <- forecast(runnerData.fit2, h=48)
  plot(forecast.runnerData)
  
#18f: Do residual diagnostic checking of your ETS model. Are the residuals white noise?
  tsdisplay(residuals(runnerData.fit2))
  
#18g: Use your chosen ETS model to forecast the next four years.
  forecast.runnerData <- forecast(runnerData.fit2, h=48)
  plot(forecast.runnerData)

#18h: Which of the two models do you prefer?
  #I think the arima model did better than the ets, so I'd go with the arima (0,1,0) with drift.


library(ggplot2)    
data("advert")

autoplot(advert[,1:2], facets = TRUE) 

#If facets = FALSE, the ts will be merged together with a color assigned to each variable.

advert %>%
  as.data.frame() %>%
  ggplot(aes(x=advert, y=sales)) +
  ylab("Sales") +
  xlab("Advert") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
  
tslm(sales ~ advert, data=advert)

#Since this dataset has multiple entries for time, it's a multivariate ts and therefore auto.arima cannot be used unless the data is converted into a ts
advert.ts = ts(advert)
arima_fit = auto.arima(advert.ts[,1])
plot(advert.ts[,1])

arima_forecast = forecast(arima_fit, h = 10)
plot(arima_forecast)

checkresiduals(arima_fit, plot = TRUE)

Arima(advert[,"sales"], xreg=advert[,"advert"],
      order=c(0,0,0))

# auto.arima(advert[,"sales"], xreg=advert[,"advert"],
#            order=c(0,0,0))


  