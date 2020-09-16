interestRates <- read.csv('file:///C:/Users/crmo/Desktop/Class/Predict413/Discussion 2/monthly-interest-rates-governmen.csv')
str(interestRates)

iR_data <- ts(interestRates[,2], start= c(1969, 01), end= c(1994, 09),frequency=12)
plot(iR_data)

iR_additive <- decompose(stocks_data, type= "additive")
plot(iR_additive)

iR_multiplicative <- decompose(stocks_data, type="multiplicative")
plot(iR_multiplicative)