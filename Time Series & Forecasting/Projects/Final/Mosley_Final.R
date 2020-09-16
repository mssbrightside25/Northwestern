library(forecast) 
library(fpp) 
library(caret) 
library(neuralnet) 
library(randomForest) 
library(psych)
library(VIM) 
library(mice) 
library(ResourceSelection) 
library(corrplot) 
library(party)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(Matrix)
library(gridExtra)

par(mfrow=c(1,1))

# Import data files
features_train = read.csv("dengue_features_train.csv")
features_test = read.csv("dengue_features_test.csv")
labels_train = read.csv("dengue_labels_train.csv")
submissions = read.csv("submission_format.csv")

# Two cities are present: San Juan and Iquitos
# Separate training data for both features and labels for both cities
features_train_sj = features_train %>% filter(city == 'sj')
features_train_iq = features_train %>% filter(city == 'iq')
labels_train_sj = labels_train %>% filter(city == 'sj')
labels_train_iq = labels_train %>% filter(city == 'iq')
features_test_sj = features_test %>% filter(city == 'sj')
features_test_iq = features_test %>% filter(city == 'iq')

full = bind_rows(features_train_sj,features_train_iq,features_test_sj,features_test_iq,labels_train_sj,labels_train_iq)

# Initial EDA
cat('\nSan Juan\n','\t features: ',features_train_sj %>% ncol,'\t entries: ',features_train_sj %>% nrow,'\t labels: ',labels_train_sj %>% nrow)
cat('\nIquitos\n','\t features: ',features_train_iq %>% ncol,'\t entries: ',features_train_iq %>% nrow,'\t labels: ',labels_train_iq %>% nrow)

head(features_train,10)
head(features_test,10)
head(features_train_sj[1:10])
head(features_train_iq[1:10])
head(features_test_sj[1:10])
head(features_test_iq[1:10])

head(labels_train,10)
head(submissions,10)

features_train_sj %>%
  mutate(index = as.numeric(row.names(.))) %>%
  ggplot(aes(index, ndvi_ne)) + 
  geom_line(colour = 'red') +
  ggtitle("Veggie Index")

#--------------------------------------------------------------------------------
features_train_original = read.csv("dengue_features_train_original.csv")
features_train_sj_original = features_train_original %>% filter(city == 'sj')

features_train_sj_original %>%
  mutate(index = as.numeric(row.names(.))) %>%
  ggplot(aes(index, ndvi_ne)) + 
  geom_line(colour = 'red') +
  ggtitle("Veggie Index - Original")
#--------------------------------------------------------------------------------


apply(features_train_sj, 2, function(x) 
  round(100 * (length(which(is.na(x))))/length(x) , digits = 1)) %>%
  as.data.frame() %>%
  `names<-`('Percent of Missing Values')

# # NA's are shown within features data of both train and test
# # Fill NA values with the most recent value used
# features_train_sj$ndvi_ne %<>% na.locf(fromLast = TRUE) 
# features_train_sj$ndvi_nw %<>% na.locf(fromLast = TRUE) 
# features_train_sj$ndvi_se %<>% na.locf(fromLast = TRUE) 
# features_train_sj$ndvi_sw %<>% na.locf(fromLast = TRUE) 
# features_train_sj$precipitation_amt_mm %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_air_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_avg_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_dew_point_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_max_air_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_min_air_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_precip_amt_kg_per_m2 %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_relative_humidity_percent %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_sat_precip_amt_mm %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_specific_humidity_g_per_kg %<>% na.locf(fromLast = TRUE) 
# features_train_sj$reanalysis_tdtr_k %<>% na.locf(fromLast = TRUE) 
# features_train_sj$station_avg_temp_c %<>% na.locf(fromLast = TRUE) 
# features_train_sj$station_diur_temp_rng_c %<>% na.locf(fromLast = TRUE) 
# features_train_sj$station_max_temp_c %<>% na.locf(fromLast = TRUE) 
# features_train_sj$station_min_temp_c %<>% na.locf(fromLast = TRUE) 
# features_train_sj$station_precip_mm %<>% na.locf(fromLast = TRUE) 


apply(features_train_iq, 2, function(x) 
  round(100 * (length(which(is.na(x))))/length(x) , digits = 1)) %>%
  as.data.frame() %>%
  `names<-`('Percent of Missing Values')

# features_train_iq$ndvi_ne %<>% na.locf(fromLast = TRUE)
# features_train_iq$ndvi_nw %<>% na.locf(fromLast = TRUE)
# features_train_iq$ndvi_se %<>% na.locf(fromLast = TRUE)
# features_train_iq$ndvi_sw %<>% na.locf(fromLast = TRUE)
# features_train_iq$precipitation_amt_mm %<>% na.locf(fromLast = TRUE)
# features_train_iq$reanalysis_air_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_iq$reanalysis_avg_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_iq$reanalysis_dew_point_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_iq$reanalysis_max_air_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_iq$reanalysis_min_air_temp_k %<>% na.locf(fromLast = TRUE) 
# features_train_iq$reanalysis_precip_amt_kg_per_m2 %<>% na.locf(fromLast = TRUE) 
# features_train_iq$reanalysis_relative_humidity_percent %<>% na.locf(fromLast = TRUE) 
# features_train_iq$reanalysis_sat_precip_amt_mm %<>% na.locf(fromLast = TRUE) 
# features_train_iq$reanalysis_specific_humidity_g_per_kg %<>% na.locf(fromLast = TRUE) 
# features_train_iq$reanalysis_tdtr_k %<>% na.locf(fromLast = TRUE) 
# features_train_iq$station_avg_temp_c %<>% na.locf(fromLast = TRUE) 
# features_train_iq$station_diur_temp_rng_c %<>% na.locf(fromLast = TRUE) 
# features_train_iq$station_max_temp_c %<>% na.locf(fromLast = TRUE) 
# features_train_iq$station_min_temp_c %<>% na.locf(fromLast = TRUE) 
# features_train_iq$station_precip_mm %<>% na.locf(fromLast = TRUE) 



# features_test_sj$ndvi_ne %<>% na.locf(fromLast = TRUE)
# features_test_iq$ndvi_ne %<>% na.locf(fromLast = TRUE)

# Remove week_start_date since this isn't needed as a feature
features_train_sj %<>% dplyr::select(-week_start_date)
features_train_iq %<>% dplyr::select(-week_start_date)

# Review labels
cat('\nSan Juan\n','\t total mean: ',labels_train_sj$total_cases %>% mean(),'\t total variance: ',labels_train_sj$total_cases %>% var())
cat('\nIquitos\n','\t total mean: ',labels_train_iq$total_cases %>% mean(),'\t total variance: ',labels_train_iq$total_cases %>% var())

# Histogram of total cases
rbind(labels_train_iq, labels_train_sj) %>% 
  ggplot(aes(x = total_cases,fill = ..count..)) + 
  geom_histogram(bins = 12) + ggtitle('Total Cases of Dengue') +
  scale_y_continuous(breaks = seq(0,700,100)) + facet_wrap(~city)

#--------------------------------------------------------------------------------------------------------------

# Correlations bewteen sj and iq training features
features_train_sj %<>% mutate('total_cases' = labels_train_sj$total_cases)
features_train_iq %<>% mutate('total_cases' = labels_train_iq$total_cases)

features_train_sj %>% 
  dplyr::select(-city, -year, -weekofyear) %>%
  cor(use = 'pairwise.complete.obs') -> M1

features_train_iq %>%
  dplyr::select(-city, -year, -weekofyear) %>%
  cor(use = 'pairwise.complete.obs') -> M2

# see the SJ correlations as barplot
sort(M1[21,-21]) %>%  
  as.data.frame %>% 
  `names<-`('correlation') %>%
  ggplot(aes(x = reorder(row.names(.), -correlation), y = correlation, fill = correlation)) + 
  geom_bar(stat='identity', colour = 'black') + scale_fill_continuous(guide = FALSE) + scale_y_continuous(limits =  c(-.15,.25)) +
  labs(title = 'San Jose Correlations', x = NULL, y = NULL) + coord_flip() -> cor1

# see the IQ correlations as barplot
sort(M2[21,-21]) %>%  
  as.data.frame %>% 
  `names<-`('correlation') %>%
  ggplot(aes(x = reorder(row.names(.), -correlation), y = correlation, fill = correlation)) + 
  geom_bar(stat='identity', colour = 'black') + scale_fill_continuous(guide = FALSE) + scale_y_continuous(limits =  c(-.15,.25)) +
  labs(title = 'Iquitos Correlations', x = NULL, y = NULL) + coord_flip() -> cor2

grid.arrange(cor1, cor2, nrow = 1)

#--------------------------------------------------------------------------------------------------------------
# From barplot corr's, reanalysis_specific_humidity_g_per_kg and reanalysis_dew_point_temp_k strongly correlate, as well
# as station_avg_temp_c and station_max_temp_c

corr_features =c('reanalysis_specific_humidity_g_per_kg',
             'reanalysis_dew_point_temp_k',
             'station_avg_temp_c',
             'station_max_temp_c')

full$total_cases = 0
full$total_cases[1:1456] = features_train

train_sj_total <- ts(features_train_sj$total_cases, frequency=52, start=c(1990,04,30)) 
train_iq_total <- ts(features_train_iq$total_cases, frequency=52, start=c(2000,07,01)) 

plot(train_sj_total)
plot(train_iq_total)

acf(train_sj_total)
pacf(train_sj_total) #6 lags

acf(train_iq_total)
pacf(train_iq_total) #3 lags

tsdisplay(train_sj_total)
tsdisplay(diff(train_sj_total,1))

tsdisplay(train_iq_total)
tsdisplay(diff(train_iq_total,1))

AA_SJ <- auto.arima(train_sj_total, xreg = features_train_sj[corr_features])
AA_SJ

AA_IQ <- auto.arima(train_iq_total, xreg = features_train_iq[corr_features])
AA_IQ

#-------------------------------------------------------------------------------------------
# Try seasonal arima with xreg

sj_fit <- Arima(train_sj_total, order=c(1,1,1), seasonal=c(0,0,0), xreg = features_train_sj[corr_features]) 
sj_arima=forecast(sj_fit, h = 260, xreg = features_test_sj[corr_features])
tsdisplay(residuals(sj_fit))
Box.test(residuals(sj_arima), lag=130, type="Ljung") #lag=130 give a 0.88 p-value
accuracy(sj_arima)
# accuracy:                     ME     RMSE     MAE   MPE  MAPE  MASE         ACF1
#           Training set 0.002297732 13.40983 8.08413 NaN  Inf 0.2212841 0.0005294263

plot(forecast(sj_fit, h = 260, xreg = features_test_sj[corr_features]))

iq_fit <- Arima(train_iq_total, order=c(0,1,2), seasonal=c(0,0,1), xreg = features_train_iq[corr_features]) 
iq_arima=forecast(iq_fit, h = 156, xreg = features_test_iq[corr_features])
tsdisplay(residuals(iq_fit))
Box.test(residuals(iq_arima), lag=130, type="Ljung") #lag=130 give a 0.95 p-value
accuracy(iq_arima)
# accuracy:                   ME       RMSE    MAE    MPE  MAPE      MASE       ACF1
#           Training set 0.01150672 7.181565 3.883022 NaN  Inf    0.4113296 -0.003559748

plot(forecast(iq_fit, h = 156, xreg = features_test_iq[corr_features]))

sj_submission <- data.frame(submissions[1:260,-4], total_cases = round(sj_arima$mean))
iq_submission <- data.frame(submissions[261:416,-4], total_cases =round(iq_arima$mean))
xreg_soln <- bind_rows(sj_submission,iq_submission)

# Couldn't submit due to NA's; found some helpful code to remove them.
# a <- xreg_soln[!is.na(xreg_soln)]

write.csv(xreg_soln, file = 'predict_XREG_soln.csv', row.names = F)
# score: 33.5024

#-------------------------------------------------------------------------------------------
# Try seasonal arima without xreg

sj_fit <- Arima(train_sj_total, order=c(1,1,1), seasonal=c(0,0,0)) 
sj_arima=forecast(sj_fit, h = 260)
tsdisplay(residuals(sj_fit))
Box.test(residuals(sj_arima), lag=130, type="Ljung") #lag=130 give a 0.89 p-value
accuracy(sj_arima)
# accuracy:                     ME       RMSE      MAE    MPE  MAPE      MASE     ACF1
#            Training set 0.001467535 13.42959   8.047587 NaN  Inf   0.2202839 0.001092614

plot(forecast(sj_fit, h = 260))

iq_fit <- Arima(train_iq_total, order=c(0,1,2), seasonal=c(0,0,1)) 
iq_arima=forecast(iq_fit, h = 156)
tsdisplay(residuals(iq_fit))
Box.test(residuals(iq_arima), lag=130, type="Ljung") #lag=130 give a 0.95 p-value
accuracy(iq_arima)
# accuracy:                  ME       RMSE      MAE   MPE  MAPE      MASE      ACF1
#           Training set 0.01292348 7.191928 3.841351 NaN  Inf   0.4069155 -0.002950598

plot(forecast(iq_fit, h = 156))

sj_submission2 <- data.frame(submissions[1:260,-4], total_cases = round(sj_arima$mean))
iq_submission2 <- data.frame(submissions[261:416,-4], total_cases =round(iq_arima$mean))
regArima_soln <- bind_rows(sj_submission2,iq_submission2)

# Couldn't submit due to NA's; found some helpful code to remove them.
# b <- regArima_soln[!is.na(regArima_soln)]

write.csv(xreg_soln, file = 'predict_NoXREG_soln.csv', row.names = F)
# score:

#-------------------------------------------------------------------------------------------
# Try randomForest

sj_forest <- randomForest(total_cases ~ ndvi_ne + ndvi_nw + ndvi_se + ndvi_sw + precipitation_amt_mm +	
  reanalysis_air_temp_k + reanalysis_avg_temp_k + reanalysis_dew_point_temp_k + reanalysis_max_air_temp_k + 
  reanalysis_min_air_temp_k + reanalysis_precip_amt_kg_per_m2 + reanalysis_relative_humidity_percent + 
  reanalysis_sat_precip_amt_mm + reanalysis_specific_humidity_g_per_kg + reanalysis_tdtr_k + station_avg_temp_c +
  station_diur_temp_rng_c + station_max_temp_c + station_min_temp_c + station_precip_mm, data = features_train_sj,
  na.action=na.exclude)
sj_forest

print(sj_forest)
# Mean of squared residuals: 1953.313

sj_rf_prediction <- predict(object=sj_forest, features_test_sj)


iq_forest <- randomForest(total_cases ~ ndvi_ne + ndvi_nw + ndvi_se + ndvi_sw + precipitation_amt_mm +	
  reanalysis_air_temp_k + reanalysis_avg_temp_k + reanalysis_dew_point_temp_k + reanalysis_max_air_temp_k + 
  reanalysis_min_air_temp_k + reanalysis_precip_amt_kg_per_m2 + reanalysis_relative_humidity_percent + 
  reanalysis_sat_precip_amt_mm + reanalysis_specific_humidity_g_per_kg + reanalysis_tdtr_k + station_avg_temp_c +
  station_diur_temp_rng_c + station_max_temp_c + station_min_temp_c + station_precip_mm, data = features_train_iq, 
  na.action=na.exclude)
iq_forest

print(iq_forest)
# Mean of squared residuals: 110.3924

iq_rf_prediction <- predict(object=iq_forest, features_test_iq)


forest_submission <- data.frame(submissions[1:260,-4], total_cases = round(sj_rf_prediction))
# a <- forest_submission[!is.na(forest_submission)]

forest_submission2 <- data.frame(submissions[261:416,-4], total_cases =round(iq_rf_prediction))
# b <- forest_submission2[!is.na(forest_submission2)]

forest_soln <- bind_rows(forest_submission,forest_submission2)
# c <- forest_soln[!is.na(forest_soln)]

write.csv(forest_soln, file = 'predict_forest_soln.csv', row.names = F)
# score: can't score due to "NA's" showing up in the results....nothing seems to work to remove it. Even tried manuallys
# and the site won't let me submit due to "IDs for submission are not correct."

#-------------------------------------------------------------------------------------------
# Try neural network (nnetar)

sj_nn <- nnetar(train_sj_total,repeats=25, size=12, decay=0.1,linout=TRUE)
sj_nn
sj_forecast=forecast(sj_nn,h=260)
sj_forecast
accuracy(sj_forecast)
# accuracy:                 ME        RMSE     MAE     MPE  MAPE     MASE     ACF1
#           Training set 0.02393724 7.846459 5.525037 -Inf  Inf   0.151235 -0.02095409

plot(forecast(sj_nn,h=260))

iq_nn <- nnetar(train_iq_total,repeats=25, size=18, decay=0.1,linout=TRUE)
iq_nn
iq_forecast=forecast(iq_nn,h=156)
iq_forecast
accuracy(iq_forecast)
# accuracy:                   ME       RMSE      MAE    MPE  MAPE      MASE       ACF1
#           Training set -0.01376942 3.680128 2.597936 -Inf  Inf    0.2752001 -0.04086611

plot(forecast(iq_nn,h=156))

nn_submission <- data.frame(submissions[1:260,-4], total_cases = round(sj_forecast$mean))
nn_submission2 <- data.frame(submissions[261:416,-4], total_cases = round(iq_forecast$mean))
nn_soln <- bind_rows(nn_submission,nn_submission2)

# Couldn't submit due to NA's; found some helpful code to remove them.
# d <- nn_soln[!is.na(nn_soln)]

write.csv(nn_soln, file = 'predict_nn_soln.csv', row.names = F)
# score: 36.5938
