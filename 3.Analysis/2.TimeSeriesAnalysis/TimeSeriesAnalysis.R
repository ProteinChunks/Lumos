### Time Series Analysis and Predictions
## Representatively extract the amount of insolation data in Gangnam area for the test
# Get the Gangnam weather data
all_df_kangnam<-read.csv("kangnam_climate.csv")
View(all_df_kangnam)
head(all_df_kangnam)

kangnam_w_list <- list()
for (i in 1:5)
{
  kangnam_w_list[[i]] <- all_df_kangnam[,i]  
}

kangnam_w_list[[1]] # temp
kangnam_w_list[[2]] # rain
kangnam_w_list[[3]] # humid
kangnam_w_list[[4]] # pm10
kangnam_w_list[[5]] # solar

## Processing with time series data and separating train and test sets
# temp, rain, humit, pm10, solar
k_ts_list <- list()
k_ts_train_list <- list()
k_ts_test_list <- list()

for(i in 1:5){
  k_ts_list[[i]] <- ts(kangnam_w_list[[i]], frequency = 12, start(2010,1))
  k_ts_train_list[[i]] <- window(k_ts_list[[i]], start = 1, end = 7.99)
  k_ts_test_list[[i]] <- window(k_ts_list[[i]], start = 8)
}

k_ts_list[[1]]
k_ts_train_list[[1]]
k_ts_test_list[[1]]

## Time series decomposition
decomp_weather_ts <- list() # Weather time series decomposition list

for(i in 1:5){
  decomp_weather_ts[[i]] <- decompose(k_ts_train_list[[i]])
}

plot(decomp_weather_ts[[1]]) # tmp 
plot(decomp_weather_ts[[2]]) # rain
plot(decomp_weather_ts[[3]]) # humid
plot(decomp_weather_ts[[4]]) # pm10
plot(decomp_weather_ts[[5]]) # solar


## Seasonal Variation Visualization and checked the variation
library(forecast)

seasonplot(k_ts_train_list[[1]], year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) # tmp
monthplot(k_ts_train_list[[1]])

seasonplot(k_ts_train_list[[2]], year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) # rain
monthplot(k_ts_train_list[[2]])

seasonplot(k_ts_train_list[[3]], year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) # humid
monthplot(k_ts_train_list[[3]])

seasonplot(k_ts_train_list[[4]], year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) # pm10
monthplot(k_ts_train_list[[4]])

seasonplot(k_ts_train_list[[5]], year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19) # solar
monthplot(k_ts_train_list[[5]])


## Predict and visualize with seven predictive models

# Temperature data
models_tmp <- list (
  mod_arima = auto.arima(k_ts_train_list[[1]], ic='aicc', stepwise=FALSE),
  mod_exponential = ets(k_ts_train_list[[1]], ic='aicc', restrict=FALSE),
  mod_neural = nnetar(k_ts_train_list[[1]], p=12, size=25),
  mod_tbats = tbats(k_ts_train_list[[1]], ic='aicc', seasonal.periods=12),
  mod_bats = bats(k_ts_train_list[[1]], ic='aicc', seasonal.periods=12),
  mod_stl = stlm(k_ts_train_list[[1]], s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(k_ts_train_list[[1]])
)
forecasts <- lapply(models_tmp, forecast, 12)
forecasts$naive <- naive(k_ts_train_list[[1]], 12)

dev.off()
par(mfrow=c(4, 2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')

for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(k_ts_test_list[[1]], col='red')
}

# Compatibility check the Temperature data. The lower the MASE value, better accurate
acc_tmp <- lapply(forecasts, function(f){
  accuracy(f, k_ts_list[[1]])[2,,drop=FALSE]
})

acc_tmp <- Reduce(rbind, acc_tmp)
row.names(acc_tmp) <- names(forecasts)
acc_tmp <- acc_tmp[order(acc_tmp[,'MASE']),]
round(acc_tmp, 2)
# As result, Neural is better to Temperature data

# Rain data
models_rain <- list (
  mod_arima = auto.arima(k_ts_train_list[[2]], ic='aicc', stepwise=FALSE),
  mod_exponential = ets(k_ts_train_list[[2]], ic='aicc', restrict=FALSE),
  mod_neural = nnetar(k_ts_train_list[[2]], p=12, size=25),
  mod_tbats = tbats(k_ts_train_list[[2]], ic='aicc', seasonal.periods=12),
  mod_bats = bats(k_ts_train_list[[2]], ic='aicc', seasonal.periods=12),
  mod_stl = stlm(k_ts_train_list[[2]], s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(k_ts_train_list[[2]])
)
forecasts <- lapply(models_rain, forecast, 12)
forecasts$naive <- naive(k_ts_train_list[[2]], 12)

dev.off()
par(mfrow=c(4, 2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')

for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(k_ts_test_list[[2]], col='red')
}

# Compatibility check the Rain data. The lower the MASE value, better accurate
acc_rain <- lapply(forecasts, function(f){
  accuracy(f, k_ts_list[[2]])[2,,drop=FALSE]
})

acc_rain <- Reduce(rbind, acc_rain)
row.names(acc_rain) <- names(forecasts)
acc_rain <- acc_rain[order(acc_rain[,'MASE']),]
round(acc_rain, 2)
# As result, Bats is better to Rain data

# Humidity data
models_hum <- list (
  mod_arima = auto.arima(k_ts_train_list[[3]], ic='aicc', stepwise=FALSE),
  mod_exponential = ets(k_ts_train_list[[3]], ic='aicc', restrict=FALSE),
  mod_neural = nnetar(k_ts_train_list[[3]], p=12, size=25),
  mod_tbats = tbats(k_ts_train_list[[3]], ic='aicc', seasonal.periods=12),
  mod_bats = bats(k_ts_train_list[[3]], ic='aicc', seasonal.periods=12),
  mod_stl = stlm(k_ts_train_list[[3]], s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(k_ts_train_list[[3]])
)
forecasts <- lapply(models_hum, forecast, 12)
forecasts$naive <- naive(k_ts_train_list[[3]], 12)

dev.off()
par(mfrow=c(4, 2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')

for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(k_ts_test_list[[3]], col='red')
}

# Compatibility check the Humidity data. The lower the MASE value, better accurate
acc_hum <- lapply(forecasts, function(f){
  accuracy(f, k_ts_list[[3]])[2,,drop=FALSE]
})

acc_hum <- Reduce(rbind, acc_hum)
row.names(acc_hum) <- names(forecasts)
acc_hum <- acc_hum[order(acc_hum[,'MASE']),]
round(acc_hum, 2)
# As result, Exponential is better to Humidity data

# PM10(Particulate Matter) data
models_pm <- list (
  mod_arima = auto.arima(k_ts_train_list[[4]], ic='aicc', stepwise=FALSE),
  mod_exponential = ets(k_ts_train_list[[4]], ic='aicc', restrict=FALSE),
  mod_neural = nnetar(k_ts_train_list[[4]], p=12, size=25),
  mod_tbats = tbats(k_ts_train_list[[4]], ic='aicc', seasonal.periods=12),
  mod_bats = bats(k_ts_train_list[[4]], ic='aicc', seasonal.periods=12),
  mod_stl = stlm(k_ts_train_list[[4]], s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(k_ts_train_list[[4]])
)
forecasts <- lapply(models_pm, forecast, 12)
forecast$naive <- naive(k_ts_train_list[[4]], 12)

dev.off()
par(mfrow=c(4, 2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')

for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(k_ts_test_list[[4]], col='red')
}

# Compatibility check the PM10(Particulate Matter) data. The lower the MASE value, better accurate
acc_pm <- lapply(forecasts, function(f){
  accuracy(f, k_ts_list[[4]])[2,,drop=FALSE]
})

acc_pm <- Reduce(rbind, acc_pm)
row.names(acc_pm) <- names(forecasts)
acc_pm <- acc_pm[order(acc_pm[,'MASE']),]
round(acc_pm, 2)
# As result, Arima is better to PM10(Particulate Matter)

# Insolation data
models_sol <- list (
  mod_arima = auto.arima(k_ts_train_list[[5]], ic='aicc', stepwise=FALSE),
  mod_exponential = ets(k_ts_train_list[[5]], ic='aicc', restrict=FALSE),
  mod_neural = nnetar(k_ts_train_list[[5]], p=12, size=25),
  mod_tbats = tbats(k_ts_train_list[[5]], ic='aicc', seasonal.periods=12),
  mod_bats = bats(k_ts_train_list[[5]], ic='aicc', seasonal.periods=12),
  mod_stl = stlm(k_ts_train_list[[5]], s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(k_ts_train_list[[5]])
)
forecasts <- lapply(models_sol, forecast, 12)
forecast$naive <- naive(k_ts_train_list[[5]], 12)

dev.off()
par(mfrow=c(4, 2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')

for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(k_ts_test_list[[5]], col='red')
}

# Compatibility check the Insolation data. The lower the MASE value, better accurate
acc_sol <- lapply(forecasts, function(f){
  accuracy(f, k_ts_list[[5]])[2,,drop=FALSE]
})

acc_sol <- Reduce(rbind, acc_sol)
row.names(acc_sol) <- names(forecasts)
acc_sol <- acc_sol[order(acc_sol[,'MASE']),]
round(acc_sol, 2)
# As result, Exponential is better to Insolation

#####################################################################################
# Temperature              => Neural
# Rain                     => Bats
# Humidity                 => Exponential
# PM10(Particluate Matter) => Arima
# Insolation               => Exponential

# The above models could be judged appropriate for each weather data
# Each fit model is applied to 25 Seoul district

# Get weather data for all 25 Seoul district
all_df_all<-read.csv("all_df_all.csv")
head(all_df_all)

# Split into Seoul district and assign to the list
gu_list_2<-list()
for(i in seq(1,2700,108)){
  print(i)
  j=(i-1)/108+1
  k=i+107
  gu_list_2[[j]]<-all_df_all[c(i:k),]
}
gu_list[[1]]
gu_list_2[[2]]

##  Extract weather data and Conversion
tmp_list<-list();ts_tmp_list<-list() # Temperature
rain_list<-list();ts_rain_list<-list() # Rain
hum_list<-list();ts_hum_list<-list() # Humidity
pm_list<-list();ts_pm_list<-list() # PM10(Particulate Matter)
sol_list<-list();ts_sol_list<-list() # (Insolation)

for(i in 1:25){
  colnames(gu_list[[i]])<-c("temp","rain","humid","pm10","solar","loc")
  tmp_list[[i]] <- gu_list[[i]]$temp
  rain_list[[i]] <- gu_list[[i]]$rain
  hum_list[[i]] <- gu_list[[i]]$humid
  pm_list[[i]] <- gu_list[[i]]$pm10
  sol_list[[i]] <- gu_list[[i]]$solar
}

## Divide into Train set and Test set
train_tmp_list<-list();test_tmp_list<-list()
train_rain_list<-list();test_rain_list<-list()
train_hum_list<-list();test_hum_list<-list()
train_pm_list<-list();test_pm_list<-list()
train_sol_list<-list();test_sol_list<-list()

for(i in 1:25){
  ts_tmp_list[[i]]<-ts(tmp_list[[i]],frequency = 12,start(2010,1))
  train_tmp_list[[i]]<-window(ts_tmp_list[[i]],start=1,end=7.99)
  test_tmp_list[[i]]<-window(ts_tmp_list[[i]],start=8)
  
  ts_rain_list[[i]]<-ts(rain_list[[i]],frequency = 12,start(2010,1))
  train_rain_list[[i]]<-window(ts_rain_list[[i]],start=1,end=7.99)
  test_rain_list[[i]]<-window(ts_rain_list[[i]],start=8)
  
  ts_hum_list[[i]]<-ts(hum_list[[i]],frequency = 12,start(2010,1))
  train_hum_list[[i]]<-window(ts_hum_list[[i]],start=1,end=7.99)
  test_hum_list[[i]]<-window(ts_hum_list[[i]],start=8)
  
  ts_pm_list[[i]]<-ts(pm_list[[i]],frequency = 12,start(2010,1))
  train_pm_list[[i]]<-window(ts_pm_list[[i]],start=1,end=7.99)
  test_pm_list[[i]]<-window(ts_pm_list[[i]],start=8)
  
  ts_sol_list[[i]]<-ts(sol_list[[i]],frequency = 12,start(2010,1))
  train_sol_list[[i]]<-window(ts_sol_list[[i]],start=1,end=7.99)
  test_sol_list[[i]]<-window(ts_sol_list[[i]],start=8)
}

## Model create and Prediction
# Temperature              => Neural; mod_neural = nnetar(k_ts_train_list[[5]], p=12, size=25),
# Rain                     => Bats; mod_bats = bats(k_ts_train_list[[5]], ic='aicc', seasonal.periods=12),
# Humidity                 => Exponential; mod_exponential = ets(k_ts_train_list[[5]], ic='aicc', restrict=FALSE),
# Pm10(Particulate Matter) => Arima; mod_arima = auto.arima(k_ts_train_list[[5]], ic='aicc', stepwise=FALSE),
# Insolation               => Exponential;  mod_exponential = ets(k_ts_train_list[[5]], ic='aicc', restrict=FALSE),

# Temperature
tmp_neural_fit_list <- list()
for(i in 1:25){
  tmp_neural_fit_list[[i]] <- nnetar(train_tmp_list[[i]], ic = 'aicc', restrict = F)
}

tmp_neural_fcast_list <-list()
for(i in 1:25){
  tmp_neural_fcast_list[[i]] <- forecast(tmp_neural_fit_list[[i]], h = 36)
}

# Rain
rain_bats_fit_list <- list()
for(i in 1:25){
  rain_bats_fit_list[[i]] <- bats(train_rain_list[[i]], ic = 'aicc', seasonal.periods=12)
}

rain_bats_fcast_list <- list()
for(i in 1:25){
  rain_bats_fcast_list[[i]] <- forecast(rain_bats_fit_list[[i]], h = 36)
}

# Humidity
hum_ets_fit_list <- list()
for(i in 1:25){
  hum_ets_fit_list[[i]] <- ets(train_hum_list[[i]], ic = 'aicc', restrict = F)
}

hum_ets_fcast_list <- list()
for(i in 1:25){
  hum_ets_fcast_list[[i]] <- forecast(hum_ets_fit_list[[i]], h = 36) 
}

# Pm10(Particulate Matter)
pm_arima_fit_list <- list()
for(i in 1:25){
  pm_arima_fit_list[[i]] <- auto.arima(train_pm_list[[i]], ic='aicc', stepwise = F)
}

pm_arima_fcast_list <- list()
for(i in 1:25){
  pm_arima_fcast_list[[i]] <- forecast(pm_arima_fit_list[[i]], h = 36)
}

# Insolation
solar_ets_fit_list <- list()
for(i in 1:25){
  solar_ets_fit_list[[i]] <- ets(train_sol_list[[i]], ic = 'aicc', restrict = F)
}

solar_ets_fcast_list <-list()
for(i in 1:25){
  solar_ets_fcast_list[[i]] <- forecast(solar_ets_fit_list[[i]], h = 36)
}


## Visualize the each weather data time series anaylsis
op <- par(mfrow = c(1,1))

# Temperature
for(i in 1:25){
  plot(tmp_neural_fcast_list[[i]], xaxt="n")
  lines(test_tmp_list[[i]], col='red')
}

tmp_neural_fcast_list[[1]]

pred_tem_neural_df<-as.data.frame(tmp_neural_fcast_list)

write.csv(pred_tem_neural_df, "pred_tem_neural_df.csv")

# Rain
for(i in 1:25){
  plot(rain_bats_fcast_list[[i]], xaxt = "n")
  lines(test_rain_list[[i]], col = 'red')
}

rain_bats_fcast_list[[1]]

pred_rain_bats_df <- as.data.frame(rain_bats_fcast_list)

write.csv(pred_rain_bats_df, "pred_rain_bats_df.csv")

# Humidity
for(i in 1:25){
  plot(hum_ets_fcast_list[[i]], xaxt = "n")
  lines(test_hum_list[[i]], col = 'red')
}

hum_ets_fcast_list[[1]]

pred_hum_ets_df <- as.data.frame(hum_ets_fcast_list)

write.csv(pred_hum_ets_df, "pred_hum_ets_df.csv")

# PM10 (Particulate Matter) 
for(i in 1:25){
  plot(pm_arima_fcast_list[[i]], xaxt = "n")
  lines(test_pm_list[[i]], col = "red")
}

pm_arima_fcast_list[[1]]

pred_pm_arima_df <- as.data.frame(pm_arima_fcast_list)

write.csv(pred_pm_arima_df, "pred_pm_arima_df.csv")

# Insolation
for(i in 1:25){
  plot(solar_ets_fcast_list[[i]], xaxt = "n")
  lines(test_sol_list[[i]], col = "red")
}

plot(solar_ets_fcast_list[[1]], xaxt = "n", main = "3년 일사량 예측")
lines(test_sol_list[[1]], col = "red")

solar_ets_fcast_list[[1]]

pred_solar_ets_df <- as.data.frame(solar_ets_fcast_list)

write.csv(pred_solar_ets_df, "pred_solar_ets_df.csv")


