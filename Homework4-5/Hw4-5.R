


library(data.table)
library(chron)
library(ellipsis)
library(forecast)
library(xts)
library(zoo)
library(stats)
setwd("/Users/Kadir/Desktop")
x<-read.csv("Metro_Interstate_Traffic_Volume.csv")
x<-data.table(x)
x$Date <- sapply(strsplit(as.character(x$date_time), " "), "[", 1)
x$Time <- sapply(strsplit(as.character(x$date_time), " "), "[", 2)
x[,Date:=as.POSIXct(x$Date,format='%Y-%m-%d' ) ]
x[,Hour:= 24 * as.numeric(times(x$Time))]
x[,date_time:=as.POSIXct(x$date_time,format='%Y-%m-%d %H:%M:%S' ) ]
x$day <- weekdays(as.Date(x$Date))
sumday<-setDT(x)[, .(sum_measurem = sum(traffic_volume)), 
                 by = .(date = as.IDate(Date))]
acf(x$traffic_volume)
#look at lag 24
#there is hourly seasonality
acf(sumday$sum_measurem)
#look at lag 7
#there is daily seasonality
x$traffic_volume24[25:48204]= x$traffic_volume [1:48180]
x$rain_1h24[25:48204]= x$rain_1h[1:48180] 
x$snow_1h24[25:48204]= x$snow_1h [1:48180]
x$temp24[25:48204]= x$temp [1:48180]
x$clouds_all24[25:48204]= x$clouds_all[1:48180]
x$weather_main24[25:48204]= x$weather_main[1:48180]
x$weather_description24[25:48204]= x$weather_description[1:48180]




test_data=tail(x,1000)
data=x[1:(nrow(x)-1000)]
fit=lm(traffic_volume~temp24+rain_1h24+snow_1h24+clouds_all24,data=data[25:47204])
summary(fit)
fit=lm(traffic_volume~temp24+rain_1h24+snow_1h24+clouds_all24+ as.factor(day)+ as.factor(Hour)+as.factor(holiday)+ as.factor(weather_main24)+as.factor(weather_description24),data=data[25:47204])
summary(fit)
fit=lm(traffic_volume~temp24+rain_1h24+snow_1h24+clouds_all24+ as.factor(day)+ as.factor(Hour)+as.factor(holiday)+ as.factor(weather_main24), data=data[25:47204])
summary(fit)
test_data[,predicted:=predict(fit,test_data)]
ts.plot(test_data$traffic_volume)
lines(test_data$predicted,col="blue")



data_ts=ts(data[,c("traffic_volume")],frequency = 168)
ts.plot(data_ts)
dec_data_ts<-decompose(data_ts,type="multiplicative")
deseasonalized=data_ts/dec_data_ts$seasonal
detrend=deseasonalized/dec_data_ts$trend
library(stats)
ar_summary=data.table()
for (i in 1:5){
  model <- arima(detrend, order=c(i,0,0))
  ar_summary=rbind(ar_summary,data.table(AIC=AIC(model),BIC=BIC(model),Lag=i))
  print(i)
}
ar_summary[order(AIC)]
model_ar <- arima(detrend, order=c(5,0,0))
print(model_ar)
ma_summary=data.table()
for (i in 1:5){
  model <- arima(detrend, order=c(0,0,i))
  ma_summary=rbind(ma_summary,data.table(AIC=AIC(model),BIC=BIC(model),Lag=i))
  print(i)
}

ma_summary[order(AIC)]
model_ma <- arima(detrend, order=c(0,0,5))
print(model_ma)
print(model_ar)
arma_summary=data.table()
for (i in 1:3){ #ar degree
  for (j in 1:3){ #ma degree
    model <- arima(detrend, order=c(i,0,j))
    arma_summary=rbind(arma_summary,data.table(AIC=AIC(model),BIC=BIC(model),Lag_ar=i,Lag_ma=j))
    print(i)
  }
}
arma_summary[order(AIC)]
model_arma=arima(detrend, order=c(3,0,3))
print(model_arma)
predictions=forecast(model_arma,h=1000)
test_data[,predicted_arima:=as.vector(predictions$mean)]
ts.plot(test_data$traffic_volume)
lines(test_data$predicted_arima,col="red")
lines(test_data$predicted,col="blue")
test_data[,average:=(test_data$predicted+test_data$predicted_arima)/2]
lines(test_data$average,col="blue")
lines(test_data$average,col="yellow")
pred_datatable=test_data[,.(date_time,traffic_volume,predicted,predicted_arima,average)]
melted=melt(pred_datatable,id.vars=c(1,2))
summary_result=melted[,list(se=(value-traffic_volume)^2,
                            ad=abs(value-traffic_volume),
                            ape=abs(value-traffic_volume)/traffic_volume,traffic_volume),by=list(date_time,variable)]

summary_result=summary_result[,list(mse=mean(se),mad=mean(ad),mape=mean(ape)),by=list(variable)]
summary_result



#xreg=cbind(day= model.matrix(~as.factor(data$day)),Hour=model.matrix(~as.factor(data$Hour)), weather_main=model.matrix(~as.factor(data$weather_main)), temp=data$temp)

#arima_model_with_var <- auto.arima(data_ts,xreg =xreg)
