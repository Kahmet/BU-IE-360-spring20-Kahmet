setwd('/Users/kadir/Desktop/')
consumption<-read.csv("RealTimeConsumption.csv")
consumptionts<-ts(consumption$Consumption..MWh.,freq=24,start=c(1,1))
consumption_dec<-decompose(consumptionts,type="multiplicative")
plot(consumption_dec$trend)
plot(consumption_dec$seasonal)
consumptionts<-ts(consumption$Consumption..MWh.[1:168],freq=24,start=c(1,1))
consumption_dec<-decompose(consumptionts,type="multiplicative")
plot(consumption_dec$seasonal,type="b",col="red")
new<-array(0,24)
consumptionday<-array(0, 1577)
for (i in 1:1577)
{
  for (j in 1:24)
  {
    
    new[j]<-consumption$Consumption..MWh.[(i-1)*24+j]
  }
  consumptionday[i]   <-sum(new)
}
consumptiondayts<-ts(consumptionday,freq=7,start=c(1,1))
consumptionday_dec<-decompose(consumptiondayts,type="multiplicative")
plot(consumptionday_dec$seasonal)
plot(consumptionday_dec$trend)
consumptiondayts<-ts(consumptionday[141:210],freq=7,start=c(1,1))
consumptionday_dec<-decompose(consumptiondayts,type="additive")
plot(consumptionday_dec$seasonal,type="b",col="blue")
new<-array(0,7)
consumptionweek<-array(0, 225)
for (i in 1:225)
{
  for (j in 1:7)
  {
    
    new[j]<-consumptionday[(i-1)*7+j]
  }
  consumptionweek[i]   <-sum(new)
}
consumptionweekts<-ts(consumptionweek,freq=52,start=c(1,1))
consumptionweek_dec<-decompose(consumptionweekts,type="multiplicative")
plot(consumptionweek_dec$seasonal,type="b",col="green")
plot(consumptionweek_dec$trend)
new<-array(0,30)
consumptionmonth<-array(0, 52)
for (i in 1:52)
{
  for (j in 1:30)
  {
    
    new[j]<-consumptionday[(i-1)*30+j]
  }
  consumptionmonth[i]   <-sum(new)
}
consumptionmonthts<-ts(consumptionmonth,freq=12,start=c(1,1))
consumptionmonth_dec<-decompose(consumptionmonthts,type="multiplicative")
plot(consumptionmonth_dec$seasonal,type="b",col="green")
plot(consumptionmonth_dec$trend)
consumption<-read.csv("RealTimeConsumption.csv")
consumptionts<-ts(consumption$Consumption..MWh.,freq=168,start=c(1,1))
consumption_dec<-decompose(consumptionts,type="multiplicative")
plot(consumption_dec$trend)
plot(consumption_dec$seasonal)
consumptionts<-ts(consumption$Consumption..MWh.[1:336],freq=168,start=c(1,1))
consumption_dec<-decompose(consumptionts,type="multiplicative")
plot(consumption_dec$seasonal,type="b",col="red")
consumptionts<-ts(consumption$Consumption..MWh.,freq=168,start=c(1,1))
logconsumptionts<-log(consumptionts)
consumptiontsdec<-decompose(logconsumptionts,type="additive")
logconsumptionts<-log(consumptionts)
consumptionts<-ts(consumption$Consumption..MWh.[1:336],freq=168,start=c(1,1))
logconsumptionts<-log(consumptionts)
consumptiontsdec<-decompose(logconsumptionts,type="additive")
plot(consumptiontsdec,type="b",col="red")
consumptionts<-ts(consumption$Consumption..MWh.,freq=168,start=c(1,1))
logconsumptionts<-log(consumptionts)
consumptiontsdec<-decompose(logconsumptionts,type="additive")
plot(consumptiontsdec,type="b",col="red")
consumptionts<-ts(consumption$Consumption..MWh.,freq=168,start=c(1,1))
consumption_dec<-decompose(consumptionts,type="multiplicative")
deseasonalized<-consumptionts/consumption_dec$seasonal
ts.plot(consumptionts)
acf(consumptionts)
ts.plot(deseasonalized)
acf(deseasonalized)
detrend<-deseasonalized/consumption_dec$trend
ts.plot(detrend)
acf(detrend, na.action = na.pass)
model <- arima(consumptionts, order=c(1,0,0))
#AR
AIC(model)
BIC(model)
model <- arima(consumptionts, order=c(2,0,0))
#AR
AIC(model)
BIC(model)
model <- arima(consumptionts, order=c(0,0,1))
#MA
AIC(model)
BIC(model)
model <- arima(consumptionts, order=c(0,0,2))
#MA
AIC(model)
BIC(model)
#second model is better(lower AIC and BIC valuesDa)
model <- arima(consumptionts, order=c(2,0,0))
model_forecast <- predict(model, n.ahead = 24)$pred
model_forecast_se <- predict(model, n.ahead =24)$se
ts.plot(consumptionts, xlim = c(224, 227))
points(model_forecast, type = "l", col = 2)
points(model_forecast - 1.96*model_forecast_se, type = "l", col = 2, lty = 2)
points(model_forecast + 1.96*model_forecast_se, type = "l", col = 2, lty = 2)
plot(model_forecast, type = "b", col = 2)
plot(model_forecast - 1.96*model_forecast_se, type = "b", col = 2, lty = 2)
plot(model_forecast + 1.96*model_forecast_se, type = "b", col = 2, lty = 2)
model <- arima(detrend, order=c(2,0,0))
model_forecast <- predict(model, n.ahead = 24)$pred
model_forecast_se <- predict(model, n.ahead =24)$se
ts.plot(detrend, xlim = c(224, 227))
points(model_forecast, type = "l", col = 2)
points(model_forecast - 1.96*model_forecast_se, type = "l", col = 2, lty = 2)
points(model_forecast + 1.96*model_forecast_se, type = "l", col = 2, lty = 2)
plot(model_forecast, type = "b", col = 2)
plot(model_forecast - 1.96*model_forecast_se, type = "b", col = 2, lty = 2)
plot(model_forecast + 1.96*model_forecast_se, type = "b", col = 2, lty = 2)






