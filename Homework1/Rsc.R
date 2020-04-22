library("zoo", lib.loc="~/R/win-library/3.6")
library("xts", lib.loc="~/R/win-library/3.6")
library("readxl", lib.loc="~/R/win-library/3.6")
library("ggplot2", lib.loc="~/R/win-library/3.6")
library("data.table", lib.loc="~/R/win-library/3.6")
setwd('/Users/kadir/Desktop/')
data<-read_xlsx("dataexcel.xlsx")
Date<-xts(data, order.by = as.Date(as.yearmon(data$Date)))
Date <- subset( Date, select = -Date )
Date<-data.table(Date)
data <- subset( data, select = -Date )
USD<-data$USD  
ts.USD<-ts(data =USD, start = 2007, end = 2019, frequency = 12,  deltat = 1)
ts.plot(ts.USD, col = 'blue', xlab = "Year", ylab = "TL Value",main="USD")
EURO<-data$EURO
ts.EURO<-ts(data =EURO, start = 2007, end = 2019, frequency = 12,  deltat = 1)
ts.plot(ts.EURO, col = 'red', xlab = "Year", ylab = "TL Value", main="EURO")
CUM<-data$ `CUM TL`
ts.CUM<-ts(data =CUM, start = 2007, end = 2019, frequency = 12,  deltat = 1)
ts.plot(ts.CUM, col = 'brown', xlab = "Year", ylab = "TL Value", main="CUM")
LOANS<-data$ LOANS
ts.LOANS<-ts(data =LOANS, start = 2007, end = 2019, frequency = 12,  deltat = 1)
ts.plot(ts.LOANS, col = 'green', xlab = "Year", ylab = "TL Value" ,main="LOANS")
CPI<-data$ CPI
ts.CPI<-ts(data =CPI, start = 2007, end = 2019, frequency = 12,  deltat = 1)
ts.plot(ts.CPI, col = 'purple', xlab = "Year", ylab = "TL Value" ,main="CPI")
EXPORT<-data$EXPORT
ts.EXPORT<-ts(data =EXPORT, start = 2007, end = 2019, frequency = 12,  deltat = 1)
ts.plot(ts.EXPORT, col = 'gray', xlab = "Year", ylab = "TL Value" ,main="EXPORT")
IMPORT<-data$IMPORT
ts.IMPORT<-ts(data =IMPORT, start = 2007, end = 2019, frequency = 12,  deltat = 1)
ts.plot(ts.IMPORT, col = 'yellow', xlab = "Year", ylab = "TL Value",main="IMPORT")

par(mfrow=c(1,2))
ts.plot(ts.USD, col = 'blue', xlab = "Year", ylab = "TL Value",main="USD")
ts.plot(ts.EURO, col = 'red', xlab = "Year", ylab = "TL Value", main="EURO")

par(mfrow=c(1,2))
ts.plot(ts.USD, col = 'blue', xlab = "Year", ylab = "TL Value",main="USD")
ts.plot(ts.CUM, col = 'brown', xlab = "Year", ylab = "TL Value", main="CUM")


par(mfrow=c(1,2))
ts.plot(ts.USD, col = 'blue', xlab = "Year", ylab = "TL Value",main="USD")
ts.plot(ts.CPI, col = 'purple', xlab = "Year", ylab = "TL Value" ,main="CPI")

par(mfrow=c(1,2))
ts.plot(ts.USD, col = 'blue', xlab = "Year", ylab = "TL Value",main="USD")
ts.plot(ts.EXPORT, col = 'gray', xlab = "Year", ylab = "TL Value" ,main="EXPORT")

par(mfrow=c(1,2))
ts.plot(ts.USD, col = 'blue', xlab = "Year", ylab = "TL Value",main="USD")
ts.plot(ts.IMPORT, col = 'yellow', xlab = "Year", ylab = "TL Value" ,main="IMPORT")

par(mfrow=c(1,2))
ts.plot(ts.IMPORT, col = 'blue', xlab = "Year", ylab = "TL Value",main="IMPORT")
ts.plot(ts.EXPORT, col = 'yellow', xlab = "Year", ylab = "TL Value" ,main="EXPORT")
