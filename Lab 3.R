library(forecast)
library(tseries)
getwd()
setwd("/Users/zeyichen/Desktop/Courses/Y2S2/Time Series Analysis/Lab/Lab 3")
data1 = scan("cow.dat")
data1
par(mfrow = c(1, 1))
ts.plot(data1)
acf(data1, lag.max = 20)
pacf(data1)
fit1_MA = arima(data1, order = c(0, 0, 7))
fit1_AR = arima(data1, order = c(2, 0, 0))
fit1_Arma = arima(data1, order = c(2, 0, 7))
# x = arima(data1, order = c(0, 0, 3))
# x
fit1_MA
fit1_AR
fit1_Arma
tsdiag(fit1_MA)
tsdiag(fit1_AR)
acf(fit1_AR$residuals)
pacf(fit1_AR$residuals)
tsdiag(fit1_Arma)
acf(fit1_MA$residuals)
pacf(fit1_MA$residuals)

data2 = as.matrix(read.table("/Users/zeyichen/Desktop/Courses/Y2S2/Time Series Analysis/Lab/Lab 3/wwwusage.txt", header = TRUE))
data2 = as.numeric(data2)
ts.plot(data2)
acf(data2, lag.max =100)
acf(diff(data2, 1), lag.max = 40)
acf(diff(log(data2), 1), lag.max = 100)
acf(diff(sqrt(data2), 1), lag.max = 100)
pacf(diff(data2, 1), lag.max = 20)
pacf(diff(log(data2), 1), lag.max = 100)
pacf(diff(sqrt(data2), 1), lag.max = 100)

data3 = diff(data2, 1)
fit2_MA = arima(data3, order = c(0, 0, 24))
fit2_AR = arima(data3, order = c(3, 0, 0))
tsdiag(fit2_MA)
tsdiag(fit2_AR)
fit2_MA
fit2_AR
acf(fit2_MA$residuals)
pacf(fit2_MA$residuals)
acf(fit2_AR$residuals)
pacf(fit2_AR$residuals)

lambda = BoxCox.lambda(data2)
lambda
data4 = diff(bxcx(data2, lambda))
data4
acf(data4, lag.max = 50)
pacf(data4, lag.max = 50)

fit_trans_MA = arima(data4, order = c(0, 0, 24))
fit_trans_AR = arima(data4, order = c(3, 0, 0))
fit_trans_MA
fit_trans_AR
tsdiag(fit_trans_AR)

x = (data4 - fit_trans_AR$residuals) + bxcx(data2, lambda)[-1]
x
y = bxcx(x, lambda, InverseQ = TRUE)
y
length(y)
y2 = append(data2[1], y)
length(y2)
plot.ts(data2)
lines(1:100, y2, type = "l", col = "red")

install.packages("FitAR")
library(FitAR)
plot.ts(data3)
lines(1:length(data4), bxcx(data4 - fit_trans_AR$residuals, lambda, InverseQ = TRUE), col = "red")
data4

?bxcx


data2diff = diff(1/data2, lag = 1)
data2diff
acf(data2diff, lag.max = 100)
data = log(data2)
ts.plot(data)
acf(data)

53.8638 * (1 - 0.2794 - 0.3183)
53.7605*(1 - 1.2724 + 0.9699)
