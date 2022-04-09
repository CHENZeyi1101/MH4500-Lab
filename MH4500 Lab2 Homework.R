### MH4500 Lab 2 ###
library(forecast)

### Question 1: masim function ###
masim <- function(para, sigsq, T){
  q = length(para)
  noise = rnorm(T + q, mean = 0, sd = 1)
  x = c(noise[1:q], rep(0, T))
  for (i in (q + 1):(q + T)){
    x[i] =  para %*% noise[i - (1:q)] + noise[i] ### Theta0 = 1
  }
  x = x[(q + 1):(q + T)]
  x
}
# "para" denotes the coefficient vector.

### Question 2: Simulation ###
set.seed(2022)
par(mfrow= c(2,1)) # Length of TS = 1000
model1 <- masim(c(0.5, 2), 1, 1000)
plot.ts(model1)
plot.ts(diff(model1))
acf(diff(model1))
acf(model1)
# From ACF, the ACF cuts-off after lag 2, which is consistent with the feature of MA(2)
# The MA(2) process is stationary, because the ACF cuts-off rapidly and the time plot fluctuates with constant variation around the mean.

# Length of TS = 200
model2 <- masim(c(0.5, 2), 1, 200)
plot.ts(model2)
acf(model2)
# From ACF, the ACF cuts-off after lag 2, which is consistent with the feature of MA(2)
# The MA(2) process is stationary.

model1_diff <- diff(model1, lag =250, differences = 1)
plot.ts(model1_diff)

# Comparing the two cases
par(mfrow = c(1, 2))
acf(model1)
acf(model2)
par(mfrow = c(1, 1))
### The interval between the dotted blue lines is wider for small T (T = 200) and narrower for large T(T = 1000)
### This observation can be explained by |r(h)| <= 1.96/sqrt(n), which is smaller for large n.

### Question 3: ARIMA model fitting
# The process is a stationary MA(2), thus p = 0, d = 0, q = 2
install.packages("forecast")
library(forecast)
fit <- Arima(model2, c(0, 1, 2))
fit
# Approximated coefficients: Theta1 = 0.2577; Theta2 = 0.4522
# The fitted model is X_t = Z_t + 0.2577*Z_t-1 + 0.4522*Z_t-2 + 0.0789
plot.ts(model2, ylab = "MA_Model2")
lines(fitted(fit), col = "red")
lines(x= time(model2), y = model2 - fit$residuals, col = "red")
# The fitted values give similar pattern to the original time series.

# Same procedure for model1
fit1 <- arima(model1, c(0, 0, 2))
fit1
# The fitted model is X_t = Z_t + 0.2804*Z_t-1 + 0.5356*Z_t-2 + 0.0039
plot.ts(model1)
lines(x= time(model1), y = model1 - fit1$residuals, col = "blue")


?auto.arima
fit3 <- auto.arima(model2)

pdf("Lab2_HW_Plots.pdf")
plot.ts(model1)
plot.ts(model2)
acf(model1)
acf(model2)
plot.ts(model2)
lines(x= time(model2), y = model2 - fit$residuals, col = "red")
plot.ts(model1)
lines(x= time(model1), y = model1 - fit1$residuals, col = "blue")
dev.off()