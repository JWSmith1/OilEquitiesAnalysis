library(forecast)
library(PerformanceAnalytics)
library(quantmod)
library(tseries)



#Import Crude Oil Futures data/ Stock Symbols(XOM, RDS, CVX, COP, PSX)
XOM_stock <- getSymbols("XOM", src = "yahoo", from = "2015-03-01", to = "2021-03-16", auto.assign = FALSE)
CVX_stock <- getSymbols("CVX", src = "yahoo", from = "2015-03-01", to = "2021-03-16", auto.assign = FALSE)
COP_stock <- getSymbols("COP", src = "yahoo", from = "2015-03-01", to = "2021-03-16", auto.assign = FALSE)
PSX_stock <- getSymbols("PSX", src = "yahoo", from = "2015-03-01", to = "2021-03-16", auto.assign = FALSE)
RDS_stock <- getSymbols("RDS-A", src = "yahoo", from = "2015-03-01", to = "2021-03-16", auto.assign = FALSE)
Crude_oil_futures <- getSymbols("CL=F", src = "yahoo", from = "2015-03-01", to = "2021-03-16", auto.assign = FALSE)
Crude_oil_futures2<- na.omit(Crude_oil_futures)


# Plot of Crude Oil, XOM, CVX, cOP, PSX, RDS closing prices
plot(Cl(XOM_stock), col = "red", main = "Closing prices for XOM, CVX, COP, PSX, RDS, and Crude Oil", ylim = c(-50, 150))
lines(Cl(CVX_stock), col = "blue")
lines(Cl(COP_stock), col = "green")
lines(Cl(PSX_stock), col = "black")
lines(Cl(RDS_stock), col = "yellow")
lines(Cl(Crude_oil_futures2), col = "purple")
legend(x = "bottomleft", legend = c("XOM", "CVX", "COP", "PSX", "RDS", "Crude Oil"), col = c("red", "blue", "green", "black", "yellow", "purple"), lty = c(1,1))

# Correlation between XOM, CVX, COP, PSX, RDS
oil_stocks <- cbind(Cl(XOM_stock), Cl(CVX_stock), Cl(COP_stock), Cl(PSX_stock), Cl(RDS_stock))
cor(oil_stocks)

# XOM stock Analysis and Forecast
XOM_close <- Cl(XOM_stock)
chart_Series(XOM_close, col = "red")
add_SMA(n = 200, on = 1, col = "blue")
add_SMA(n = 50, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)
addVo(log.scale = FALSE)

XOM_log <- log(XOM_close)
head(XOM_log, n = 10)
plot(XOM_log, main = "Log transformation of XOM Closing prices")

XOM_acf_log <- acf(XOM_log, n = 320)
XOM_acf_log
XOM_pacf_log <- pacf(XOM_log, n = 320)
XOM_pacf_log

XOM_diff <- diff(XOM_log, lag = 1)
XOM_diff <- na.locf(XOM_diff, na.rm = TRUE, fromLast = TRUE)
plot(XOM_diff, main = "Differencing of Log-Transformed XOM Closing Prices")

XOM_adf_log <- adf.test(XOM_log, alternative = c("stationary", "explosive"), k = 0)
XOM_adf_log
XOM_adf_diff <- adf.test(XOM_diff, alternative = c("stationary", "explosive"), k = 0)
XOM_adf_diff

XOM_acf_diff <- acf(XOM_diff)
XOM_acf_diff
XOM_pacf_diff <- pacf(XOM_diff)

XOM_diff_arima <- auto.arima(XOM_diff, stationary = TRUE, ic = c("aicc", "aic", "bic"), trace = TRUE)
summary(XOM_diff_arima)
checkresiduals(XOM_diff_arima)

XOM_diff_fitarima <- arima(XOM_diff, order = c(2, 0, 2))
summary(XOM_diff_fitarima)
XOM_diff_forecast <- forecast(XOM_diff_fitarima, h = 100)
XOM_diff_forecast
plot(XOM_diff_forecast, main = "Forecast of Differenced Data")
checkresiduals(XOM_diff_fitarima)

XOM_log_arima <- arima(XOM_log, order = c(2, 1, 2))
summary(XOM_log_arima)
XOM_log_forecast <- forecast(XOM_log_arima, h = 100)
XOM_log_forecast
XOM_a <- ts(XOM_log)
XOM_log_forecast %>% autoplot() + autolayer(XOM_a) 


# CVX Analysis and Forecast
CVX_close <- Cl(CVX_stock)
chart_Series(CVX_close, col = "red")
add_SMA(n = 200, on = 1, col = "blue")
add_SMA(n = 50, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)
addVo(log.scale = FALSE)

CVX_log <- log(CVX_close)
head(CVX_log, n = 10)
plot(CVX_log, main = "Log transformation of CVX Closing prices")

CVX_acf_log <- acf(CVX_log, n = 320)
CVX_acf_log
CVX_pacf_log <- pacf(CVX_log, n = 320)
CVX_pacf_log

CVX_diff <- diff(CVX_log, lag = 1)
CVX_diff <- na.locf(CVX_diff, na.rm = TRUE, fromLast = TRUE)
plot(CVX_diff, main = "Differencing of Log-Transformed CVX Closing Prices")

CVX_adf_log <- adf.test(CVX_log, alternative = c("stationary", "explosive"), k = 0)
CVX_adf_log
CVX_adf_diff <- adf.test(CVX_diff, alternative = c("stationary", "explosive"), k = 0)
CVX_adf_diff

CVX_acf_diff <- acf(CVX_diff)
CVX_acf_diff
CVX_pacf_diff <- pacf(CVX_diff)

CVX_diff_arima <- auto.arima(CVX_diff, stationary = TRUE, ic = c("aicc", "aic", "bic"), trace = TRUE)
summary(CVX_diff_arima)
checkresiduals(CVX_diff_arima)

CVX_diff_fitarima <- arima(CVX_diff, order = c(3, 0, 4))
summary(CVX_diff_fitarima)
CVX_diff_forecast <- forecast(CVX_diff_fitarima, h = 100)
CVX_diff_forecast
plot(CVX_diff_forecast, main = "Forecast of CVX differenced data")
checkresiduals(CVX_diff_fitarima)

CVX_log_fitarima <- arima(CVX_log, order = c(3, 1, 4))
summary(CVX_log_fitarima)
CVX_log_forecast <- forecast(CVX_log_fitarima, h = 100)
CVX_log_forecast
CVX_a <- ts(CVX_log)
CVX_log_forecast %>% autoplot() + autolayer(CVX_a)

#RDS.A Analysis and Forecast
RDS_close <- Cl(RDS_stock)
chart_Series(RDS_close, col = "red")
add_SMA(n = 200, on = 1, col = "blue")
add_SMA(n = 50, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)
addVo(log.scale = FALSE)

RDS_log <- log(RDS_Close)
head(RDS_log, n = 10)
plot(RDS_log, main = "Log transformation of RDS.A Closing Prices")

RDS_acf_log <- acf(RDS_log, n = 320)
RDS_acf_log
RDS_pacf_log <- pacf(RDS_log, n = 320)
RDS_pacf_log

RDS_diff <- diff(RDS_log, lag = 1)
RDS_diff <-na.locf(RDS_diff, na.rm = TRUE, fromLast = TRUE)
plot(RDS_diff, main = "Differnecing of Log-Transformed RDS.A Closing Prices")

RDS_adf_log <- adf.test(RDS_log, alternative = c("stationary", "explosive"), k = 0)
RDS_adf_log
RDS_adf_diff <- adf.test(RDS_diff, alternative = c("stationary", "explosive"), k = 0)
RDS_adf_diff

RDS_acf_diff <- acf(RDS_diff)
RDS_acf_diff
RDS_pacf_diff <- pacf(RDS_diff)
RDS_pacf_diff

RDS_diff_arima <- auto.arima(RDS_diff, stationary = TRUE, ic = c("aicc", "aic", "bic"), trace = TRUE)
summary(RDS_diff_arima)
checkresiduals(RDS_diff_arima)

RDS_diff_fitarima <- arima(RDS_diff, order = c(3, 0, 2))
summary(RDS_diff_fitarima)
RDS_diff_forecast <- forecast(RDS_diff_fitarima, h = 100)
RDS_diff_forecast
plot(RDS_diff_forecast, main = "Forecast of RDS.A differenced data")
checkresiduals(RDS_diff_fitarima)

RDS_log_fitarima <- arima(RDS_log, order = c(3, 1, 2))
summary(RDS_log_fitarima)
RDS_log_forecast <- forecast(RDS_log_fitarima, h = 100)
RDS_log_forecast
RDS_a <- ts(RDS_log)
RDS_log_forecast %>% autoplot() + autolayer(RDS_a)

# PSX Analysis and Forecast
PSX_close <- Cl(PSX_stock)
chart_Series(PSX_close, col = "red")
add_SMA(n = 200, on = 1, col = "blue")
add_SMA(n = 50, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)
addVo(log.scale = FALSE)

PSX_log <- log(PSX_close)
head(PSX_log, n = 10)
plot(PSX_log, main = "Log transformation of PSX Closing Prices")

PSX_diff <- diff(PSX_log, lag = 1)
PSX_diff <-na.locf(PSX_diff, na.rm = TRUE, fromLast = TRUE)
plot(PSX_diff, main = "Differnecing of Log-Transformed PSX Closing Prices")

PSX_adf_log <- adf.test(PSX_log, alternative = c("stationary", "explosive"), k = 0)
PSX_adf_log
PSX_adf_diff <- adf.test(PSX_diff, alternative = c("stationary", "explosive"), k = 0)
PSX_adf_diff

PSX_acf_diff <- acf(PSX_diff)
PSX_acf_diff
PSX_pacf_diff <- pacf(PSX_diff)
PSX_pacf_diff

PSX_diff_arima <- auto.arima(PSX_diff, stationary = TRUE, ic = c("aicc", "aic", "bic"), trace = TRUE)
summary(PSX_diff_arima)
checkresiduals(PSX_diff_arima)

PSX_diff_fitarima <- arima(PSX_diff, order = c(2, 0, 2))
summary(PSX_diff_fitarima)
PSX_diff_forecast <- forecast(PSX_diff_fitarima, h = 100)
PSX_diff_forecast
plot(PSX_diff_forecast, main = "Forecast of PSX differenced data")
checkresiduals(PSX_diff_fitarima)

PSX_log_fitarima <- arima(PSX_log, order = c(2, 1, 2))
summary(PSX_log_fitarima)
PSX_log_forecast <- forecast(PSX_log_fitarima, h = 100)
PSX_log_forecast
PSX_a <- ts(PSX_log)
PSX_log_forecast %>% autoplot() + autolayer(PSX_a)

# ConocoPhillips Analysis and Forecast
COP_close <- Cl(COP_stock)
chart_Series(COP_close, col = "red")
add_SMA(n = 200, on = 1, col = "blue")
add_SMA(n = 50, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)
addVo(log.scale = FALSE)

COP_log <- log(COP_close)
head(COP_log, n = 10)
plot(COP_log, main = "Log transformation of COP Closing Prices")

COP_diff <- diff(COP_log, lag = 1)
COP_diff <-na.locf(COP_diff, na.rm = TRUE, fromLast = TRUE)
plot(COP_diff, main = "Differnecing of Log-Transformed COP Closing Prices")

COP_adf_log <- adf.test(COP_log, alternative = c("stationary", "explosive"), k = 0)
COP_adf_log
COP_adf_diff <- adf.test(COP_diff, alternative = c("stationary", "explosive"), k = 0)
COP_adf_diff

COP_acf_diff <- acf(COP_diff)
COP_acf_diff
COP_pacf_diff <- pacf(COP_diff)
COP_pacf_diff

COP_diff_arima <- auto.arima(COP_diff, stationary = TRUE, ic = c("aicc", "aic", "bic"), trace = TRUE)
summary(COP_diff_arima)
checkresiduals(COP_diff_arima)

COP_diff_fitarima <- arima(COP_diff, order = c(0, 0, 2))
summary(COP_diff_fitarima)
COP_diff_forecast <- forecast(COP_diff_fitarima, h = 100)
COP_diff_forecast
plot(COP_diff_forecast, main = "Forecast of COP differenced data")
checkresiduals(COP_diff_fitarima)

COP_log_fitarima <- arima(COP_log, order = c(0, 1, 2))
summary(COP_log_fitarima)
COP_log_forecast <- forecast(COP_log_fitarima, h = 100)
COP_log_forecast
COP_a <- ts(COP_log)
COP_log_forecast %>% autoplot() + autolayer(COP_a)

# Crude Oil Graph, Log transformation and Differenced data
crude_oil_close <- Cl(Crude_oil_futures2)
chart_Series(crude_oil_close, col = "red")
add_SMA(n = 200, on = 1, col = "blue")
add_SMA(n = 50, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)
addVo(log.scale = FALSE)

crude_oil_log <- log(crude_oil_close)
head(crude_oil_log, n = 10)
plot(crude_oil_log, main = "Log transformation of Crude Oil Prices")

crude_oil_diff <- diff(crude_oil_log, lag = 1)
crude_oil_diff <-na.locf(crude_oil_diff, na.rm = TRUE, fromLast = TRUE)
plot(crude_oil_diff, main = "Differnecing of Log-Transformed Crude Oil Prices")

















