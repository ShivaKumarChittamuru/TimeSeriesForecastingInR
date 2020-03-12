
#### Project Trending Data - Spain LFPR - Labor force participate rate of Spain to measure both employment and unemployment of the country - yearly data
#things to look out for in trend data - change points and trend dampening

library(readr)
spain_lfpr <- read_csv("C:/Users/shchitt.NORTHAMERICA/Desktop/Machine Learning/TimeSeriesForecastingInR/spain_lfpr.csv")
class(spain_lfpr)
spain = ts(spain_lfpr$x, start = 1980)

plot(spain, ylab = "Labor Force Participation Rate 25-54")


# How to de-trend a time series?
trModel <- lm(spain ~ c(1:length(spain)))
plot(resid(trModel), type="l")  # resid(trModel) contains the de-trended series.


# Exponential smoothing for trend-only data is holt
library(forecast)
holttrend = holt(spain, h = 5)
summary(holttrend)
plot(holttrend)

# Reactiveness is adjusted by smoothing parameters ranges from 0 to 1, where is 0 means more smoother and 1 means more reactive 
# Level (l) smoothing parameter is alpha
# Trend (b) smoothing parameter is beta
# Number of forecasting steps is h
# y = l + hb

# The rate cannot go higher than 100% => a change point is inherent in the data, so a damping parameter is needed to damp the trend

# Phi auto generated
plot(holt(spain, h = 15, damped = T))
# To see the generated value for phi
summary(holt(spain, h = 15, damped = T))

# Manual setting of phi
plot(holt(spain, h = 15, damped = T, phi = 0.8))


# Arima auto generated
spainarima = auto.arima(spain, 
                        stepwise = F, 
                        approximation = F)

summary(spainarima)

plot(forecast(spainarima, h = 5))


# Overview plot
holttrend = holt(spain, h = 10)
holtdamped = holt(spain, h = 10, damped = T, phi = 0.8)
arimafore = forecast(spainarima, h = 10)

autoplot(holttrend)
autoplot(holtdamped)
autoplot(arimafore)

class(holttrend)
class(arimafore)


library(ggplot2)
# 3 Forecast Lines as Comparison
autoplot(spain) +
  autolayer(holttrend$mean, series = "Holt Linear Trend") +
  autolayer(holtdamped$mean, series = "Holt Damped Trend") +
  autolayer(arimafore$mean, series = "ARIMA") +
  xlab("year") + ylab("Labour Force Participation Rate Age 25-54") + 
  guides(colour=guide_legend(title="Forecast Method")) + theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Spain") + theme(plot.title=element_text(family="Times", hjust = 0.5, color = "blue", face="bold", size=15))




