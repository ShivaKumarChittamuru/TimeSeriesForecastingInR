### US Inflation Rates with us_infl.csv - a monthly dataset to measure the change in purchasing power

library(readr)
library(ggplot2)
us_data <- read_csv("C:/Users/shchitt.NORTHAMERICA/Desktop/Machine Learning/TimeSeriesForecastingInR/us_infl.csv")
class(us_data)

# First exploratory plot
plot.ts(us_data$x)

# Converting to a time series
usinfl = ts(us_data$x, start = 2003, frequency = 12)
length(usinfl)

# Time series plot with proper x axis
plot(usinfl)

# Seasonal decomposition
decompose(usinfl)

# Visualization
plot(decompose(usinfl))

# STL decomposition
plot(stl(usinfl, s.window = 7)) #s.window is number of required seasonal cycles
plot(stl(usinfl, s.window = 2)) 
plot(stl(usinfl, s.window = 12)) 

# How to de-seasonalize time series data?
library(forecast)
ts.stl <- stl(usinfl,s.window = 7)  # decompose the TS
ts.sa <- seasadj(ts.stl)  # de-seasonalize
plot(usinfl, type="l")  # original series
plot(ts.sa, type="l")  # seasonal adjusted
seasonplot(ts.sa, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: Inflation") # seasonal frequency set as 12 for monthly data.


# stl forecasting - feed ts into stlf  or feed an STL object into forecast() function
plot(stlf(usinfl, method = "ets"))

# comparison with a standard ets forecast
plot(forecast(ets(usinfl), h = 24))

# using autoplot
library(ggplot2)
autoplot(stlf(usinfl, method = "ets"))

# seasonal arima
usinflarima = auto.arima(usinfl, 
                             stepwise = F, 
                             approximation = F, 
                             trace = T)
summary(usinflarima)

# forecasting
forec = forecast(usinflarima)

# forecast plot
plot(forec)

# ARIMA(0,1,1)(0,1,1)(12) catch all model
usinflarima2 = Arima(usinfl, 
                     order = c(0, 1, 1), 
                     seasonal = c(0, 1, 1))
summary(usinflarima2)

# forecast and plot
forec = forecast(usinflarima2)
plot(forec)


# Holt Winters exponential smoothing for seasonal-only data
plot(hw(usinfl, h = 24))

# forecast of ets
plot(forecast(ets(usinfl), h = 24))

# monthplot
ggmonthplot(usinfl)

# seasonplot
ggseasonplot(usinfl)


############
# Model Comparision by splitting into train and test sets


traininfl = window(usinfl, start = c(2003,1), end = c(2015,12))
testinfl = window(usinfl, start = c(2016,1), end = c(2017,12))

length(traininfl)
length(testinfl)

plot(traininfl)
plot(testinfl)

# STL Decomposition

stl_model <- stl(traininfl, s.window = 7)
summary(stl_model)
plot(stl_model)

forecast_stl = forecast(stl_model, h=24)
class(forecast_stl)

accuracy(forecast_stl$mean, testinfl)

library(caret)
RMSE(forecast_stl$mean, testinfl)
R2(forecast_stl$mean, testinfl, form = "traditional")

hist(forecast_stl$residuals)
acf(forecast_stl$residuals)
pacf(forecast_stl$residuals)



# Exponential Smoothing

ets_infl = ets(traininfl)
summary(ets_infl)

forecast_ets = forecast(ets_infl, h = 24)

accuracy(forecast(hw(traininfl), h=24)$mean, testinfl)
accuracy(forecast_ets$mean, testinfl)

library(caret)
RMSE(forecast_ets$mean, testinfl)
R2(forecast_ets$mean, testinfl, form = "traditional")

hist(forecast_ets$residuals)
acf(forecast_ets$residuals)
pacf(forecast_ets$residuals)


# Holt-Winter ETS method for Seasonal-only data
hw_infl = hw(traininfl)
summary(hw_infl)

forecast_hw = forecast(hw(traininfl), h=24)



# ARIMA

arima_infl = Arima(traininfl, 
                     order = c(0, 1, 1), 
                     seasonal = c(0, 1, 1))
summary(arima_infl)

forecast_arima = forecast(ets_infl, h = 24)

accuracy(forecast_arima$mean, testinfl)

library(caret)
RMSE(forecast_arima$mean, testinfl)
R2(forecast_arima$mean, testinfl, form = "traditional")

hist(forecast_arima$residuals)
acf(forecast_arima$residuals)
pacf(forecast_arima$residuals)



## interactive data visualization with dygraph

install.packages("dygraphs")
library(dygraphs)

arima_data = forecast_arima$mean
lower = ts(forecast_arima$lower[,2], start=c(2016,1), frequency=12)
upper = ts(forecast_arima$upper[,2], start=c(2016,1), frequency=12)

dygraph_data <- cbind(usinfl, lower, upper, arima_data)
class(dygraph_data)

dygraph(dygraph_data, main = "US Inflation Rates") %>% 
  dyRangeSelector() %>% 
  dySeries(name = "usinfl", label = "Inflation Data") %>%
  dySeries(c("lower", "arima_data", "upper"), label = "Arima") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Monthly Inflation Rate") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey")

