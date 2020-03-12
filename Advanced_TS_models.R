## ARIMA for non-seasonal univariate datasets - (p,d,q)
auto.arima(lynx)

#to get a trace of all combinations and their respective aic information criteria
lynx_arima1 = auto.arima(lynx, trace = T)

#to avoid any approximations and have all computations to get a robust arima model
lynx_arima2 = auto.arima(lynx, stepwise = F, approximation = F)

plot(forecast(lynx_arima1, h = 10))
plot(forecast(lynx_arima2, h = 10))



## ARIMA for Seasonal univariate datasets - (p,d,q)(P,D,Q)[frequency]
auto.arima(nottem)

#to get a trace of all combinations and their respective aic information criteria
nottem_arima1 = auto.arima(nottem, trace = T)

#to avoid any approximations and have all computations to get a robust arima model
nottem_arima2 = auto.arima(nottem, stepwise = F, approximation = F)

plot(forecast(nottem_arima1, h = 16))
plot(forecast(nottem_arima2, h = 16))





## Exponential smoothing - ets - (Error, Trend, Seasonality) - either added(A) or multiplied(M) or omitted/not-present(N) or automated(Z)
library(forecast)

# Using function ets
etsmodel = ets(nottem)
etsmodel
# no trend is present as expected and seasonality, error are added

# Plotting the model vs original
plot(nottem, lwd = 3)
lines(etsmodel$fitted, col = "red")

# Plotting the forecast
plot(forecast(etsmodel, h = 16))

# Changing the prediction interval
plot(forecast(etsmodel, h = 16, level = 95))

# Manually setting the ets model
etsmodmult = ets(nottem, model ="MZM")
etsmodmult

# Plot as comparison
plot(nottem, lwd = 3)
lines(etsmodmult$fitted, col = "red")








## ets on non-seasonal data
lynx_ets = ets(lynx)
lynx_ets
# neither trend nor seasonality are present as expected so modeled on error

# Plotting the model vs original
plot(lynx, lwd = 3)
lines(lynx_ets$fitted, col = "red")

# Plotting the forecast
plot(forecast(lynx_ets, h = 2))

# Changing the prediction interval
plot(forecast(lynx_ets, h = 4, level = 95))

# Manually setting the ets model
lynx_ets_add = ets(lynx, model ="AZZ")
lynx_ets_add

# Plot as comparison
plot(lynx, lwd = 3)
lines(lynx_ets_add$fitted, col = "red")

plot(forecast(lynx_ets_add, h = 8))

# the above shows arima is preferred over ets on non-seasonal lynx data







#random walk - defined by ARIMA(0,1,0) as there is no autocorrelation present i.e. random, uncorrelated noise
#example is stock data which is mostly random noise, which is why it is hard to model them and predict them
rw <- cumsum(rnorm(200))
auto.arima(rw)
ets(rw)


