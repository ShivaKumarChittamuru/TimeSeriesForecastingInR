## Annual Lynx trappings in Canada - an yearly time Series dataset

head(lynx)

# extracting time stamps
time(lynx)

# how long is the dataset?
length(lynx)

plot(lynx)

mean(lynx)
median(lynx)

#identify median using sort function
sort(lynx)

# quantiles
quantile(lynx)

# deciles
quantile(lynx, prob = seq(0, 1, length = 11), type = 5)


## Preparing Class ts

# random uniform data between 10 and 45
unif_data = runif(n = 50, min = 10, max = 45)
plot.ts(unif_data)

# packing it into a quarterly time series; starting from 3rd quarter of 1956
mytimeseries = ts(data = unif_data, start = c(1956,3), frequency = 4)
plot(mytimeseries)
time(mytimeseries)
class(mytimeseries)

class(lynx)

# a typical mts dataset
class(EuStockMarkets)
head(EuStockMarkets)
plot(EuStockMarkets)

## ts plots

#nottem - a monthly data of temperature measurements in Nottingham
plot(nottem)
head(nottem)
time(nottem)

# decompose to single components
plot(decompose(nottem))
plot(decompose(mytimeseries))

# Are line plot best?
library(ggplot2)
nottem %>%
  ggplot(aes(x = time(nottem), y = 1)) +
  geom_tile(aes(fill = nottem)) +
  scale_fill_gradient2(
    low = "navy", mid = "yellow", high = "red", midpoint = 50
  ) +
  ylab("") + scale_y_discrete(expand = c(0, 0))


#plot(decompose(lynx))  #Error in decompose(lynx) : time series has no or less than 2 periods
my_ts = ts(data = unif_data, start = 1956, frequency = 1)
plot(decompose(my_ts)) #Error in decompose(my_ts) : time series has no or less than 2 periods


# time series of normally distributed data has no trend, constant mean and variance, and no autocorrelation => stationarity
plot.ts(rnorm(500))

#random walk - changing mean or varinace with a trend => so non-stationarity
plot.ts(cumsum(rnorm(500)))


# Seasonality plots
library(forecast)
ggseasonplot(nottem)




## How to create lags?

mytimeseries
laggedTS <- lag(mytimeseries, 3)
leadTS <- lag(mytimeseries, -3)


## Stationarity

test = ts(c(rnorm(100,2,1), rnorm(100,50,1)), start = 1)

plot(test)

plot(diff(test)) # make time series stationary


### Unit Root Tests - Tests for stationarity - Augmented Dickey Fuller (adf) test

x = rnorm(1000) # random normal data

library(tseries)
adf.test(x)
acf(x, lag.max = 20) 
pacf(x, lag.max = 20)
# acf and pacf plots are within 5% confidence intervals which indicates there is no autocorrelation

plot(nottem) # seasonal data
acf(nottem, lag.max = 20) 
pacf(nottem, lag.max = 20)
# there is a clear autocorrelation
adf.test(nottem)
# but stationarity can be achieved through differencing of lags as illustrated with adf test


plot(lynx)
acf(lynx, lag.max = 20) 
pacf(lynx, lag.max = 20)
# there is a clear autocorrelation
adf.test(lynx)
# but stationarity can be achieved through differencing of lags as illustrated with adf test



## Differencing
# 1 different section
test = ts(c(rnorm(100,2,1), rnorm(100,50,1)), start = 1)
plot(test)
plot(diff(test))

# 2 different sections
test = ts(c(rnorm(100,2,1), rnorm(100,50,1), rnorm(100,80,1)), start = 1)
plot(test)
plot(diff(test))



# Augmented Dickey Fuller Unit Root Tests
# ADF test removes autocorrelation and checks for stationarity

#Test on white noise
TT <- 100
wn <- rnorm(TT)  # white noise
plot.ts(wn)
adf.test(wn)   #Augmented DF
adf.test(wn, k=0) #Plain DF, no lags
acf(wn, lag.max = 20) 
pacf(wn, lag.max = 20) # no clear correlation


#Test on white noise with trend
intercept <- 1
wnt <- wn + 1:TT + intercept
plot.ts(wnt)
tseries::adf.test(wnt)
#The null hypothesis is still rejected. adf.test() uses a model that allows an intercept and trend.
tseries::adf.test(wnt, k=0)
acf(wnt, lag.max = 20) #highly correlated on the previous lag due to linear trend
pacf(wnt, lag.max = 20)



#Test on random walk (non-stationary)
rw <- cumsum(rnorm(TT))
plot.ts(rw)
tseries::adf.test(rw)
#The null hypothesis is NOT rejected as the p-value is greater than 0.05.
tseries::adf.test(rw, k = 0)
acf(rw, lag.max = 20) 
pacf(rw, lag.max = 20)


# Use differencing to make it stationary
plot.ts(diff(rw))
adf.test(diff(rw))
adf.test(diff(rw), k=0)


# Use log transformation or differencing or De-trending to achieve stationarity.

