

## Sales of a restaurant on a beach
library(readr)
restaurant <- read_csv("C:/Users/shchitt.NORTHAMERICA/Desktop/Machine Learning/TimeSeriesForecastingInR/restaurant.csv")
class(restaurant)
head(restaurant)

## Data Cleaning

# check the quotes while importing to get 2 columns
class(restaurant$V2)

# chopping off the useless quotes at 2 positions
library(tidyr)
revenue <- separate(restaurant, col = V2, 
                    sep = c(3, -2), 
                    into = c("rest", "data", "rest2"))

# all the relevant data is in column "data"
head(revenue)

# class is still a character (with some missing data)
class(revenue$data)

# conversion to time series
myts <- ts(as.numeric(revenue$data),
           start = 1998, frequency = 12)

# data is still not clean (outliers and NAs)
summary(myts)

# all in one cleaning tool
library(forecast)
myts <- tsclean(myts)

# check the data
summary(myts)

plot(myts)

# set up a Neural Network model, p = lagged values used as inputs, P = seasonal lag (observations from previous seasons), k = number of nodes in the hidden layer
mynnetar <- nnetar(myts)
summary(mynnetar)

# forecasting 3 years with the model
nnetforecast <- forecast(mynnetar, h = 36, PI = T)
library(ggplot2)
autoplot(nnetforecast)


## interactive data visualization with dygraph

# data we need for the graph
data <- nnetforecast$x
lower <- ts(nnetforecast$lower[,2], start=c(2018,1), frequency=12)
upper <- ts(nnetforecast$upper[,2], start=c(2018,1), frequency=12)
pforecast <- nnetforecast$mean

dygraph_data <- cbind(data, lower, upper, pforecast)
class(dygraph_data)

install.packages("dygraphs")
library(dygraphs)

dygraph(dygraph_data, main = "Beach Restaurant") %>% 
  dyRangeSelector() %>% 
  dySeries(name = "data", label = "Revenue Data") %>%
  dySeries(c("lower","pforecast","upper"), label = "Revenue Forecast") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "Monthly Revenue USD") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(axisLineColor = "navy", gridLineColor = "grey")


