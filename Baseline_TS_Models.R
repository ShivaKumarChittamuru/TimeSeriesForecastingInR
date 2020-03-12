## Simple Models = Good baseline models for time series forecasting

set.seed(100)
testts <- ts(rnorm(300), start = 1919, frequency = 4)
plot(testts)

library(forecast)
meanmodel <- meanf(testts, h=20)
naivemodel <- naive(testts, h=20)
driftmodel <- rwf(testts, h=20, drift = T)

plot(meanmodel, plot.conf = F, main = "")
lines(naivemodel$mean, col=123, lwd = 2)
lines(driftmodel$mean, col=22, lwd = 2)
legend("topleft",lty=1,col=c(4,123,22),
       legend=c("Mean method","Naive method","Drift Method"))


##

## Accuracy and model comparison
set.seed(95)
myts <- ts(rnorm(400), start = c(1919,1), frequency = 4)

# Training set (80% of whole time series)
mytstrain <- window(myts, start = 1919, end = 1999)

plot(mytstrain)

# The 3 models we want to test
library(forecast)
meanmodel <- meanf(mytstrain, h=80)
naivemodel <- naive(mytstrain, h=80)
driftmodel <- rwf(mytstrain, h=80, drift = T)

# Extracting the test set
mytstest <- window(myts, start = 2000)

accuracy(meanmodel, mytstest)
accuracy(naivemodel, mytstest)
accuracy(driftmodel, mytstest)

# Obviously mean model wins as the normal distribution is centered at mean


library(forecast)
forecast(meanmodel, h = 80)
class(forecast(meanmodel, h = 80))

d = mytstest-meanmodel$mean
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
R2 = 1-(sum((d)^2)/sum((meanmodel$mean-mean(meanmodel$mean))^2))


library(caret)
RMSE(meanmodel$mean, mytstest)
R2(meanmodel$mean, mytstest, form = "traditional")



## Analysing Residuals

# Variance and mean of the mean model
var(meanmodel$residuals)
mean(meanmodel$residuals) #0
plot(meanmodel$residuals) # since mean is 0, residual plot is equal to the actual plot (residual values equal actual values)


# Deleting the NA at the front of the vector
naivwithoutNA <- naivemodel$residuals
naivwithoutNA <- naivwithoutNA[2:320]
var(naivwithoutNA)
mean(naivwithoutNA)
plot(naivemodel$residuals)


driftwithoutNA <- driftmodel$residuals
driftwithoutNA <- driftwithoutNA[2:320]
var(driftwithoutNA)
mean(driftwithoutNA)
plot(driftmodel$residuals)

# Histogram of distribution
hist(meanmodel$residuals)
hist(naivemodel$residuals)
hist(driftmodel$residuals)

# Autcorrelation
acf(meanmodel$residuals)
pacf(meanmodel$residuals)

#the above plots indicate that autocorrelation was well captured by the mean model and only randomness  i.e zero mean and constant variance
#that is not the case in the below plots as there is still some autocorrelation that is not captured by the models
# if the resiudal are still correlated as below, fix it by differencing or transformation

acf(naivwithoutNA)
acf(driftwithoutNA)
