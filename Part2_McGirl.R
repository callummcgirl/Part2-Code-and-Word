beer <- read.csv("R/Beer_Production.csv")

beer = as.vector(t(data[,-c(1,14)]))
beer = ts(beer,start=1956,frequency=12)

BoxCox.lambda(beer)

plot(beer,ylog='Beer Production')
names(beer)
summary(beer)
head(beer)

beer

abline(reg = lm(beer~time(beer)))

plot(aggregate(beer,FUN=mean))

five_years<- head(beer,60)
five_years
ts.plot(five_years,ylab='Beer Production',xlab='Time in Months')


plot(log(beer))
plot(diff(log(beer)))


acf(diff(log(beer)))
pacf(diff(log(beer)))

fit <- arima(log(beer), c(0,1,1),seasonal = list(order = c(0,1,1), period=12))
pred <- predict(fit, n.ahead = 10*12)
pred1 <- 2.718^pred$pred
ts.plot(beer, 2.718^pred$pred, log = 'y', lty = c(1,3))

datawide <- ts(beer, frequency=12, start=c(1956),end=c(1994,8))
fit <- arima(log(datawide), c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
pred <- predict(fit, n.ahead=10*12)
pred1 <- 2.718^pred$pred
data1 <- head(pred1,12)

predicted <- round(data1, digits = 0)

original <- tail(beer, 12)

ts.plot(beer, 2.718^pred$pred, log = 'y', lty = c(1,3))
