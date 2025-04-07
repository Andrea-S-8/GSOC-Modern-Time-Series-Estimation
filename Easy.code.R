## R Script for GSOC tests
## Modern time series estimation
## EASY

library(forecast)

# AR1
set.seed(1)
data=arima.sim(model=list(ar=0.8),n=500)
taperedacf(data,calc.ci=F)
acf(data)
taperedpacf(data,calc.ci=F)
pacf(data)


## MA1
set.seed(2)
data=arima.sim(model=list(ma=0.8),n=500)
taperedacf(data,calc.ci=F)
acf(data)
taperedpacf(data,calc.ci=F)
pacf(data)


## AR(2)
set.seed(3)
data=arima.sim(model=list(ar=c(0.7,-0.1)),n=500)
taperedacf(data,calc.ci=F)
acf(data)
taperedpacf(data,calc.ci=F)
pacf(data)


## ARMA(1,1)
set.seed(4)
data=arima.sim(model=list(ar=0.8,ma=-0.4),n=500)
taperedacf(data,calc.ci=F)
acf(data)
taperedpacf(data,calc.ci=F)
pacf(data)

## SMA4(1)
set.seed(5)
data=arima.sim(model=list(ma=c(0,0,0,0.8)),n=500)
taperedacf(data,calc.ci=F)
acf(data)
taperedpacf(data,calc.ci=F)
pacf(data)
