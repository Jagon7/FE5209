setwd("~/Desktop/FE5209_project/")
#install.packages("readxl")

library("readxl")
library(tseries)
library(fGarch)
library(rugarch)
library(forecast)

my_data = read.csv("Data.csv")
STI = ts(my_data$STI[1:718])
UKX = ts(my_data$FTSE.100[1:718])
SPX = ts(my_data[1:718,12])
par(mfrow = c(1,3))
plot(STI)
plot(UKX)
plot(SPX)

STI_in = window(STI, start = 1, end = 696)
UKX_in = window(UKX, start = 1, end = 696)
SPX_in = window(SPX, start = 1, end = 696)
par(mfrow = c(1,1))

plot(STI_in)
plot(UKX_in)
plot(SPX_in)

STI_out = window(STI, start = 696, end = 718)
UKX_out = window(UKX, start = 696, end =  718)
SPX_out = window(SPX, start = 696, end =  718)

par(mfrow = c(1,1))

plot(STI_out)
plot(UKX_out)


plot(SPX_out)


#####UK#########
UK.logr = diff(log(UKX_in))
plot(UK.logr)
UK.logrT = diff(log(UKX_out))
acf(UK.logr)
pacf(UK.logr)

auto = auto.arima(UK.logr,stepwise = FALSE)
armaUK = arima(UK.logr,c(2,0,3))
armaUK


stdres = residuals(armaUK)/armaUK$sigma2
acf(stdres^2)
Box.test(stdres^2, type="Ljung-Box")  # existence of GARCH effect
uk.spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(
    armaOrder = c(2,3), include.mean = FALSE))
UKarmagarch = ugarchfit(uk.spec,UK.logr)
show(UKarmagarch)
uk.f = ugarchforecast(UKarmagarch,n.ahead=718-697+1,data=UK.logr)
#ts(uk.f@forecast$seriesFor,start=2)
#data.frame(UK.logrT)-data.frame(ts(uk.f@forecast$seriesFor,start=2))
# covert logR to Price 
uk.pre_logR = data.frame(ts(uk.f@forecast$seriesFor,start=2))

uk.pre_logR_cumsum = exp(cumsum(uk.pre_logR))
uk.pre_Price = UKX_out[1]*uk.pre_logR_cumsum
plot(ts(uk.pre_Price), ylim = c(6000, 8000), col = "red")
lines(ts(UKX_out))
test_metric = sum(abs((uk.pre_Price-UKX_out[-1])/UKX_out[-1]))/length(UKX_out[-1]) #0.01933888 
test_metric
uk.fit_logR = UKarmagarch@fit$fitted.values

# covert logR to Price 
uk.fit_logR = data.frame(ts(UKarmagarch@fit$fitted.values,start=2))
uk.fit_logR_cumsum = exp(cumsum(uk.fit_logR))

uk.fit_Price = UKX_in[1]*uk.fit_logR_cumsum


fit_metric= sum(abs((uk.fit_Price-UKX_in[-1])/UKX_in[-1]))/length(UKX_in[-1])
fit_metric
plot(uk.fit_Price)



##########US############
US.logr = diff(log(SPX_in))
US.logrT = diff(log(SPX_out))
plot(US.logr)
plot(SPX)
abline(v = 696, col = "blue")
adf.test(US.logr)

acf(US.logr)
pacf(US.logr)
auto = auto.arima(US.logr,stepwise = FALSE)
auto
armaUS = arima(US.logr,c(2,0,2))
armaUS

stdres = residuals(armaUS)/armaUS$sigma2
acf(stdres^2)
Box.test(stdres^2, type="Ljung-Box")  # reject; existence of GARCH effect
us.spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(
  armaOrder = c(2,2), include.mean = FALSE))
USarmagarch = ugarchfit(us.spec,US.logr)
show(USarmagarch)
us.f = ugarchforecast(USarmagarch,n.ahead=718-697+1,data=US.logr)

us.pre_logR = data.frame(ts(us.f@forecast$seriesFor,start=2))
us.pre_logR_cumsum = exp(cumsum(us.pre_logR))
us.pre_Price = SPX_out[1]*us.pre_logR_cumsum


plot(ts(us.pre_Price), col = "red", ylim = c(6800, 7700))
lines(ts(UKX_out))
test_metric = sum(abs((us.pre_Price-SPX_out[-1])/SPX_out[-1]))/length(SPX_out[-1]) 
test_metric
us.fit_logR = USarmagarch@fit$fitted.values
# covert logR to Price 
us.fit_logR = data.frame(ts(USarmagarch@fit$fitted.values,start=2))
us.fit_logR_cumsum = exp(cumsum(us.fit_logR))
us.fit_Price = SPX_in[1]*us.fit_logR_cumsum

fit_metric= sum(abs((us.fit_Price-SPX_in[-1])/SPX_in[-1]))/length(SPX_in[-1])
fit_metric
plot(us.fit_Price)


#####SG#########
SG.logr = diff(log(STI_in))
plot(SG.logr)
SG.logrT = diff(log(STI_out))
acf(SG.logr)
pacf(SG.logr)

auto = auto.arima(SG.logr,stepwise = FALSE)
armaSG = arima(SG.logr,c(0,0,5))
armaSG


stdres = residuals(armaSG)/armaSG$sigma2
acf(stdres^2)
Box.test(stdres^2, type="Ljung-Box")  # existence of GARCH effect
sg.spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(
  armaOrder = c(0,5), include.mean = FALSE))
SGarmagarch = ugarchfit(sg.spec,SG.logr)
show(SGarmagarch)
sg.f = ugarchforecast(SGarmagarch,n.ahead=718-697+1,data=SG.logr)
# covert logR to Price 
sg.pre_logR = data.frame(ts(sg.f@forecast$seriesFor,start=2))

sg.pre_logR_cumsum = exp(cumsum(sg.pre_logR))
sg.pre_Price = STI_out[1]*sg.pre_logR_cumsum
plot(ts(sg.pre_Price), col = "red",  ylim = c(3100, 3250))
lines(ts(STI_out))
test_metric = sum(abs((sg.pre_Price-STI_out[-1])/STI_out[-1]))/length(STI_out[-1]) 
test_metric
sg.fit_logR = SGarmagarch@fit$fitted.values

# covert logR to Price 
sg.fit_logR = data.frame(ts(SGarmagarch@fit$fitted.values,start=2))
sg.fit_logR_cumsum = exp(cumsum(sg.fit_logR))

sg.fit_Price = STI_in[1]*sg.fit_logR_cumsum


fit_metric= sum(abs((sg.fit_Price-STI_in[-1])/STI_in[-1]))/length(STI_in[-1])
fit_metric
plot(ts(sg.fit_Price))







