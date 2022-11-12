library(tseries)
library(dplyr)
library(tidyr)
library(MTS)
library(vars)
library(zoo)

uk_index<- read.csv(paste(getwd(),"/m_UK_index.csv",sep=""))
us_index<- read.csv(paste(getwd(),"/m_US_index.csv",sep=""))
sg_index<- read.csv(paste(getwd(),"/m_SG_index.csv",sep=""))
daily = read.csv(paste(getwd(),"/Data.csv",sep=""))

####stock index########

################## us ##########################
us.daily  <- data.frame(daily$SPX,daily$US.Generic.Govt.2.Yr,daily$US.Generic.Govt.10.Yr,daily$Brent,daily$VIX,daily$CDX.IG.CDSI.GEN.5Y.Corp)
us_train <- us.daily[1:630,]
us_vali <- us.daily[631:696,]
us.train_d1 <- diff(ts(us_train))
us.vali_d1 <- diff(ts(us_vali))

MTSplot(us.train_d1)

for (i in 1:(dim(us.train_d1)[2])){
  print(adf.test(us.train_d1[,i]))
}

ccm(us.train_d1)
us.daily.order <- VARorder(us.train_d1,maxp = 5)
res=c(0,0)
#us.temp <- VAR(us.train_d1,p=4)
#f = predict(us.temp,n.ahead = 65)
library(MLmetrics)
library(Metrics)
# use validation set to find VAR order
##    SMAPE is used instead since there are 0s and near 0 in actual data => give inf 
for (i in 1:5){
  us.temp <- VAR(us.train_d1,p=i)
  #show(us.temp)
  f = predict(us.temp,h=65)
  zeroIndext = which(us.vali_d1[,1]==0)
  metric = mape(us.vali_d1[-zeroIndext,1],f$fcst$daily.SPX[-zeroIndext,1])
  #print(metric)
  if (i==1){
    res[1]=1
    res[2]=metric
  }
  else{
    if (metric<=res[2]){
      res[1]=i
      res[2]=metric
    }
  }
}
print(res)

us.tv<- diff(ts(us.daily[1:696,]))
us.dailyVAR <- VAR(us.tv,p=1)
show(us.dailyVAR)
f= predict(us.dailyVAR,n.ahead = 21)
us.testd1 = diff(ts(us.daily[697:718,]))
zeroIndex = which(us.testd1[,1]==0)
metric = mape(us.testd1[-zeroIndex,1],f$fcst$daily.SPX[-zeroIndex,1])
#metric1 = sum(abs(us.testd1[-zeroIndex,1]-f$fcst$daily.SPX[-zeroIndex,1])/abs(us.testd1[-zeroIndex,1]))/(length(us.testd1[-zeroIndex,1]))
print(metric)  # 101%
ts.plot(us.testd1[,1],ts(f$fcst$daily.SPX[,1],start = 2))

################## uk ##########################

uk.daily  <- data.frame(daily$FTSE.100,daily$UK.2yr.swap.rate,daily$Brent,daily$VIX)
uk_train <- uk.daily[1:630,]
uk_vali <- uk.daily[631:696,]
uk.train_d1 <- diff(ts(uk_train))
uk.vali_d1 <- diff(ts(uk_vali))
uk.tv<- diff(ts(uk.daily[1:696,]))
uk.testd1 = diff(ts(uk.daily[697:718,]))

MTSplot(uk.train_d1)

for (i in 1:(dim(uk.train_d1)[2])){
  print(adf.test(uk.train_d1[,i]))
}

ccm(uk.train_d1)
uk.daily.order <- VARorder(uk.train_d1,maxp = 5)

# use validation set to find VAR order
res=c(0,0) 
for (i in 1:7){
  uk.temp <- VAR(uk.train_d1,p=i)
  f = predict(uk.temp,h=65)
  zeroIndext = which(uk.vali_d1[,1]==0)
  metric = mape(uk.vali_d1[-zeroIndext,1],f$fcst$daily.FTSE.100[-zeroIndext,1])
  #print(metric)
  if (i==1){
    res[1]=1
    res[2]=metric
  }
  else{
    if (metric<=res[2]){
      res[1]=i
      res[2]=metric
    }
  }
}
print(res)


uk.dailyVAR <- VAR(uk.tv,p=2)
show(uk.dailyVAR)
f= predict(uk.dailyVAR,n.ahead = 21)
zeroIndext = which(uk.vali_d1[,1]==0)
metric = smape(uk.testd1[-zeroIndext,1],f$fcst$daily.FTSE.100[-zeroIndext,1])

print(metric)  # 197.14%
ts.plot(uk.testd1[-zeroIndext,1],ts(f$fcst$daily.FTSE.100[-zeroIndext,1],start = 2))

################## sg ##########################
# not run
sg.daily  <- data.frame(daily$STI,daily,daily$Brent,daily$VIX)
sg_train <- sg.daily[1:630,]
sg_vali <- sg.daily[631:696,]
sg.train_d1 <- diff(ts(sg_train))
sg.vali_d1 <- diff(ts(sg_vali))
sg.tv<- diff(ts(sg.daily[1:696,]))
sg.testd1 = diff(ts(sg.daily[697:718,]))

MTSplot(uk.train_d1)

for (i in 1:(dim(uk.train_d1)[2])){
  print(adf.test(uk.train_d1[,i]))
}

ccm(uk.train_d1)
uk.daily.order <- VARorder(uk.train_d1,maxp = 5)

# use validation set to find VAR order
res=c(0,0) 
for (i in 1:7){
  uk.temp <- VAR(uk.train_d1,p=i)
  f = predict(uk.temp,h=65)
  zeroIndext = which(uk.vali_d1[,1]==0)
  metric = mape(uk.vali_d1[-zeroIndext,1],f$fcst$daily.FTSE.100[-zeroIndext,1])
  #print(metric)
  if (i==1){
    res[1]=1
    res[2]=metric
  }
  else{
    if (metric<=res[2]){
      res[1]=i
      res[2]=metric
    }
  }
}
print(res)


uk.dailyVAR <- VAR(uk.tv,p=2)
show(uk.dailyVAR)
f= predict(uk.dailyVAR,n.ahead = 21)
zeroIndext = which(uk.vali_d1[,1]==0)
metric = smape(uk.testd1[-zeroIndext,1],f$fcst$daily.FTSE.100[-zeroIndext,1])

print(metric)  # 197.14%
ts.plot(uk.testd1[-zeroIndext,1],ts(f$fcst$daily.FTSE.100[-zeroIndext,1],start = 2))


#### unemployment rate#####


################## uk ##########################
uk_ts <- ts(uk_index[,c(2,5,8,9)])  # FTSE CPI PMI Unemployment
MTSplot(uk_ts)
uk_ts[,c(1,3)]= log(uk_ts[,c(1,3)]) # log FTSE and PMI 
MTSplot(uk_ts)

# adf test
for (i in 1:(dim(uk_ts)[2])){
  print(adf.test(uk_ts[,i])$p.value)
}

temp = uk_ts[,3]
uk_d1 = diff(uk_ts,lag=1)
uk_d1[,3] = temp[2:32]  # first diff except log(PMI)

MTSplot(uk_d1)

for (i in 1:(dim(uk_d1)[2])){
  print(adf.test(uk_d1[,i])$p.value)
} 

ccm(uk_d1)
uk.order <- VARorder(uk_d1,maxp=4)
show(uk.order) #choose lag 1 based on ccm and LR test
uk.var <- VAR(uk_d1,p=1)
show(uk.var)
predict(uk.var,n.ahead = 1)
#uk.error = real - (3.5-0.1945434)

################## us ##########################
us_ts <- ts(us_index[,c(8,12,16,17)]) #SPX, CPI, PMI, Unemployment
MTSplot(us_ts)
us_ts[,c(1,3)]= log(us_ts[,c(1,3)]) # log FTSE and PMI 
MTSplot(us_ts)

for (i in 1:(dim(us_ts)[2])){
  print(adf.test(us_ts[,i])$p.value)
}

us_d1 = diff(us_ts,lag=1)
MTSplot(us_d1)

for (i in 1:(dim(us_ts)[2])){
  print(adf.test(us_d1[,i])$p.value)
}

ccm(us_d1) # VAR 1
us.order <- VARorder(us_d1,maxp=4) #VAR(0)
us.var <- VAR(us_d1,p=1)  # a bit spurious
show(us.var)
predict(us.var,n.ahead =1)  #change in unemploy = -2.7138



################## sg ##########################
sg_ts <- ts(sg_index[,c(2,4,7,8)]) #SPX, CPI, PMI, Unemployment
MTSplot(sg_ts)
sg_ts[,c(1,3)]= log(sg_ts[,c(1,3)]) # log FTSE and PMI 
MTSplot(sg_ts)

for (i in 1:(dim(sg_ts)[2])){
  print(adf.test(sg_ts[,i])$p.value)
}

sg_d1 = diff(sg_ts,lag=1)
MTSplot(sg_d1)

for (i in 1:(dim(sg_d1)[2])){
  print(adf.test(sg_d1[,i])$p.value)
}

ccm(sg_d1) # VAR 0
sg.order <- VARorder(sg_d1,maxp=4) #VAR(0)
sg.var <- VAR(sg_d1,p=1)  # a bit spurious
show(sg.var)
predict(sg.var,n.ahead =1) #-0.1817856


