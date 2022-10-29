library(tseries)
library(dplyr)
library(tidyr)
library(MTS)

#Data <- read.csv('/Users/Jagonii/Desktop/Data.csv')
Data<- read.csv(paste(getwd(),"/Data.csv",sep=""))
Data

US_index <- Data[, c(1, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 18, 19, 24, 25)]
UK_index <- Data[, c(1, 11, 17, 22, 23, 27, 28)]
SG_index <- Data[, c(1, 10, 16, 20, 21, 26)]

# General:Brent, VIX, FED FUND Rate, M1
# US
US_index %>% 
  gather(Series, Value, -Date) %>%
  group_by(Series) %>%
  summarise(
    box.pvalue = Box.test(Value, lag=1, type="Ljung-Box")$p.value,
    box = Box.test(Value, lag=1, type="Ljung-Box")$p.value<0.05,
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05,
    kpss.pvalue=kpss.test(Value)$p.value,
    kpss=kpss.test(Value)$p.value>0.05,
  )

d1_US_index <- as.data.frame(lapply(US_index[, -c(1, 12, 14)], diff, lag=1))

# I didn't include date, VIX, and US.CPI.MoM in this round of test
d1_US_index %>% 
  gather(Series, Value) %>%
  group_by(Series) %>%
  summarise(
    box.pvalue = Box.test(Value, lag=1, type="Ljung-Box")$p.value,
    box = Box.test(Value, lag=1, type="Ljung-Box")$p.value<0.05,
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05,
    kpss.pvalue=kpss.test(Value)$p.value,
    kpss=kpss.test(Value)$p.value>0.05,
  )


# UK
UK_index %>% 
  gather(Series, Value, -Date) %>%
  group_by(Series) %>%
  summarise(
    box.pvalue = Box.test(Value, lag=20, type="Ljung-Box")$p.value,
    box = Box.test(Value, lag=20, type="Ljung-Box")$p.value<0.05,
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05,
    kpss.pvalue=kpss.test(Value)$p.value,
    kpss=kpss.test(Value)$p.value>0.05,
  )

d1_UK_index <- as.data.frame(lapply(UK_index[, -c(1, 2, 5)], diff, lag=1))

# I didn't include date, FTSE100, and UK.MoM.CPI in this round of test
d1_UK_index %>% 
  gather(Series, Value) %>%
  group_by(Series) %>%
  summarise(
    box.pvalue = Box.test(Value, lag=1, type="Ljung-Box")$p.value,
    box = Box.test(Value, lag=1, type="Ljung-Box")$p.value<0.05,
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05,
    kpss.pvalue=kpss.test(Value)$p.value,
    kpss=kpss.test(Value)$p.value>0.05,
  )


# SG
SG_index %>% 
  gather(Series, Value, -Date) %>%
  group_by(Series) %>%
  summarise(
    box.pvalue = Box.test(Value, lag=20, type="Ljung-Box")$p.value,
    box = Box.test(Value, lag=20, type="Ljung-Box")$p.value<0.05,
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05,
    kpss.pvalue=kpss.test(Value)$p.value,
    kpss=kpss.test(Value)$p.value>0.05,
  )

d1_SG_index <- as.data.frame(lapply(SG_index[, -c(1, 4, 6)], diff, lag=1))

# I didn't include date, SG.IP.YoY, and SG.CPI.MoM in this round of test
d1_SG_index %>% 
  gather(Series, Value) %>%
  group_by(Series) %>%
  summarise(
    box.pvalue = Box.test(Value, lag=1, type="Ljung-Box")$p.value,
    box = Box.test(Value, lag=1, type="Ljung-Box")$p.value<0.05,
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05,
    kpss.pvalue=kpss.test(Value)$p.value,
    kpss=kpss.test(Value)$p.value>0.05,
  )

ccm(d1_US_index)
us_order = VARorder(d1_US_index)
