library(tseries)
library(dplyr)
library(tidyr)

Data <- read.csv("/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/Data.csv")
Data

US_index <- Data[, c(1, 4, 5, 6, 7, 8, 9, 12, 13, 14, 15, 18, 19, 24, 25, 29)]
UK_index <- Data[, c(1, 11, 17, 22, 23, 27, 28, 30)]
SG_index <- Data[, c(1, 10, 16, 20, 21, 26, 31)]

# General:Brent, VIX, FED FUND Rate, M1
# US
US_index %>% 
  gather(Series, Value, -Date) %>%
  group_by(Series) %>%
  summarise(
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05
  )

d1_US_index <- as.data.frame(lapply(US_index[, -c(1, 10, 12)], diff, lag=1))
insert_df <- US_index[2:length(US_index[,1]),c(1, 10, 12)]
d1_US_index <- cbind(insert_df, d1_US_index)

d1_US_index %>% 
  gather(Series, Value, -Date) %>%
  group_by(Series) %>%
  summarise(
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05
  )


# UK
UK_index %>% 
  gather(Series, Value, -Date) %>%
  group_by(Series) %>%
  summarise(
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05
  )

d1_UK_index <- as.data.frame(lapply(UK_index[, -c(1, 2, 5)], diff, lag=1))
insert_df <- UK_index[2:length(UK_index[,1]),c(1, 2, 5)]
d1_UK_index <- cbind(insert_df, d1_UK_index)

d1_UK_index %>% 
  gather(Series, Value, -Date) %>%
  group_by(Series) %>%
  summarise(
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05
  )


# SG
SG_index %>% 
  gather(Series, Value, -Date) %>%
  group_by(Series) %>%
  summarise(
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05
  )

d1_SG_index <- as.data.frame(lapply(SG_index[, -c(1, 4, 6)], diff, lag=1))
insert_df <- SG_index[2:length(SG_index[,1]),c(1, 4, 6)]
d1_SG_index <- cbind(insert_df, d1_SG_index)

d1_SG_index %>% 
  gather(Series, Value, -Date) %>%
  group_by(Series) %>%
  summarise(
    adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
    adf = adf.test(Value, alternative = "stationary")$p.value<0.05
  )

write.csv(d1_US_index, "/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/d1_US_index.csv")
write.csv(d1_UK_index, "/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/d1_UK_index.csv")
write.csv(d1_SG_index, "/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/d1_SG_index.csv")
