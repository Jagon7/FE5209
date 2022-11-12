library(data.table)
library(Matrix)
library(dplyr)
library(MLmetrics)
library(caret)
library(forecast)
require(xts)

########################################## TBATS MODEL ####################################################
# US
# train test split
m_US <- read.csv("/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/m_US_index.csv")
train_US <- head(m_US, 31)
test_US <- tail(m_US, 1)

# plot
US <- ts(m_US[, 17], start = c(2020,1), end = c(2022,8), frequency = 12)
plot(as.xts(US), main = 'US Monthly Unemployment Rate (Time Series Data)')

US_ts <- ts(train_US[, 17], start = c(2020,1), end = c(2022,7),frequency = 12)
US_model_tbats <- tbats(US_ts)

# MAPE for training dataset
MAPE(US_model_tbats$fitted.values, train_US$Monthly.Unemployment.Rate) # MAPE: 0.1111886

# Predict
predict_US <- predict(US_model_tbats)
predict_US
model_tbats.forecast_US <- forecast(US_model_tbats,  h = 6)
plot(model_tbats.forecast_US)

summary(US_model_tbats)
plot(US_model_tbats)
for_tbats <- forecast::forecast(US_model_tbats, h = 1)
df_tbats = as.data.frame(for_tbats)
test_US$tbats = df_tbats$'Point Forecast'
MAPE(test_US$tbats, test_US$Monthly.Unemployment.Rate) # US Unemployment Rate MAPE 0.02702899

############################################################################################################
# UK
m_UK <- read.csv("/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/m_UK_index.csv")
train_UK <- head(m_UK, 31)
test_UK <- tail(m_UK, 1)

# plot
UK <- ts(m_UK[, 9], start = c(2020,1), end = c(2022,8), frequency = 12)
plot(as.xts(UK), main = 'UK Monthly Unemployment Rate (Time Series Data)')

UK_ts <- ts(train_UK[, 9], start = c(2020,1), end = c(2022,7),frequency = 12)
UK_model_tbats <- tbats(UK_ts)

# MAPE for training dataset
MAPE(UK_model_tbats$fitted.values, train_UK$Monthly.Unemployment.Rate) # MAPE: 0.01763086

# Predict
predict_UK <- predict(UK_model_tbats)
predict_UK
model_tbats.forecast_UK <- forecast(UK_model_tbats,  h = 6)
plot(model_tbats.forecast_UK)

summary(UK_model_tbats)
plot(UK_model_tbats)
for_tbats <- forecast::forecast(UK_model_tbats, h = 1)
df_tbats = as.data.frame(for_tbats)
test_UK$tbats = df_tbats$'Point Forecast'
MAPE(test_UK$tbats, test_UK$Monthly.Unemployment.Rate)# UK Unemployment Rate MAPE 0.08583349

############################################################################################################
# SG
m_SG <- read.csv("/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/m_SG_index.csv")
train_SG <- head(m_SG, 31)
test_SG <- tail(m_SG, 1)

SG <- ts(m_SG[, 8], start = c(2020,1), end = c(2022,8), frequency = 12)
plot(as.xts(SG), main = 'SG Monthly Unemployment Rate (Time Series Data)')

SG_ts <- ts(train_SG[, 8], start = c(2020,1), end = c(2022,7),frequency = 12)

SG_model_tbats <- tbats(SG_ts)

# MAPE for training dataset
MAPE(SG_model_tbats$fitted.values, train_SG$Monthly.Unemployment.Rate) # MAPE: 0.04604015

# Predict
predict <- predict(SG_model_tbats)
predict
model_tbats.forecast_SG <- forecast(SG_model_tbats,  h = 6)
plot(model_tbats.forecast_SG)

summary(SG_model_tbats)
plot(SG_model_tbats)
for_tbats <- forecast::forecast(SG_model_tbats, h = 1)
df_tbats = as.data.frame(for_tbats)
test_SG$tbats = df_tbats$'Point Forecast'
MAPE(test_SG$tbats, test_SG$Monthly.Unemployment.Rate) # SG Unemployment Rate MAPE 0.1071013

###########################################################################################################
