library(data.table)
library(Matrix)
library(dplyr)
library(MLmetrics)
library(caret)
library(randomForest)
library(gbm)
library(xgboost)
library(lightgbm)
library(forecast)

########################################## TBATS MODEL ####################################################
# define MPAE
mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))
  return (mape)
}

# US
m_US <- read.csv("/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/m_US_index.csv")
train <- head(m_US, round(length(m_US$Date) * 0.8))
h <- length(m_US$Date) - length(train$Date)
test <- tail(m_US, h)

#x_train <- train[,2:16]
#y_train <- train[,17]
#x_test <- test[,2:16]
#y_test <- test[,17]

dat_ts <- ts(train[, 17])

model_tbats <- tbats(dat_ts)
summary(model_tbats)

for_tbats <- forecast::forecast(model_tbats, h = 6)
df_tbats = as.data.frame(for_tbats)
test$tbats = df_tbats$'Point Forecast'
mape(test$Monthly.Unemployment.Rate, test$tbats) 
# US Unemployment Rate MAPE 0.04293467

# UK
m_UK <- read.csv("/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/m_UK_index.csv")
train <- head(m_UK, round(length(m_UK$Date) * 0.8))
h <- length(m_UK$Date) - length(train$Date)
test <- tail(m_UK, h)

dat_ts <- ts(train[, 9])

model_tbats <- tbats(dat_ts)
summary(model_tbats)

for_tbats <- forecast::forecast(model_tbats, h = 6)
df_tbats = as.data.frame(for_tbats)
test$tbats = df_tbats$'Point Forecast'
mape(test$Monthly.Unemployment.Rate, test$tbats) 
# UK Unemployment Rate MAPE 0.06133777


# SG
m_SG <- read.csv("/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/m_SG_index.csv")
train <- head(m_SG, round(length(m_SG$Date) * 0.8))
h <- length(m_SG$Date) - length(train$Date)
test <- tail(m_SG, h)

dat_ts <- ts(train[, 8])

model_tbats <- tbats(dat_ts)
summary(model_tbats)

for_tbats <- forecast::forecast(model_tbats, h = 6)
df_tbats = as.data.frame(for_tbats)
test$tbats = df_tbats$'Point Forecast'
mape(test$Monthly.Unemployment.Rate, test$tbats) 
# SG Unemployment Rate MAPE 0.1928157

###########################################################################################################






rf = randomForest(Monthly.Unemployment.Rate ~ ., data = train)
print(rf)

predictions = predict(rf, newdata = train)
mape(train$Monthly.Unemployment.Rate, predictions)

predictions = predict(rf, newdata = test)
mape(test$Monthly.Unemployment.Rate, predictions) 

varImpPlot(rf)

rf_revised1 = randomForest(Monthly.Unemployment.Rate ~ .-Date - NASDAQ - US.Condition.of.All.Federal.Reserve.Banks.Total.Assets.. - UST.10.2.Spread - M1.Index, data = train)
print(rf_revised1) 
varImpPlot(rf_revised1)

predictions = predict(rf_revised1, newdata = train)
mape(train$Monthly.Unemployment.Rate, predictions) 

predictions = predict(rf_revised1, newdata = test)
mape(test$Monthly.Unemployment.Rate, predictions) 


rf_revised2 = randomForest(Monthly.Unemployment.Rate ~ .-Date - NASDAQ - US.Condition.of.All.Federal.Reserve.Banks.Total.Assets.. - UST.10.2.Spread - M1.Index - CBOE.Equity.Put.Call.Ratio, data = train)
print(rf_revised2) 
varImpPlot(rf_revised2)


#define final training and testing sets
xgb_train = xgb.DMatrix(data = as.matrix(x_train), label = y_train)
xgb_test = xgb.DMatrix(data = as.matrix(x_test), label = y_test)

#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteartion
xgbmodel_US = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

# minimum RMSE score is achieved at iteration 6
#define final model
model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 6, verbose = 0)
summary(model_xgboost)

#use model to make predictions on test data
y_pred = predict(model_xgboost, xgb_test)

# performance metrics on the test data

mean((y_test - y_pred)^2) #mse - Mean Squared Error

caret::RMSE(y_test, y_pred) #rmse - Root Mean Squared Error

y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

x = 1:length(y_test)                   # visualize the model, actual and predicted data
plot(x, y_test, col = "red", type = "l")
lines(x, y_pred, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original y_test", "predicted y_test"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))





gbm.fit <- gbm(
  formula = Monthly.Unemployment.Rate ~ .-Date,
  distribution = "gaussian",
  data = train,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


dtrain = lgb.Dataset(x_train, label = y_train)
dtest = lgb.Dataset.create.valid(dtrain, x_test, label = y_test)

# define parameters
params = list(
  objective = "regression"
  , metric = "l2"
  , min_data = 1L
  , learning_rate = .3
)

# validataion data
valids = list(test = dtest)



# m_US %>% 
#   gather(Series, Value, -Date) %>%
#   group_by(Series) %>%
#   summarise(
#     adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
#     adf = adf.test(Value, alternative = "stationary")$p.value<0.05
#   )
# 
# d1_m_US <- as.data.frame(lapply(m_US[, -c(1, 12, 17)], diff, lag=1))
# insert_df <- m_US[2:length(m_US[,1]),c(1, 12, 17)]
# d1_m_US <- cbind(insert_df, d1_m_US)
# 
# d1_m_US %>% 
#   gather(Series, Value, -Date) %>%
#   group_by(Series) %>%
#   summarise(
#     adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,  
#     adf = adf.test(Value, alternative = "stationary")$p.value<0.05
#   )
# 
# d2_m_US <- as.data.frame(lapply(d1_m_US[, -c(1, 2, 3, 6, 9, 15, 16, 17)], diff, lag=1))
# insert_df <- d1_m_US[2:length(d1_m_US[,1]),c(1, 2, 3, 6, 9, 15, 16, 17)]
# d2_m_US <- cbind(insert_df, d2_m_US)

# d2_m_US %>% 
#   gather(Series, Value, -Date) %>%
#   group_by(Series) %>%
#   summarise(
#     adf.pvalue = adf.test(Value, alternative = "stationary")$p.value,      adf = adf.test(Value, alternative = "stationary")$p.value<0.05
#   )





