library(data.table)
library(Matrix)
library(dplyr)
library(MLmetrics)
library(lightgbm)

# US
m_US <- read.csv("/Users/Jagonii/Desktop/Singapore Master's Degree/NUS MFE/Semester1/Financial Econometrics/group project data/m_US_index.csv")
train <- head(m_US, round(length(m_US$Date) * 0.8))
h <- length(m_US$Date) - length(train$Date)
test <- tail(m_US, h)

x_train <- train[,2:16]
x_test <- train[,17]
y_train <- test[,2:16]
y_test <- test[,17]

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





