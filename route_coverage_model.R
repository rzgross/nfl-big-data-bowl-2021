library(car)
library(caret)
library(randomForest)
library(tidyverse)
library(xgboost)


# read in data
cb_wr_data <- read_csv("CB_WR_data.csv") 
cb_wr_data <- cb_wr_data %>% select(-X1)

# data for modeling receiver success
target_routes <- cb_wr_data %>%
  filter(targetOff == T, manCoverage == T) %>%
  ungroup() %>%
  select(successOff, nFrames:WRToCBRatio)

# data for modeling coverage
coverage_data <- cb_wr_data %>%
  select(manCoverage, nFrames:WRToCBRatio) %>%
  mutate(manCoverage = ifelse(manCoverage == T, 1, 0))

# train-test split - we'll start with 90:10
train_index <- sample(1:nrow(coverage_data), .9 * nrow(coverage_data), replace = F)
X_train <- coverage_data[train_index, -1]
Y_train <- coverage_data[train_index, 1]
X_test <- coverage_data[-train_index, -1]
Y_test <- coverage_data[-train_index, 1]


# full coverage dataset for predictions
full_cb_wr_data <- read_csv("full_cb_wr_data.csv")

full_coverage_data <- full_cb_wr_data %>%
  select(manCoverage, nFrames:WRToCBRatio, heightAdv) %>%
  mutate(manCoverage = ifelse(manCoverage == T, 1, 0),
         route = replace(route, route == "WHEEL", NA ),
         route = replace(route, route == "undefined", NA )) %>%
  relocate(heightAdv, .before=route)


  
# gradient boosting
dtrain <- xgb.DMatrix(data = data.matrix(X_train), label = data.matrix(Y_train))
dtest <- xgb.DMatrix(data = data.matrix(X_test), label = data.matrix(Y_test))
watchlist = list(train = dtrain, test = dtest)


### basic xgboost ###

basic_boost <- xgb.train(data = dtrain, eta = 0.1, nrounds = 20, max_depth = 5, colsample_bytree = 0.5, objective = "binary:logistic", watchlist = watchlist, eval_metric = "error")
summary(basic_boost)

pred_basic_boost <- predict(basic_boost,dtest)
confusionMatrix(data = as.factor((pred_basic_boost>0.5)*1), reference = as.factor(Y_test$manCoverage))

# predictions on full data
X_full <- full_coverage_data[,-1]
Y_full <- full_coverage_data[,1]
dfull <- xgb.DMatrix(data = data.matrix(X_full), label = data.matrix(Y_full))

pred_full <- predict(basic_boost,dfull)
pred_full 
sum(full_cb_wr_data$pred_man >.3, na.rm=T)/45329 # proportion of routes greater than a threshold

full_cb_wr_data <- full_cb_wr_data %>% mutate(pred_man = pred_full)  #add predictions to dataset

write.csv(full_cb_wr_data, file ='full_cb_wr_data_preds.csv')


### xgboost with parameter tuning ###

lowest_error_list = list()
parameters_list = list()
set.seed(2021)
for(iter in 1:1000){
  param <- list(booster = 'gbtree',
                objective = 'binary:logistic',
                max_depth = sample(3:10,1),
                eta=runif(1,.01,.3),
                subsample=runif(1,.7,1),
                colsample_bytree = runif(1,.6,1)
                )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)

# Use randomly created parameters to create 10,000 XGBoost-models
for (row in 1:nrow(parameters_df)){
  set.seed(2021)
  mdcv <- xgb.train(data=dtrain,
                    booster = "gbtree",
                    objective = "binary:logistic",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    nrounds= 300,
                    eval_metric = "error",
                    early_stopping_rounds= 30,
                    print_every_n = 100,
                    watchlist = list(train= dtrain, val= dtest)
  )
  lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$val_error))
  lowest_error_list[[row]] <- lowest_error
}

# Create object that contains all accuracy's
lowest_error_df = do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)

# Quickly display highest accuracy
max(randomsearch$`1 - min(mdcv$evaluation_log$val_error)`)

write_csv(randomsearch, "randomsearch.csv")

# Prepare table
randomsearch <- as.data.frame(randomsearch) %>%
  rename(val_acc = `1 - min(mdcv$evaluation_log$val_error)`) %>%
  arrange(-val_acc)


# Tuned-XGBoost model
params <- list(booster = "gbtree", 
               objective = "binary:logistic",
               max_depth = randomsearch[1,]$max_depth,
               eta = randomsearch[1,]$eta,
               subsample = randomsearch[1,]$subsample,
               colsample_bytree = randomsearch[1,]$colsample_bytree
               )
xgb_tune <- xgb.train(params = params,
                       data = dtrain,
                       nrounds =1000,
                       print_every_n = 10,
                       eval_metric = "error",
                       early_stopping_rounds = 30,
                       watchlist = list(train= dtrain, test= dtest))

# evaluation on train/test set and cross validation
pred_xgb_tune_test<- predict(xgb_tune,dtest)
confusionMatrix(data = as.factor((pred_xgb_tune_test>0.5)*1), reference = as.factor(Y_test$manCoverage))
summary(pred_xgb_tune_test)

pred_xgb_tune_train <- predict(xgb_tune,dtrain)
confusionMatrix(data = as.factor((pred_xgb_tune_train>0.5)*1), reference = as.factor(Y_train$manCoverage))
summary(pred_xgb_tune_train)

xgb.cv(params = params,
       data = dtrain,
       nrounds =1000,
       print_every_n = 10,
       eval_metric = "error",
       early_stopping_rounds = 30,
       watchlist = list(train= dtrain, test= dtest),
       nfold=10)

# predictions on full data
X_full <- full_coverage_data[,-1]
Y_full <- full_coverage_data[,1]
dfull <- xgb.DMatrix(data = data.matrix(X_full), label = data.matrix(Y_full))

pred_full <- predict(xgb_tune, dfull)
pred_full 
sum(full_cb_wr_data$pred_man >.3, na.rm=T)/45329 # proportion of routes greater than a threshold

full_cb_wr_data <- full_cb_wr_data %>% mutate(pred_man = pred_full)  #add predictions to dataset

write.csv(full_cb_wr_data, file ='full_cb_wr_data_preds.csv')

