library(tidyverse)
library(xgboost)
library(caret)

# Route Success Classification Model
coverage_preds <- read_csv("full_cb_wr_data_preds.csv")
coverage_preds <- coverage_preds[,-1]

plays <- read_csv("plays.csv")
games <- read_csv("games.csv")
plays <- plays %>%
  left_join(games, by = "gameId") %>%
  select(gameId, playId, quarter, down, yardsToGo, yardlineNumber, yardlineSide, offenseFormation, defendersInTheBox, numberOfPassRushers, preSnapVisitorScore, preSnapHomeScore, possessionTeam, homeTeamAbbr, visitorTeamAbbr) %>%
  mutate(yardline_100 = case_when(
    yardlineSide == possessionTeam ~ yardlineNumber,
    yardlineSide != possessionTeam ~ 100 - yardlineNumber,
    yardlineNumber == 50 ~ 50
  ),
         poss_home = ifelse(possessionTeam == homeTeamAbbr,
                            T,
                            F),
         poss_score_diff = ifelse(poss_home == T,
                                  preSnapHomeScore - preSnapVisitorScore,
                                  preSnapVisitorScore - preSnapHomeScore)
         ) %>%
  select(gameId, playId, quarter, down, yardsToGo, yardline_100, offenseFormation, defendersInTheBox, numberOfPassRushers, poss_score_diff)

coverage_preds <- coverage_preds %>%
  left_join(plays, by = c("gameId", "playId"))
# what minimum probability of man coverage do we want?
threshold <- 0.5

# to train our model of route success, we want only one-on-ones and targeted routes
target_route_data <- coverage_preds %>%
  filter(isTarget == T & pred_man > threshold) %>%
  select(offenseWin, nFrames:WRToCBRatio, heightAdv, -route, quarter, down, yardsToGo, yardline_100, offenseFormation, defendersInTheBox, numberOfPassRushers, poss_score_diff) %>%
  mutate(offenseWin = ifelse(offenseWin == T, 1, 0))

# train/test split - we'll start with 90:10
set.seed(01072021)
train_index <- sample(1:nrow(target_route_data), .9 * nrow(target_route_data), replace = F)
X_train <- target_route_data[train_index, -1]
Y_train <- target_route_data[train_index, 1]
X_test <- target_route_data[-train_index, -1]
Y_test <- target_route_data[-train_index, 1]

# format data for xgboost
dtrain <- xgb.DMatrix(data = data.matrix(X_train), label = data.matrix(Y_train))
dtest <- xgb.DMatrix(data = data.matrix(X_test), label = data.matrix(Y_test))
watchlist = list(train = dtrain, test = dtest)


### xgboost with parameter tuning ###

lowest_error_list = list()
parameters_list = list()
set.seed(01072021)
for(iter in 1:100){
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
  set.seed(01072021)
  mdcv <- xgb.train(data=dtrain,
                    booster = "gbtree",
                    objective = "binary:logistic",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    nrounds= 100,
                    eval_metric = "logloss", # we plan to multiply probabilities so log loss is preferable
                    early_stopping_rounds= 10,
                    print_every_n = 100,
                    watchlist = list(train= dtrain, val= dtest)
  )
  lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$val_error))
  lowest_error_list[[row]] <- lowest_error
}

# Create object that contains all log losses
lowest_error_df = do.call(rbind, lowest_error_list)

# Bind columns of log loss values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)

# Quickly display best logg loss
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
                      nrounds = 100,
                      print_every_n = 10,
                      eval_metric = "logloss",
                      early_stopping_rounds = 10,
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
full_coverage_data <- coverage_preds %>%
  filter(pred_man > threshold) %>%
  mutate(offenseWin = ifelse(offenseWin == T, 1, 0)) %>%
  select(offenseWin, nFrames:WRToCBRatio, heightAdv, -route, quarter, down, yardsToGo, yardline_100, offenseFormation, defendersInTheBox, numberOfPassRushers, poss_score_diff)

X_full <- full_coverage_data %>% select(-offenseWin)
Y_full <- full_coverage_data$offenseWin
dfull <- xgb.DMatrix(data = data.matrix(X_full), label = data.matrix(Y_full))

pred_full <- predict(xgb_tune, dfull)
pred_full

# get a final data set to throw into Elo rating system
final_matchup_data <- coverage_preds %>% 
  filter(pred_man > threshold) %>%
  mutate(pred_offenseWin = pred_full) %>%  #add predictions to dataset
  select(gameId:start_time, isTarget, offenseWin, pred_offenseWin) %>%
  mutate(offenseWin = ifelse(isTarget == T,
                             offenseWin,
                             pred_offenseWin)) %>%
  select(-pred_offenseWin)

write_csv(final_matchup_data, file = "final_matchup_data.csv")
