### NFL Big Data Bowl Ranking Models

### ELO Model

### Import Libraries and Files
library("elo")
library("tidyverse")
require(ggplot2)
library("randomcoloR")
setwd('/Users/zbradlow/Desktop')
finalMatchupData<-read.csv('final_matchup_data.csv', stringsAsFactors = FALSE)
CBWRData <- finalMatchupData

### Data Cleaning
CBWRData <- CBWRData[order(CBWRData[,'start_time']),]
CBWRData[,'offWin'] <- round(CBWRData[,'offenseWin'])
CBWRData[,'defWin'] <- round(1 - CBWRData[,'offenseWin'])
CBWRData[,'MOV'] <- abs(CBWRData[,'offenseWin'] - (1 - CBWRData[,'offenseWin']))
CBWRData <- CBWRData %>% select('week', 'playId', 'offWin', 'offPlayerName', 'defWin', 'defenseTeam', 'defPlayerName', 'offenseWin', 'possessionTeam', 'MOV') %>% distinct()

# Optimizing K Value
opt_mses<-c()
sequences<-seq(1,20,1)
for (i in 1:length(sequences)){
  testModel<-(elo.run2(score(CBWRData[,'offWin'],CBWRData[,"defWin"])~CBWRData[,"offPlayerName"]+CBWRData[,"defPlayerName"],
                       data = CBWRData,k=CBWRData[,'MOV'] * sequences[i],initial.elos=1000))
  opt_mses[i]<-summary(testModel)$mse
}

### Outputs Optimal K-Value
sequences[which.min(opt_mses)]

### ELO Model Given Optimal K-Value
kValue = sequences[which.min(opt_mses)]
CBWRData[,'playOrder'] = seq(1,30631,1)
weekToWeekCutOffs <- c(1-1)
for(i in 1:17) {
  weekToWeekCutOffs[i+1] <- as.integer(max(CBWRData[CBWRData[,'week']==i,'playOrder']))
}
offPlayers <- c(unique(CBWRData[,'offPlayerName']))
defPlayers <- c(unique(CBWRData[,'defPlayerName']))
players <- c(offPlayers, defPlayers)
weeklyRankings <- data.frame(players)
colnames(weeklyRankings) <- 'playerName'
for (i in 1:(length(weekToWeekCutOffs)-1)) {
  eloModel<-(elo.run2(score(CBWRData[1:weekToWeekCutOffs[i+1],'offWin'],CBWRData[1:weekToWeekCutOffs[i+1],"defWin"])~CBWRData[1:weekToWeekCutOffs[i+1],"offPlayerName"]+CBWRData[1:weekToWeekCutOffs[i+1],"defPlayerName"],
                      data = CBWRData[1:weekToWeekCutOffs[i+1],],k=CBWRData[1:weekToWeekCutOffs[i+1],'MOV'] * kValue,initial.elos=1000))
  temp_rankings<-data.frame(final.elos(eloModel))
  colname = paste("Week", i, "ELO")
  colnames(temp_rankings)<-c(colname)
  temp_rankings[,'playerName']<-row.names(temp_rankings)
  weeklyRankings = left_join(weeklyRankings, temp_rankings, by='playerName')
}

# Spaghetti Plot of All Players
vector<-c(rep(0,17))
plot(vector,type = "o",col ="black", xlab = "Week", ylab = "ELO Rating",ylim=c(920,1080),
     main = "ELO Rating by Week by Player")
for(j in 1:414){
  vector<-c()
  for (i in 2:18){
    vector[i-1]<-weeklyRankings[j,i]
    lines(vector, type = "o",col=randomColor(1, hue="random"))
  }
}
axis(side=1,at=c(seq(1,17,1)))

# Optimizing How Many Times to Iterate Over
iterate_mses <- c()
for(i in 1:20){
  newInitialElos<-final.elos(eloModel)
  iterateTestModel<-(elo.run2(score(CBWRData[,'offWin'],CBWRData[,"defWin"])~CBWRData[,"offPlayerName"]+CBWRData[,"defPlayerName"],
                      data = CBWRData,k=CBWRData[,'MOV'] * kValue,initial.elos=newInitialElos))
  iterate_mses[i] <- summary(iterateTestModel)$mse
}

### Outputs Optimal Number of Iterations
which.min(iterate_mses)

### Given Optimal Number of Iterations
for(i in which.min(iterate_mses)){
  newInitialElos<-final.elos(eloModel)
  finalELOModel<-(elo.run2(score(CBWRData[,'offWin'],CBWRData[,"defWin"])~CBWRData[,"offPlayerName"]+CBWRData[,"defPlayerName"],
                              data = CBWRData,k=CBWRData[,'MOV'] * kValue,initial.elos=newInitialElos))
}
summary(finalELOModel)
finalRankings<-data.frame(final.elos(finalELOModel))
colnames(finalRankings)<-c("ELO")
finalRankings[,'playerName']<-row.names(finalRankings)
finalRankings<-arrange(finalRankings,desc(finalRankings[,"ELO"]))

# Add in Wins, Losses, ELO, and SOS
def_player_wins <- CBWRData %>%
  group_by(defPlayerName) %>%
  summarise(Wins = sum(offWin)+sum(defWin)-sum(offenseWin), Losses = sum(offenseWin), Routes = sum(offWin)+sum(defWin)) 
def_player_wins = left_join(def_player_wins, finalRankings, by = c('defPlayerName' = 'playerName'))
colnames(def_player_wins) = c('defPlayerName', 'defWins', 'defLosses', 'defRoutes', 'defELO')

off_player_wins <- CBWRData %>%
  group_by(offPlayerName) %>%
  summarise(Wins = sum(offenseWin), Losses = sum(offWin)+sum(defWin)-sum(offenseWin), Routes = sum(offWin)+sum(defWin)) 
off_player_wins = left_join(off_player_wins, finalRankings, by = c('offPlayerName' = 'playerName'))
colnames(off_player_wins) = c('offPlayerName', 'offWins', 'offLosses', 'offRoutes', 'offELO')

CBWRData = left_join(CBWRData, off_player_wins, by = 'offPlayerName')
CBWRData = left_join(CBWRData, def_player_wins, by = 'defPlayerName')

defOppELO <- CBWRData %>%
  group_by(defPlayerName) %>%
  summarise(MeanOffELO = mean(offELO)) 
def_player_wins[,'MeanOffELO'] = defOppELO[,'MeanOffELO']

offOppELO <- CBWRData %>%
  group_by(offPlayerName) %>%
  summarise(MeanDefELO = mean(defELO)) 
off_player_wins[,'MeanDefELO'] = offOppELO[,'MeanDefELO']

### Bradley Terry Model
# Data Cleaning
finalMatchupData <- finalMatchupData[order(finalMatchupData[,'start_time']),]
offPlayers <- c(finalMatchupData[,'offPlayerName'])
defPlayers <- c(finalMatchupData[,'defPlayerName'])
players <- c(offPlayers, defPlayers)
playersReverseOrder <- c(defPlayers, offPlayers)
BTData = data.frame(players)
BTData['Opponent'] = playersReverseOrder
BTData[1:30631, 'Win'] = finalMatchupData[,'offenseWin']
BTData[30632:61262, 'Win'] = 1 - finalMatchupData[,'offenseWin']
colnames(BTData) <- c('playerName', 'oppPlayerName', 'wins')

# Summarizing Wins and Initial Ratings
rating_scale = 1000
team_wins <- BTData %>%
  group_by(playerName) %>%
  summarise(Wins = sum(wins)) %>%
  mutate(Rating = rating_scale)  %>%
  filter(Wins >= 50)

# Get Number of Wins and Games Played for Each Pairing
pairsData <- BTData %>% 
  group_by(playerName, oppPlayerName) %>%
  summarise(Wins = sum(wins),
            Routes = n())

# Initialize Metrics to Run Bradley-Terry Model
step_save <- team_wins %>%
  select(playerName, Rating) %>%
  mutate(Step = 1)
iter = team_wins
keep_steps = TRUE
dif <- 1000
i <- 2

# Bradley-Terry Model that Runs Until Expected Number of Wins and Actual Number of Wins Converges
while(dif > rating_scale/100) {
  new_rating <- c()
  for(j in 1:length(unique(team_wins$playerName))) {
    gp <- sum(pairsData$Routes[pairsData$playerName == iter$playerName[j]])
    rate <- iter$Rating[j]
    opp <- iter$Rating[-j]
    new_rating[j] <- team_wins$Wins[j] * sum(gp/(rate + opp))^-1 #* rate,
  }
  
  # Normalize new ratings and update the dataframe
  norm_rating <- (new_rating / sum(new_rating)) * sum(team_wins$Rating)
  iter$Rating <- norm_rating
  
  # Calculate difference between old and new ratings by step
  dif <- sum(abs(step_save$Rating[step_save$Step == i-1] - norm_rating))
  
  # Add step to dataframe
  step_save <- step_save %>%
    bind_rows(iter %>%
                select(playerName, Rating) %>%
                mutate(Step = i))
  i <- i + 1
}

# Comparing ELO and BT Rankings
BTRankings <- step_save %>% filter(Step == max(Step)) %>% select(playerName, Rating)
colnames(BTRankings) <- c('defPlayerName', 'BTELO')
combinedRankings <- left_join(def_player_wins, BTRankings, by = 'defPlayerName')
combinedRankings <- combinedRankings[complete.cases(combinedRankings),]

# Linear Regression Comparing Both Models
rankingModelComparison = lm(BTELO ~ defELO, data=combinedRankings)
summary(rankingModelComparison)

# Plot to Show Convergence
step_save %>%
  ggplot(aes(Step, Rating, group = paste(playerName))) +
  geom_line() +
  geom_label(data = step_save %>% 
               group_by(playerName) %>%
               filter(Step == max(Step)) %>%
               ungroup(),
             aes(Step, Rating, label = paste(playerName)), alpha = 0.5)
