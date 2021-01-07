####################
# NFL Big Data Bowl 2021
# Zach Bradlow, Zach Drapkin, Ryan Gross, Sarah Hu
####################
library(tidyverse)

setwd(getwd())
##########
# Reading in & cleaning data

## Read in week 1 of tracking data
week_1 <- read_csv("week1.csv")

## Read in weeks 2-17
more_weeks <- NULL
for(i in 2:17){
  more_weeks <- bind_rows(more_weeks, 
                          read_csv(paste("week",i,".csv", sep = "")
                          ))
}

all_weeks <- bind_rows(week_1, more_weeks)
rm(more_weeks)
rm(week_1)

# write_csv(all_weeks, "all_weeks.csv")

games <- read_csv("games.csv") %>% 
  select(gameId, homeTeamAbbr, visitorTeamAbbr, week)

plays <- read_csv("plays.csv") %>%
  select(gameId, playId, possessionTeam, playType, playDescription, yardlineSide, yardlineNumber, passResult, epa, isDefensivePI)

all_weeks <- left_join(all_weeks, games, by = "gameId")

plays <- left_join(plays, games, by = "gameId") %>%
  mutate(defenseTeam = ifelse(possessionTeam == homeTeamAbbr, # define which team is on defense
                              visitorTeamAbbr,
                              homeTeamAbbr),
         defenseWin = ifelse((passResult != "C" & !isDefensivePI) | epa <= 0, T, F),
         offenseWin = ifelse((passResult == "C" | isDefensivePI == T) & epa > 0, T, F)
  ) # defense wins if no defensive PI and no complete pass, or if pass is complete and not positive EPA

all_weeks <- left_join(all_weeks,
                       plays %>%
                         select(gameId, playId, possessionTeam, yardlineSide, yardlineNumber),
                       by = c("gameId", "playId"))

all_weeks <- all_weeks %>%
  mutate(defenseTeam = ifelse(possessionTeam == homeTeamAbbr, # define which team is on defense
                              visitorTeamAbbr,
                              homeTeamAbbr),
         teamAbbr = ifelse(team == "home",
                           homeTeamAbbr,
                           visitorTeamAbbr),
         onDefense = ifelse(defenseTeam == teamAbbr, T, F), # is a player on defense?
         onOffense = ifelse(possessionTeam == teamAbbr, T, F)) %>% # is a player on offense?
  select(-homeTeamAbbr, -visitorTeamAbbr)

all_weeks <- all_weeks %>%
  mutate(x = ifelse(playDirection == "left", # standardize x
                    120-x, 
                    x), 
         y = ifelse(playDirection == "left", # standardize y
                    160/3 - y, 
                    y),
         dir = ifelse(playDirection == "left", # standardize dir
                      ifelse(dir <= 180,
                             dir + 180,
                             dir - 180),
                      dir),
         o = ifelse(playDirection == "left", # standardize o
                    ifelse(o <= 180,
                           o + 180,
                           o - 180),
                    o),
         dir = pi*dir/180, # convert dir to radians
         o = pi*o/180, # convert o to radians
         dx = dis * sin(dir),
         dy = dis * cos(dir),
         xLineOfScrimmage = case_when(
           possessionTeam == yardlineSide ~ 10 + yardlineNumber,
           defenseTeam == yardlineSide ~ 110 - yardlineNumber,
           yardlineNumber == 50 ~ 60
         ),
         xDepth = x - xLineOfScrimmage) # difference between line of scrimmage and x coordinate

all_weeks <- all_weeks %>%
  group_by(gameId, playId, frameId) %>%
  # filter("Football" %in% displayName) %>%
  filter("QB" %in% position) %>%
  mutate(#xFootball = x[displayName == "Football"], # coordinates of the football during a given frame
         #yFootball = y[displayName == "Football"],
         xQB = first(x[position == "QB"]), # coordinates of the QB
         yQB = first(y[position == "QB"]),
         oQB = first(o[position == "QB"])) %>%
  ungroup()

all_weeks <- all_weeks %>%
  group_by(gameId, playId) %>%
  mutate(beforeThrowOrSack = ifelse(frameId < first(frameId[event %in% c("qb_sack", "qb_strip_sack", "tackle", "run", "pass_forward", "pass_shovel", "qb_spike")]),T,F))

all_weeks <- all_weeks %>%
  filter(beforeThrowOrSack == T, displayName != "Football")


opp_sep_df <- all_weeks %>%
  filter((onDefense == T & position == "CB") | onOffense == T, beforeThrowOrSack == T)

# Function to find separation between offensive and defensive players adapted from Jarrod Pelkofer's Kaggle notebook
get_opponent_separation <- function(play_data_df) {
  
  # data frame of only defense players
  def_team_df <- play_data_df %>% 
    filter(onDefense == T) %>% 
    select(
      # play description columns
      time, week, possessionTeam, defenseTeam,
      # frame/ event cols
      frameId, event, 
      # defensive player columns
      nflIdDef = nflId, defPlayerName = displayName, defPos = position, teamDef = teamAbbr, 
      xDef = x, yDef = y, sDef = s, aDef = a, disDef = dis, oDef = o, dirDef = dir,
      # other columns
      dx, dy, xQB, yQB, oQB, xLineOfScrimmage, xDepth 
    )
  
  # data frame of only offense players
  off_team_df <- 
    play_data_df %>% 
    filter(onOffense == T) %>% 
    select(
      # frame
      frameId,
      # offensive player columns
      nflIdOff = nflId, offPlayerName = displayName, offPos = position, teamOff = teamAbbr, 
      xOff = x, yOff = y, sOff = s, aOff = a, disOff = dis, oOff = o, dirOff = dir, route
    )
  
  # join data on frame id, calculate distance defensive player is from each offensive player
  distance_to_opp_df <- 
    def_team_df %>% left_join(off_team_df, by = "frameId") %>% 
    mutate(distanceFromOpponent = sqrt((xDef - xOff)^2 + (yDef - yOff)^2)) 
  
  return(distance_to_opp_df)
  
}

# Calculate frame-by-frame separation for each offense-defense player combination
opp_sep_df <- opp_sep_df %>%
  nest(play_data = c(-gameId, -playId)) %>%
  mutate(separation_data = map(play_data, ~ get_opponent_separation(.x))) %>% 
  select(-play_data) %>% 
  unnest(separation_data)

# write_csv(opp_sep_df, "opp_sep_df.csv")

# Now we want to figure out who is closest to whom on each play
matchup_data <- opp_sep_df %>%
  group_by(gameId, playId, nflIdDef, defPlayerName, nflIdOff, offPlayerName) %>%
  mutate(mean_distance = mean(distanceFromOpponent)) %>%
  ungroup() %>%
  group_by(gameId, playId, nflIdDef, defPlayerName) %>%
  filter(mean_distance == min(mean_distance), defPos == "CB" & offPos == "WR") # filtering for only matchups with closest offensive player and make sure those are WR-CB

# Engineer all the features we want before we summarize for a given play - adapted from Joe Andruzzi's notebook which adapts from CMU group paper
matchup_data <- matchup_data %>%
  group_by(gameId, playId, frameId, nflIdDef, defPlayerName) %>%
  mutate(dirDiff = dirDef - dirOff,
         slopeQB = atan2(yQB - yDef, xQB - xDef),
         slopeOpp = atan2(yOff - yDef, xOff - xDef),
         diffQB = slopeQB - oDef,
         diffQB = abs(ifelse(diffQB >= -1*pi & diffQB <= pi,
                             diffQB,
                             case_when(
                               diffQB > pi ~ diffQB - (2*pi),
                               diffQB < -1*pi ~ diffQB + (2*pi)))),
         diffOpp = slopeOpp - oDef,
         diffOpp = abs(ifelse(diffOpp >= -1*pi & diffOpp <= pi,
                              diffOpp,
                              case_when(
                                diffOpp > pi ~ diffOpp - (2*pi),
                                diffOpp < -1*pi ~ diffOpp + (2*pi)))),
         lookingAtQB = ifelse(diffQB < diffOpp, T, F),
         sidelineDist = abs(min(c(yDef, 160/3 - yDef)))
  ) %>%
  ungroup()

write_csv(matchup_data, "matchup_data.csv")

mate_sep_df <- all_weeks %>%
  filter(onDefense == T)

get_teammate_separation <- function(play_data_df){
  
  # data frame of CBs
  def_team_df <- play_data_df %>%
    filter(position == "CB") %>%
    select(
      # play description columns
      time, week, possessionTeam, defenseTeam,
      # frame/ event cols
      frameId, event, 
      # key defensive player columns
      nflIdDef = nflId, defPlayerName = displayName, defPos = position, xDef = x, yDef = y
    )

  
  
    
  # data frame of all defensive players
  teammate_df <- 
    play_data_df %>% 
    filter(onDefense == T) %>% 
    select(
      # frame
      frameId,
      # offensive player columns
      nflIdMate = nflId, matePlayerName = displayName, matePos = position, xMate = x, yMate = y
    )
  
  # join data on frame id, calculate distance defensive player is from each teammate
  distance_to_teammate_df <- 
    def_team_df %>% left_join(teammate_df, by = "frameId") %>% 
    filter(nflIdDef != nflIdMate) %>% # remove rows where the two players are the same (since the join does 11*11 rather than 11*10)
    mutate(distanceFromTeammate = sqrt((xDef - xMate)^2 + (yDef - yMate)^2)) 
  
  return(distance_to_teammate_df)
}

mate_sep_df <- mate_sep_df %>%
  nest(play_data = c(-gameId, -playId)) %>%
  mutate(mate_data = map(play_data, ~ get_teammate_separation(.x))) %>% 
  select(-play_data) %>% 
  unnest(mate_data)

# write_csv(mate_sep_df, "mate_sep_df.csv")

nearest_teammate <- mate_sep_df %>%
  group_by(gameId, playId, frameId, nflIdDef, defPlayerName) %>%
  summarise(minMateDist = min(distanceFromTeammate)) 

# write_csv(nearest_teammate, "nearest_teammate.csv")

final_tracking_df <- left_join(matchup_data, nearest_teammate, by = c("gameId", "playId", "frameId", "nflIdDef", "defPlayerName"))

final_tracking_df <- final_tracking_df %>%
  group_by(gameId, playId, week, nflIdDef, defPlayerName, defenseTeam, defPos, nflIdOff, offPlayerName, possessionTeam, offPos) %>%
  summarise(start_time = first(time),
            nFrames = max(frameId), # number of frames in the play
            xVar = var(xDef), # variance of x coordinate
            yVar = var(yDef), # variance of y coordinate
            sVar = var(sDef), # variance of speed
            sMax = max(sDef), # maximum defender speed
            dxVar = var(dx), # variance of x velocity
            dyVar = var(dy), # variance of y velocity
            xLOS = first(xLineOfScrimmage), # x position of the line of scrimmage
            maxDepth = max(xOff - xLineOfScrimmage), # maximum downfield depth for offensive player
            endDepth = xOff[max(frameId)] - xLineOfScrimmage[max(frameId)], # downfield depth for offensive player at final frame before throw/sack
            sidelineDistMean = mean(sidelineDist), # mean distance from sideline
            sidelineDistVar = var(sidelineDist), # variance of distance from sideline
            oppDistMean = mean(distanceFromOpponent), # mean separation from opponent
            oppDistVar = var(distanceFromOpponent), # variance of separation from opponent
            oppDistMax = max(distanceFromOpponent), # maximum distance from opponent
            mateDistMean = mean(minMateDist), # mean distance from nearest teammate
            mateDistVar = var(minMateDist), # variance of distance from nearest teammate
            mateDistMax = max(minMateDist), # maximum distance from nearest teammate
            dirDiffMean = mean(dirDiff), # mean difference in direction between player & opponent
            dirDiffVar = var(dirDiff), # variance of difference in direction between player & opponent
            diffOppMean = mean(diffOpp), # mean difference between orientation and orientation toward nearest opponent
            diffOppVar = var(diffOpp), # variance of difference between orientation and orientation toward nearest opponent
            diffQBMean = mean(diffQB), # mean difference between orientation and orientation toward QB
            diffQBVar = var(diffQB), # variance of difference between orientation and orientation toward QB
            lookingAtQBMean = mean(lookingAtQB), # mean of where defender is looking (1 = QB, 0 = WR)
            lookingAtQBVar = var(lookingAtQB), # variance of where defender is looking
            ratMean = mean(distanceFromOpponent/minMateDist), # mean ratio of distance from opponent to distance from nearest teammate
            ratVar = var(distanceFromOpponent/minMateDist), # variance of ratio of distance from opponent to distance from nearest teammate
            route = first(route) # receiver route
  )

final_tracking_df <- final_tracking_df %>%
  filter(nFrames > 9)

# Now we just need to have some positional summaries for each play
play_positions <- all_weeks %>%
  filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarise(WR = sum(position == "WR" & onOffense == T), # how many WRs
            TE = sum(position == "TE" & onOffense == T), # how many TEs
            RB = sum(position == "RB" & onOffense == T), # how many RBs
            CB = sum(position == "CB" & onDefense == T), # how many CBs
            FS = sum(position == "FS" & onDefense == T), # how many FSs
            WRToCBRatio = WR/CB) # ratio of WRs to CBs

# Merge that in
cb_wr_data <- left_join(final_tracking_df, play_positions,
                        by = c("gameId", "playId"))

# Merge in coverages, targets, and play outcome data
coverages_week1 <- read_csv("archive/coverages_week1.csv")
targets <- read_csv("archive/targetedReceiver.csv")

cb_wr_data <- left_join(cb_wr_data, coverages_week1, by = c("gameId", "playId"))

cb_wr_data <- cb_wr_data %>%
  mutate(manCoverage = ifelse(coverage %in% c("Cover 0 Man", "Cover 1 Man", "Cover 2 Man"), T, F)) %>%
  select(-coverage)

cb_wr_data <- left_join(cb_wr_data, targets, by = c("gameId", "playId"))

cb_wr_data <- cb_wr_data %>%
  mutate(isTarget = ifelse((targetNflId != nflIdOff) | is.na(targetNflId), F, T)) %>%
  select(-targetNflId)

cb_wr_data <- left_join(cb_wr_data,
                        plays %>%
                          select(gameId, playId, epa, defenseWin, offenseWin),
                        by = c("gameId", "playId"))

cb_wr_data <- cb_wr_data %>%
  filter(!is.na(offenseWin) & !is.na(defenseWin))

# write_csv(cb_wr_data, file = "full_cb_wr_data.csv")

# just week 1 to train coverage classifier
train_coverages <- cb_wr_data %>%
  filter(week == 1)

# write_csv(train_coverages, file = "train_coverages.csv")

# just targeted routes to train offense/defense win classifier
train_wins <- cb_wr_data %>%
  filter(isTarget == T)

# write_csv(train_wins, file = "train_wins.csv")

# adding height advantage/difference to the data set 
players <- read_csv("players.csv") %>%
  select(nflId, height, displayName) %>%
  mutate(height = eval(parse(text = str_replace(height, "-", "*12+"))))

off_heights <- players %>%
  filter(nflId %in% cb_wr_data$nflIdOff) %>%
  mutate(nflIdOff = nflId, heightOff = height)

def_heights <- players %>%
  filter(nflId %in% cb_wr_data$nflIdDef) %>%
  select(nflIdDef = nflId, heightDef = height)

cb_wr_data <- cb_wr_data %>%
  left_join(def_heights, by = "nflIdDef") %>%
  left_join(off_heights, by = "nflIdOff") %>%
  mutate(heightAdv = heightDef - heightOff)


