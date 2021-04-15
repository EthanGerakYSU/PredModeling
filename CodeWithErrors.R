#https://www.kaggle.com/abharg16/predicting-epl-team-season-win-percentages/data

library(RSQLite)
library(dplyr)
library(xgboost)
library(Matrix)
library(kableExtra)
library(knitr)


con <- dbConnect(drv=RSQLite::SQLite(), dbname="database.sqlite")

tables <- dbListTables(con)
tables <- tables[tables != "sqlite_sequence"]
matches = dbReadTable(con, "Match")
teams = dbReadTable(con, "Team")
colnames(matches)[colnames(matches)=="id"] <- "match_ID"
Team_Attributes = dbReadTable(con, "Team_Attributes")

dbDisconnect(con)









matches <- left_join(matches, teams, by = c("home_team_api_id" = "team_api_id")) %>%
  select(-home_team_api_id) %>%
  rename( home_team = team_long_name) 

matches <- left_join(matches , teams, by = c('away_team_api_id' = "team_api_id")) %>%
  select (-away_team_api_id) %>%
  rename(away_team = team_long_name)

Team_Attributes <- left_join(Team_Attributes, teams, by='team_api_id') %>%
  select (-c(team_api_id) ) %>%
  rename(team_name = team_long_name)

head(Team_Attributes)





matches$home_result <- NA

matches$home_result[which(matches$home_team_goal > matches$away_team_goal)] <- "W"
matches$home_result[which(matches$home_team_goal < matches$away_team_goal)] <- "L"

home_win_perc <- filter(matches, home_result == "W") %>%
  group_by(season,home_team) %>%
  summarise(home_win_perc = sum(home_result == "W")/19)

away_win_perc <- filter(matches, home_result == "L") %>%
  group_by(season,away_team) %>%
  summarise(away_team_win_perc = sum(home_result == "L")/19)

head(home_win_perc)
head(away_win_perc)

#rm(EPL_ID)
#rm(matches)
#rm(teams)




Team_Attributes <- select(Team_Attributes, -buildUpPlayDribbling)


Team_Attributes$date[which(Team_Attributes$date == '2010-02-22 00:00:00')] = '2009/2010'
Team_Attributes$date[which(Team_Attributes$date == '2011-02-22 00:00:00')] = '2010/2011'
Team_Attributes$date[which(Team_Attributes$date == '2012-02-22 00:00:00')] = '2011/2012'
Team_Attributes$date[which(Team_Attributes$date == '2013-09-20 00:00:00')] = '2013/2014'
Team_Attributes$date[which(Team_Attributes$date == '2014-09-20 00:00:00')] = '2014/2015'
Team_Attributes$date[which(Team_Attributes$date == '2014-09-19 00:00:00')] = '2014/2015'
Team_Attributes$date[which(Team_Attributes$date == '2015-09-10 00:00:00')] = '2015/2016'
Team_Attributes$date[which(Team_Attributes$date == '2015-09-20 00:00:00')] = '2015/2016'

Team_Attributes <- rename(Team_Attributes, season = date )
Team_Attributes <- rename(Team_Attributes, home_team = team_name)

head(Team_Attributes)




Dataset <- inner_join(Team_Attributes, home_win_perc, by = c('home_team', 'season'))
away_win_perc <- rename(away_win_perc, team = away_team)
Dataset <- rename(Dataset, team = home_team)
Dataset <- inner_join(Dataset, away_win_perc, by = c('team', 'season'))

Dataset$win_perc = (Dataset$home_win_perc + Dataset$away_team_win_perc)/2


head(Dataset)

#rm(home_win_perc, Team_Attributes)
#dbDisconnect(db)
#rm(db)










numeric_predictors <- c('buildUpPlaySpeed','buildUpPlayPassing','chanceCreationPassing',
                         'chanceCreationCrossing','chanceCreationShooting', 'defencePressure', 
                        'defenceAggression','defenceTeamWidth')
Dataset[,numeric_predictors] = sapply(Dataset[,numeric_predictors], as.numeric)

non_factor_variables <- append(numeric_predictors, 'win_perc')
factor_variables <- colnames(Dataset[,-which(colnames(Dataset) %in% non_factor_variables)])
Dataset[,factor_variables] <- lapply(Dataset[,factor_variables], as.factor)


smp_size <- floor(0.75 * nrow(Dataset))
set.seed(5)
train_ind <- sample(seq_len(nrow(Dataset)), size = smp_size)

train <- Dataset[train_ind,]
test <- Dataset[-train_ind,]

labels <- select(test, c(team, season))

train <- select(train, -season)
test <- select(test, -season)

X_train <- select(train, -win_perc)
X_train <- model.matrix ( ~. +0, data = X_train)
y_train <- train$win_perc

X_test <- select(test, -win_perc)
X_test <- model.matrix(~. +0, data = X_test)
y_test <- test$win_perc





Xg_train <- xgb.DMatrix(data = X_train, label = y_train)
Xg_test <- xgb.DMatrix(data = X_test, label = y_test)


final_model <- xgb.train(data = Xg_train, 
                         nrounds = 20,
                         eta = 0.1,
                         objective = "reg:linear")






y_pred <- predict(final_model, Xg_test)
Results <- (cbind(y_pred, y_test, labels))
print(Results)


