                    # PACKAGES
library(tidyverse)
library(data.table)

                    ### TEAM RATINGS ###
## EXTRACT RELEVANT FEATURES FROM MATCHES, TEAMS, AND LEAGUE TABLES
toolazytotype <- c("home_player_1", "home_player_2", "home_player_3", "home_player_4", "home_player_5", "home_player_6",
                   "home_player_7", "home_player_8", "home_player_9", "home_player_10", "home_player_11",
                   "away_player_1", "away_player_2", "away_player_3", "away_player_4", "away_player_5", "away_player_6",
                   "away_player_7", "away_player_8", "away_player_9", "away_player_10", "away_player_11")

matches <- matches %>%
  select(country_id, date, 
         match_api_id, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal, goal,
         shoton, shotoff, foulcommit, card, cross, corner, possession,
         all_of(toolazytotype))

teams <- teams %>%
  select(team_api_id, team_long_name) %>%
  rename(team = team_long_name)

league <- league %>%
  select(country_id, name) %>%
  rename(league = name)

## SUBSTITUTE TEAM NAME FOR API ID
df1 <- left_join(matches, teams, by = c("home_team_api_id" = "team_api_id")) %>%
  select(-home_team_api_id, -away_team_api_id) %>%
  rename(home_team = team)

df2 <- left_join(matches, teams, by = c("away_team_api_id" = "team_api_id")) %>%
  select(-away_team_api_id, `home_team_api_id`) %>%
  rename(away_team = team)

dataset <- select(df2, away_team, match_api_id) %>%
  left_join(df1, df2, by = "match_api_id") %>%
  relocate(country_id, date, home_team, away_team)

## SUBSTITUTE LEAGUE NAME FOR API ID
dataset <- left_join(dataset, league, by = "country_id") %>%
  select(-country_id) %>%
  relocate(league)

## CREATING LINEUP SCORE FOR EACH TEAM
player_Attributes <- player_Attributes %>%
  select(player_api_id, date, overall_rating)

player <- player %>%
  select(player_api_id)

player_final <- left_join(player, player_Attributes, by = "player_api_id")

player_final <- player_final %>%
  rename(date2 = date)

## TRANSPORT PLAYER RATING TO DATASET
test <- dataset %>%
  select(-goal, -shoton, -shotoff, -foulcommit, -card, -cross, -corner, -possession)

test <- test %>%
  drop_na()

test$date <- as.Date(as.character(test$date))
player_final$date2 <- as.Date(player_final$date)

test <- test %>%
  inner_join(player_final, by = c("home_player_1" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_1, -date2, -dateDiff) %>%
  rename(home_player_1_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_2" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_2, -date2, -dateDiff) %>%
  rename(home_player_2_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_3" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_3, -date2, -dateDiff) %>%
  rename(home_player_3_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_4" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_4, -date2, -dateDiff) %>%
  rename(home_player_4_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_5" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_5, -date2, -dateDiff) %>%
  rename(home_player_5_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_6" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_6, -date2, -dateDiff) %>%
  rename(home_player_6_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_7" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_7, -date2, -dateDiff) %>%
  rename(home_player_7_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_8" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_8, -date2, -dateDiff) %>%
  rename(home_player_8_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_9" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_9, -date2, -dateDiff) %>%
  rename(home_player_9_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_10" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_10, -date2, -dateDiff) %>%
  rename(home_player_10_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("home_player_11" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-home_player_11, -date2, -dateDiff) %>%
  rename(home_player_11_rating = overall_rating)

test <- test %>%
  mutate(home_team_rating = (home_player_1_rating + home_player_2_rating + home_player_3_rating + home_player_4_rating +
                               home_player_5_rating + home_player_6_rating + home_player_7_rating + home_player_8_rating +
                               home_player_9_rating + home_player_10_rating + home_player_11_rating)/11) %>%
  select(-home_player_1_rating, -home_player_2_rating, -home_player_3_rating, -home_player_4_rating, -home_player_5_rating,
         -home_player_6_rating, -home_player_7_rating, -home_player_8_rating, -home_player_9_rating, -home_player_10_rating,
         -home_player_11_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_1" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_1, -date2, -dateDiff) %>%
  rename(away_player_1_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_2" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_2, -date2, -dateDiff) %>%
  rename(away_player_2_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_3" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_3, -date2, -dateDiff) %>%
  rename(away_player_3_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_4" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_4, -date2, -dateDiff) %>%
  rename(away_player_4_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_5" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_5, -date2, -dateDiff) %>%
  rename(away_player_5_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_6" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_6, -date2, -dateDiff) %>%
  rename(away_player_6_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_7" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_7, -date2, -dateDiff) %>%
  rename(away_player_7_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_8" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_8, -date2, -dateDiff) %>%
  rename(away_player_8_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_9" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_9, -date2, -dateDiff) %>%
  rename(away_player_9_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_10" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_10, -date2, -dateDiff) %>%
  rename(away_player_10_rating = overall_rating)

test <- test %>%
  inner_join(player_final, by = c("away_player_11" = "player_api_id")) %>%
  mutate(dateDiff = abs(date - date2)) %>%
  group_by(match_api_id) %>%
  dplyr::slice(which.min(dateDiff)) %>%
  dplyr::select(-away_player_11, -date2, -dateDiff) %>%
  rename(away_player_11_rating = overall_rating)

test <- test %>%
  mutate(away_team_rating = (away_player_1_rating + away_player_2_rating + away_player_3_rating + away_player_4_rating +
                               away_player_5_rating + away_player_6_rating + away_player_7_rating + away_player_8_rating +
                               away_player_9_rating + away_player_10_rating + away_player_11_rating)/11) %>%
  select(-away_player_1_rating, -away_player_2_rating, -away_player_3_rating, -away_player_4_rating, -away_player_5_rating,
         -away_player_6_rating, -away_player_7_rating, -away_player_8_rating, -away_player_9_rating, -away_player_10_rating,
         -away_player_11_rating)

## SAVE FILE
readr::write_csv(dataset, file = "team_ratings.csv")

                    ### TEST TO TRY AND MAKE THE ABOVE CODE WAY SMALLER ###
tmp <- matches %>%
  select(date, match_api_id, home_player_X1:away_player_11) %>%
  pivot_longer(home_player_X1:away_player_11, names_to = "player", values_to = "rating") %>%
  left_join(player_final, by = c("rating" = "player_api_id")) %>%
  drop_na()



tmp2 <- tmp %>%
  group_by(match_api_id, player) %>%
  mutate(date = as.Date(date),
         date2 = as.Date(date2),
         dateDiff = abs(date - date2)) %>%
  dplyr::slice(which.min(dateDiff))


summary(tmp3$player)

tmp %>%
  select(-rating, -date2) %>%
  group_by(match_api_id)
  pivot_wider(names_from = player, values_from = overall_rating) %>%
  mutate(home_team_rating = mean(c(home_player_1, home_player_2, home_player_3, home_player_4,
                                   home_player_5, home_player_6, home_player_7, home_player_8,
                                   home_player_9, home_player_10, home_player_11)),
         away_team_rating = mean(c(away_player_1, away_player_2, away_player_3, away_player_4,
                                   away_player_5, away_player_6, away_player_7, away_player_8,
                                   away_player_9, away_player_10, away_player_11)))
  
                    ### WIN STREAK ###
  
## HOME WIN STREAK
home_win_streak <- Dataset %>%
  select(home_team_name_S, match_score, season, match_api_id) %>%
  group_by(home_team_name_S, season) %>%
  mutate(match_score = ifelse(match_score == "Win",
                              1,
                              0),
        home_win_streak = rowid(rleid(match_score)) * match_score) %>%
  ungroup() %>%
  select(match_api_id, home_win_streak)
  
## AWAY WIN STREAK
away_win_streak <- Dataset %>%
  select(away_team_name_S, match_score, season, match_api_id) %>%
  group_by(away_team_name_S, season) %>%
  mutate(match_score = ifelse(match_score == "Win",
                              1,
                              0),
          away_win_streak = rowid(rleid(match_score)) * match_score) %>%
  ungroup() %>%
  select(match_api_id, away_win_streak)

  
## COMBINE DATA SETS
win_streak_data <- bind_cols(home_win_streak %>% arrange(match_api_id),
                             away_win_streak %>% arrange(match_api_id)) %>%
  select(-match_api_id...3) %>%
  rename(match_api_id = match_api_id...1)

## SAVE FILE
readr::write_csv(win_streak_data, file = "team_win_streaks.csv") 
