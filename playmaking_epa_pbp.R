library(tidyverse)

library(tidyverse)

#Load season data from nflfastR repo
#data for each season
games_1999 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_1999.rds'))
games_2000 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2000.rds'))
games_2001 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2001.rds'))
games_2002 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2002.rds'))
games_2003 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2003.rds'))
games_2004 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2004.rds'))
games_2005 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2005.rds'))
games_2006 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2006.rds'))
games_2007 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2007.rds'))
games_2008 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2008.rds'))
games_2009 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2009.rds'))
games_2010 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2010.rds'))
games_2011 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2011.rds'))
games_2012 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2012.rds'))
games_2013 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2013.rds'))
games_2014 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2014.rds'))
games_2015 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2015.rds'))
games_2016 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2016.rds'))
games_2017 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2017.rds'))
games_2018 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.rds'))
games_2019 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
games_2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

#nerge all seasons into one object
games_1999_2020 <- bind_rows(games_1999, games_2000, games_2001, games_2002, games_2003, games_2004, games_2005, games_2006, games_2007, games_2008, games_2009, games_2010, 
                             games_2011, games_2012, games_2013, games_2014, games_2015, games_2016, games_2017, games_2018, games_2019, games_2020)

#Create dataframe of defensive plays only
defense_pbp <- games_1999_2020 %>%
  filter(play_type != "punt" & play_type != "kickoff" & !is.na(play_type), penalty != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa, lateral_sack_player_id, lateral_sack_player_name, interception_player_id, interception_player_name, lateral_interception_player_id, lateral_interception_player_name, blocked_player_id, blocked_player_name, qb_hit_1_player_id, qb_hit_1_player_name, qb_hit_2_player_id, qb_hit_2_player_name, forced_fumble_player_1_player_id, forced_fumble_player_1_player_name, forced_fumble_player_2_player_id, forced_fumble_player_2_player_name, solo_tackle_1_player_id, solo_tackle_1_player_name, solo_tackle_2_player_id, solo_tackle_2_player_name, assist_tackle_1_player_id, assist_tackle_1_player_name, assist_tackle_2_player_id, assist_tackle_2_player_name, assist_tackle_3_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_4_player_name, pass_defense_1_player_id, pass_defense_1_player_name, pass_defense_2_player_id, pass_defense_2_player_name, fumble_recovery_1_player_id, fumble_recovery_1_player_name, fumble_recovery_1_team, fumble_recovery_2_player_id, fumble_recovery_2_player_name, interception, solo_tackle_1_team, solo_tackle_2_team, assist_tackle_1_team, assist_tackle_2_team, assist_tackle_3_team, assist_tackle_4_team, fumble_recovery_1_team, fumble_recovery_2_team, forced_fumble_player_1_team, forced_fumble_player_2_team, fumble_lost)

#Create subdataframes for different types of plays made
interceptions_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(interception_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = interception_player_id, player_name = interception_player_name)

blocked_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(blocked_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = blocked_player_id, player_name = blocked_player_name)

qb_hit_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(qb_hit_1_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = qb_hit_1_player_id, player_name = qb_hit_1_player_name)

qb_hit_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(qb_hit_2_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = qb_hit_2_player_id, player_name = qb_hit_2_player_name)

forced_fumble_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_1_player_id), forced_fumble_player_1_team != posteam) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = forced_fumble_player_1_player_id, player_name = forced_fumble_player_1_player_name)

forced_fumble_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_2_player_id), forced_fumble_player_2_team != posteam) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = forced_fumble_player_2_player_id, player_name = forced_fumble_player_2_player_name)

solo_tackle_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(solo_tackle_1_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = solo_tackle_1_player_id, player_name = solo_tackle_1_player_name)

solo_tackle_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(solo_tackle_2_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = solo_tackle_2_player_id, player_name = solo_tackle_2_player_name)

assist_tackle_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(assist_tackle_1_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_1_player_id, player_name = assist_tackle_1_player_name)

assist_tackle_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(assist_tackle_2_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_2_player_id, player_name = assist_tackle_2_player_name)

assist_tackle_3_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(assist_tackle_3_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_3_player_id, player_name = assist_tackle_3_player_name)

assist_tackle_4_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(assist_tackle_4_player_id), interception != 1, fumble_lost != 1) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_4_player_id, player_name = assist_tackle_4_player_name)

pass_defense_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(pass_defense_1_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = pass_defense_1_player_id, player_name = pass_defense_1_player_name)

pass_defense_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(pass_defense_2_player_id)) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = pass_defense_2_player_id, player_name = pass_defense_2_player_name)

fumble_recovery_1_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(fumble_recovery_1_player_id), fumble_recovery_1_team != posteam) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = fumble_recovery_1_player_id, player_name = fumble_recovery_1_player_name)

fumble_recovery_2_pbp <- defense_pbp %>%
  filter(!is.na(epa), !is.na(fumble_recovery_2_player_id), fumble_recovery_2_team != posteam) %>%
  select(season, week, game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = fumble_recovery_2_player_id, player_name = fumble_recovery_2_player_name)

#MERGE SUBDATAFRAMES including duplicates
defense_df_merged <- rbind(interceptions_pbp, 
                           blocked_pbp, 
                           qb_hit_1_pbp, qb_hit_2_pbp, 
                           forced_fumble_1_pbp, forced_fumble_2_pbp,
                           solo_tackle_1_pbp, solo_tackle_2_pbp,
                           assist_tackle_1_pbp, assist_tackle_2_pbp, assist_tackle_3_pbp, assist_tackle_4_pbp,
                           pass_defense_1_pbp, pass_defense_2_pbp,
                           fumble_recovery_1_pbp, fumble_recovery_2_pbp
                           )

#remove duplicate plays for individual players—same play can count for multiple players
defense_df_merged <- defense_df_merged %>%
  distinct(play_id, player_id, .keep_all = TRUE)

#Summarize plays made, playmaking EPA and run vs. pass breakdowns
playmaking_epa_1999_2020 <- defense_df_merged %>%
  filter(epa < 0) %>%
  group_by(player_id) %>%
  summarise(
    player_name = paste(unique(player_name), collapse = ', '),
    team = paste(unique(defteam), collapse = ', '),
    plays_made = length(epa), 
    playmaking_epa = abs(sum(epa)),
    pass_playmaking_epa = abs(sum(epa[play_type == "pass"])),
    rush_playmaking_epa = abs(sum(epa[play_type == "run"])), 
    playmaking_wpa = abs(sum(wpa[!is.na(wpa)]))
            )
