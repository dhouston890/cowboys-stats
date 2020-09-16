library(tidyverse)

#Load season data from nflfastR repo
games_2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))


#Create dataframe of defensive plays only
defense_pbp_2020 <- games_2020 %>%
  filter(play_type != "punt" & play_type != "kickoff" & !is.na(play_type)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa, lateral_sack_player_id, lateral_sack_player_name, interception_player_id, interception_player_name, lateral_interception_player_id, lateral_interception_player_name, blocked_player_id, blocked_player_name, qb_hit_1_player_id, qb_hit_1_player_name, qb_hit_2_player_id, qb_hit_2_player_name, forced_fumble_player_1_player_id, forced_fumble_player_1_player_name, forced_fumble_player_2_player_id, forced_fumble_player_2_player_name, solo_tackle_1_player_id, solo_tackle_1_player_name, solo_tackle_2_player_id, solo_tackle_2_player_name, assist_tackle_1_player_id, assist_tackle_1_player_name, assist_tackle_2_player_id, assist_tackle_2_player_name, assist_tackle_3_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_4_player_name, pass_defense_1_player_id, pass_defense_1_player_name, pass_defense_2_player_id, pass_defense_2_player_name, fumble_recovery_1_player_id, fumble_recovery_1_player_name, fumble_recovery_1_team, fumble_recovery_2_player_id, fumble_recovery_2_player_name, interception, solo_tackle_1_team, solo_tackle_2_team, assist_tackle_1_team, assist_tackle_2_team, assist_tackle_3_team, assist_tackle_4_team, fumble_recovery_1_team, fumble_recovery_2_team, forced_fumble_player_1_team, forced_fumble_player_2_team, fumble_lost)

#Create subdataframes for different types of plays made
interceptions_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(interception_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = interception_player_id, player_name = interception_player_name)

blocked_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(blocked_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = blocked_player_id, player_name = blocked_player_name)

qb_hit_1_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(qb_hit_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = qb_hit_1_player_id, player_name = qb_hit_1_player_name)

qb_hit_2_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(qb_hit_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = qb_hit_2_player_id, player_name = qb_hit_2_player_name)

forced_fumble_1_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_1_player_id), forced_fumble_player_1_team != posteam) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = forced_fumble_player_1_player_id, player_name = forced_fumble_player_1_player_name)

forced_fumble_2_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_2_player_id), forced_fumble_player_2_team != posteam) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = forced_fumble_player_2_player_id, player_name = forced_fumble_player_2_player_name)

solo_tackle_1_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(solo_tackle_1_player_id), interception != 1, fumble_lost != 1) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = solo_tackle_1_player_id, player_name = solo_tackle_1_player_name)

solo_tackle_2_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(solo_tackle_2_player_id), interception != 1, fumble_lost != 1) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = solo_tackle_2_player_id, player_name = solo_tackle_2_player_name)

assist_tackle_1_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(assist_tackle_1_player_id), interception != 1, fumble_lost != 1) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_1_player_id, player_name = assist_tackle_1_player_name)

assist_tackle_2_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(assist_tackle_2_player_id), interception != 1, fumble_lost != 1) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_2_player_id, player_name = assist_tackle_2_player_name)

assist_tackle_3_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(assist_tackle_3_player_id), interception != 1, fumble_lost != 1) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_3_player_id, player_name = assist_tackle_3_player_name)

assist_tackle_4_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(assist_tackle_4_player_id), interception != 1, fumble_lost != 1) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = assist_tackle_4_player_id, player_name = assist_tackle_4_player_name)

pass_defense_1_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(pass_defense_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = pass_defense_1_player_id, player_name = pass_defense_1_player_name)

pass_defense_2_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(pass_defense_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = pass_defense_2_player_id, player_name = pass_defense_2_player_name)

fumble_recovery_1_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(fumble_recovery_1_player_id), fumble_recovery_1_team != posteam) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = fumble_recovery_1_player_id, player_name = fumble_recovery_1_player_name)

fumble_recovery_2_pbp_2020 <- defense_pbp_2020 %>%
  filter(!is.na(epa), !is.na(fumble_recovery_2_player_id), fumble_recovery_2_team != posteam) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, wp, wpa,
         player_id = fumble_recovery_2_player_id, player_name = fumble_recovery_2_player_name)

#Merge subdataframes, including duplicates
defense_2020_df_merged <- rbind(interceptions_pbp_2020, 
                           blocked_pbp_2020, 
                           qb_hit_1_pbp_2020, qb_hit_2_pbp_2020, 
                           forced_fumble_1_pbp_2020, forced_fumble_2_pbp_2020,
                           solo_tackle_1_pbp_2020, solo_tackle_2_pbp_2020,
                           assist_tackle_1_pbp_2020, assist_tackle_2_pbp_2020, assist_tackle_3_pbp_2020, assist_tackle_4_pbp_2020,
                           pass_defense_1_pbp_2020, pass_defense_2_pbp_2020,
                           fumble_recovery_1_pbp_2020, fumble_recovery_2_pbp_2020
                           )

#Remove duplicate plays for individual players—the same play can count for different players
defense_2020_df_merged <- defense_2020_df_merged %>%
  distinct(play_id, player_id, .keep_all = TRUE)

#Summarize plays made, playmaking EPA and run vs. pass breakdowns
playmaking_epa_2020 <- defense_2020_df_merged %>%
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

#View tables
View(playmaking_epa_2020)
