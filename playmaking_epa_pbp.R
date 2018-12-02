library(nflscrapR)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)

#SCRAPE SEASON DATA
season_pbp_2018 <- scrape_season_play_by_play(2018, "reg")

#CREATE DATAFRAME FOR LEAGUE
defense_pbp_2018 <- season_pbp_2018 %>%
  filter(play_type != "punt" & play_type != "kickoff" & !is.na(play_type)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa, lateral_sack_player_id, lateral_sack_player_name, interception_player_id, interception_player_name, lateral_interception_player_id, lateral_interception_player_name, blocked_player_id, blocked_player_name, qb_hit_1_player_id, qb_hit_1_player_name, qb_hit_2_player_id, qb_hit_2_player_name, forced_fumble_player_1_player_id, forced_fumble_player_1_player_name, forced_fumble_player_2_player_id, forced_fumble_player_2_player_name, solo_tackle_1_player_id, solo_tackle_1_player_name, solo_tackle_2_player_id, solo_tackle_2_player_name, assist_tackle_1_player_id, assist_tackle_1_player_name, assist_tackle_2_player_id, assist_tackle_2_player_name, assist_tackle_3_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_3_player_name, assist_tackle_4_player_id, assist_tackle_4_player_name, pass_defense_1_player_id, pass_defense_1_player_name, pass_defense_2_player_id, pass_defense_2_player_name, fumble_recovery_1_player_id, fumble_recovery_1_player_name, fumble_recovery_2_player_id, fumble_recovery_2_player_name)

#CREATE SUBDATAFRAMES FOR LEAGUE
interceptions_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(interception_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = interception_player_id, player_name = interception_player_name)

blocked_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(blocked_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = blocked_player_id, player_name = blocked_player_name)

qb_hit_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(qb_hit_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = qb_hit_1_player_id, player_name = qb_hit_1_player_name)

qb_hit_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(qb_hit_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = qb_hit_2_player_id, player_name = qb_hit_2_player_name)

forced_fumble_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = forced_fumble_player_1_player_id, player_name = forced_fumble_player_1_player_name)

forced_fumble_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(forced_fumble_player_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = forced_fumble_player_2_player_id, player_name = forced_fumble_player_2_player_name)

solo_tackle_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(solo_tackle_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = solo_tackle_1_player_id, player_name = solo_tackle_1_player_name)

solo_tackle_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(solo_tackle_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = solo_tackle_2_player_id, player_name = solo_tackle_2_player_name)

assist_tackle_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(assist_tackle_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = assist_tackle_1_player_id, player_name = assist_tackle_1_player_name)

assist_tackle_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(assist_tackle_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = assist_tackle_2_player_id, player_name = assist_tackle_2_player_name)

assist_tackle_3_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(assist_tackle_3_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = assist_tackle_3_player_id, player_name = assist_tackle_3_player_name)

assist_tackle_4_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(assist_tackle_4_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = assist_tackle_4_player_id, player_name = assist_tackle_4_player_name)

pass_defense_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(pass_defense_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = pass_defense_1_player_id, player_name = pass_defense_1_player_name)

pass_defense_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(pass_defense_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = pass_defense_2_player_id, player_name = pass_defense_2_player_name)

fumble_recovery_1_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(fumble_recovery_1_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = fumble_recovery_1_player_id, player_name = fumble_recovery_1_player_name)

fumble_recovery_2_pbp_2018 <- defense_pbp_2018 %>%
  filter(!is.na(epa), !is.na(fumble_recovery_2_player_id)) %>%
  select(game_id, play_id, posteam, defteam, qtr, time, down, ydstogo, side_of_field, yrdln, yards_gained, desc, play_type, ep, epa,
         player_id = fumble_recovery_2_player_id, player_name = fumble_recovery_2_player_name)

#MERGE SUBDATAFRAMES including duplicates
defense_2018_df_merged <- rbind(interceptions_pbp_2018, 
                           blocked_pbp_2018, 
                           qb_hit_1_pbp_2018, qb_hit_2_pbp_2018, 
                           forced_fumble_1_pbp_2018, forced_fumble_2_pbp_2018,
                           solo_tackle_1_pbp_2018, solo_tackle_2_pbp_2018,
                           assist_tackle_1_pbp_2018, assist_tackle_2_pbp_2018, assist_tackle_3_pbp_2018, assist_tackle_4_pbp_2018,
                           pass_defense_1_pbp_2018, pass_defense_2_pbp_2018,
                           fumble_recovery_1_pbp_2018, fumble_recovery_2_pbp_2018
                           )

#remove duplicate plays for individual players BUT WITHOUT REMOVING SAME
#use code below, but qualify that you're removing duplicates of same play_id AND player_name 
defense_2018_df_merged <- defense_2018_df_merged %>%
  distinct(play_id, player_id, .keep_all = TRUE)

#CREATE FINAL CALCULATION
playmaking_epa_2018 <- defense_2018_df_merged %>%
  filter(epa < 0) %>%
  group_by(player_id) %>%
  summarise(
    player_name = paste(unique(player_name), collapse = ', '),
    team = paste(unique(defteam), collapse = ', '),
    plays_made = length(epa), 
    playmaking_epa = abs(sum(epa)),
    pass_playmaking_epa = abs(sum(epa[play_type == "pass"])),
    rush_playmaking_epa = abs(sum(epa[play_type == "run"]))
            )

#Create Cowboys dataframe
cowboys_playmaking_2018 <- defense_2018_df_merged %>%
  filter(epa < 0 & defteam == "DAL") %>%
  group_by(player_id) %>%
  summarise(
    player_name = paste(unique(player_name), collapse = ', '),
    team = paste(unique(defteam), collapse = ', '),
    plays_made = length(epa), 
    playmaking_epa = abs(sum(epa)),
    pass_playmaking_epa = abs(sum(epa[play_type == "pass"])),
    rush_playmaking_epa = abs(sum(epa[play_type == "run"]))
  )

#Create Cowboys chart w/snap counts (must do this manually from FO data)
defense_chart_2018 <- ggplot(aes(cowboys_playmaking_2018$snap_count, cowboys_playmaking_2018$playmaking_epa)) + 
  geom_point(color = 'royalblue2') + 
  geom_text_repel(data = cowboys_playmaking_2018, aes(cowboys_playmaking_2018$snap_count, cowboys_playmaking_2018$playmaking_epa, label = cowboys_playmaking_2018$player_name), size = 3, color = 'royalblue4') +
  geom_abline(slope = 0.04660413, intercept = 0, linetype = "dashed", col = "gray") +
  labs(
    x = "Snap count",
    y = "Playmaking EPA",
    title = "Playmaking leaders on defense: Jaylon Smith is all the way back",
    subtitle = "Dashed line represents average Cowboys playmaking rate, Weeks 1-10",
    caption = "Data from nflscrapR, created by Ron Yurko, Sam Ventura and Maksim Horowitz"
  )

defense_chart_2018

