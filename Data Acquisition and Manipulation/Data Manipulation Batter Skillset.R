library(dplyr)

setwd("/path/Data")

fielding_stats <- read.csv("2024_Fielding_Stats.csv")

all_pitches <- read.csv("2024_Statcast_Pitches.csv")

savant_leaderboards <- read.csv("savant_leaderboards.csv")

sprint_speeds <- read.csv("sprint_speed.csv")

arm_strength <- read.csv("arm_strength.csv")

#first, let's manipulate the "all_pitches" document to calculate a bunch of 
#metrics we care about (GB%, CSW% etc.) against different handedness/pitch type.
#let's start by labling all pitches as the pitch types in question

View(all_pitches %>% group_by(pitch_type) %>% summarise(n = n()))

all_pitches <- all_pitches %>% 
  dplyr::mutate(is_fastball = case_when(pitch_type %in% c("FF", "SI") ~ 1,
                                        pitch_type == "FC" & release_speed >= 89 ~ 1, 
                                        TRUE ~ 0),
                is_breaking_pitch = case_when(pitch_type %in% c("CU", "KC", "SL", "ST", "SV") ~ 1,
                                              pitch_type == "FC" & release_speed < 89 ~ 1, 
                                              TRUE ~ 0),
                is_offspeed = case_when(pitch_type %in% c("FS", "CH", "FO") ~ 1,
                                        TRUE ~ 0))

#let's add in a "is swing", "is in strike zone", "is ground ball" etc.

View(all_pitches %>% group_by(description) %>% summarise(n = n()))

all_pitches <- all_pitches %>% 
  #going to be filtering out bunts eventually, but for now we can call bunts
  #swings and missed bunts whiffs
  dplyr::mutate(is_swing = case_when(description %in% c("bunt_foul_tip",
                                                        "foul", "foul_bunt", "foul_tip",
                                                        "hit_into_play", "missed_bunt",
                                                        "swinging_strike", "swinging_strike_blocked") ~ 1,
                                        TRUE ~ 0),
                #calling foul tips whiffs 
                is_whiff = case_when(description %in% c("foul_tip", "bunt_foul_tip", "missed_bunt",
                                                        "swinging_strike", "swinging_strike_blocked") ~ 1,
                                     TRUE ~ 0),
                is_called_strike = case_when(description %in% c("called_strike") ~ 1,
                                        TRUE ~ 0),
                is_ball = case_when(description %in% c("ball", "blocked_ball", "hit_by_pitch") ~ 1,
                                    TRUE ~ 0),
                #want to ensure we're catching successful bunts that are hit into play as well
                is_bunt_attempt = case_when(description %in% c("bunt_foul_tip", "foul_bunt",
                                                               "missed_bunt") ~ 1,
                                            grepl("bunt", des) & description == "hit_into_play" ~ 1, 
                                    TRUE ~ 0),
  #the actual strike zone and the called zone are different, but for these purposes, I'm going to use
  #a zone one inch wider on both sides than the "true" strike zone (19 inches wide rather than 17 
  #inches wide). 
                in_zone = case_when(plate_x >= -(9.5/12) & plate_x <= (9.5/12) & 
                                      plate_z >= sz_bot & plate_z <= sz_top ~ 1,
                                    TRUE ~ 0),
                is_gb = as.numeric(bb_type == "ground_ball"),
                is_ball_in_play = as.numeric(description == "hit_into_play"),
  #we also want to add situation (2k or N2K) to see how the player changes 
  #their approach on different counts
                is_2K = as.numeric(strikes == 2),
  #also important is whether the balls is a "hard hit" ball, will be using 
  #the threshold of 95 EV as a benchmark here
                is_hard_hit = as.numeric(launch_speed > 95)
)


View(all_pitches %>% dplyr::filter(is_called_strike == 1) %>% group_by(in_zone) %>% summarise(n = n()))

View(all_pitches %>% dplyr::filter(is_ball == 1) %>% group_by(in_zone) %>% summarise(n = n()))

#78% of called strikes are "in zone" with the one inch barrier compared to 70% without the barrier
#whereas 2.5 % of balls are "in zone" with the barrier compared to 1.5% without. 
#This appears to be more accurate than the "true" strike zone.  

#now, let's get some percents by pitch type!

results_by_pitch_type <- all_pitches %>% 
  filter(is_bunt_attempt == 0) %>% 
  group_by(player_name, batter, is_fastball, is_breaking_pitch, is_offspeed) %>% 
  summarise(n = n(),
            swing_pct = mean(is_swing),
            zone_swing_pct = mean(is_swing * in_zone) / mean(in_zone),
            o_swing_pct = mean(is_swing * (1 - in_zone)) / mean(1 - in_zone),
            whiff_pct = mean(is_whiff),
            csw_pct = mean(is_whiff + is_called_strike),
            xwoba = mean(estimated_woba_using_speedangle, na.rm = T),
            hard_hit_pct = mean(is_hard_hit, na.rm = T),
            gb_pct = mean(is_gb * is_ball_in_play) / mean(is_ball_in_play),
  ) %>% 
  ungroup() %>% 
  #would definitely like more sample here 
  dplyr::filter(n > 100)

ranges_by_pitch_type <- results_by_pitch_type %>% 
  group_by(is_fastball, is_breaking_pitch, is_offspeed) %>% 
  summarise(mean_swing_pct = mean(swing_pct),
            sd_swing_pct = sd(swing_pct),
            mean_zone_swing_pct = mean(zone_swing_pct),
            sd_zone_swing_pct = sd(zone_swing_pct),
            mean_o_swing_pct = mean(o_swing_pct),
            sd_o_swing_pct = sd(o_swing_pct),
            mean_whiff_pct = mean(whiff_pct),
            sd_whiff_pct = sd(whiff_pct),
            mean_xwoba = mean(xwoba),
            sd_xwoba = sd(xwoba),
            mean_gb_pct = mean(gb_pct),
            sd_gb_pct = sd(gb_pct),
            mean_hard_hit_pct = mean(hard_hit_pct),
            sd_hard_hit_pct = sd(hard_hit_pct))



results_by_pitch_type <- results_by_pitch_type %>% 
  #20-80 everything! I'm actually not including xwoba here because we don't have enough sample
  #and don't want to pass on bad information, but swing decision metrics stabilize much more
  #quickly
  inner_join(ranges_by_pitch_type, by = c("is_fastball", "is_breaking_pitch", "is_offspeed")) %>% 
  mutate(swing_pct_20_80 = ((swing_pct - mean_swing_pct) / sd_swing_pct) * (-10) + 50,
         zone_swing_pct_20_80 = ((zone_swing_pct - mean_zone_swing_pct) / sd_zone_swing_pct) * 10 + 50,
         chase_pct_20_80 = ((o_swing_pct - mean_o_swing_pct) / sd_o_swing_pct) * (-10) + 50,
         whiff_pct_20_80 = ((whiff_pct - mean_whiff_pct) / sd_whiff_pct) * (-10) + 50,
         gb_pct_20_80 = ((gb_pct - mean_gb_pct) / sd_gb_pct) * (-10) + 50,
         hard_hit_pct_20_80 = ((hard_hit_pct - mean_hard_hit_pct) / sd_hard_hit_pct) * (10) + 50,
  )

all_pitch_types_present <- results_by_pitch_type %>% 
  group_by(batter) %>% 
  summarise(n = n()) %>% 
  filter(n == 3) %>% 
  pull(batter)

results_by_pitch_type_wide <- results_by_pitch_type %>% 
  mutate(pitch_type = case_when(is_fastball == 1 ~ "Fastball", is_breaking_pitch == 1 ~ "Breaking",
                                TRUE ~ "Offspeed")) %>% 
  filter(batter %in% all_pitch_types_present) %>% 
  select(-c("is_fastball", "is_breaking_pitch", "is_offspeed", "mean_swing_pct", "sd_swing_pct", "mean_zone_swing_pct", "sd_zone_swing_pct", "mean_o_swing_pct", 
            "sd_o_swing_pct", "mean_whiff_pct", "sd_whiff_pct", "mean_xwoba", "sd_xwoba" , "mean_gb_pct",
            "sd_gb_pct", "mean_hard_hit_pct", "sd_hard_hit_pct")) %>% 
  tidyr::pivot_wider(names_from = pitch_type, values_from = c("n", "swing_pct", "zone_swing_pct", "o_swing_pct", "whiff_pct",
                                                            "csw_pct", "xwoba", "hard_hit_pct", "gb_pct",
                                                            "swing_pct_20_80", "zone_swing_pct_20_80", "chase_pct_20_80",
                                                            "whiff_pct_20_80", "gb_pct_20_80", "hard_hit_pct_20_80"))


results_by_handedness <- all_pitches %>% 
  filter(is_bunt_attempt == 0) %>% 
  group_by(player_name, batter, stand, p_throws) %>% 
  dplyr::summarise(n = n(),
            swing_pct = mean(is_swing),
            zone_swing_pct = mean(is_swing * in_zone) / mean(in_zone),
            o_swing_pct = mean(is_swing * (1 - in_zone)) / mean(1 - in_zone),
            whiff_pct = mean(is_whiff),
            csw_pct = mean(is_whiff + is_called_strike),
            xwoba = mean(estimated_woba_using_speedangle, na.rm = T),
            hard_hit_pct = mean(is_hard_hit, na.rm = T),
            gb_pct = mean(is_gb * is_ball_in_play) / mean(is_ball_in_play),
  ) %>% 
  ungroup() %>% 
  #would like more sample here as well
  dplyr::filter(n > 150)

ranges_by_handedness <- results_by_handedness %>% 
  group_by(stand, p_throws) %>% 
  summarise(mean_swing_pct = mean(swing_pct),
            sd_swing_pct = sd(swing_pct),
            mean_zone_swing_pct = mean(zone_swing_pct),
            sd_zone_swing_pct = sd(zone_swing_pct),
            mean_o_swing_pct = mean(o_swing_pct),
            sd_o_swing_pct = sd(o_swing_pct),
            mean_whiff_pct = mean(whiff_pct),
            sd_whiff_pct = sd(whiff_pct),
            mean_xwoba = mean(xwoba),
            sd_xwoba = sd(xwoba),
            mean_gb_pct = mean(gb_pct),
            sd_gb_pct = sd(gb_pct),
            mean_hard_hit_pct = mean(hard_hit_pct),
            sd_hard_hit_pct = sd(hard_hit_pct))



results_by_handedness <- results_by_handedness %>% 
  #20-80 all swing decision metrics 
  inner_join(ranges_by_handedness, by = c("stand", "p_throws")) %>% 
  mutate(swing_pct_20_80 = ((swing_pct - mean_swing_pct) / sd_swing_pct) * (-10) + 50,
         zone_swing_pct_20_80 = ((zone_swing_pct - mean_zone_swing_pct) / sd_zone_swing_pct) * 10 + 50,
         chase_pct_20_80 = ((o_swing_pct - mean_o_swing_pct) / sd_o_swing_pct) * (-10) + 50,
         whiff_pct_20_80 = ((whiff_pct - mean_whiff_pct) / sd_whiff_pct) * (-10) + 50,
         gb_pct_20_80 = ((gb_pct - mean_gb_pct) / sd_gb_pct) * (-10) + 50,
         hard_hit_pct_20_80 = ((hard_hit_pct - mean_hard_hit_pct) / sd_hard_hit_pct) * (10) + 50,
  )


both_hands_present <- results_by_handedness %>% 
  group_by(batter) %>% 
  summarise(n = n()) %>% 
  filter(n == 2) %>% 
  pull(batter)

results_by_handedness_wide <- results_by_handedness %>% 
  filter(batter %in% both_hands_present) %>% 
  select(-c("stand", "mean_swing_pct", "sd_swing_pct", "mean_zone_swing_pct", "sd_zone_swing_pct", "mean_o_swing_pct", 
            "sd_o_swing_pct", "mean_whiff_pct", "sd_whiff_pct", "mean_xwoba", "sd_xwoba" , "mean_gb_pct",
            "sd_gb_pct", "mean_hard_hit_pct", "sd_hard_hit_pct")) %>% 
  tidyr::pivot_wider(names_from = p_throws, values_from = c("n", "swing_pct", "zone_swing_pct", "o_swing_pct", "whiff_pct",
                                                         "csw_pct", "xwoba", "hard_hit_pct", "gb_pct",
                                                         "swing_pct_20_80", "zone_swing_pct_20_80", "chase_pct_20_80",
                                                         "whiff_pct_20_80", "gb_pct_20_80", "hard_hit_pct_20_80"))

results_by_count <- all_pitches %>% 
  filter(is_bunt_attempt == 0) %>% 
  group_by(player_name, batter, is_2K) %>% 
  dplyr::summarise(n = n(),
                   swing_pct = mean(is_swing),
                   zone_swing_pct = mean(is_swing * in_zone) / mean(in_zone),
                   o_swing_pct = mean(is_swing * (1 - in_zone)) / mean(1 - in_zone),
                   whiff_pct = mean(is_whiff),
                   csw_pct = mean(is_whiff + is_called_strike),
                   xwoba = mean(estimated_woba_using_speedangle, na.rm = T),
                   hard_hit_pct = mean(is_hard_hit, na.rm = T),
                   gb_pct = mean(is_gb * is_ball_in_play) / mean(is_ball_in_play),
                   #since some hitters change their swing from 2K to N2K, we want to add in
                   #swing speed/length 
                   bat_speed = mean(bat_speed, na.rm = T),
                   swing_length = mean(swing_length, na.rm = T),
                   
  ) %>% 
  ungroup() %>% 
  dplyr::filter(n > 300)

ranges_by_count <- results_by_count %>% 
  group_by(is_2K) %>% 
  summarise(mean_swing_pct = mean(swing_pct),
            sd_swing_pct = sd(swing_pct),
            mean_zone_swing_pct = mean(zone_swing_pct),
            sd_zone_swing_pct = sd(zone_swing_pct),
            mean_o_swing_pct = mean(o_swing_pct),
            sd_o_swing_pct = sd(o_swing_pct),
            mean_whiff_pct = mean(whiff_pct),
            sd_whiff_pct = sd(whiff_pct),
            mean_xwoba = mean(xwoba),
            sd_xwoba = sd(xwoba),
            mean_gb_pct = mean(gb_pct),
            sd_gb_pct = sd(gb_pct),
            mean_hard_hit_pct = mean(hard_hit_pct),
            sd_hard_hit_pct = sd(hard_hit_pct),
            mean_bat_speed = mean(bat_speed),
            sd_bat_speed = sd(bat_speed),
            mean_swing_length = mean(swing_length),
            sd_swing_length = sd(swing_length),
  )



results_by_count <- results_by_count %>% 
  #20-80 with swing metrics added in now
  inner_join(ranges_by_count, by = c("is_2K")) %>% 
  mutate(swing_pct_20_80 = ((swing_pct - mean_swing_pct) / sd_swing_pct) * (-10) + 50,
         zone_swing_pct_20_80 = ((zone_swing_pct - mean_zone_swing_pct) / sd_zone_swing_pct) * 10 + 50,
         chase_pct_20_80 = ((o_swing_pct - mean_o_swing_pct) / sd_o_swing_pct) * (-10) + 50,
         whiff_pct_20_80 = ((whiff_pct - mean_whiff_pct) / sd_whiff_pct) * (-10) + 50,
         gb_pct_20_80 = ((gb_pct - mean_gb_pct) / sd_gb_pct) * (-10) + 50,
         hard_hit_pct_20_80 = ((hard_hit_pct - mean_hard_hit_pct) / sd_hard_hit_pct) * (10) + 50,
         bat_speed_20_80 = ((bat_speed - mean_bat_speed) / sd_bat_speed) * (10) + 50,
         swing_length_20_80 = ((swing_length - mean_swing_length) / sd_swing_length) * (-10) + 50,
  )

both_counts_present <- results_by_count %>% 
  group_by(batter) %>% 
  summarise(n = n()) %>% 
  filter(n == 2) %>% 
  pull(batter)

results_by_count_wide <- results_by_count %>% 
  filter(batter %in% both_counts_present) %>% 
  mutate(count = case_when(is_2K == 1 ~ "2K", TRUE ~ "N2K")) %>% 
  select(-c("is_2K", "mean_swing_pct", "sd_swing_pct", "mean_zone_swing_pct", "sd_zone_swing_pct", "mean_o_swing_pct", 
            "sd_o_swing_pct", "mean_whiff_pct", "sd_whiff_pct", "mean_xwoba", "sd_xwoba" , "mean_gb_pct",
            "sd_gb_pct", "mean_hard_hit_pct", "sd_hard_hit_pct", "mean_bat_speed", "sd_bat_speed", "mean_swing_length",
            "sd_swing_length")) %>% 
  tidyr::pivot_wider(names_from = count, values_from = c("n", "swing_pct", "zone_swing_pct", "o_swing_pct", "whiff_pct",
                                                         "csw_pct", "xwoba", "hard_hit_pct", "gb_pct", "bat_speed", "swing_length",
                                                         "swing_pct_20_80", "zone_swing_pct_20_80", "chase_pct_20_80",
                                                         "whiff_pct_20_80", "gb_pct_20_80", "hard_hit_pct_20_80", "bat_speed_20_80",
                                                         "swing_length_20_80"))

results_by_count_wide <- results_by_count_wide %>% 
  dplyr::mutate(bat_speed_lost_to_2K = bat_speed_2K - bat_speed_N2K)

results_by_count_wide <- results_by_count_wide %>% 
  dplyr::mutate(bat_speed_lost_to_2K_20_80 = ((bat_speed_lost_to_2K - mean(results_by_count_wide$bat_speed_lost_to_2K)) / sd(results_by_count_wide$bat_speed_lost_to_2K)) * 10 +50)
  
results_overall <- all_pitches %>% 
  filter(is_bunt_attempt == 0) %>% 
  group_by(player_name, batter) %>% 
  dplyr::summarise(n = n(),
                   swing_pct = mean(is_swing),
                   zone_swing_pct = mean(is_swing * in_zone) / mean(in_zone),
                   o_swing_pct = mean(is_swing * (1 - in_zone)) / mean(1 - in_zone),
                   whiff_pct = mean(is_whiff),
                   csw_pct = mean(is_whiff + is_called_strike),
                   xwoba = mean(estimated_woba_using_speedangle, na.rm = T),
                   hard_hit_pct = mean(is_hard_hit, na.rm = T),
                   gb_pct = mean(is_gb * is_ball_in_play) / mean(is_ball_in_play),
                   bat_speed = mean(bat_speed, na.rm = T),
                   swing_length = mean(swing_length, na.rm = T)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(n > 500)

ranges_overall <- results_overall %>% 
  group_by() %>% 
  summarise(mean_swing_pct = mean(swing_pct),
            sd_swing_pct = sd(swing_pct),
            mean_zone_swing_pct = mean(zone_swing_pct),
            sd_zone_swing_pct = sd(zone_swing_pct),
            mean_o_swing_pct = mean(o_swing_pct),
            sd_o_swing_pct = sd(o_swing_pct),
            mean_whiff_pct = mean(whiff_pct),
            sd_whiff_pct = sd(whiff_pct),
            mean_xwoba = mean(xwoba),
            sd_xwoba = sd(xwoba),
            mean_gb_pct = mean(gb_pct),
            sd_gb_pct = sd(gb_pct),
            mean_hard_hit_pct = mean(hard_hit_pct),
            sd_hard_hit_pct = sd(hard_hit_pct),
            mean_bat_speed = mean(bat_speed),
            sd_bat_speed = sd(bat_speed),
            mean_swing_length = mean(swing_length),
            sd_swing_length = sd(swing_length),
  )



results_overall <- results_overall %>% 
  #20-80 with xwoba added in
  bind_cols(ranges_overall) %>% 
  mutate(swing_pct_20_80 = ((swing_pct - mean_swing_pct) / sd_swing_pct) * (-10) + 50,
         zone_swing_pct_20_80 = ((zone_swing_pct - mean_zone_swing_pct) / sd_zone_swing_pct) * 10 + 50,
         chase_pct_20_80 = ((o_swing_pct - mean_o_swing_pct) / sd_o_swing_pct) * (-10) + 50,
         whiff_pct_20_80 = ((whiff_pct - mean_whiff_pct) / sd_whiff_pct) * (-10) + 50,
         gb_pct_20_80 = ((gb_pct - mean_gb_pct) / sd_gb_pct) * (-10) + 50,
         hard_hit_pct_20_80 = ((hard_hit_pct - mean_hard_hit_pct) / sd_hard_hit_pct) * (10) + 50,
         xwoba_20_80 = ((xwoba - mean_xwoba) / sd_xwoba) * (10) + 50,
         bat_speed_20_80 = ((bat_speed - mean_bat_speed) / sd_bat_speed) * (10) + 50,
         swing_length_20_80 = ((swing_length - mean_swing_length) / sd_swing_length) * (-10) + 50,
  )

bunt_tendencies <- all_pitches %>% 
  group_by(player_name, batter) %>% 
  dplyr::summarise(bunt_attempt_probability = mean(is_bunt_attempt), n = n()) %>% 
  dplyr::filter(n > 250)

sprint_speeds <- sprint_speeds %>% 
  dplyr::filter(competitive_runs > 20) %>% 
  dplyr::select(player_id, sprint_speed)

sprint_speeds <- sprint_speeds %>% 
  dplyr::mutate(sprint_speed_20_80 = ((sprint_speed - mean(sprint_speeds$sprint_speed)) / sd(sprint_speeds$sprint_speed)) * (10) + 50)

arm_strength <- arm_strength %>% 
  dplyr::mutate(where_on_field = case_when(primary_position %in% c(4, 5, 6) | 
                                             (primary_position %in% c(10, 11) & (total_throws_inf > total_throws_of) & 
                                             (total_throws_inf > total_throws_1b)) ~ "Infield",
                                           primary_position %in% c(7, 8, 9) | 
                                             (primary_position %in% c(10, 11) & (total_throws_of > total_throws_1b)) ~ "Outfield")
                )

arm_strength_if_of_range <- arm_strength %>% 
  group_by(where_on_field) %>% 
  summarise(mean_arm_strength = mean(arm_overall),
            sd_arm_strength = sd(arm_overall)) %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(where_on_field))

arm_strength <- arm_strength %>% 
  inner_join(arm_strength_if_of_range) %>%
  dplyr::mutate(arm_strength_20_80 = ((arm_overall - mean_arm_strength) / sd_arm_strength) * 10 + 50)

fielding_stats <- fielding_stats %>% 
  select(PlayerName, xMLBAMID, Pos, Plays, Defense) %>% 
  arrange(desc(Plays)) %>% 
  filter(Plays > 50) %>% 
  group_by(xMLBAMID) %>% 
  mutate(is_primary = case_when(row_number() == 1 ~ "Primary", TRUE ~ "Secondary"),
         Defense_per_play = Defense/Plays) %>% 
  filter(is_primary != "Secondary") 

fielding_stats_range <- fielding_stats %>% 
  group_by(Pos) %>% 
  dplyr::summarise(mean_defense_per_play = mean(Defense_per_play),
                   sd_defense_per_play = sd(Defense_per_play))

fielding_stats <- fielding_stats %>% 
  inner_join(fielding_stats_range, by = "Pos") %>% 
  mutate(def_per_play_20_80 = ((Defense_per_play - mean_defense_per_play) / sd_defense_per_play) * (10) + 50)
  
batter_hand <- all_pitches %>% 
  group_by(batter, stand, p_throws) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  group_by(batter, p_throws) %>% 
  slice(1) %>% 
  ungroup() %>% 
  tidyr::pivot_wider(names_from = p_throws, values_from = c("stand", "n")) %>% 
  filter(!is.na(stand_L), !is.na(stand_R)) %>% 
  mutate(batter_handedness = case_when(stand_L == "L" & stand_R == "L" ~ "L", 
                                 stand_L == "R" & stand_R == "R" ~ "R", 
                                 TRUE ~ "S")) %>% 
  select(batter, batter_handedness)


all_information_for_shiny_app <- results_by_pitch_type_wide %>% 
  inner_join(results_by_count_wide, by = c("player_name", "batter")) %>% 
  inner_join(results_by_handedness_wide, by = c("player_name", "batter")) %>% 
  inner_join(results_overall, by = c("player_name", "batter")) %>% 
  inner_join(bunt_tendencies %>% rename(bunt_opportunities = n), by = c("player_name", "batter")) %>% 
  inner_join(arm_strength %>% rename(batter = player_id) %>% select(batter, arm_overall, where_on_field, arm_strength_20_80),
             by = "batter") %>% 
  inner_join(batter_hand, by = "batter") %>% 
  inner_join(sprint_speeds %>% rename(batter = player_id), by = "batter") %>% 
  inner_join(fielding_stats %>% rename(batter = xMLBAMID), by = "batter")


setwd("/Users/billyohandley/Downloads/Cardinals Project/Data")

#saving all the manipulated data off! 

write.csv(all_information_for_shiny_app, "All_2024_Info.csv")




