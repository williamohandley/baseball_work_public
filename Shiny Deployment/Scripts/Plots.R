library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(shiny)
library(bslib)
library(dplyr)
library(shinyjs)


#I wanted to put my own spin on the statcast format, but I do think its recognizably is really good here
#wanted to go 20-80 rather than percentiles because 20-80 is fantastic! Great for both coaches and statisticians 

statcast_remove <- theme(panel.grid = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text = element_blank(),
                        panel.background = element_blank(),
                        legend.position = "none",
                        strip.background = element_blank()
)

statcast_text <- theme(strip.text = element_text(size = 12, face = 'bold')) + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', margin=margin(10,0,10,0))) + 
  theme(strip.text.x = element_text(hjust = .125))

#only really going to comment the first one because they're all pretty similar. would ideally have
#one function where you could just input which of the plots you'd generate

percentiles_plot_overall <- function(batter_data, name_of_player) {
  
  for_graph <- batter_data %>% 
    select(PlayerName, Pos, swing_pct_20_80, zone_swing_pct_20_80, chase_pct_20_80,
           whiff_pct_20_80, gb_pct_20_80, hard_hit_pct_20_80, xwoba_20_80, bat_speed_20_80, swing_length_20_80,
           n) %>% 
    #want descriptive names for the graph
    rename(`Swing%` = swing_pct_20_80, `Zone Swing%` = zone_swing_pct_20_80, 
           `Chase%` = chase_pct_20_80, `Whiff%` = whiff_pct_20_80, 
           `GB%` = gb_pct_20_80, `Hard Hit%` = hard_hit_pct_20_80, `Bat Speed` = bat_speed_20_80, 
           `Swing Length` = swing_length_20_80, `xWOBA` = xwoba_20_80
    ) %>% 
    tidyr::pivot_longer(cols = c(`Swing%`, `Zone Swing%`, 
                                 `Chase%`, `Whiff%`, 
                                 `GB%`, `Hard Hit%`, 
                                 `Bat Speed`, `Swing Length`, 
                                 `xWOBA`),
                        names_to = "Metric",
                        values_to = "Value") %>% 
    dplyr::mutate(Value = round(Value),
                  Value = case_when(Value < 20 ~ 20, Value > 80 ~ 80, TRUE ~ Value),
                  #this is used because the text isn't quite centered on the actual online app, 
                  #so this centers it
                  Value_p_1 = Value + 1)
  
  #we also want to display the actual percents, not just the 20-80 values
  true_pcts <- batter_data %>%
    select(PlayerName, Pos, swing_pct, zone_swing_pct, o_swing_pct,
           whiff_pct, gb_pct, hard_hit_pct, xwoba, bat_speed, swing_length,
           n) %>% 
    rename(`Swing%` = swing_pct, `Zone Swing%` = zone_swing_pct, 
           `Chase%` = o_swing_pct, `Whiff%` = whiff_pct, 
           `GB%` = gb_pct, `Hard Hit%` = hard_hit_pct, `Bat Speed` = bat_speed, 
           `Swing Length` = swing_length, `xWOBA` = xwoba
    ) %>% 
    tidyr::pivot_longer(cols = c(`Swing%`, `Zone Swing%`, 
                                 `Chase%`, `Whiff%`, 
                                 `GB%`, `Hard Hit%`, 
                                 `Bat Speed`, `Swing Length`, 
                                 `xWOBA`),
                        names_to = "Metric",
                        values_to = "True_Value") %>% 
    dplyr::mutate(True_Value = case_when(Metric %in% c("Bat Speed") ~ format(round(True_Value, 0), nsmall = 0),
                                         Metric %in% c("Swing Length") ~ format(round(True_Value, 1), nsmall = 1),
                                         Metric %in% c("xWOBA") ~ format(round(True_Value, 3), nsmall = 3),
                                         TRUE ~ format(round(True_Value * 100, 0), nsmall = 0)))
  
  for_graph <- for_graph %>% 
    inner_join(true_pcts, by = "Metric") %>% 
    #different labels for different metrics
    mutate(units = case_when(Metric %in% c("Bat Speed") ~ " MPH",
                             Metric %in% c("Swing Length") ~ " FT",
                             Metric %in% c("xWOBA") ~ "",
                             TRUE ~ "%"))
  
  statsCols <- for_graph %>% pull(Metric)
  
  percPlot <- ggplot(for_graph) +
    #my spin on statcast: top few rows are the little knobs at the end and in the middle
    geom_segment(aes(x = 20, xend = 80, y = Metric, yend = Metric), color = "#9b9b9b", linewidth = 1.5) +
    geom_point(aes(x = 20, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 50, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 80, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    #then we actuall put the point on
    geom_point(data = for_graph, 
               aes(Value, Metric, fill = Value), 
               size = 8, alpha = 1, shape = 21, stroke = 1) +
    #found the issue that the high values were less red than the low values were blue, if that makes sense. 
    #this workaround fixed it
    scale_fill_gradient2(midpoint = 50, low = "blue", mid = "#F1F1F1", high = "#ff000d", limits = c(30, 70)) +
    geom_point(data = for_graph %>% filter(Value > 70),
               aes(Value, Metric), 
               fill = "red", size = 8, shape = 21, stroke = 1) +
    geom_point(data = for_graph %>% filter(Value < 30),
               aes(Value, Metric), 
               fill = "blue", size = 8, shape = 21, stroke = 1) +
    xlim(10,90) +
    #the actual number
    geom_text(aes(x = Value_p_1, y = Metric, label = Value), 
              size = 3, 
              fontface = 'bold',
              color = ifelse(for_graph$Value > 40 & for_graph$Value < 60, "black", "white")
    ) +
    #labels with the true percents
    geom_text(aes(x = 15, y = 20, hjust = 0, label = paste0(True_Value, units)), 
              size = 3.5,  
              color = 'black'
    ) +
    #we facet differently for different variables, was important to me that we're able to compare
    #like to like
    facet_wrap(~ factor(Metric, levels = statsCols), scales='free_y', ncol = 3) +
    statcast_text +
    statcast_remove +
    expand_limits(y = c(0 - 15, 10 + 15))
  
  return(girafe(ggobj = percPlot))
}

percentiles_plot_defense <- function(batter_data, name_of_player) {

  for_graph <- batter_data %>% 
    select(PlayerName, Pos, def_per_play_20_80, arm_strength_20_80, sprint_speed_20_80,
           n) %>% 
    rename(`Defense at Primary Position` = def_per_play_20_80, `Arm Strength` = arm_strength_20_80, 
           `Sprint Speed` = sprint_speed_20_80
    ) %>% 
    tidyr::pivot_longer(cols = c(`Defense at Primary Position`, `Arm Strength`, 
                                 `Sprint Speed`),
                        names_to = "Metric",
                        values_to = "Value") %>% 
    dplyr::mutate(Value = round(Value),
                  Value = case_when(Value < 20 ~ 20, Value > 80 ~ 80, TRUE ~ Value),
                  Value_p_1 = Value + .25)
  
  true_pcts <- batter_data %>%
    select(Plays, Defense, arm_overall, sprint_speed) %>% 
    rename(`Defense at Primary Position` = Defense, `Arm Strength` = arm_overall, 
           `Sprint Speed` = sprint_speed
    ) %>% 
    tidyr::pivot_longer(cols = c(`Defense at Primary Position`, `Arm Strength`, 
                                 `Sprint Speed`),
                        names_to = "Metric",
                        values_to = "True_Value") %>% 
    dplyr::mutate(True_Value = case_when(Metric %in% c("Defense at Primary Position") ~ format(round(True_Value, 1), nsmall = 1),
                                         Metric %in% c("Arm Strength") ~ format(round(True_Value, 0), nsmall = 0),
                                         TRUE ~ format(round(True_Value, 1), nsmall = 1)))
  
  for_graph <- for_graph %>% 
    inner_join(true_pcts, by = "Metric") %>% 
    mutate(units = case_when(Metric %in% c("Defense at Primary Position") ~ paste0(" Runs on ", Plays, " plays at ", Pos),
                             Metric %in% c("Arm Strength") ~ " MPH",
                             Metric %in% c("Sprint Speed") ~ " FT/S",
                             TRUE ~ "%"))
  
  statsCols <- for_graph %>% pull(Metric)
  
  percPlot <- ggplot(for_graph) +
    geom_segment(aes(x = 20, xend = 80, y = Metric, yend = Metric), color = "#9b9b9b", linewidth = 1.5) +
    geom_point(aes(x = 20, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 50, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 80, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(data = for_graph, 
               aes(Value, Metric, fill = Value), 
               size = 8, alpha = 1, shape = 21, stroke = 1) +
    scale_fill_gradient2(midpoint = 50, low = "blue", mid = "#F1F1F1", high = "#ff000d", limits = c(30, 70)) +
    geom_point(data = for_graph %>% filter(Value > 70),
               aes(Value, Metric), 
               fill = "red", size = 8, shape = 21, stroke = 1) +
    geom_point(data = for_graph %>% filter(Value < 30),
               aes(Value, Metric), 
               fill = "blue", size = 8, shape = 21, stroke = 1) +
    xlim(10,90) +
    # Metric Label
    geom_text(aes(x = Value_p_1, y = Metric, label = Value), 
              size = 3, 
              fontface = 'bold',
              color = ifelse(for_graph$Value > 40 & for_graph$Value < 60, "black", "white")
    ) +
    # Data
    geom_text(aes(x = 15, y = 20, hjust = 0, label = paste0(True_Value, units)), 
              size = 3.5,  
              color = 'black'
    ) +
    facet_wrap(~ factor(Metric, levels = statsCols), scales='free_y', ncol = 1) +
    statcast_text +
    statcast_remove +
    expand_limits(y = c(0 - 15, 10 + 15))
  
  return(girafe(ggobj = percPlot))
}



percentiles_plot_handedness <- function(batter_data, name_of_player, handedness_filter) {
  
  handedness_filter <- c(paste0(handedness_filter, " v L"), paste0(handedness_filter, " v R"))
  

  for_graph <- batter_data %>% 
    select(PlayerName, Pos, swing_pct_20_80_L, zone_swing_pct_20_80_L, chase_pct_20_80_L,
           whiff_pct_20_80_L, gb_pct_20_80_L, hard_hit_pct_20_80_L, n_L, swing_pct_20_80_R, zone_swing_pct_20_80_R, chase_pct_20_80_R,
           whiff_pct_20_80_R, gb_pct_20_80_R, hard_hit_pct_20_80_R, n_R, ) %>% 
    rename(`Swing% v L` = swing_pct_20_80_L, `Zone Swing% v L` = zone_swing_pct_20_80_L, 
           `Chase% v L` = chase_pct_20_80_L, `Whiff% v L` = whiff_pct_20_80_L, 
           `GB% v L` = gb_pct_20_80_L, `Hard Hit% v L` = hard_hit_pct_20_80_L, 
           `Swing% v R` = swing_pct_20_80_R, `Zone Swing% v R` = zone_swing_pct_20_80_R, 
           `Chase% v R` = chase_pct_20_80_R, `Whiff% v R` = whiff_pct_20_80_R, 
           `GB% v R` = gb_pct_20_80_R, `Hard Hit% v R` = hard_hit_pct_20_80_R) %>% 
    tidyr::pivot_longer(cols = c(`Swing% v L`, `Swing% v R`, 
                                 `Zone Swing% v L`, `Zone Swing% v R`, 
                                 `Chase% v L`, `Chase% v R`, 
                                 `Whiff% v L`, `Whiff% v R`,
                                 `GB% v L`, `GB% v R`, 
                                 `Hard Hit% v L`, `Hard Hit% v R`),
                        names_to = "Metric",
                        values_to = "Value") %>% 
    dplyr::mutate(Value = round(Value),
                  Value = case_when(Value < 20 ~ 20, Value > 80 ~ 80, TRUE ~ Value),
                  Value_p_1 = Value + .5)
  
  true_pcts <- batter_data %>%
    select(swing_pct_L, swing_pct_R, zone_swing_pct_L, zone_swing_pct_R, o_swing_pct_L, o_swing_pct_R, whiff_pct_L,
    whiff_pct_R, hard_hit_pct_L, hard_hit_pct_R, gb_pct_L, gb_pct_R) %>%
    rename(`Swing% v L` = swing_pct_L, `Zone Swing% v L` = zone_swing_pct_L, 
           `Chase% v L` = o_swing_pct_L, `Whiff% v L` = whiff_pct_L, 
           `GB% v L` = gb_pct_L, `Hard Hit% v L` = hard_hit_pct_L, 
           `Swing% v R` = swing_pct_R, `Zone Swing% v R` = zone_swing_pct_R, 
           `Chase% v R` = o_swing_pct_R, `Whiff% v R` = whiff_pct_R, 
           `GB% v R` = gb_pct_R, `Hard Hit% v R` = hard_hit_pct_R) %>% 
    tidyr::pivot_longer(cols = c(`Swing% v L`, `Zone Swing% v L`, 
                                 `Chase% v L`, `Whiff% v L`, 
                                 `GB% v L`, `Hard Hit% v L`, 
                                 `Swing% v R`, `Zone Swing% v R`, 
                                 `Chase% v R`, `Whiff% v R`, 
                                 `GB% v R`, `Hard Hit% v R`),
                        names_to = "Metric",
                        values_to = "True_Value") %>% 
    dplyr::mutate(True_Value = round(True_Value * 100))
  
  for_graph <- for_graph %>% 
    inner_join(true_pcts, by = "Metric") %>% 
    dplyr::filter(Metric %in% handedness_filter)
  
  statsCols <- for_graph %>% pull(Metric)
  
  percPlot <- ggplot(for_graph) +
    geom_segment(aes(x = 20, xend = 80, y = Metric, yend = Metric), color = "#9b9b9b", linewidth = 1.5) +
    geom_point(aes(x = 20, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 50, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 80, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(data = for_graph, 
               aes(Value, Metric, fill = Value), 
               size = 8, alpha = 1, shape = 21, stroke = 1) +
    scale_fill_gradient2(midpoint = 50, low = "blue", mid = "#F1F1F1", high = "#ff000d", limits = c(30, 70)) +
    geom_point(data = for_graph %>% filter(Value > 70),
               aes(Value, Metric), 
               fill = "red", size = 8, shape = 21, stroke = 1) +
    geom_point(data = for_graph %>% filter(Value < 30),
               aes(Value, Metric), 
               fill = "blue", size = 8, shape = 21, stroke = 1) +
    xlim(10,90) +
    # Metric Label
    geom_text(aes(x = Value_p_1, y = Metric, label = Value), 
              size = 3, 
              fontface = 'bold',
              color = ifelse(for_graph$Value > 40 & for_graph$Value < 60, "black", "white")
    ) +
    # Data
    geom_text(aes(x = 15, y = 20, hjust = 0, label = paste0(True_Value, "%")), 
              size = 3.5,  
              color = 'black'
    ) +
    facet_wrap(~ factor(Metric, levels = statsCols), scales='free_y', ncol = 2) +
    statcast_text +
    statcast_remove +
    expand_limits(y = c(0 - 15, 10 + 15))
  
  return(girafe(ggobj = percPlot))
}

percentiles_plot_count <- function(batter_data, name_of_player, count_filter) {
  
  count_filter <- c(paste0(count_filter, " 2K"), paste0(count_filter, " N2K"))
  
  for_graph <- batter_data %>% 
    select(PlayerName, Pos, swing_pct_20_80_2K, zone_swing_pct_20_80_2K, chase_pct_20_80_2K,
           whiff_pct_20_80_2K, gb_pct_20_80_2K, hard_hit_pct_20_80_2K, n_2K, swing_pct_20_80_N2K, zone_swing_pct_20_80_N2K, chase_pct_20_80_N2K,
           whiff_pct_20_80_N2K, gb_pct_20_80_N2K, hard_hit_pct_20_80_N2K, n_N2K, bat_speed_20_80_N2K,
           bat_speed_20_80_2K, swing_length_20_80_N2K, swing_length_20_80_2K) %>% 
    rename(`Swing% 2K` = swing_pct_20_80_2K, `Zone Swing% 2K` = zone_swing_pct_20_80_2K, 
           `Chase% 2K` = chase_pct_20_80_2K, `Whiff% 2K` = whiff_pct_20_80_2K, 
           `GB% 2K` = gb_pct_20_80_2K, `Hard Hit% 2K` = hard_hit_pct_20_80_2K, 
           `Bat Speed 2K` = bat_speed_20_80_2K, `Swing Length 2K` = swing_length_20_80_2K, 
           `Swing% N2K` = swing_pct_20_80_N2K, `Zone Swing% N2K` = zone_swing_pct_20_80_N2K, 
           `Chase% N2K` = chase_pct_20_80_N2K, `Whiff% N2K` = whiff_pct_20_80_N2K, 
           `GB% N2K` = gb_pct_20_80_N2K, `Hard Hit% N2K` = hard_hit_pct_20_80_N2K,
           `Bat Speed N2K` = bat_speed_20_80_N2K, `Swing Length N2K` = swing_length_20_80_N2K) %>% 
    tidyr::pivot_longer(cols = c(`Swing% N2K`, `Swing% 2K`, 
                                 `Zone Swing% N2K`, `Zone Swing% 2K`, 
                                 `Chase% N2K`, `Chase% 2K`, 
                                 `Whiff% N2K`, `Whiff% 2K`,
                                 `GB% N2K`, `GB% 2K`, 
                                 `Hard Hit% N2K`, `Hard Hit% 2K`,
                                 `Bat Speed N2K`, `Bat Speed 2K`, 
                                 `Swing Length N2K`, `Swing Length 2K`
                                 ),
                        names_to = "Metric",
                        values_to = "Value") %>% 
    dplyr::mutate(Value = round(Value),
                  Value = case_when(Value < 20 ~ 20, Value > 80 ~ 80, TRUE ~ Value),
                  Value_p_1 = Value + .5)
  
  true_pcts <- batter_data %>%
    select(swing_pct_2K, zone_swing_pct_2K, o_swing_pct_2K,
           whiff_pct_2K, gb_pct_2K, hard_hit_pct_2K, swing_pct_N2K, zone_swing_pct_N2K, o_swing_pct_N2K,
           whiff_pct_N2K, gb_pct_N2K, hard_hit_pct_N2K, bat_speed_N2K,
           bat_speed_2K, swing_length_N2K, swing_length_2K) %>%
    rename(`Swing% 2K` = swing_pct_2K, `Zone Swing% 2K` = zone_swing_pct_2K, 
           `Chase% 2K` = o_swing_pct_2K, `Whiff% 2K` = whiff_pct_2K, 
           `GB% 2K` = gb_pct_2K, `Hard Hit% 2K` = hard_hit_pct_2K, 
           `Bat Speed 2K` = bat_speed_2K, `Swing Length 2K` = swing_length_2K, 
           `Swing% N2K` = swing_pct_N2K, `Zone Swing% N2K` = zone_swing_pct_N2K, 
           `Chase% N2K` = o_swing_pct_N2K, `Whiff% N2K` = whiff_pct_N2K, 
           `GB% N2K` = gb_pct_N2K, `Hard Hit% N2K` = hard_hit_pct_N2K,
           `Bat Speed N2K` = bat_speed_N2K, `Swing Length N2K` = swing_length_N2K) %>% 
    tidyr::pivot_longer(cols = c(`Swing% N2K`, `Swing% 2K`, 
                                 `Zone Swing% N2K`, `Zone Swing% 2K`, 
                                 `Chase% N2K`, `Chase% 2K`, 
                                 `Whiff% N2K`, `Whiff% 2K`,
                                 `GB% N2K`, `GB% 2K`, 
                                 `Hard Hit% N2K`, `Hard Hit% 2K`,
                                 `Bat Speed N2K`, `Bat Speed 2K`, 
                                 `Swing Length N2K`, `Swing Length 2K`
    ),
    names_to = "Metric",
                        values_to = "True_Value") %>% 
    dplyr::mutate(True_Value = case_when(Metric %in% c("Bat Speed 2K", 
                                                       "Bat Speed N2K") ~ format(round(True_Value, 0), nsmall = 0),
                                         Metric %in% c("Swing Length 2K", 
                                                       "Swing Length N2K") ~ format(round(True_Value, 1), nsmall = 1),
                                         TRUE ~ format(round(True_Value * 100, 0), nsmall = 0)))
  
  for_graph <- for_graph %>% 
    inner_join(true_pcts, by = "Metric") %>% 
    mutate(units = case_when(Metric %in% c("Bat Speed 2K", 
                                           "Bat Speed N2K") ~ " MPH",
                             Metric %in% c("Swing Length 2K", 
                                           "Swing Length N2K") ~ " FT",
                             TRUE ~ "%")) %>% 
    dplyr::filter(Metric %in% count_filter)
  
  statsCols <- for_graph %>% pull(Metric)
  
  percPlot <- ggplot(for_graph) +
    geom_segment(aes(x = 20, xend = 80, y = Metric, yend = Metric), color = "#9b9b9b", linewidth = 1.5) +
    geom_point(aes(x = 20, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 50, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 80, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(data = for_graph, 
               aes(Value, Metric, fill = Value), 
               size = 8, alpha = 1, shape = 21, stroke = 1) +
    scale_fill_gradient2(midpoint = 50, low = "blue", mid = "#F1F1F1", high = "#ff000d", limits = c(30, 70)) +
    geom_point(data = for_graph %>% filter(Value > 70),
               aes(Value, Metric), 
               fill = "red", size = 8, shape = 21, stroke = 1) +
    geom_point(data = for_graph %>% filter(Value < 30),
               aes(Value, Metric), 
               fill = "blue", size = 8, shape = 21, stroke = 1) +
    xlim(10,90) +
    # Metric Label
    geom_text(aes(x = Value_p_1, y = Metric, label = Value), 
              size = 3, 
              fontface = 'bold',
              color = ifelse(for_graph$Value > 40 & for_graph$Value < 60, "black", "white")
    ) +
    # Data
    geom_text(aes(x = 15, y = 20, hjust = 0, label = paste0(True_Value, units)), 
              size = 3.5,  
              color = 'black'
    ) +
    facet_wrap(~ factor(Metric, levels = statsCols), scales='free_y', ncol = 2) +
    statcast_text +
    statcast_remove +
    expand_limits(y = c(0 - 15, 10 + 15))
  
  return(girafe(ggobj = percPlot))
}


percentiles_plot_pitch_type <- function(batter_data, name_of_player, pitch_type_filter) {
  
  
  pitch_type_filter <- c(paste0(pitch_type_filter, " FB"), paste0(pitch_type_filter, " BB"),
                         paste0(pitch_type_filter, " OS"))
  
  for_graph <- batter_data %>% 
    select(PlayerName, Pos, swing_pct_20_80_Fastball, zone_swing_pct_20_80_Fastball, chase_pct_20_80_Fastball,
           whiff_pct_20_80_Fastball, gb_pct_20_80_Fastball, hard_hit_pct_20_80_Fastball, n_Fastball, swing_pct_20_80_Breaking, zone_swing_pct_20_80_Breaking, chase_pct_20_80_Breaking,
           whiff_pct_20_80_Breaking, gb_pct_20_80_Breaking, hard_hit_pct_20_80_Breaking, n_Breaking, swing_pct_20_80_Offspeed, zone_swing_pct_20_80_Offspeed, chase_pct_20_80_Offspeed,
           whiff_pct_20_80_Offspeed, gb_pct_20_80_Offspeed, hard_hit_pct_20_80_Offspeed, n_Offspeed) %>% 
    rename(`Swing% FB` = swing_pct_20_80_Fastball, `Zone Swing% FB` = zone_swing_pct_20_80_Fastball, 
           `Chase% FB` = chase_pct_20_80_Fastball, `Whiff% FB` = whiff_pct_20_80_Fastball, 
           `GB% FB` = gb_pct_20_80_Fastball, `Hard Hit% FB` = hard_hit_pct_20_80_Fastball, 
           `Swing% BB` = swing_pct_20_80_Breaking, `Zone Swing% BB` = zone_swing_pct_20_80_Breaking, 
           `Chase% BB` = chase_pct_20_80_Breaking, `Whiff% BB` = whiff_pct_20_80_Breaking, 
           `GB% BB` = gb_pct_20_80_Breaking, `Hard Hit% BB` = hard_hit_pct_20_80_Breaking,
           `Swing% OS` = swing_pct_20_80_Offspeed, `Zone Swing% OS` = zone_swing_pct_20_80_Offspeed, 
           `Chase% OS` = chase_pct_20_80_Offspeed, `Whiff% OS` = whiff_pct_20_80_Offspeed, 
           `GB% OS` = gb_pct_20_80_Offspeed, `Hard Hit% OS` = hard_hit_pct_20_80_Offspeed,
           ) %>% 
    tidyr::pivot_longer(cols = c(`Swing% FB`, `Swing% BB`, `Swing% OS`, 
                                 `Zone Swing% FB`, `Zone Swing% BB`, `Zone Swing% OS`, 
                                 `Chase% FB`,  `Chase% BB`, `Chase% OS`, 
                                 `Whiff% FB`, `Whiff% BB`, `Whiff% OS`, 
                                 `GB% FB`, `GB% BB`, `GB% OS`, 
                                 `Hard Hit% FB`, `Hard Hit% BB`,`Hard Hit% OS`
    ),
                        names_to = "Metric",
                        values_to = "Value") %>% 
    dplyr::mutate(Value = round(Value),
                  Value = case_when(Value < 20 ~ 20, Value > 80 ~ 80, TRUE ~ Value),
                  Value_p_1 = Value + 1)
  
  true_pcts <- batter_data %>%
    select(swing_pct_Fastball, zone_swing_pct_Fastball, o_swing_pct_Fastball,
           whiff_pct_Fastball, gb_pct_Fastball, hard_hit_pct_Fastball, swing_pct_Breaking, zone_swing_pct_Breaking, o_swing_pct_Breaking,
           whiff_pct_Breaking, gb_pct_Breaking, hard_hit_pct_Breaking, swing_pct_Offspeed, zone_swing_pct_Offspeed, o_swing_pct_Offspeed,
           whiff_pct_Offspeed, gb_pct_Offspeed, hard_hit_pct_Offspeed) %>% 
    rename(`Swing% FB` = swing_pct_Fastball, `Zone Swing% FB` = zone_swing_pct_Fastball, 
           `Chase% FB` = o_swing_pct_Fastball, `Whiff% FB` = whiff_pct_Fastball, 
           `GB% FB` = gb_pct_Fastball, `Hard Hit% FB` = hard_hit_pct_Fastball, 
           `Swing% BB` = swing_pct_Breaking, `Zone Swing% BB` = zone_swing_pct_Breaking, 
           `Chase% BB` = o_swing_pct_Breaking, `Whiff% BB` = whiff_pct_Breaking, 
           `GB% BB` = gb_pct_Breaking, `Hard Hit% BB` = hard_hit_pct_Breaking,
           `Swing% OS` = swing_pct_Offspeed, `Zone Swing% OS` = zone_swing_pct_Offspeed, 
           `Chase% OS` = o_swing_pct_Offspeed, `Whiff% OS` = whiff_pct_Offspeed, 
           `GB% OS` = gb_pct_Offspeed, `Hard Hit% OS` = hard_hit_pct_Offspeed,
    ) %>% 
    tidyr::pivot_longer(cols = c(`Swing% FB`, `Swing% BB`, `Swing% OS`, 
                                 `Zone Swing% FB`, `Zone Swing% BB`, `Zone Swing% OS`, 
                                 `Chase% FB`,  `Chase% BB`, `Chase% OS`, 
                                 `Whiff% FB`, `Whiff% BB`, `Whiff% OS`, 
                                 `GB% FB`, `GB% BB`, `GB% OS`, 
                                 `Hard Hit% FB`, `Hard Hit% BB`,`Hard Hit% OS`
                                 ),
                        names_to = "Metric",
                        values_to = "True_Value") %>% 
    dplyr::mutate(True_Value = round(True_Value * 100, 0))
  
  for_graph <- for_graph %>% 
    inner_join(true_pcts, by = "Metric") %>% 
    mutate(units = "%") %>% 
    dplyr::filter(Metric %in% pitch_type_filter)
  
  
  statsCols <- for_graph %>% pull(Metric)
  
  percPlot <- ggplot(for_graph) +
    geom_segment(aes(x = 20, xend = 80, y = Metric, yend = Metric), color = "#9b9b9b", linewidth = 1.5) +
    geom_point(aes(x = 20, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 50, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(aes(x = 80, y = Metric), size = 2.5, alpha = 1, stroke = 0.5, color = '#9b9b9b') +
    geom_point(data = for_graph, 
               aes(Value, Metric, fill = Value), 
               size = 8, alpha = 1, shape = 21, stroke = 1) +
    scale_fill_gradient2(midpoint = 50, low = "blue", mid = "#F1F1F1", high = "#ff000d", limits = c(30, 70)) +
    geom_point(data = for_graph %>% filter(Value > 70),
               aes(Value, Metric), 
               fill = "red", size = 8, shape = 21, stroke = 1) +
    geom_point(data = for_graph %>% filter(Value < 30),
               aes(Value, Metric), 
               fill = "blue", size = 8, shape = 21, stroke = 1) +
    xlim(10,90) +
    # Metric Label
    geom_text(aes(x = Value_p_1, y = Metric, label = Value), 
              size = 3, 
              fontface = 'bold',
              color = ifelse(for_graph$Value > 40 & for_graph$Value < 60, "black", "white")
    ) +
    # Data
    geom_text(aes(x = 15, y = 20, hjust = 0, label = paste0(True_Value, units)), 
              size = 3.5,  
              color = 'black'
    ) +
    facet_wrap(~ factor(Metric, levels = statsCols), scales='free_y', ncol = 3) +
    statcast_text +
    statcast_remove +
    expand_limits(y = c(0 - 15, 10 + 15))
  
  return(girafe(ggobj = percPlot))
}

