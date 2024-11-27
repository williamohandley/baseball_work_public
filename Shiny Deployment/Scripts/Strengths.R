
#the big thing i was trying to do here was make this all readable and make it sound like it 
#came from a human rather than a machine. So instead of creating one function, I custom made
#phrases for each metric (bat speed, whiff rate, etc.) trying to add specific terminology 
#(expand the zone) so it feels more natural. I also included different adjectives 
#depending on how above curve a certain stat was



strengths_paragraph <- function(Name_Input, data_frame) {
  to_look_at <- data_frame %>% 
    dplyr::filter(PlayerName == Name_Input)
  
  name <- Name_Input
  
  last_name <- paste(strsplit(name, " ")[[1]][-1], collapse = " ")
  
  strengths <- to_look_at %>% 
    select(PlayerName, Pos, contains("20_80")) %>% 
    tidyr::pivot_longer(cols = contains("20_80"),
                        names_to = "Metric",
                        values_to = "Value") %>% 
    dplyr::filter(Value >= 60) %>% 
    mutate(word = case_when(Value < 70 ~ "very",
                            Value < 80 ~ "extremely",
                            TRUE ~ "unbelievably"))
  
  
  Pos <- to_look_at %>% 
    pull(Pos)
  
  strength_description <- ""
  
  if("swing_pct_20_80" %in% strengths$Metric){
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "swing_pct_20_80") %>% 
      pull(word)
    
    swing_pct <- to_look_at %>% 
      pull(swing_pct)
    
    swing_pct <- round(swing_pct * 100)
    
    strength_description <- paste0(strength_description, name, " is selective at the plate, only swinging at ", swing_pct, "% of pitches he saw this past season compared to a league average of 48%. ")
    
    name <- "He"
    
  }
  
  if("chase_pct_20_80" %in% strengths$Metric){
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "chase_pct_20_80") %>% 
      pull(word)
    
    strength_description <- paste0(strength_description, name, " did a good job not expanding the zone in 2024, his chase rate was ", word_choice," low compared to the rest of the league. ")
    
    name <- "He"
    
  }
  
  
  
  
  
  if("gb_pct_20_80" %in% strengths$Metric){
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "gb_pct_20_80") %>% 
      pull(word)
    
    gb_pct <- to_look_at %>% 
      pull(gb_pct)
    
    gb_pct <- round(gb_pct * 100)
    
    strength_description <- paste0(strength_description, "Pitchers didn't do a good job of forcing ", last_name, " to get on top of the ball, and he kept his ground ball rate low. ")
    
    name <- "He"
    
  }
  
  if("hard_hit_pct_20_80" %in% strengths$Metric){
    
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "hard_hit_pct_20_80") %>% 
      pull(word)
    
    hard_hit_pct <- to_look_at %>% 
      pull(hard_hit_pct)
    
    hard_hit_pct <- round(hard_hit_pct * 100)
    
    
    strength_description <- paste0(strength_description, name, " hit the ball hard, and he hit the ball hard ", word_choice, " often. In fact, only ", hard_hit_pct, "% of the balls ", last_name, " hit in play had an exit velocity above 95 MPH. ")
    
    name <- "He"
  }
  
  if("bat_speed_20_80" %in% strengths$Metric){
    
    bat_speed <- to_look_at %>% 
      pull(bat_speed)
    
    bat_speed <- round(bat_speed - 69.67041 )
    
    strength_description <- paste0(strength_description, name, " has one of the faster bat speeds in baseball. The league average batspeed is 70 MPH, but ", last_name, " was ", bat_speed, " MPH above that on average. For comparison, Aaron Judge is 6 MPH above average. ")
    
    name <- "He"
  }
  
  if("swing_length_20_80" %in% strengths$Metric){
    
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "swing_length_20_80") %>% 
      pull(word)
    
    strength_description <- paste0(strength_description, name, " has a ", word_choice, " short, compact swing, which should enable him to make contact a higher percentage of the time. ")
    
    name <- "He"
  }
  
  if("whiff_pct_20_80" %in% strengths$Metric){
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "whiff_pct_20_80") %>% 
      pull(word)
    
    value <- strengths %>% 
      dplyr::filter(Metric == "whiff_pct_20_80") %>% 
      pull(Value)
    
    value <- round(value)
    
    if("swing_length_20_80" %in% strengths$Metric){
      strength_description <- paste0(strength_description, "In fact, when ")
    }else{
      strength_description <- paste0(strength_description, "When ")
    }
    
    strength_description <- paste0(strength_description, last_name, " swung, he mostly made contact, having a ", word_choice, " low whiff percent that puts him at a ", value, " on the 20-80 scale. ")
    
    name <- "He"
    
  }
  
  
  if("def_per_play_20_80" %in% strengths$Metric){
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "def_per_play_20_80") %>% 
      pull(word)
    
    defense <- strengths %>% 
      dplyr::filter(Metric == "def_per_play_20_80") %>% 
      pull(Value)
    
    defense <- round(defense)
    
    strength_description <- paste0(strength_description, "Defensively, ", last_name, " is a ", word_choice, " good defender for his position, with our defensive metrics putting a ", defense, " on his ", Pos, " ability based on the plays he was able to make in 2024. ")
    
    name <- "He"
  }
  
  if("arm_strength_20_80" %in% strengths$Metric){
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "arm_strength_20_80") %>% 
      pull(word)
    
    strength_description <- paste0(strength_description, name, " has a ", word_choice, " strong arm for ", Pos, ". ")
    
    name <- "He"
  }
  
  if("sprint_speed_20_80" %in% strengths$Metric){
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "sprint_speed_20_80") %>% 
      pull(word)
    
    speed <- strengths %>% 
      dplyr::filter(Metric == "sprint_speed_20_80") %>% 
      pull(Value)
    
    speed <- round(speed)
    
    
    strength_description <- paste0(strength_description, name, " is ", word_choice, " fast on the basepaths, with ", speed, " speed. ")
    
    name <- "He"
  }
  
  if("xwoba_20_80" %in% strengths$Metric){
    word_choice <- strengths %>% 
      dplyr::filter(Metric == "xwoba_20_80") %>% 
      pull(word)
    
    strength_description <- paste0(strength_description, "All of this added up to ", last_name, " being one of the best hitters in the league in 2024.")
    
    name <- "He"
  }
  
  
  if (strength_description == ""){
    if (Pos %in% c("SS", "CF")){
      strength_description <- paste0(strength_description, "There wasn't anything that ", last_name, " did extremely well in 2024, but he plays ", Pos, " at the major league level.")
      
    }else{
      strength_description <- paste0(strength_description, "There wasn't anything that ", last_name, " did that jumped off the page last year.")
      
    }
    
  }
  
  
  return(strength_description)
  
}




weaknesses_paragraph <- function(Name_Input, data_frame) {
  to_look_at <- data_frame %>% 
    dplyr::filter(PlayerName == Name_Input)
  
  name <- Name_Input
  
  last_name <- paste(strsplit(name, " ")[[1]][-1], collapse = " ")
  
  weaknesses <- to_look_at %>% 
    select(PlayerName, Pos, contains("20_80")) %>% 
    tidyr::pivot_longer(cols = contains("20_80"),
                        names_to = "Metric",
                        values_to = "Value") %>% 
    dplyr::filter(Value <= 40) %>% 
    mutate(word = case_when(Value > 30 ~ "very",
                            Value > 20 ~ "extremely",
                            TRUE ~ "unbelievably"))
  
  
  Pos <- to_look_at %>% 
    pull(Pos)
  
  weakness_description <- ""
  
  if("swing_pct_20_80" %in% weaknesses$Metric){
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "swing_pct_20_80") %>% 
      pull(word)
    
    swing_pct <- to_look_at %>% 
      pull(swing_pct)
    
    swing_pct <- round(swing_pct * 100)
    
    weakness_description <- paste0(weakness_description, name, " is a free swinger at the plate, taking hacks at ", swing_pct, "% of pitches he saw this past season compared to a league average of 48%. ")
    
    name <- "He"
    
  }
  
  if("chase_pct_20_80" %in% weaknesses$Metric){
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "chase_pct_20_80") %>% 
      pull(word)
    
    weakness_description <- paste0(weakness_description, name, " really expanded the zone in 2024, his chase rate was ", word_choice," high compared to the rest of the league. ")
    
    name <- "He"
    
  }
  
  
  
  
  
  if("gb_pct_20_80" %in% weaknesses$Metric){
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "gb_pct_20_80") %>% 
      pull(word)
    
    gb_pct <- to_look_at %>% 
      pull(gb_pct)
    
    vs_mean <- round((gb_pct - 0.4230732) * 100)
    
    gb_pct <- round(gb_pct * 100)
    
    weakness_description <- paste0(weakness_description, name, " gets on top of the ball a lot, his GB% is ", gb_pct, "%, ", vs_mean, "% above the league average. ")
    
    name <- "He"
    
  }
  
  if("hard_hit_pct_20_80" %in% weaknesses$Metric){
    
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "hard_hit_pct_20_80") %>% 
      pull(word)
    
    hard_hit_pct <- to_look_at %>% 
      pull(hard_hit_pct)
    
    hard_hit_pct <- round(hard_hit_pct * 100)
    
    
    weakness_description <- paste0(weakness_description, name, " hits the ball hard ", word_choice, " rarely. In fact, ", hard_hit_pct, "% of the balls ", last_name, " hit in play had an exit velocity above 95 MPH. ")
    
    name <- "He"
  }
  
  if("bat_speed_20_80" %in% weaknesses$Metric){
    
    bat_speed <- to_look_at %>% 
      pull(bat_speed)
    
    bat_speed <- round(69.67041 - bat_speed)
    
    weakness_description <- paste0(weakness_description, name, " has one of the slower bat speeds in baseball. The league average batspeed is 70 MPH, but ", last_name, " was ", bat_speed, " MPH below that on average. ")
    
    name <- "He"
  }
  
  if("swing_length_20_80" %in% weaknesses$Metric){
    
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "swing_length_20_80") %>% 
      pull(word)
    
    weakness_description <- paste0(weakness_description, name, " has a ", word_choice, " long swing, which is not that big of an issue as long as he's swinging fast, but can impact his ability to make contact. ")
    
    name <- "He"
  }
  
  if("whiff_pct_20_80" %in% weaknesses$Metric){
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "whiff_pct_20_80") %>% 
      pull(word)
    
    value <- weaknesses %>% 
      dplyr::filter(Metric == "whiff_pct_20_80") %>% 
      pull(Value)
    
    value <- round(value)
    
    if("swing_length_20_80" %in% weaknesses$Metric){
      weakness_description <- paste0(weakness_description, "In fact, when ")
    }else{
      weakness_description <- paste0(weakness_description, "When ")
    }
    
    weakness_description <- paste0(weakness_description, last_name, " swung, he struggled to make contact, having a ", word_choice, " high whiff percent that puts him at a ", value, " on the 20-80 scale. ")
    
    name <- "He"
    
  }
  
  
  if("def_per_play_20_80" %in% weaknesses$Metric){
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "def_per_play_20_80") %>% 
      pull(word)
    
    defense <- weaknesses %>% 
      dplyr::filter(Metric == "def_per_play_20_80") %>% 
      pull(Value)
    
    defense <- round(defense)
    
    weakness_description <- paste0(weakness_description, "Defensively, ", last_name, " is a ", word_choice, " poor defender for his position, with our defensive metrics putting a ", defense, " on his ", Pos, " ability based on the plays he couldn't make in 2024. ")
    
    name <- "He"
  }
  
  if("arm_strength_20_80" %in% weaknesses$Metric){
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "arm_strength_20_80") %>% 
      pull(word)
    
    weakness_description <- paste0(weakness_description, name, " has a ", word_choice, " weak arm for ", Pos, ". ")
    
    name <- "He"
  }
  
  if("sprint_speed_20_80" %in% weaknesses$Metric){
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "sprint_speed_20_80") %>% 
      pull(word)
    
    speed <- weaknesses %>% 
      dplyr::filter(Metric == "sprint_speed_20_80") %>% 
      pull(Value)
    
    speed <- round(speed)
    
    
    weakness_description <- paste0(weakness_description, name, " is ", word_choice, " slow on the basepaths, with ", speed, " speed. ")
    
    name <- "He"
  }
  
  if("xwoba_20_80" %in% weaknesses$Metric){
    word_choice <- weaknesses %>% 
      dplyr::filter(Metric == "xwoba_20_80") %>% 
      pull(word)
    
    weakness_description <- paste0(weakness_description, "All of this added up to ", last_name, " being in the lower tier of hitters in the league in 2024.")
    
    name <- "He"
  }
  
  if (weakness_description == ""){
    if (Pos %in% c("1b", "LF")){
      weakness_description <- paste0(weakness_description, "The only real weakness in ", last_name, "'s game is that he plays ", Pos, ", although he doesn't even play the position poorly. ")
      
    }else{
      weakness_description <- paste0(weakness_description, "There aren't any glaring weaknesses in ", last_name, "'s game.")
      
    }
    
  }
  
  
  return(weakness_description)
  
}

