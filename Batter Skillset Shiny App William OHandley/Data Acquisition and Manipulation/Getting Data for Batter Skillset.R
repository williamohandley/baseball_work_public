library(baseballr)
library(dplyr)

#want to get all 2024 statcast data. Savant only lets you get 25000 rows at a time, 
#have to set up a way to get all of the pitches. getting 5 days worth of pitches at a
#time using a for loop

all_pitches <- c()

start_date <- as.Date("2024-03-27")
end_date <- as.Date("2024-03-31")

pitch_rows_previous <- 0

for (i in 1:100) {
  
  #using the baseballR package to grab the data
  #if you're unable to access these functions for any reason,
  #the baseballR_functions doc has any functions that I use along with
  #their dependencies.
  all_pitches <- statcast_search(start_date = start_date, 
                                     end_date = end_date, 
                                     player_type = 'batter') %>% 
    dplyr::bind_rows(all_pitches)
  
  
  
  #only need to go up to the end of the regular season
  if (end_date > "2024-10-01"){
    stop("You did it")
  }
  
  #to make sure we're not at that upper 25000 limit, printing the number of pitches 
  #in each loop
  pitch_rows_new <- nrow(all_pitches)
  
  print(pitch_rows_new - pitch_rows_previous )
  
  if ((pitch_rows_new - pitch_rows_previous) > 25000){
    stop("Too many rows in timespan")
  }
  
  pitch_rows_previous <- pitch_rows_new
  
  start_date <- start_date + 5
  end_date <- end_date + 5
  
  #ensure i know where we are 
  print(end_date)
}

View(all_pitches %>% group_by(game_date) %>% summarise(n = n()))

#taking out playoff games

all_pitches <- all_pitches %>% 
  dplyr::filter(game_date < "2024-10-01")

setwd("/Users/billyohandley/Downloads/Cardinals Project/Data")

#saving them off so that we can access all of this much more easily 

write.csv(all_pitches, "2024_Statcast_Pitches.csv")

#now I want to get other info from fangraphs on players' position-specifc 
#fielding results 

positions <- c("c", "1b", "2b", "3b", "ss", "lf", "cf", "rf")

fielding_stats <- c()

for (pos in positions) {
  fielding_stats <- fg_fielder_leaders(startseason = 2024, endseason = 2024, pos = pos) %>% 
    bind_rows(fielding_stats)
}

View(fielding_stats %>% group_by(Pos) %>% summarise(n = n()))

setwd("/Path/Data")

#saving them off so that we can access all of this much more easily 

write.csv(fielding_stats, "2024_Fielding_Stats.csv")


#I'm also getting some baserunning/overall data from statcast leaderboards
#directly
