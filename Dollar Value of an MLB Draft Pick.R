library(dplyr)
library(ggplot2)
library(baseballr)

#Estimateing the dollar value of an MLB draft pick!

#for year-by-year data player data, we can scrape the fangraphs leaderboards using the
#fg_batter_leaders and fg_pitcher_leaders functions from the baseballr package. 
#The functions weren't working earlier, so I looked into them and made a PR
#to the github page with some bug fixes, and now they work! You'll want to install
#the most recent versions directly from github rather than from CRAN using
#devtools::install_github(repo = "BillPetti/baseballr")

#i'll be using the baseballr suite of functions throughout the code

batter_data <- tibble()

pitcher_data <- tibble()

for (i in 1980:2023) {
  
  batter_data <- fg_batter_leaders(startseason = i, endseason = i) %>% 
    bind_rows(batter_data)
  
  pitcher_data <- fg_pitcher_leaders(startseason = i, endseason = i) %>% 
    bind_rows(pitcher_data)
  
}

batters_per_season <- batter_data %>% 
  group_by(Season) %>% 
  summarise(n = n())

#looks like there's a major uptick in players in 2002? 

pitchers_per_season <- pitcher_data %>% 
  group_by(Season) %>% 
  summarise(n = n())

#not really seeing the same on the pitcher side. Went to Fangraph's website just
#to make sure this was on their end and not on the scraper's end, and it is. Interesting!

#we can use baseballr's "mlb_draft" function to scrape draft info from 1980 up until 
#2006. I chose 2006 because the youngest players from that year are 35 now, and if I 
#pulled in data from a later draft, there might be players who still have years of t
#eam control (and therefore, value) that would be unaccounted for. 
#Something to note: using earlier years might bias the results, as teams have presumably
#gotten better over the years at identifying drafted talent in earlier rounds, so drafts
#like 2000 might be more similar in terms of talent to 2024 than drafts like 1980

draft_data <- tibble()

for (i in 1980:2006) {
  
  draft_data <- mlb_draft(i) %>% 
    bind_rows(draft_data)
  
}

#I ended up noticing a bunch of inconsistencies in MLB's draft database and 
#baseball references' (we'll get to that later), so the piece of code below
#is just cleaning up the data issues I found in MLB's database. I made
#sure these issues were on MLB's end before making these changes

draft_data <- draft_data %>% 
  mutate(year = as.numeric(year),
         pick_number = case_when(pick_number == 30 & year == 1988 ~ 28,
                                 pick_number > 27 & pick_number < 30 & year == 1988 ~ pick_number + 1,
                                 pick_number == 78 & year == 1988 ~ 79,
                                 pick_number == 79 & year == 1988 ~ 78,
                                 pick_number > 86 & year == 1989 ~ pick_number + 1,
                                 pick_number == 74 & year == 1991 ~ 77,
                                 pick_number > 74 & pick_number < 78 & year == 1991 ~ pick_number - 1,
                                 pick_number == 81 & year == 1992 ~ 79,
                                 pick_number > 78 & pick_number < 81 & year == 1992 ~ pick_number + 1,
                                 TRUE ~ pick_number),
         person_full_name = case_when(person_full_name == "James Heuser" ~ "Josh Burrus",
                                      person_full_name == "David Guthrie" ~ "Kevin Bass",
                                      person_full_name == "Johnny Walker" ~ "Paul Carey",
                                      TRUE ~ person_full_name),
         person_id = case_when(person_id == 450400 ~ 111990,
                               person_id == 441873 ~ 441870,
                               person_id == 445159 ~ 455159,
                               TRUE ~ person_id)) %>% 
  add_row(pick_number = 87, year = 1989, person_full_name = "Paul Cluff",
          #players table doesn't have a person_id for Paul Cluff, so I'm making one up
          person_id = 24601) %>% 
  filter(pick_number <= 100)

pick_counter <- draft_data %>% 
  group_by(pick_number) %>% 
  summarise(n = n())

year_counter <- draft_data %>% 
  group_by(year) %>% 
  summarise(n = n())

#we're not missing any picks!! 

#none of this has any information on whether the player ended up signing with the team,
#which is pretty important information. the baseballr functions don't have the capability 
#to find that info either. Something I could do is group by player and essentially
#mark the last time they were drafted, assuming that other times they were drafted before
#they didn't sign. The only issue with this is that it would assume that teams signed
#players who who were drafted, didn't sign, and were never drafted again

#this information is present on baseball reference, and I can build a scraping 
#function to scrape it from the site and load it into R. I made sure this 
#use case is allowed by the site's data usage/scraping policy.

draft_pick_signing_scraper <- function(year, round) {
  
  url <- paste0("https://www.baseball-reference.com/draft/?draft_round=", round, "&year_ID=",
                year, "&draft_type=junreg&query_type=year_round")
  
  html_doc <- url %>% 
    xml2::read_html()
  
  tables <- html_doc %>% 
    rvest::html_elements("table")
  
  #extract the data from the body
  
  table_data <- tables %>% 
    purrr::map_df(~{
      rows <- .x %>% rvest::html_elements("tbody") %>% rvest::html_elements("tr")
      row_data <- rows %>% purrr::map_df(~{
        columns <- .x %>% rvest::html_elements("td") %>% rvest::html_text()
        as_tibble(t(columns))
      })
      return(row_data)
    })
  
  #extract the column names from the header
  
  table_names <- tables %>% 
    purrr::map_df(~{
      header_rows <- .x %>% rvest::html_elements("thead") %>% rvest::html_elements("tr")
      header_columns <- header_rows %>% purrr::map_df(~{
        columns <- .x %>% rvest::html_elements("th") %>% rvest::html_text()
        as_tibble(t(columns))
      })
      return(header_columns)
    }) 
  
  #the table has two columns named "G," let's replace one with "G_pitched" for 
  #games pitched
  
  table_names[[18]] <- "G_pitched"
  
  table_data <- tibble(column = year) %>% 
    bind_cols(table_data) %>% 
    purrr::set_names(table_names) %>% 
    #removing the (minors) and the * to make the table look cleaner
    mutate_all(~stringr::str_squish(stringr::str_replace_all(., "\\(minors\\)|\\*", ""))) %>% 
    #making the overall picks numeric 
    mutate(OvPck = as.numeric(OvPck),
           Year = as.numeric(Year))
  
  #this is so I don't get rate limited
  Sys.sleep(10)
  
  return(table_data)
}

#now that we have the scraper, let's get info on which players signed. takes a bit
#to run due to the rate limiter, so for user benefit, I have the function print off 
#what year we're on

signing_info <- tibble()

for (i in 1980:2006) {
  
  for (n in 1:4) {
    signing_info <- draft_pick_signing_scraper(i, n) %>% 
      bind_rows(signing_info)
    
  }
  
  print(i)
}

signing_info <- signing_info %>% 
  #Billy Cannon (https://en.wikipedia.org/wiki/Billy_Cannon_Jr) is skipped
  #by baseball reference, so I need to fix that
  mutate(OvPck = case_when(OvPck > 73 & Year == 1980 ~ OvPck + 1, 
                           TRUE ~ OvPck)) %>% 
  filter(OvPck <= 100) %>% 
  select(Year, OvPck, Signed, Name)

signing_info <- signing_info %>% 
  filter(OvPck <= 100)

pick_counter <- signing_info %>% 
  group_by(OvPck) %>% 
  summarise(n = n())

year_counter <- signing_info %>% 
  group_by(Year) %>% 
  summarise(n = n())

#dataset is complete (minus Billy Cannon, who we don't need here)

#let's pull in the additional chadwick info on these players, now that
#we've added players here. we can do this using the chadwick_player_lu()
#function 

people <- chadwick_player_lu()

#now, let's combine all the data into one tibble, with only the information we need
#present

batter_data <- batter_data %>% 
  select(Season, xMLBAMID, PlayerName, WAR) %>% 
  rename("Batter_WAR" = "WAR")

pitcher_data <- pitcher_data %>% 
  select(Season, xMLBAMID, PlayerName, WAR) %>% 
  rename("Pitcher_WAR" = "WAR")

all_player_data <- batter_data %>% 
  full_join(pitcher_data, by = c("PlayerName", "Season", "xMLBAMID")) 

people <- people %>% 
  mutate(full_name = paste0(name_first, " ", name_last)) %>% 
  select(key_mlbam, mlb_played_first, mlb_played_last, full_name)

all_mlb_draft_data <- draft_data %>% 
  select(pick_number, person_id, person_full_name, year, person_mlb_debut_date) %>% 
  left_join(people, by = c("person_id" = "key_mlbam")) %>% 
  #note here: there are a handful of players who's number in the chadwick database
  #is either not present or incorrect. None of those players ended up making the major
  #leagues
  left_join(signing_info, by = c("year" = "Year", "pick_number" = "OvPck")) %>% 
  #looked into all the "unknowns," of those, Thomas Powers didn't sign, all the rest did
  #we also have to replace the "NA" value with a "N" (Billy Cannon)
  mutate(Signed = case_when(Signed == "Unk" & person_full_name == "Thomas Powers" ~ "N", 
                            Signed == "Unk" & person_full_name != "Thomas Powers" ~ "Y", 
                            is.na(Signed) ~ "N",
                            TRUE ~ Signed))

#let's check out the players who's names don't match and ensure that it's not an issue

names_dont_match <- all_mlb_draft_data %>% 
  rowwise() %>%
  mutate(string_distance = min(stringdist::stringdistmatrix(person_full_name, Name) / pmin(nchar(person_full_name), nchar(Name)))) %>% 
  filter(string_distance > .5)

#no issues! our draft data set is complete

rm(list = setdiff(ls(), c("all_mlb_draft_data", "all_player_data")))

all_player_data <- all_player_data %>% 
  replace(is.na(.), 0) %>% 
  mutate(season_WAR = Batter_WAR + Pitcher_WAR) %>% 
  arrange(Season) %>% 
  group_by(xMLBAMID) %>% 
  mutate(year_in_majors = row_number(), first_year = min(Season),
         #in order to properly discount/calculate the value of the player year
         #by year, we need to know how many years after they were drafted 
         #each season takes place
         years_since_first = Season - first_year, 
         years_since_first_col_name = paste0("years_since_first_", year_in_majors)) %>% 
  ungroup() %>% 
  #only looking at the first 7 years of their careers, as that's more or less the
  #amount of team control you would get from a first round pick 
  filter(year_in_majors <= 7) %>% 
  select(xMLBAMID, PlayerName, season_WAR, year_in_majors, years_since_first, 
         years_since_first_col_name) 

war_in_each_season <- all_player_data %>% 
  mutate(year_in_majors = paste0("year_", year_in_majors)) %>% 
  select(xMLBAMID, PlayerName, season_WAR, year_in_majors) %>% 
  tidyr::pivot_wider(names_from = year_in_majors, values_from = season_WAR) 

years_after_season_one <- all_player_data %>% 
  select(xMLBAMID, PlayerName, years_since_first, years_since_first_col_name) %>% 
  tidyr::pivot_wider(names_from = years_since_first_col_name, values_from = years_since_first)

all_player_data <- war_in_each_season %>% 
  inner_join(years_after_season_one, by = c("xMLBAMID", "PlayerName"))

draft_picks_with_value <- all_mlb_draft_data %>% 
  select(pick_number, person_id, year, mlb_played_first, Signed, person_full_name) %>% 
  left_join(all_player_data, by = c("person_id" = "xMLBAMID"))

names_dont_match <- draft_picks_with_value %>% 
  rowwise() %>%
  mutate(string_distance = min(stringdist::stringdistmatrix(person_full_name, PlayerName) / pmin(nchar(person_full_name), nchar(PlayerName)))) %>% 
  filter(string_distance > .5)

#I'm making the assumption here that $/WAR in 2024 will end up being ~ 9.5 million,
#and that $/WAR will increase with inflation by 6% every year. I'm also making the 
#assumption that the WACC of an MLB team is both greater than both the Imputed Loan Interest 
#Rate (ILIR) and inflation, and is = to 8%, which I'll use as a discount rate to calculate
#the present value of the contract in 2024 dollars. Essentially: I'm predicting that
#the amount the return on investment the a team would be able to get on their own money
#is greater than the amount that $/WAR will go up every season.

on_field_value_calculator <- function(year, years_after_draft, years_since_first) {
  value <- case_when(!(is.na(year)) ~ 9500000 * (1.06 / 1.08) ^ (years_after_draft + years_since_first) * year, TRUE ~ 0)
  
  return(value)
  
}

#the current min salary is 740000, and it's supposed to go up 20000 a year for the next
#few years, so let's assume that it goes up 20000 a year every season

min_salary_calculator <- function(year, years_after_draft, years_since_first) {
  
  value <- case_when(!(is.na(year)) ~ (740000 + 20000 * (years_after_draft + years_since_first)) * (1 / 1.08) ^ (years_after_draft + years_since_first) , TRUE ~ 0)
  
  return(value)
}

draft_picks_with_value <- draft_picks_with_value %>% 
  mutate(years_after_draft = mlb_played_first - year,
         #first, i calculate and discount the value of every season
         on_field_value_in_2024_dollars = on_field_value_calculator(year_1, years_after_draft, years_since_first_1) +
           on_field_value_calculator(year_2, years_after_draft, years_since_first_2) +
           on_field_value_calculator(year_3, years_after_draft, years_since_first_3) +
           on_field_value_calculator(year_4, years_after_draft, years_since_first_4) +
           on_field_value_calculator(year_5, years_after_draft, years_since_first_5) +
           on_field_value_calculator(year_6, years_after_draft, years_since_first_6) +
           on_field_value_calculator(year_7, years_after_draft, years_since_first_7),
         #then i calculate the minimum salary of those seasons in 2024 dollars
         total_min_salary_in_2024_dollars = min_salary_calculator(year_1, years_after_draft, years_since_first_1) +
           min_salary_calculator(year_2, years_after_draft, years_since_first_2) +
           min_salary_calculator(year_3, years_after_draft, years_since_first_3) +
           min_salary_calculator(year_4, years_after_draft, years_since_first_4) +
           min_salary_calculator(year_5, years_after_draft, years_since_first_5) +
           min_salary_calculator(year_6, years_after_draft, years_since_first_6) +
           min_salary_calculator(year_7, years_after_draft, years_since_first_7),
         #then, if it's an arbitration year, I calculate the expected arbitration 
         #salary in 2024 dollars. I'm predicting that the value of arbitration
         #will start at 1.75, 2.75, and 3.75 million above minimum for every WAR,
         #which are all slightly above the value Ben Clemens calculated for arb
         #salaries between 2013 and 2021, and that it will increase at the same 
         #rate as $/WAR. I'm using the mean of the previous two
         #years of production as the WAR value for the calculation
         arb_1_salary_pre_discount = 
           case_when(!is.na(year_5) & (year_4 + year_3) > 0 ~ (year_4 + year_3) / 2 * 1750000 * (1.06) ^ (years_after_draft + years_since_first_5), 
                     TRUE ~ 0),
         arb_2_salary_pre_discount = 
           case_when(!is.na(year_6) ~ (year_5 + year_4) / 2 * 2750000 * (1.06) ^ (years_after_draft + years_since_first_6), TRUE ~ 0),
         arb_2_salary_pre_discount = 
           case_when(!is.na(year_6) & arb_2_salary_pre_discount < arb_1_salary_pre_discount ~ 
                       arb_1_salary_pre_discount,
                     TRUE ~ arb_2_salary_pre_discount),
         arb_3_salary_pre_discount = 
           case_when(!is.na(year_7) ~ (year_6 + year_5) / 2 * 3750000 * (1.06) ^ (years_after_draft + years_since_first_7),
                     TRUE ~ 0),
         arb_3_salary_pre_discount = 
           case_when(!is.na(year_6) & arb_3_salary_pre_discount < arb_2_salary_pre_discount ~ 
                       arb_2_salary_pre_discount,
                     TRUE ~ arb_3_salary_pre_discount),
         #I'm adding it up to get the total salary, with the stipulation that
         #arbitration salary cannot decrease from one year to the next.
         total_salary_in_2024_dollars = total_min_salary_in_2024_dollars + 
           case_when(!is.na(years_since_first_5) ~ arb_1_salary_pre_discount / (1.08) ^ (years_after_draft + years_since_first_5), TRUE ~ 0) +
           case_when(!is.na(years_since_first_6) ~ arb_2_salary_pre_discount / (1.08) ^ (years_after_draft + years_since_first_6), TRUE ~ 0) +
           case_when(!is.na(years_since_first_7) ~ arb_3_salary_pre_discount / (1.08) ^ (years_after_draft + years_since_first_7), TRUE ~ 0),
         #once that's added up, I can calculate the net present value of each player.
         value_in_2024_dollars = on_field_value_in_2024_dollars - total_salary_in_2024_dollars) %>% 
  select(person_full_name, pick_number, year, Signed, value_in_2024_dollars)

signed_player_value_projection <- draft_picks_with_value %>% 
  filter(Signed == "Y")

ggplot(signed_player_value_projection, aes(x = pick_number, y = value_in_2024_dollars, color = year)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

#this looks like it could be modeled with a GAM or with an exponential
#decay function. I'll try both methods.

#using cross validation to calculate the RMSE of the model (final model will use all
#data present). I'm trying two models: a gam with a cubic spline and a NLS model with
#an exponential functional form 

set.seed(100)

signed_player_value_projection <- signed_player_value_projection %>% 
  #splitting into test sets
  mutate(test_group = sample(1:5, nrow(signed_player_value_projection), replace = TRUE)) 

gam_predictions <- tibble()

for (n in 1:5) {
  train_players <- signed_player_value_projection %>% 
    filter(test_group != n)  
  
  test_players <- signed_player_value_projection %>% 
    filter(test_group == n) 
  
  value_of_pick_number <- mgcv::gam(value_in_2024_dollars ~ s(pick_number, bs = "cs"),
                                    data = train_players)
  
  gam_predictions <- test_players %>% 
    bind_cols(tibble(gam_prediction = predict(value_of_pick_number, newdata = test_players))) %>% 
    bind_rows(gam_predictions)
}

test_rmse_gam <- gam_predictions %>% 
  group_by() %>% 
  summarise(rmse = sqrt(mean((gam_prediction - value_in_2024_dollars)^2)))

#test RMSE for GAM = $4.31 million

nls_predictions <- tibble()

for (n in 1:5) {
  train_players <- signed_player_value_projection %>% 
    filter(test_group != n)  
  
  test_players <- signed_player_value_projection %>% 
    filter(test_group == n) 
  
  value_of_pick_number <- nls(value_in_2024_dollars ~ n * exp(r * (pick_number)), 
                              start = list(n = 70000000, r = -.75),
                              data = train_players)
  
  nls_predictions <- test_players %>% 
    bind_cols(tibble(nls_prediction = predict(value_of_pick_number, newdata = test_players))) %>% 
    bind_rows(nls_predictions)
}


test_rmse_nls <- nls_predictions %>% 
  group_by() %>% 
  summarise(rmse = sqrt(mean((nls_prediction - value_in_2024_dollars)^2)))

#test rmse for NLS = $4.33 million

#we can also calculate the test RMSE for the average of both methods

test_rmse_average <- gam_predictions %>% 
  bind_cols(nls_predictions %>% select(nls_prediction)) %>% 
  mutate(mean_prediction = (nls_prediction + gam_prediction) / 2) %>% 
  group_by() %>% 
  summarise(rmse = sqrt(mean((mean_prediction - value_in_2024_dollars)^2)))

#test rmse for the averaged methods = 43164278

#going to stick with the GAM over the NLS or the combination of the two

value_of_pick_number <- mgcv::gam(value_in_2024_dollars ~ s(pick_number, bs = "cs"),
                                  data = signed_player_value_projection)

predict(value_of_pick_number, newdata = tibble(pick_number = 17))

#the model predicts that a 17th overall pick who signs with a team is projected to 
#have a present value of ~$26.7 million. Now, to tackle 2 more things: the signing 
#bonus of that player and the probability of the 17th pick being signed

#the 17th overall pick had a slot value of $4,169,700 in 2023 and $3,794,800 in 2022, an
#increase of ~ 1.10 times. The slot value only went up 1.05 times from 2021 to 2022,
#so my guess is this increase is more of a Covid-bounce back effect than the norm.
#these increases are tied to league revenue, so I'll guess that the bonus pool goes up
#by 1.08 times, meaning that the slot value would be ~ 4500000. The expected value of 
#the signing bonus should just be the slot value, meaning that the surplus value of 
#the 17th pick (assuming you sign the player!) is ~ $22.2 million. Now, I'll work 
#on the situations when you don't sign the player.

#note: this passage was written before MLB announced slot values for the 2024 season,
#the slot value of the 17th pick ended up being $4.53 million! pretty good!

ggplot(all_mlb_draft_data, aes(x = pick_number, y = Signed)) +
  geom_point()

all_mlb_draft_data <- all_mlb_draft_data %>% 
  mutate(Signed = as.numeric(Signed == "Y"))

glm_model <- glm(Signed ~ pick_number, data = all_mlb_draft_data, family = "binomial")

predict(glm_model, newdata = tibble(pick_number = 17))

plogis(3.266589)

#the glm model has the probability of the pick signing at 96.3%
#meaning that value of 17th pick in 2024 = .963 * 22.2 + .037 * value of 18th pick in 2025

predict(value_of_pick_number, newdata = tibble(pick_number = 18))

#the 18th pick in 2025 is valued at 25.4 million. we have to subtract the estimated
#signing bonus and then discount to 2024 dollars

(25400000 - 4020000 * (1.08 ^ 2)) / (1.08)

#so the value is 19.2 million. this is also assuming they sign, but I'm going to 
#make that assumption so I have a stopping point

.963 * 22200000 + .037 * 19200000

#If permitted to do so, a team should pay up to $22.1 million for the 17th
#overall pick, which is the expected surplus value of the selection.  
