## This script contains code for preparing a population-normalized dataset
## for subsequent geospatial analysis via Tableau.

library(tidyverse)
library(janitor)
library(lubridate)

merge_df <- read_csv("all-police-stops.csv")
main_path <- paste0(getwd(), "/raw-data/")

# Summarizing a dataframe containing the raw number of police stops per precinct
precinct_stops <- merge_df %>% 
  filter(!is.na(stop_location_precinct), stop_location_precinct != "#NULL!",
         stop_location_precinct != "208760") %>% 
  mutate(stop_location_precinct = as.integer(stop_location_precinct)) %>% 
  group_by(stop_location_precinct) %>% 
  summarize(total_stops = n())

# Loading in precinct population data
nyc_pop <- read_csv(paste0(main_path,"nyc_precinct_2020pop.csv")) %>% 
  select(precinct, P1_001N) %>% 
  rename(population = P1_001N)

# Calculating stops per 1000 residents in each police precinct
precinct_stops_by_pop <- inner_join(precinct_stops, nyc_pop, by = c("stop_location_precinct" = "precinct")) %>% 
  mutate(stops_per_1000 = (total_stops/population) * 1000)

write_csv(precinct_stops_by_pop, "precinct_stops_by_pop.csv")
                        