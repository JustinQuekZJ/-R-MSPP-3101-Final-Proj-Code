## This script contains code for all visualizations used in the final presentation.

library(tidyverse)
library(janitor)
library(lubridate)

merge_df <- read_csv("all-police-stops.csv")

##### VISUALIZATION #####

## 1. Visualizing stops by year
stops_per_year <- merge_df %>% 
  group_by(year) %>% 
  summarize(total_stops = n()) ##no. of stops per year

stops_year_viz <- ggplot(stops_per_year)+
  geom_line(mapping = aes(x = year, y = total_stops), color = "#0f018c", size=1)+
  geom_vline(xintercept = c(2014, 2021), linetype = "dashed", color = "darkgrey")+
  scale_y_continuous(
    limits = c(0, 50000),
    labels = scales::unit_format(scale = 1/1000, unit = "K")
  )+
  scale_x_continuous(
    breaks = seq(2014, 2023, by = 1)
  )+
  labs(
    title = "Police stops have decreased since the start of the De Blasio Administration",
    subtitle = "No. of police stops year-on-year",
    y = "No. of Police Stops",
    x = NULL,
    caption = "Source: https://www.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page"
  )+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(size = 0.7),  # Default grid lines
    panel.grid.minor = element_line(size = 0.25)  # Default minor grid lines
  )

stops_year_viz

ggsave("[1] stops_year_viz.png", stops_year_viz, width = 10, height = 6)

## 2. Visualizing stops by year, broken down by race
race_stops_per_year <- merge_df %>% 
  group_by(year, updated_race) %>% 
  summarize(total_stops = n()) ##no. of stops per year

race_stops_year_viz <- ggplot(race_stops_per_year)+
  aes(x = year, y = total_stops, fill = reorder(updated_race, -total_stops))+
  geom_area(position = "stack", alpha = 0.8) +
  geom_vline(xintercept = c(2014, 2021), linetype = "dashed", color = "darkgrey")+
  scale_y_continuous(
    limits = c(0, 50000),
    labels = scales::unit_format(scale = 1/1000, unit = "K")
  )+
  scale_x_continuous(
    breaks = seq(2014, 2023, by = 1)
  )+
  scale_fill_manual(values = c("BLACK" = "#fb8072","HISPANIC" = "#80b1d3","WHITE" = "#fdb462","ASIAN / PACIFIC ISLANDER" = "#8dd3c7",
                               "MIDDLE EASTERN/SOUTHWEST ASIAN" = "#ffffb3","AMERICAN INDIAN/ALASKAN NATIVE" = "#bebada"))+
  labs(
    title = "Blacks and Hispanics make up >80% of police stops",
    subtitle = "No. of police stops year-on-year",
    y = "No. of Police Stops",
    x = NULL,
    fill = NULL,
    caption = "Source: https://www.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page"
  )+
  theme(
    legend.key.size = unit(0.3, "cm"), 
    legend.spacing.y = unit(0.2, "cm",),
    panel.grid.major = element_line(size = 0.7),  
    panel.grid.minor = element_line(size = 0.25),
    legend.position = "none"
  )+
  theme_minimal()
  
race_stops_year_viz

ggsave("[2] race_stops_year_viz.png", race_stops_year_viz, width = 10, height = 6)


## 3. Visualizing stops vs population composition per race
merge_df_2 <- merge_df %>% 
  mutate(updated_race = case_when(
    updated_race == "ASIAN / PACIFIC ISLANDER" ~ "ASIAN",
    updated_race == "MIDDLE EASTERN/SOUTHWEST ASIAN" ~ "ASIAN",
    updated_race == "AMERICAN INDIAN/ALASKAN NATIVE" ~ "AMERICAN INDIAN",
    TRUE ~ updated_race
  ))

race_prop <- merge_df_2 %>% 
  group_by(updated_race) %>% 
  summarize(total_stops = n()) %>% 
  mutate(prop_stops = total_stops/nrow(merge_df_2)) %>% 
  mutate(prop_pop = c(0.0018, 0.148, 0.203, 0.291, 0.305))

# Convert to long format to plot side-by-side bar plot
race_prop_long <- race_prop %>%
  pivot_longer(cols = c(prop_stops, prop_pop), names_to = "measure", values_to = "value")

race_prop_long_viz <- ggplot(race_prop_long, aes(x = updated_race, y = value, fill = measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6)+
  scale_fill_manual(name = NULL, 
                    values = c("prop_stops" = "#02077d", "prop_pop" = "#595959"),
                    labels = c("prop_stops" = "Police Stops", "prop_pop" = "Population"))+
  labs(
    title = "Blacks are overrepresented in police stops",
    subtitle = "Comparison of stops vs population proportions by race",
    y = "Proportion",
    x = NULL,
    caption = "Source: https://www.census.gov/quickfacts/fact/table/newyorkcitynewyork"
  )+
  theme_minimal()+
  theme(
    panel.grid.major = element_line(size = 0.6),
    panel.grid.minor = element_line(size = 0.6)
  )

race_prop_long_viz

ggsave("[3] race_prop_long_viz.png", race_prop_long_viz, width = 10, height = 6)


## 4. Visualizing arrests per race

arrests_by_race <- merge_df_2 %>% 
  group_by(updated_race, suspect_arrested_flag) %>% 
  summarize(total_stops = n())

arrest_prop_viz <- ggplot(arrests_by_race, aes(x = updated_race, y = total_stops, fill = suspect_arrested_flag))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65)+
  scale_fill_manual(name = "Arrested?", 
                    values = c("Y" = "#02077d", "N" = "#595959"),
                    labels = c("Y" = "Yes", "N" = "No"))+
  scale_y_continuous(
    labels = scales::unit_format(scale = 1/1000, unit = "K")
  )+
  labs(
    title = "Arrest rates similar across races but more Blacks stopped",
    subtitle = "Comparison of arrests by race",
    y = "No. of stops",
    x = NULL,
    caption = "Source: https://www.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page"
  )+
  theme(
    legend.key.size = unit(0.3, "cm"), 
    legend.spacing.y = unit(0.2, "cm",),
    panel.grid.major = element_line(size = 0.7),  
    panel.grid.minor = element_line(size = 0.25),
    legend.position = "none"
  )+
  theme_minimal()

arrest_prop_viz

ggsave("[4] arrest_prop_viz.png", arrest_prop_viz, width = 10, height = 6)


## 5. Comparison with major crimes

maj_crimes <- read_csv("major-crimes-nyc.csv") %>% 
  clean_names() %>% 
  mutate(total = murder+rape+felony_sex+robbery+fel_assault+g_larceny+shooting)

maj_crimes_summarized <- maj_crimes %>% 
  group_by(race) %>% 
  summarize(total_crimes = sum(total)) %>% 
  mutate(prop_crimes = total_crimes/sum(total_crimes))

stop_vs_crime <- inner_join(race_prop, maj_crimes_summarized, 
                            by = c("updated_race" = "race"))

stop_vs_crime_long <- stop_vs_crime %>%
  pivot_longer(cols = c(prop_crimes, prop_stops), names_to = "measure", values_to = "value")

stop_vs_crime_viz <- ggplot(stop_vs_crime_long, aes(x = updated_race, y = value, fill = measure))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6)+
  scale_fill_manual(name = NULL, 
                    values = c("prop_stops" = "#02077d", "prop_crimes" = "#a81d02"),
                    labels = c("prop_stops" = "Police Stops", "prop_crimes" = "Major Crime"))+
  labs(
    title = "Stop proportions are similar to major crime proportions",
    subtitle = "Comparison of stops vs major crime proportions by race",
    y = "Proportion",
    x = NULL,
    caption = "Source: https://www.nyc.gov/site/nypd/stats/reports-analysis/crime-enf.page"
  )+
  theme(
    legend.key.size = unit(0.3, "cm"), 
    legend.spacing.y = unit(0.2, "cm",),
    panel.grid.major = element_line(size = 0.6),  
    panel.grid.minor = element_line(size = 0.6),
    legend.position = "none"
  )+
  theme_minimal()

stop_vs_crime_viz

ggsave("[5] stop_vs_crime_viz.png", stop_vs_crime_viz, width = 10, height = 6)
