# Copyright 2015 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


library(dplyr) #data munging
library(lubridate) #wrangling dates
library(rcaaqs) #rcaaqs functions, rcaaqs available on GitHub https://github.com/bcgov/rcaaqs
library(bcmaps) #airzone map
library(readr)
library(stringr)

if (!exists("ozone_raw")) load("tmp/ozone_raw.RData")

## Set constants for 3-year analysis
max_year <- 2018
min_year <- max_year-2

stn_names <- read_csv("data/stn_names_reporting.csv") %>% 
  mutate(ems_id = str_pad(ems_id, 7, "left", "0")) %>% 
  rename(orig_stn_name = station_name)

# Combine two squamish stationsone in 2015 and the other in 2016-17)
# for a complete record
squamish_ems_ids <- c("0310172", "E304570")
combo_squamish_id <- paste(squamish_ems_ids, collapse = "-")

## Clean station data (lowercase column names, remove pseudo-duplicates,
## subset to those stations analysed):
## OLD == closed stns; 
## _60 == meteorological stns;
## Met == meteorological stns using Campbell loggers; 
## BAM == Beta Attenuation Monitoring for PM measurement.
## (note: air pollutant stns mostly using Envidas Ultimate loggers)
## Then join to stn_names to get clean reporting names

select_pattern <- "_60$|Met$|OLD$|BAM$|Squamish Gov't Bldg"
stations_clean <- rename_all(stations, tolower) %>% 
  mutate(ems_id = ifelse(ems_id %in% squamish_ems_ids, 
                         combo_squamish_id, 
                         gsub("_.+$", "", ems_id))) %>% 
  group_by(ems_id) %>%
  filter(n() == 1 | 
           !grepl(select_pattern, station_name) | 
           all(grepl(select_pattern, station_name))) %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  top_n(1, station_name) %>% 
  ungroup() %>% 
  left_join(stn_names, by = "ems_id") %>% 
  mutate(station_name = case_when(ems_id == combo_squamish_id ~ "Squamish", 
                                  is.na(reporting_name) ~ station_name,
                                  TRUE ~ reporting_name))


## Change columns to match rcaaqs defaults, change to lowercase, create year column, filter for 3 year analysis,
## Subtract 1 second so reading is assigned to previous hour using rcaaqs::format_caaqs_dt()
## Deal with negative values using rcaaqs::clean_neg()
ozone_clean_data <- ozone_all %>% 
  filter(!is.na(RAW_VALUE)) %>% #remove padded NAs
  mutate(date_time = format_caaqs_dt(DATE_PST), 
         year = year(date_time),
         month = month(date_time),
         day = day(date_time)) %>% 
  filter(year <= max_year) %>% 
  select(-DATE_PST, -STATION_NAME, -STATION_NAME_FULL) %>% 
  rename_all(tolower) %>% 
  rename(value = raw_value) %>% 
  mutate(value = clean_neg(value, type = "ozone")) %>% 
  distinct() %>% #remove duplicate records if any
## Fill in missing hourly readings with NA using rcaaqs::date_fill()
  group_by(ems_id) %>% 
  do(., date_fill(., date_col = "date_time",
                  fill_cols = c("ems_id", "station_name"),
                  interval = "1 hour")) %>% 
  ungroup()

squamish <- filter(
  ozone_clean_data, (ems_id == squamish_ems_ids[1] & year == 2015) | 
      (ems_id == squamish_ems_ids[2] & year %in% 2016:2017)
  ) %>%
  mutate(ems_id = combo_squamish_id)

ozone_clean_data <- ozone_clean_data %>% 
  filter(!ems_id %in% squamish_ems_ids) %>% 
  bind_rows(squamish) %>% 
  inner_join(select(stations_clean, ems_id, station_name), 
             by = "ems_id")

## Summarize ozone sites in clean ozone dataframe
ozone_site_summary <- ozone_clean_data %>%
  group_by(ems_id, station_name) %>%
  summarize(min_date = min(date_time), 
            max_date = max(date_time), 
            n_hours = n(), 
            n_readings = length(na.omit(value)), 
            percent_readings = n_readings / n_hours) %>%
  ungroup() %>%
  arrange(station_name)

## Assign airzone for each station

#get airzone map (sf object) from bcmaps package
azone <- bcmaps::airzones()

#assign airzones to stations
stations_az <- stations_clean %>% 
  filter(ems_id %in% unique(ozone_site_summary$ems_id), 
         !is.na(latitude), !is.na(longitude)) %>% 
  assign_airzone(airzones = azone,
                 coords = c("longitude", "latitude")) %>% 
  select(ems_id, station_name, city, lat, lon, airzone)

ozone_clean_data <- ozone_clean_data %>% 
  semi_join(stations_az, by = "ems_id")


## Save Clean Data Objects
save(ozone_clean_data, stations_clean, stations_az, ozone_site_summary,
     min_year, max_year, azone, file = "tmp/ozone_clean.RData")

