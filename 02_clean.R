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


library("dplyr") #data munging
library("lubridate") #wrangling dates
library("rcaaqs") #rcaaqs functions, rcaaqs available on GitHub https://github.com/bcgov/rcaaqs

if (!exists("ozone_raw")) load("tmp/ozone_raw.RData")

## Set constants for 3-year analysis
min_year <- 2015
max_year <- 2017

## Change columns to match rcaaqs defaults, change to lowercase, create year column, filter for 3 year analysis,
## Subtract 1 second so reading is assigned to previous hour using rcaaqs::format_caaqs_dt(),
## Deal with negative values using rcaaqs::clean_neg()
ozone <- mutate(ozone_all, 
               date_time = format_caaqs_dt(DATE_PST), 
               year = year(date_time)) %>% 
  filter(year >= min_year, year <= max_year) %>% 
  select(-DATE_PST) %>% 
  rename_all(tolower) %>% 
  rename(value = raw_value) %>% 
  mutate(value = clean_neg(value, type = "ozone"))


## Fill in missing hourly readings with NA using rcaaaqs::date_fill()
ozone <- group_by(ozone, ems_id, station_name)
ozone <- do(ozone, date_fill(., date_col = "date_time", fill_cols = c("ems_id", "station_name"), interval = "1 hour"))

## Summarize ozone sites in clean ozone dataframe
ozone_site_summary <- ozone %>%
  group_by(ems_id, station_name) %>%
  summarize(min_date = min(date_time), 
            max_date = max(date_time), 
            n_hours = n(), 
            n_readings = length(na.omit(value)), 
            percent_readings = n_readings / n_hours) %>%
  ungroup() %>%
  arrange(station_name) %>%
  as.data.frame()

## Clean station data - lowercase column names, remove pseudo-duplicates, subset to those stations analysed
stations_clean <- rename_all(stations, tolower) %>% 
  mutate(ems_id = gsub("-[0-9]$", "", ems_id)) %>%
  group_by(ems_id) %>%
  filter(n() == 1 | 
           !grepl("_60$|Met$|OLD$", station_name)) %>% 
  filter(ems_id %in% unique(ozone_site_summary$ems_id))


## Create Exceptional Evenst (EEs) and Transboundary Flows (TFs) dataframe for determining AQMS Air Zone Management Levels
## Two days in 2015 were flagged as exceptional events: 
## July 8 and 9, 2015 for Agassiz Municipal Hall (E293810)
## These days are removed -- as a result of suspected wildfire influence -- for determining AQMS Air Zone Management Levels

ee.tf.exclusions  <- data.frame(ems_id = "E293810", station_name = "Agassiz Municipal Hall",
                      start = as.Date("2015-07-08"), end = as.Date("2015-07-10"))


save(ozone, stations_clean, ozone_site_summary, ee.tf.exclusions, min_year, max_year, file = "tmp/ozone_clean.RData")
