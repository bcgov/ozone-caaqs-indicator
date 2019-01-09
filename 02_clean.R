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

if (!exists("ozone_raw")) load("tmp/ozone_raw.RData")


## Set constants for 3-year analysis
min_year <- 2015
max_year <- 2017


## Change columns to match rcaaqs defaults, change to lowercase, create year column, filter for 3 year analysis,
## Subtract 1 second so reading is assigned to previous hour using rcaaqs::format_caaqs_dt()
## Deal with negative values using rcaaqs::clean_neg()
ozone_3yrs <- ozone_all %>% 
  select(-DATE, -TIME) %>% 
  mutate(date_time = format_caaqs_dt(DATE_PST), 
         year = year(date_time),
         month = month(date_time),
         day = day(date_time)) %>% 
  filter(year >= min_year, year <= max_year) %>% 
  select(-DATE_PST) %>% 
  rename_all(tolower) %>% 
  rename(value = raw_value) %>% 
  mutate(value = clean_neg(value, type = "ozone")) %>% 
  distinct() #remove duplicate records


## Fill in missing hourly readings with NA using rcaaqs::date_fill()
ozone_clean_data <- ozone_3yrs %>% 
  group_by(ems_id, station_name) %>% 
  do(., date_fill(., date_col = "date_time",
                  fill_cols = c("ems_id", "station_name"),
                  interval = "1 hour")) %>% 
  ungroup()


## Summarize ozone sites in clean ozone dataframe
ozone_site_summary <- ozone_clean_data %>%
  group_by(ems_id, station_name) %>%
  summarize(min_date = min(date_time), 
            max_date = max(date_time), 
            n_hours = n(), 
            n_readings = length(na.omit(value)), 
            percent_readings = n_readings / n_hours) %>%
  ungroup() %>%
  arrange(station_name) %>%
  as.data.frame()


## Clean station data (lowercase column names, remove pseudo-duplicates,
## subset to those stations analysed):
## OLD == closed stns; 
## _60 == meteorological stns;
## Met == meteorological stns using Campbell loggers; 
## BAM == Beta Attenuation Monitoring for PM measurement.
## (note: air pollutant stns mostly using Envidas Ultimate loggers)

stations_clean <- rename_all(stations, tolower) %>% 
  mutate(ems_id = gsub("-[0-9]$", "", ems_id)) %>%
  group_by(ems_id) %>%
  filter(!grepl("_60$|Met$|OLD$|_Old$|(Met)|BAM$", station_name)) %>%
  filter(n() == 1) %>%
  filter(ems_id %in% unique(ozone_site_summary$ems_id)) 


## Assign airzone for each station

#get airzone map (sf object) from bcmaps package
azone <- bcmaps::airzones()

#assign airzones to stations
stations_az <- assign_airzone(stations_clean, airzones = azone,
                              coords = c("longitude", "latitude")) %>% 
  select(ems_id, station_name, city, lat, lon, airzone)



## Save Clean Data Objects
save(ozone_clean_data, stations_az, ozone_site_summary,
     min_year, max_year, azone, file = "tmp/ozone_clean.RData")


# ## TEMP FIX for Smithers Station EMS_ID
# library(dplyr)
# ozone_all <- ozone_all %>% 
#   mutate(EMS_ID = case_when(EMS_ID == "E206589_1" ~ "E206589",
#                             TRUE ~ EMS_ID))