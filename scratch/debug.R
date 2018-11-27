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

library(rcaaqs) #rcaaqs functions
library(dplyr) #data munging

## Load the tmp file if ozone doesn't exist
if (!exists("o3_caaqs_all")) load("tmp/analysed.RData")

issues <- c("E229797", "E206271")

o3_issues_yearly <- extract_yearly(o3_caaqs_all) %>% 
  filter(ems_id %in% issues)

o3_caaqs_yearly <- extract_yearly(o3_caaqs_all)

write.csv(o3_caaqs_yearly, "tmp/ozone_yearly_caaqs_2015-2017.csv", row.names = FALSE)

ozone_full <- ozone_all %>%
  rename(site = STATION_NAME) %>% 
  mutate(date = format_caaqs_dt(DATE_PST)) %>% 
  rename_all(tolower) %>% 
  group_by(ems_id, site) %>%
  do(date_fill(., date_col = "date", fill_cols = c("ems_id", "site"),  interval = "1 hour")) %>%
  mutate(year = year(floor_date(date, "year")),
         month = month(floor_date(date, "month")),
         day = day(floor_date(date, "day"))) %>%
  ungroup()

########
#########

library(rcaaqs)
library(dplyr)
library(bcmaps)

ozone <- o3_caaqs(o3_sample_data, 
                  by = c("ems_id", "site"))

exclusions <- get_daily(ozone) %>% 
  filter(max8hr > 55) %>% 
  select(ems_id, site, date)

ozone_caaqs <- caaqs_management(ozone, exclude_df = exclusions, exclude_df_dt = "date")

o3 <- get_caaqs(ozone_caaqs)

az <- bcmaps::airzones()

stations_file <- system.file("air_stations_2018-11-07.csv", package = "rcaaqs")

stations <- readr::read_csv(stations_file, na = "N/A") %>%  # Read csv file
  rename_all(tolower) %>%                         # Rename columns to lower case
  filter(ems_id %in% unique(o3$ems_id)) %>%       # Filter to ids in our o3 data
  select(ems_id, latitude, longitude) %>%         # Select only the important columns
  mutate(latitude = as.numeric(latitude),         # Transform lat and lon to numeric
         longitude = as.numeric(longitude)) %>%
  distinct()  

o3_results <- left_join(o3, stations, by = "ems_id")

ozone_stn_az <- assign_airzone(o3_results, airzones = az,
                               coords = c("longitude", "latitude"))

## Get airzone caaqs metric
ozone_az <- airzone_metric(ozone_stn_az)


foo <- ozone_stn_az %>% 
  rename("station_id" = "ems_id")
foo_az <- airzone_metric(foo, station_id = "station_id")

