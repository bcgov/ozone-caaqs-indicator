# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

# DataBC 

# Create output files for the BC data catalogue, combining with previous years' data


source("00_setup.R")

library("readr")
library("dplyr")
library("stringr")

library("bcdata")

# Join old and new ------------------------

## Stations ----------------------------------

stations_old <- bcdc_get_data(
  '9b7a9e74-9274-4f97-be81-ce4ee475077d', 
  resource = 'f8923733-6dfe-47b7-bc57-aaa8f176c67c') %>%
  rename(latitude = lat, longitude = lon) %>%
  select(-based_on_incomplete, -city) %>%
  rename(station_id = ems_id) 

stations_summary <- read_rds("data/datasets/ozone_results.rds") %>%
  select(-flag_yearly_incomplete, -flag_two_of_three_years,
         -flag_daily_incomplete, -region) %>%
  rename(station_name = site, latitude = lat, longitude = lon) %>%
  mutate(caaqs_ambient = as.character(caaqs_ambient),
         mgmt_level = as.character(mgmt_level),
         station_id = station_name)

setdiff(names(stations_old), names(stations_summary))
setdiff(names(stations_summary), names(stations_old))

bind_rows(stations_old, stations_summary) %>%
  select(caaqs_year, airzone, station_name, station_id, 
         latitude, longitude, metric, n_years, min_year, max_year, 
         metric_value_ambient, caaqs_ambient,
         excluded, metric_value_mgmt, mgmt_level) %>%
  arrange(caaqs_year, airzone, station_name) %>% 
  write_csv("out/databc/ozone_stations_summary.csv", na = "")


## Airzones -------------------

airzones_old <- bcdc_get_data(
  '9b7a9e74-9274-4f97-be81-ce4ee475077d', 
  resource = '00b1af32-499e-49a4-ba91-6b8b331a6629') %>%
  mutate(metric = "o3",
         n_years_mgmt = n_years,
         n_years_ambient = n_years,
         rep_stn_name_ambient = NA,
         rep_stn_name_mgmt = NA) %>%
  select(-n_years)

airzones_summary <- read_rds("data/datasets/az_ambient.rds") %>%
  mutate(rep_stn_name_ambient = rep_stn_id_ambient,
         rep_stn_name_mgmt = rep_stn_id_mgmt, 
         caaqs_ambient = as.character(caaqs_ambient), 
         mgmt_level = as.character(mgmt_level),
         caaqs_year = .env$rep_year)

# Check for additional or missing columns
setdiff(names(airzones_old), names(airzones_summary))
setdiff(names(airzones_summary), names(airzones_old))

bind_rows(airzones_old, airzones_summary) %>%
  select(caaqs_year, airzone, metric, 
         n_years_ambient, metric_value_ambient, caaqs_ambient, 
         rep_stn_name_ambient, rep_stn_id_ambient,
         excluded, n_years_mgmt, metric_value_mgmt, mgmt_level, 
         rep_stn_name_mgmt, rep_stn_id_mgmt) %>%
  arrange(caaqs_year, airzone) %>% 
  write_csv("out/databc/ozone_airzones_summary.csv", na = "")
