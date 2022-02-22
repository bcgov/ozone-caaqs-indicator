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

## Stations

stations_old <- bcdc_get_data('9b7a9e74-9274-4f97-be81-ce4ee475077d', 
                              resource = 'f8923733-6dfe-47b7-bc57-aaa8f176c67c') %>%
  rename(latitude = lat, longitude = lon) %>%
  select(-based_on_incomplete)

stations_summary <- read_rds("data/datasets/ozone_results.rds") %>%
  select(-c(flag_yearly_incomplete, flag_two_of_three_years,flag_daily_incomplete)) %>%
  rename(station_name = site, latitude = lat, longitude = lon) %>%
  mutate(caaqs_ambient = as.character(caaqs_ambient),
         mgmt_level = as.character(mgmt_level))

setdiff(names(stations_old), names(stations_summary))

bind_rows(stations_old, stations_summary) %>%
  arrange(caaqs_year) %>% 
  write_csv("out/databc/ozone_site_summary.csv", na = "")


## Ambient CAAQs by airzone 

az_ambient_old <- bcdc_get_data('9b7a9e74-9274-4f97-be81-ce4ee475077d', 
                                resource = '00b1af32-499e-49a4-ba91-6b8b331a6629')

az_ambient <- read_rds("data/datasets/az_ambient.rds") %>%
  select(c(airzone, metric, n_years = n_years_ambient, 
           metric_value_ambient, metric_value_mgmt, mgmt_level,
           caaqs_ambient, rep_stn_id_ambient, rep_stn_id_mgmt,
           excluded)) %>%
  #rename_with(.cols = ends_with("_ambient"), ~ str_remove(., "_ambient")) %>%
  mutate(caaqs_ambient = as.character(caaqs_ambient), 
         caaqs_year = .env$rep_year,
         mgmt_level = as.character(mgmt_level))

# Check for additional or missing columns
setdiff(names(az_ambient_old), names(az_ambient))

bind_rows(az_ambient_old, az_ambient) %>%
  arrange(caaqs_year) %>% 
  write_csv("out/databc/ozone_airzone_summary.csv", na = "")
