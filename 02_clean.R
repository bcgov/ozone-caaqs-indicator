# Copyright 2015 Province of British Columbia
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

source("00_setup.R")

library("readr")
library("dplyr")
library("tidyr")
library("purrr")
library("forcats")
library("lubridate")
library("stringr")
library("ggplot2")
library("patchwork")

library("rcaaqs")
library("bcmaps")

library("janitor")
library("assertr")


options("rcaaqs.timezone" = "Etc/GMT+8")

# Load Data ---------------------------------
stations <- read_csv("data/raw/caaqs_stationlist.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  rename(lon = long)

ozone <- read_rds("data/raw/ozone_caaqs.Rds") %>%
  as_tibble()

az <- airzones()


# Clean Stations -------------------------------------------------------------

# - lowercase column names
# - subset to those stations analysed

stations_clean <- stations %>%
  
  # Look for problems
  assert(within_bounds(-90, 90), lat) %>%
  assert(within_bounds(-180, 180), lon) %>%
  
  # Use airzones from bcmaps
  select(-airzone) %>%
  assign_airzone(airzones = az, 
                 station_id = "site", 
                 coords = c("lon", "lat")) %>%
  assert(not_na, airzone) %>%
  
  # Only keep stations for ozone
  filter(ozone) %>%
  select(site, region, airzone, lat, lon)


# Clean ozone -----------------------------------------------------------------

## Overall clean -------------
ozone_clean <- ozone %>% 
  
  # Format dates, only keep dates in range
  mutate(date_time = format_caaqs_dt(date_time), 
         year = year(date_time)) %>% 
  filter(year <= rep_year) %>% 
  
  # Clean negative values
  mutate(value = clean_neg(value, type = "ozone")) %>% 
  
  # Fill dates
  nest(data = -site) %>%
  mutate(data = map(
    data, ~date_fill(., date_col = "date_time", interval = "1 hour"))) %>%
  unnest(data)


# Check timeseries problems -----------------------
# - Check for missing/extra observations

t <- ozone_clean %>%
  nest(ts = -site) %>%
  mutate(n_distinct = map_int(ts, ~n_distinct(.$date_time)),
         n = map_int(ts, nrow),
         n_expect = map_dbl(ts, ~as.numeric(difftime(max(.$date_time), 
                                                     min(.$date_time), 
                                                     units = "hours")))) %>%
  filter(n_expect != n - 1, 
         n_distinct != n) %>%
  verify(nrow(.) == 0)

# None!


# Last details -----------------------

# Only keep stations with data
stations_clean <- semi_join(stations_clean, ozone_clean, by = "site")

# Write data ------------------------------
write_rds(stations_clean, "data/datasets/stations_clean.rds")
write_rds(ozone_clean, "data/datasets/ozone_clean.rds", compress = "gz")
