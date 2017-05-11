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


library("dplyr") # data munging
library("lubridate") # for format_date()
library("rcaaqs") # rcaaqs functions, rcaaqs available on GitHub https://github.com/bcgov/rcaaqs

if (!exists("ozone_all")) load("tmp/ozone_raw.RData")

## Set constants
min_year <- 2013
max_year <- 2015

## Select years and removed Chetwynd SW BCOGC MAML data
ozone <- ozone_all[ozone_all$year >= min_year & ozone_all$year <= max_year,]
ozone <- filter(ozone, ems_id != "E299970")

# Format date column using rcaaqs::format_date()
#ozone$date_time <- format_date(ozone$date_time)
ozone$date_time <- as.POSIXct(format_date(ozone$date_time))

## Deal with negative values using rcaaqs::clean_neg()
ozone$value <- clean_neg(ozone$value, "ozone")

## Fill in missing hourly readings with NA, create y, m, d columns
ozone <- group_by(ozone, ems_id, site)
ozone <- do(ozone, date_fill(., date_col = "date_time", fill_cols = c("ems_id", "site"), interval = "1 hour"))
ozone <- ungroup(ozone)
ozone$year <- as.integer(format(ozone$date_time, "%Y"))
ozone$month <- as.integer(format(ozone$date_time, "%m"))
ozone$day <- as.integer(format(ozone$date_time, "%d"))

## Summarize sites
site_summary <- ozone %>%
  group_by(ems_id, site) %>%
  summarize(min_date = min(date_time), 
            max_date = max(date_time), 
            n_hours = n(), 
            n_readings = length(na.omit(value)), 
            percent_readings = n_readings / n_hours) %>%
  ungroup() %>%
  arrange(site) %>%
  as.data.frame()

## Convert stations column names to lowercase:
names(stations) <- tolower(names(stations))

## Subset station file list of ems_sites for just sites analyzed
ozone_sites <- stations[stations$ems_id %in% site_summary$ems_id,] 
#removes "Chetwynd SW BCOGC MAML" from ozone data frame
#"Chetwynd SW BCOGC MAML" station not in stations, only 3 weeks of data from 2014


## Remove duplicates
typos <- c("Chilliwack Airport_", "Elk Falls Dogwood OLD", "Pitt Meadows Meadowlands School_")

ozone_sites <- ozone_sites %>% 
  mutate(station_name = gsub("(\\s+|_)(\\d+|Met|BAM)", "", station_name), 
         station_owner = gsub("[Ss]hared - ", "Shared ", station_owner)) %>% 
  select(ems_id, station_name, latitude, longitude) %>% 
  filter(!station_name %in% typos) %>% 
  group_by(station_name) %>% 
  slice(which.max(latitude))

## Merge site attributes into ozone_sites
ozone_sites <- merge(ozone_sites, site_summary, by = "ems_id")
ozone_sites <- select(ozone_sites, -site)

## Subset ozone data file so only analyze the pertinent sites:
#ozone <- ozone[ozone$ems_id %in% ozone_sites$ems_id, ]

save(ozone, ozone_sites, min_year, max_year, file = "tmp/ozone_clean.RData")
