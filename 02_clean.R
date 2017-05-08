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

#library("magrittr")
library("dplyr") # data munging
library("lubridate") # for format_date()
library("rcaaqs") # rcaaqs functions, rcaaqs available on GitHub https://github.com/bcgov/rcaaqs

if (!exists("ozone")) load("tmp/ozone_raw.RData")

## Set constants
min_year <- 2013
max_year <- 2015

ozone <- ozone_all[ozone_all$year >= min_year & ozone_all$year <= max_year,]

# Format date column
ozone$date_time <- format_date(ozone$date_time)

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

## Subset ems_sites for just sites analyzed
ozone_sites <- stations[stations$ems_id %in% site_summary$ems_id,]

## Remove duplicates and clean up
typos <- c("Central Saanich Stellys CrossRoad", "Victoria Royal Roads", 
           "Agassiz Municiapl Hall", "Langford Lakewood School", 
           "Langford Lakewood  School")

ozone_sites <- ozone_sites %>% 
  mutate(stationname = gsub("(\\s+|_)(\\d+|Met|BAM)", "", stationname), 
         owner = gsub("[Ss]hared - ", "Shared ", owner)) %>% 
  select(ems_id, stationname, latitude, longitude, display_name) %>% 
  filter(!stationname %in% typos) %>% 
  group_by(stationname) %>% 
  slice(which.max(latitude))

## Merge site attributes into ozone_sites
ozone_sites <- merge(ozone_sites, site_summary, by = "ems_id")

## Subset to only use those with data up to at least 2012
ozone_sites <- ozone_sites[ozone_sites$max_date >= as.POSIXct("2012-01-01"), ]

## Subset ozone so only analyze the pertinent sites:
ozone <- ozone[ozone$ems_id %in% ozone_sites$ems_id, ]

save(ozone, ozone_sites, min_year, max_year, file = "tmp/ozone_clean.RData")
