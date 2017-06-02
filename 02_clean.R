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
min_year <- 2014
max_year <- 2016

## Convert ozone_all column names to lowercase:
names(ozone_all) <- tolower(names(ozone_all))

##create y, m, d columns and select years for 3 year analysis
ozone_all$year <- as.integer(format(ozone_all$date_pst, "%Y"))
ozone_all$month <- as.integer(format(ozone_all$date_pst, "%m"))
ozone_all$day <- as.integer(format(ozone_all$date_pst, "%d"))

ozone <- ozone_all[ozone_all$year >= min_year & ozone_all$year <= max_year,]

# Format date column using rcaaqs::format_date()
#ozone$DATE_PST <- format_date(ozone$DATE_PST)

## Deal with negative values using rcaaqs::clean_neg()
ozone$raw_value <- clean_neg(ozone$raw_value, "ozone")

## Fill in missing hourly readings with NA
ozone <- group_by(ozone, ems_id, station_name)
ozone <- do(ozone, date_fill(., date_col = "date_pst", fill_cols = c("ems_id", "station_name"), interval = "1 hour"))


## Summarize sites
site_summary <- ozone %>%
  group_by(ems_id, station_name) %>%
  summarize(min_date = min(date_pst), 
            max_date = max(date_pst), 
            n_hours = n(), 
            n_readings = length(na.omit(raw_value)), 
            percent_readings = n_readings / n_hours) %>%
  ungroup() %>%
  arrange(station_name) %>%
  as.data.frame()

## Convert stations column names to lowercase:
names(stations) <- tolower(names(stations))

## Subset station file list of ems_sites for just sites analyzed
ozone_sites <- stations[stations$ems_id %in% site_summary$ems_id,]


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

## Create Exceptional Evenst (EEs) and Transboundary Flows (TFs) dataframe
## for determining AQMS Air Zone Management Levels
## Two days in 2016 were flagged as exceptional events: 
## July 8 and 9, 2015 for Agassiz Municipal Hall (E293810)
## These days are removed -- as a result of suspected wildfire influence --
## for determining AQMS Air Zone Management Levels

ee.tf.exclusions  <- data.frame(ems_id = "E293810", site = "Agassiz Municipal Hall",
                      start = as.Date("2015-07-08"), end = as.Date("2015-07-10"))

save(ozone, ozone_sites, ee.tf.exclusions, min_year, max_year, file = "tmp/ozone_clean.RData")
