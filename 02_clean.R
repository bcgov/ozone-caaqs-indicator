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

##create y, m, d columns and select years for 3 year analysis
ozone_all$year <- as.integer(format(ozone_all$DATE_PST, "%Y"))
ozone_all$month <- as.integer(format(ozone_all$DATE_PST, "%m"))
ozone_all$day <- as.integer(format(ozone_all$DATE_PST, "%d"))

ozone <- ozone_all[ozone_all$year >= min_year & ozone_all$year <= max_year,]


# Format date column using rcaaqs::format_date()
#ozone$DATE_PST <- format_date(ozone$DATE_PST)

## Deal with negative values using rcaaqs::clean_neg()
ozone$RAW_VALUE <- clean_neg(ozone$RAW_VALUE, "ozone")

## Fill in missing hourly readings with NA
ozone <- group_by(ozone, EMS_ID, STATION_NAME)
ozone <- do(ozone, date_fill(., date_col = "DATE_PST", fill_cols = c("EMS_ID", "STATION_NAME"), interval = "1 hour"))


## Summarize sites
site_summary <- ozone %>%
  group_by(EMS_ID, STATION_NAME) %>%
  summarize(min_date = min(DATE_PST), 
            max_date = max(DATE_PST), 
            n_hours = n(), 
            n_readings = length(na.omit(RAW_VALUE)), 
            percent_readings = n_readings / n_hours) %>%
  ungroup() %>%
  arrange(STATION_NAME) %>%
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

## Create Exceptional Evenst (EEs) and Transboundary Flows (TFs) dataframe
## for determining AQMS Air Zone Management Levels
## Two days in 2016 were flagged as exceptional events: 
## July 8 and 9, 2015 for Agassiz Municipal Hall (E293810)
## These days are removed -- as a result of suspected wildfire influence --
## for determining AQMS Air Zone Management Levels

ee.tf.exclusions  <- data.frame(ems_id = "E293810", site = "Agassiz Municipal Hall",
                      start = as.Date("2015-07-08"), end = as.Date("2015-07-10"))

save(ozone, ozone_sites, ee.tf.exclusions, min_year, max_year, file = "tmp/ozone_clean.RData")
