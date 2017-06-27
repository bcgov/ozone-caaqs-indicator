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
library("rcaaqs") # rcaaqs functions, rcaaqs available on GitHub https://github.com/bcgov/rcaaqs

if (!exists("ozone_all")) load("tmp/ozone_raw.RData")

## Set constants
min_year <- 2014
max_year <- 2016

## Convert ozone_all column names to lowercase
names(ozone_all) <- tolower(names(ozone_all))

## Change column names top `rcaaqs` defaults
colnames(ozone_all)[which(names(ozone_all) == "date_pst")] <- "date_time"
colnames(ozone_all)[which(names(ozone_all) == "raw_value")] <- "value"

## Subtract 1 second so reading is assigned to previous hour using rcaaqs::format_caaqs_dt()
ozone_all$date_time <- format_caaqs_dt(ozone_all$date_time)

## Create y, m, d columns and select years for 3 year analysis
ozone_all$year <- as.integer(format(ozone_all$date_time, "%Y"))
ozone_all$month <- as.integer(format(ozone_all$date_time, "%m"))
ozone_all$day <- as.integer(format(ozone_all$date_time, "%d"))

ozone <- ozone_all[ozone_all$year >= min_year & ozone_all$year <= max_year,]

## Deal with negative values using rcaaqs::clean_neg()
ozone$value <- clean_neg(ozone$value, "ozone")

## Fill in missing hourly readings with NA
ozone <- group_by(ozone, ems_id, station_name)
ozone <- do(ozone, date_fill(., date_col = "date_time", fill_cols = c("ems_id", "station_name"), interval = "1 hour"))

## Summarize sites in clean dataframe
site_summary <- ozone %>%
  group_by(ems_id, station_name) %>%
  summarize(min_date = min(date_time), 
            max_date = max(date_time), 
            n_hours = n(), 
            n_readings = length(na.omit(value)), 
            percent_readings = n_readings / n_hours) %>%
  ungroup() %>%
  arrange(station_name) %>%
  as.data.frame()

## Convert stations column names to lowercase
names(stations) <- tolower(names(stations))

## Subset station file list of ems_sites for just sites analyzed in clean dataframe
ozone_sites <- stations[stations$ems_id %in% site_summary$ems_id,]

## Remove duplicates from ozone_sites
typos <- c("Pitt Meadows Meadowlands School")

ozone_sites <- ozone_sites %>% 
  mutate(station_name = gsub("(\\s+|_)(\\d+|Met|BAM|OLD)", "", station_name), 
         station_owner = gsub("[Ss]hared - ", "Shared ", station_owner)) %>% 
  select(ems_id, station_name, latitude, longitude) %>% 
  filter(!station_name %in% typos) %>% 
  group_by(station_name) %>% 
  slice(which.max(latitude))

## Merge site attributes from site_summary into ozone_sites
ozone_sites <- merge(ozone_sites, site_summary, by = "ems_id")
ozone_sites <- select(ozone_sites, -station_name.y)
colnames(ozone_sites)[which(names(ozone_sites) == "station_name.x")] <- "station_name"


## Create Exceptional Evenst (EEs) and Transboundary Flows (TFs) dataframe
## for determining AQMS Air Zone Management Levels
## Two days in 2015 were flagged as exceptional events: 
## July 8 and 9, 2015 for Agassiz Municipal Hall (E293810)
## These days are removed -- as a result of suspected wildfire influence --
## for determining AQMS Air Zone Management Levels

ee.tf.exclusions  <- data.frame(ems_id = "E293810", station_name = "Agassiz Municipal Hall",
                      start = as.Date("2015-07-08"), end = as.Date("2015-07-10"))

save(ozone, ozone_sites, ee.tf.exclusions, min_year, max_year, file = "tmp/ozone_clean.RData")
