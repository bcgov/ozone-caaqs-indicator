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

library("openair")
library("rcaaqs")
library("dplyr")
library("lubridate")
library("magrittr")
library("sp")
library("rgdal")
library("bcmaps")

# Load the tmp file if ozone doesn't exist (need inherits = FALSE as "ozone" 
# exists in 'maps' package on which 'openair' depends)
if (!exists("ozone", inherits = FALSE)) load("tmp/ozone_clean.RData")

## Set constants
o3_standard <- 63

## Calculate 8 hr rolling average, right-justified, with at least 
## 6/8 valid observations (data.thresh = 75)
ozone_8hr_roll <- ozone %>%
  rename(date = date_time) %>% 
  group_by(ems_id, site) %>% 
  do(rollingMean(., pollutant = "value", width = 8, data.thresh = 75, 
                 new.name = "rolling8", align = "right"))

## Calculate the daily maximum 8hr rolling average, plus data completeness stuff
daily_8hr_roll_max <- ozone_8hr_roll %>%
  group_by(ems_id, site, year, 
           date = as.Date(date, tz = attr(ozone$date, "tz"))) %>%
  summarise(nReadings = length(na.omit(rolling8)), 
            max8hr = max(rolling8, na.rm = TRUE), 
            exceed = max8hr > o3_standard, # 63 ppb is the CAAQ standard
            valid = nReadings >= 18, # 75% (18) valid 8-hr rolling averages per day
            flag = exceed & !valid) # Flag for data incomplete, but used

## Calculate the annual 4th highest daily maximum 8hr rolling average
annual_4th_daily_max <- daily_8hr_roll_max %>%
  filter(valid | flag) %>% # Use valid 8hr max OR those flagged
  group_by(ems_id, site, year) %>%
  mutate(rank = rank(max8hr, na.last = FALSE)) %>%
  arrange(year, desc(rank)) %>% # sort by decreasing rank
  slice(4L) %>%  # choose 4th highest reading
  select(-rank)

## Determine valid site-year combos to use in 3yr average calculation
## (from CAAQs rules: pg 16))
site_year_check <- daily_8hr_roll_max %>%
  group_by(ems_id, site, year) %>%
  filter(quarter(date) %in% c(2,3)) %>% #Get only dates in Q2 & Q3
  summarise(poss_days_q2_3 = 182, # 182 days in Q2 + Q3
            n_valid_q2_3 = length(which((valid | flag) & !is.na(max8hr))), 
            percent_valid_q2_3 = n_valid_q2_3 / poss_days_q2_3) %>%
  ungroup()

## Use only annual 4th highest max hr rolling average where the 
## site-year combo is valid (i.e., enough readings in a year)
annual_4th_daily_max <- merge(annual_4th_daily_max, site_year_check[,-2], 
                              by = c("ems_id", "year")) %>%
  filter(percent_valid_q2_3 >= .75 | exceed)

## Calculate 3 yr average
three_yr_avg <- annual_4th_daily_max %>%
  group_by(ems_id, site) %>%
  summarise(caaq_year_min = min(year), 
            caaq_year_max = max(year), 
            caaq_nYears = n(), 
            based_on_incomplete = any(percent_valid_q2_3 < 0.75 | flag),
            caaq_metric = ifelse(caaq_nYears < 2, NA_real_, round(mean(max8hr)))) %>%
  ungroup() %>%
  mutate(caaq_status = ifelse(caaq_metric <= o3_standard, "Achieved", "Not Achieved"), 
         caaq_level = cut_management(caaq_metric, "o3", output = "labels", drop_na = TRUE), 
         caaq_category_html = cut_management(caaq_metric, "o3", output = "breaks_h", drop_na = TRUE),
         caaq_category_u = cut_management(caaq_metric, "o3", output = "breaks_u", drop_na = TRUE))

## Add three year average values to ozone sites
ozone_sites <- three_yr_avg %>% 
  select(-site) %>%
  filter(!is.na(caaq_metric)) %>% 
  merge(ozone_sites, ., by = "ems_id")

## Do mapping
## Convert ozone_sites to SpatialPointsDataFrame and put in the same projection
## as the airzone map:
coordinates(ozone_sites) <- ~longitude + latitude
proj4string(ozone_sites) <- CRS("+init=epsg:4617")
ozone_sites <- spTransform(ozone_sites, CRS(proj4string(airzone_map)))

## Add regional districts to ozone_sites
regional_districts_analysis <- spTransform(regional_districts_analysis, 
                                           CRS(proj4string(airzone_map)))
ozone_sites$regional_district <- over(ozone_sites, regional_districts_analysis)[[1]]

## Get airzone information into ozone_sites
ozone_sites$Airzone <- over(ozone_sites, airzone_map)[["Airzone"]]

## Get CAAQ value and achievement status into airzones.
az_metric <- airzone_metric(ozone_sites@data, n_years = "caaq_nYears", 
                 az = "Airzone", val = "caaq_metric", 
                 keep = c(rep_station_id = "ems_id", 
                          rep_station_name = "display_name", 
                          nyears = "caaq_nYears"))
airzone_map <- sp::merge(airzone_map, as.data.frame(az_metric), by = "Airzone")

airzone_map$caaq_status <- cut_achievement(airzone_map$caaq_metric, "o3", output = "labels")
airzone_map$caaq_level <- cut_management(airzone_map$caaq_metric, "o3", output = "labels")
airzone_map$caaq_category_html <- cut_management(airzone_map$caaq_metric, "o3", output = "breaks_h")
airzone_map$caaq_category_u <- cut_management(airzone_map$caaq_metric, "o3", output = "breaks_u")

dir.create("tmp", showWarnings = FALSE)
save.image("tmp/analysed.RData")
