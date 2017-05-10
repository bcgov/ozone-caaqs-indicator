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

#library("openair")
library("rcaaqs") # caaqs functions
#library("dplyr")
#library("lubridate")
#library("magrittr")
library("sp") # converting to spatial dataframe
#library("rgdal")
library("bcmaps") #air zone map
library("dplyr") # for glimpse()

# Load the tmp file if ozone doesn't exist (need inherits = FALSE as "ozone" 
# exists in 'maps' package on which 'openair' depends)
if (!exists("ozone", inherits = FALSE)) load("tmp/ozone_clean.RData")

## Set constants
#o3_standard <- 63

## Compute the daily rolling 8 hour average
rolling_avg <- o3_rolling_8hr_avg(ozone, by = c("ems_id"))
glimpse(rolling_avg)

# Compute the daily maximum
daily_max_o3 <- o3_daily_max(rolling_avg, by = c("ems_id", "site"))
glimpse(daily_max_o3)

# Compute the 4th highest daily maximum
ann_4th_highest <- o3_ann_4th_highest(daily_max_o3, by = c("ems_id", "site"))
glimpse(ann_4th_highest)

# Compute the rolling three year average
three_yr_avg <- o3_three_yr_avg(ann_4th_highest, by = c("ems_id", "site"))
glimpse(three_yr_avg)

# Calculate the number of years contributing to rolling 3-year average
three_yr_avg <- three_yr_avg %>% 
  group_by(ems_id, site) %>%
  mutate(nyr = ifelse(valid == "FALSE" & flag_two_of_three_years == "FALSE", "<2",
                      ifelse(valid == "TRUE" & flag_two_of_three_years == "FALSE", 3, 2))) %>% 
  mutate(n = n())

# Extract 2013-2015 3-year average where nyr = 2 or 3 & round caaqs metric to 0 sig figs
three_yr_avg_caaqs <- three_yr_avg %>% 
        filter(nyr != "<2") %>% 
        filter(ozone_metric, nyr == 3 & n == 3 | nyr == 2 & n == 2) %>% 
        mutate(o3_caaqs_metric = round(ozone_metric, digits = 0))


## Add three year average caaqs to ozone sites and drop some columns
ozone_caaqs <- three_yr_avg_caaqs %>% 
  merge(ozone_sites, ., by = "ems_id") %>% 
  select(-c(site, n, valid, flag_two_of_three_years, quarter_1, quarter_2,
            quarter_3, quarter_4, max8hr, flag_year_based_on_incomplete_data, n))


## Do mapping
## Convert ozone_caaqs to SpatialPointsDataFrame and put in the same projection
## as the airzone map from bcmaps:

ozone_caaqs_sp <- ozone_caaqs

# converting characters to numbers
ozone_caaqs_sp$latitude <- as.double(ozone_caaqs_sp$latitude)
ozone_caaqs_sp$longitude <- as.double(ozone_caaqs_sp$longitude)
ozone_caaqs_sp$nyr <- as.integer(ozone_caaqs_sp$nyr)

# setting projections
coordinates(ozone_caaqs_sp) <- c("longitude", "latitude")
proj4string(ozone_caaqs_sp) <- "+init=epsg:4617"
ozone_caaqs_sp <- spTransform(ozone_caaqs_sp, CRSobj = proj4string(airzones))


## Get airzone information into ozone_caaqs_sp
ozone_caaqs_sp$Airzone <- over(ozone_caaqs_sp, airzones)[["Airzone"]]

## Get CAAQ value and achievement status into airzones
az_metric <- airzone_metric(ozone_caaqs_sp@data, n_years = "nyr", 
                 az = "Airzone", val = "o3_caaqs_metric", 
                 keep = c(rep_station_id = "ems_id", 
                          rep_station_name = "station_name"))

airzone_map <- airzones
airzone_map <- sp::merge(airzone_map, as.data.frame(az_metric), by = "Airzone")

airzone_map$caaq_status <- cut_achievement(airzone_map$o3_caaqs_metric, "o3", output = "labels")
airzone_map$caaq_level <- cut_management(airzone_map$o3_caaqs_metric, "o3", output = "labels")
airzone_map$caaq_category_html <- cut_management(airzone_map$o3_caaqs_metric, "o3", output = "breaks_h")
airzone_map$caaq_category_u <- cut_management(airzone_map$o3_caaqs_metric, "o3", output = "breaks_u")

dir.create("tmp", showWarnings = FALSE)
save.image("tmp/analysed.RData")
