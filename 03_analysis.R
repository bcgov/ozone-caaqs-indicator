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
#library("sp")
#library("rgdal")
#library("bcmaps")

# Load the tmp file if ozone doesn't exist (need inherits = FALSE as "ozone" 
# exists in 'maps' package on which 'openair' depends)
if (!exists("ozone", inherits = FALSE)) load("tmp/ozone_clean.RData")

## Set constants
o3_standard <- 63


## Compute the daily rolling 8 hour average
rolling_avg <- o3_rolling_8hr_avg(ozone, by = c("ems_id", "site"))
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
