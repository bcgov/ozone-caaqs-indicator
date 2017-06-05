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

library("rcaaqs") # rcaaqs functions
library("sp") # converting to spatial dataframe
library("bcmaps") #air zone map
library("dplyr") # for glimpse()

# Load the tmp file if ozone doesn't exist (need inherits = FALSE as "ozone" 
# exists in 'maps' package on which 'openair' depends)
if (!exists("ozone", inherits = FALSE)) load("tmp/ozone_clean.RData")

#########################################################################
## Ambient Station and AirZone CAAQS Metric and Achievement Analysis
#########################################################################

## Compute the daily rolling 8 hour average
rolling_avg <- o3_rolling_8hr_avg(ozone, by = c("ems_id", "station_name"))
glimpse(rolling_avg)

# Compute the daily maximum
daily_max_o3 <- o3_daily_max(rolling_avg, by = c("ems_id", "station_name"))
glimpse(daily_max_o3)

# Compute the 4th highest  daily maximum
ann_4th_highest <- o3_ann_4th_highest(daily_max_o3, by = c("ems_id", "station_name"))
glimpse(ann_4th_highest)

# Compute the rolling three year average
three_yr_avg <- o3_three_yr_avg(ann_4th_highest, by = c("ems_id", "station_name"))
glimpse(three_yr_avg)


# Calculate the number of years contributing to rolling 3-year average & max and min years
ozone_caaqs <- three_yr_avg %>% 
  group_by(ems_id, station_name) %>%
  mutate(nyr = ifelse(valid == "FALSE" & flag_two_of_three_years == "FALSE", "<2",
                      ifelse(valid == "TRUE" & flag_two_of_three_years == "FALSE", 3, 2))) %>% 
  mutate(n = n()) %>% 
  mutate(caaq_year_min = min(year), caaq_year_max = max(year))

         
# Extract 2013-2015 3-year average where nyr = 2 or 3 & round ozone caaqs metric to 0 sig figs
ozone_caaqs <- ozone_caaqs %>% 
      filter(nyr != "<2") %>% 
      filter(ozone_metric, nyr == 3 & n == 3 | nyr == 2 & n == 2) %>% 
      mutate(caaq_metric = round(ozone_metric, digits = 0))


## Determine station achievements with o3_standard <- 63
ozone_caaqs$caaq_status <- cut_achievement(ozone_caaqs$caaq_metric, "o3", output = "labels")
ozone_caaqs$caaq_category_html <- cut_management(ozone_caaqs$caaq_metric, "o3", output = "breaks_h")
ozone_caaqs$caaq_category_u <- cut_management(ozone_caaqs$caaq_metric, "o3", output = "breaks_u")


## Add info from ozone sites to ozone_caaqs dataframe & drop some columns
ozone_caaqs <- ozone_caaqs %>% 
  merge(ozone_sites, ., by = c("ems_id")) %>% 
  select(-c(n_hours, year, valid, station_name.y,
            flag_two_of_three_years, valid_in_year, quarter_1, quarter_2,
            quarter_3, quarter_4, max8hr, valid_year, exceed, 
            flag_year_based_on_incomplete_data, n))
colnames(ozone_caaqs)[which(names(ozone_caaqs) == "station_name.x")] <- "station_name"

## Do mapping
## Convert ozone_caaqs to SpatialPointsDataFrame and put in the same projection
## as the airzone map from bcmaps:

ozone_caaqs_map <- ozone_caaqs

# converting characters to numbers
ozone_caaqs_map$latitude <- as.double(ozone_caaqs_map$latitude)
ozone_caaqs_map$longitude <- as.double(ozone_caaqs_map$longitude)
ozone_caaqs_map$nyr <- as.integer(ozone_caaqs_map$nyr)

# setting projections
coordinates(ozone_caaqs_map) <- c("longitude", "latitude")
proj4string(ozone_caaqs_map) <- "+init=epsg:4617"
ozone_caaqs_map <- spTransform(ozone_caaqs_map, CRSobj = proj4string(airzones))


## Get airzone information into ozone_caaqs_sp
ozone_caaqs_map$Airzone <- over(ozone_caaqs_map, airzones)[["Airzone"]]

## Get air zone CAAQ value and achievement status into airzones
az_metric <- airzone_metric(ozone_caaqs_map@data, n_years = "nyr", 
                 az = "Airzone", val = "caaq_metric", 
                 keep = c(rep_station_id = "ems_id", 
                          rep_station_name = "station_name"))

ambient_airzone_map <- airzones
ambient_airzone_map <- sp::merge(ambient_airzone_map, as.data.frame(az_metric), by = "Airzone")

ambient_airzone_map$caaq_status <- cut_achievement(ambient_airzone_map$caaq_metric, "o3", output = "labels")
ambient_airzone_map$caaq_category_html <- cut_achievement(ambient_airzone_map$caaq_metric, "o3", output = "breaks_h")
ambient_airzone_map$caaq_category_u <- cut_achievement(ambient_airzone_map$caaq_metric, "o3", output = "breaks_u")


#####################################################################################
## AQMS AirZone Management Levels Excluding EEs and TFs
####################################################################################

## Recompute the daily rolling 8 hour average with EEs and TFs removed
ml_rolling_avg <- o3_rolling_8hr_avg(ozone, by = c("ems_id", "station_name"),
                                     exclude_df = ee.tf.exclusions,
                                     exclude_df_dt = c("start", "end"))
glimpse(ml_rolling_avg)

# Recompute the daily maximum with EEs and TFs removed
ml_daily_max_o3 <- o3_daily_max(ml_rolling_avg, by = c("ems_id", "station_name"),
                                exclude_df = ee.tf.exclusions,
                                exclude_df_dt = c("start", "end"))
glimpse(ml_daily_max_o3)

# Recompute the 4th highest  daily maximum with EEs and TFs removed
ml_ann_4th_highest <- o3_ann_4th_highest(ml_daily_max_o3, by = c("ems_id", "station_name"),
                                         exclude_df = ee.tf.exclusions,
                                         exclude_df_dt = c("start", "end"))
glimpse(ml_ann_4th_highest)

# Compute the rolling three year average for Management Level assignment
ml_three_yr_avg <- o3_three_yr_avg(ml_ann_4th_highest, by = c("ems_id", "station_name"))
glimpse(ml_three_yr_avg)


# Calculate the number of years contributing to rolling 3-year average & max and min years
#  for Management Level assignment
ml_ozone_caaqs <- ml_three_yr_avg %>% 
  group_by(ems_id, station_name) %>%
  mutate(nyr = ifelse(valid == "FALSE" & flag_two_of_three_years == "FALSE", "<2",
                      ifelse(valid == "TRUE" & flag_two_of_three_years == "FALSE", 3, 2))) %>% 
  mutate(n = n()) %>% 
  mutate(caaq_year_min = min(year), caaq_year_max = max(year))


# Extract 2013-2015 3-year average where nyr = 2 or 3 & round ozone caaqs metric to 0 sig figs
ml_ozone_caaqs <- ml_ozone_caaqs %>% 
  filter(nyr != "<2") %>% 
  filter(ozone_metric, nyr == 3 & n == 3 | nyr == 2 & n == 2) %>% 
  mutate(caaq_mgt_level_metric = round(ozone_metric, digits = 0))


## Determine station management level colours for Management Level reporting
ml_ozone_caaqs$caaq_mgmt_category_html <- cut_management(ml_ozone_caaqs$caaq_mgt_level_metric, "o3", output = "colour")
ml_ozone_caaqs$caaq_mgmt_cat <- cut_management(ml_ozone_caaqs$caaq_mgt_level_metric, "o3", output = "labels")

## Add info from ozone sites to ml_ozone_caaqs dataframe & drop some columns
ml_ozone_caaqs <- ml_ozone_caaqs %>% 
  merge(ozone_sites, ., by = "ems_id") %>% 
  select(-c(n_hours, year, valid, station_name.y,
            flag_two_of_three_years, valid_in_year, quarter_1, quarter_2,
            quarter_3, quarter_4, max8hr, valid_year, exceed, 
            flag_year_based_on_incomplete_data, n))
colnames(ml_ozone_caaqs)[which(names(ml_ozone_caaqs) == "station_name.x")] <- "station_name"

## Do mapping
## Convert ml_ozone_caaqs to SpatialPointsDataFrame and put in the same projection
## as the airzone map from bcmaps:

ml_ozone_caaqs_map <- ml_ozone_caaqs

# converting characters to numbers
ml_ozone_caaqs_map$latitude <- as.double(ml_ozone_caaqs_map$latitude)
ml_ozone_caaqs_map$longitude <- as.double(ml_ozone_caaqs_map$longitude)
ml_ozone_caaqs_map$nyr <- as.integer(ml_ozone_caaqs_map$nyr)

# setting projections
coordinates(ml_ozone_caaqs_map) <- c("longitude", "latitude")
proj4string(ml_ozone_caaqs_map) <- "+init=epsg:4617"
ml_ozone_caaqs_map <- spTransform(ml_ozone_caaqs_map, CRSobj = proj4string(airzones))


## Get airzone information into ozone_caaqs_sp
ml_ozone_caaqs_map$Airzone <- over(ml_ozone_caaqs_map, airzones)[["Airzone"]]

## Get AQMS Management Level status into airzones
ml_az_metric <- airzone_metric(ml_ozone_caaqs_map@data, n_years = "nyr", 
                            az = "Airzone", val = "caaq_mgt_level_metric", 
                            keep = c(rep_station_id = "ems_id", 
                                     rep_station_name = "station_name"))

ml_airzone_map <- airzones
ml_airzone_map <- sp::merge(ml_airzone_map, as.data.frame(ml_az_metric), by = "Airzone")

ml_airzone_map$caaq_mngt_level <- cut_management(ml_airzone_map$caaq_mgt_level_metric, "o3", output = "labels")
ml_airzone_map$caaq_mngt_colour <- cut_management(ml_airzone_map$caaq_mgt_level_metric, "o3", output = "colour")


dir.create("tmp", showWarnings = FALSE)
save.image("tmp/analysed.RData")
