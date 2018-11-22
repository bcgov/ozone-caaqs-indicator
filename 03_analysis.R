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

library(rcaaqs) #rcaaqs functions
library(dplyr) #data munging
library(bcmaps) #airzone map


## Load the tmp file if ozone doesn't exist
if (!exists("ozone")) load("tmp/ozone_clean.RData")


## Get airzone map (sf object) from bcmaps
az <- bcmaps::airzones()


## Ambient Ozone CAAQS Analysis 

#caaqs calculations
ozone_caaqs_all <- o3_caaqs(ozone, by = c("ems_id", "station_name"))

ozone_caaqs_df <- extract_caaqs(ozone_caaqs_all)

#filter for 2017 caaqs (based on 2015-2017)
ozone_caaqs <- ozone_caaqs_df %>% 
  group_by(ems_id) %>% 
  filter(caaqs_year == max(caaqs_year),
         n_years > 1) 

# write.csv(ozone_caaqs, "tmp/ozone_caaqs_2017.csv", row.names = FALSE)

#add info from stations_clean to ozone_caaqs  & drop some columns
ozone_caaqs <- ozone_caaqs %>% 
  left_join(stations_clean, by = c("ems_id", "station_name")) %>% 
  select(c(ems_id, station_name, city, longitude, latitude, caaqs_year, min_year,  max_year,
           n_years, metric_value, caaqs, mgmt, based_on_incomplete = flag_yearly_incomplete)) 

#assign airzone for each station
ozone_stn_az <- assign_airzone(ozone_caaqs, airzones = az, coords = c("longitude", "latitude"))

#get airzone caaqs metric
ozone_az <- airzone_metric(ozone_stn_az)



## Management Ozone CAAQS Analysis 

#caaqs calculations
mgmt_ozone_caaqs_all <- o3_caaqs(ozone, by = c("ems_id", "station_name"),
                                 exclude_df = ee.tf.exclusions,
                                 exclude_df_dt = c("start", "end"))

mgmt_ozone_caaqs_df <- extract_caaqs(mgmt_ozone_caaqs_all)

#filter for 2017 management caaqs (based on 2015-2017)
mgmt_ozone_caaqs <- mgmt_ozone_caaqs_df %>% 
  group_by(ems_id) %>% 
  filter(caaqs_year == max(caaqs_year),
         n_years > 1) 

# write.csv(mgmt_ozone_caaqs, "tmp/mgmt_ozone_caaqs_2017.csv", row.names = FALSE)

#add info from stations_clean to mgmt_ozone_caaqs & drop some columns
mgmt_ozone_caaqs <- mgmt_ozone_caaqs %>% 
  left_join(stations_clean, by = c("ems_id", "station_name")) %>% 
  select(c(ems_id, station_name, city, longitude, latitude, caaqs_year, min_year,  max_year,
           n_years, metric_value, caaqs, mgmt, based_on_incomplete = flag_yearly_incomplete)) 

#assign airzone for each station
mgmt_ozone_stn_az <- assign_airzone(mgmt_ozone_caaqs, airzones = az, coords = c("longitude", "latitude"))

#get airzone management caaqs metric
mgmt_ozone_az <- airzone_metric(mgmt_ozone_stn_az)



## Save Ozone CAAQS & Management CAAQS objects
save(ozone_caaqs_all, ozone_caaqs_df, ozone_caaqs, ozone_stn_az, ozone_az,
     mgmt_ozone_caaqs_all, mgmt_ozone_caaqs_df, mgmt_ozone_caaqs, mgmt_ozone_stn_az, mgmt_ozone_az,
     file = "tmp/analysed.RData")





# ## Compute the daily rolling 8 hour average
# rolling_avg <- rcaaqs:::o3_rolling_8hr_avg(ozone, by = c("ems_id", "station_name"))
# glimpse(rolling_avg)
# 
# ## Compute the daily maximum
# daily_max_o3 <- o3_daily_max(rolling_avg, by = c("ems_id", "station_name"))
# glimpse(daily_max_o3)
# 
# ## Compute the 4th highest  daily maximum
# ann_4th_highest <- o3_ann_4th_highest(daily_max_o3, by = c("ems_id", "station_name"))
# glimpse(ann_4th_highest)
# 
# ## Compute the rolling three year average
# three_yr_avg <- o3_three_yr_avg(ann_4th_highest, by = c("ems_id", "station_name"))
# glimpse(three_yr_avg)
# 
# ## Calculate the number of years contributing to rolling 3-year average & max and min years
# ozone_caaqs <- three_yr_avg %>% 
#   group_by(ems_id, station_name) %>%
#   mutate(nyr = ifelse(valid == "FALSE" & flag_two_of_three_years == "FALSE", "<2",
#                       ifelse(valid == "TRUE" & flag_two_of_three_years == "FALSE", 3, 2))) %>% 
#   mutate(n = n()) %>% 
#   mutate(caaq_year_min = min(year), caaq_year_max = max(year))
# 
# ## Extract 2015-2017 3-year average where nyr = 2 or 3 & round ozone caaqs metric to 0 sig figs
# ozone_caaqs <- ozone_caaqs %>% 
#       filter(nyr != "<2") %>% 
#       filter(ozone_metric, nyr == 3 & n == 3 | nyr == 2 & n == 2) %>% 
#       mutate(caaq_metric = round_caaqs(ozone_metric))
# 
# 
# ## Add info from stations_clean to ozone_caaqs dataframe & drop some columns
# ozone_caaqs <- ozone_caaqs %>% 
#   left_join(stations_clean, by = c("ems_id", "station_name")) %>% 
#   select(c(ems_id, station_name, longitude, latitude, caaq_year_min, caaq_year_max, caaq_nYears = nyr,
#            based_on_incomplete = flag_year_based_on_incomplete_data, caaq_metric))
# 
# ## Converting characters to numbers for caaq_nYears
# ozone_caaqs$caaq_nYears <- as.integer(ozone_caaqs$caaq_nYears)
# 
# ## Determine station achievements with o3_standard <- 63
# ozone_caaqs$caaq_status <- cut_achievement(ozone_caaqs$caaq_metric, "o3", output = "labels")
# ozone_caaqs$caaq_category_html <- cut_management(ozone_caaqs$caaq_metric, "o3", output = "breaks_h")
# ozone_caaqs$caaq_category_u <- cut_management(ozone_caaqs$caaq_metric, "o3", output = "breaks_u")
# 
# ## Convert ozone_caaqs to SpatialPointsDataFrame and put in the same projection as bcmaps::airzones
# ## to add Air Zones to the df
# ozone_caaqs_map <- ozone_caaqs
# 
# airzone_map <- bcmaps::airzones(class = "sp")
# 
# ## Setting projections to match bcmaps::airzones
# coordinates(ozone_caaqs_map) <- c("longitude", "latitude")
# proj4string(ozone_caaqs_map) <- "+init=epsg:4617"
# ozone_caaqs_map <- spTransform(ozone_caaqs_map, CRSobj = proj4string(airzone_map))
# 
# ## Get airzone information into ozone_caaqs_map
# ozone_caaqs_map$Airzone <- over(ozone_caaqs_map, airzone_map)[["Airzone"]]
# 
# ## Get air zone CAAQS value and achievement status for each airzone
# az_metric <- airzone_metric(ozone_caaqs_map@data, n_years = "caaq_nYears", 
#                  az = "Airzone", val = "caaq_metric", 
#                  keep = c(rep_station_id = "ems_id", 
#                           rep_station_name = "station_name"))
# 
# ## Add air zone CAAQS value and achievement status to an airzone map
# ambient_airzone_map <- airzone_map
# ambient_airzone_map <- sp::merge(ambient_airzone_map, as.data.frame(az_metric), by = "Airzone")
# 
# ## Determine air zone achievements with o3_standard <- 63
# ambient_airzone_map$caaq_status <- cut_achievement(ambient_airzone_map$caaq_metric, "o3", output = "labels")
# ambient_airzone_map$caaq_category_html <- cut_achievement(ambient_airzone_map$caaq_metric, "o3", output = "breaks_h")
# ambient_airzone_map$caaq_category_u <- cut_achievement(ambient_airzone_map$caaq_metric, "o3", output = "breaks_u")
# 
# 
# #####################################################################################
# ## AQMS AirZone Management Levels Excluding EEs and TFs
# ####################################################################################
# 
# ## Recompute the daily rolling 8 hour average with EEs and TFs removed
# ml_rolling_avg <- o3_rolling_8hr_avg(ozone, by = c("ems_id", "station_name"),
#                                      exclude_df = ee.tf.exclusions,
#                                      exclude_df_dt = c("start", "end"))
# glimpse(ml_rolling_avg)
# 
# ## Recompute the daily maximum with EEs and TFs removed
# ml_daily_max_o3 <- o3_daily_max(ml_rolling_avg, by = c("ems_id", "station_name"),
#                                 exclude_df = ee.tf.exclusions,
#                                 exclude_df_dt = c("start", "end"))
# glimpse(ml_daily_max_o3)
# 
# ## Recompute the 4th highest  daily maximum with EEs and TFs removed
# ml_ann_4th_highest <- o3_ann_4th_highest(ml_daily_max_o3, by = c("ems_id", "station_name"),
#                                          exclude_df = ee.tf.exclusions,
#                                          exclude_df_dt = c("start", "end"))
# glimpse(ml_ann_4th_highest)
# 
# ## Compute the rolling three year average for Management Level assignment
# ml_three_yr_avg <- o3_three_yr_avg(ml_ann_4th_highest, by = c("ems_id", "station_name"))
# glimpse(ml_three_yr_avg)
# 
# ## Calculate the number of years contributing to rolling 3-year average & max and min years
# ## for Management Level assignment
# ml_ozone_caaqs <- ml_three_yr_avg %>% 
#   group_by(ems_id, station_name) %>%
#   mutate(nyr = ifelse(valid == "FALSE" & flag_two_of_three_years == "FALSE", "<2",
#                       ifelse(valid == "TRUE" & flag_two_of_three_years == "FALSE", 3, 2))) %>% 
#   mutate(n = n()) %>% 
#   mutate(caaq_year_min = min(year), caaq_year_max = max(year))
# 
# ## Extract 2014-2016 3-year average where nyr = 2 or 3 & round ozone caaqs metric to 0 sig
# ## figs for Management Level assignment
# ml_ozone_caaqs <- ml_ozone_caaqs %>% 
#   filter(nyr != "<2") %>% 
#   filter(ozone_metric, nyr == 3 & n == 3 | nyr == 2 & n == 2) %>% 
#   mutate(caaq_mgt_level_metric = round_caaqs(ozone_metric))
# 
# ## Add info from stations_clean to ml_ozone_caaqs dataframe & drop some columns
# ml_ozone_caaqs <- ml_ozone_caaqs %>% 
#   left_join(stations_clean, by = c("ems_id", "station_name")) %>% 
#   select(c(ems_id, station_name, longitude, latitude, caaq_year_min, caaq_year_max, caaq_nYears = nyr,
#            based_on_incomplete = flag_year_based_on_incomplete_data, caaq_mgt_level_metric))
# 
# ## Converting characters to numbers for caaq_nYears
# ml_ozone_caaqs$caaq_nYears <- as.integer(ml_ozone_caaqs$caaq_nYears)
# 
# ## Determine station management level colours for Management Level reporting
# ml_ozone_caaqs$caaq_mgmt_category_html <- cut_management(ml_ozone_caaqs$caaq_mgt_level_metric, "o3", output = "colour")
# ml_ozone_caaqs$caaq_mgmt_cat <- cut_management(ml_ozone_caaqs$caaq_mgt_level_metric, "o3", output = "labels")
# 
# ## Convert ml_ozone_caaqs to SpatialPointsDataFrame and put in the same projection as bcmaps::airzones
# ## to add Air Zones to the df
# ml_ozone_caaqs_map <- ml_ozone_caaqs
# 
# ## Setting projections to match bcmaps::airzones
# coordinates(ml_ozone_caaqs_map) <- c("longitude", "latitude")
# proj4string(ml_ozone_caaqs_map) <- "+init=epsg:4617"
# ml_ozone_caaqs_map <- spTransform(ml_ozone_caaqs_map, CRSobj = proj4string(airzone_map))
# 
# ## Get airzone information into ml_ozone_caaqs_map
# ml_ozone_caaqs_map$Airzone <- over(ml_ozone_caaqs_map, airzone_map)[["Airzone"]]
# 
# ## Get AQMS Management Level status for each airzone
# ml_az_metric <- airzone_metric(ml_ozone_caaqs_map@data, n_years = "caaq_nYears", 
#                             az = "Airzone", val = "caaq_mgt_level_metric", 
#                             keep = c(rep_station_id = "ems_id", 
#                                      rep_station_name = "station_name"))
# 
# ## Add air zone CAAQS management value and management level to an airzone map
# ml_airzone_map <- airzone_map
# ml_airzone_map <- sp::merge(ml_airzone_map, as.data.frame(ml_az_metric), by = "Airzone")
# 
# ## Determine air zone management levels
# ml_airzone_map$caaq_mngt_level <- cut_management(ml_airzone_map$caaq_mgt_level_metric, "o3", output = "labels")
# ml_airzone_map$caaq_mngt_colour <- cut_management(ml_airzone_map$caaq_mgt_level_metric, "o3", output = "colour")
# 
# 
# dir.create("tmp", showWarnings = FALSE)
# save.image("tmp/analysed.RData")
