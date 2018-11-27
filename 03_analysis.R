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


## Ambient Ozone CAAQS analysis 
ozone_caaqs <- o3_caaqs(ozone, by = c("ems_id", "station_name"))

ozone_caaqs_ambient_df <- get_caaqs(ozone_caaqs)


## Create Exceptional Events (EEs) and Transboundary Flows (TFs) dataframe for 
## determining AQMS Air Zone Management Levels
## Two days in 2015 were flagged as exceptional events: 
## July 8 and 9, 2015 for Agassiz Municipal Hall (E293810)
## These days are removed (as a result of suspected wildfire influence) 
## for determining AQMS Air Zone Management Levels

exclusion_dates <- c("2015-07-08", "2015-07-09")

exclusions <- get_daily(ozone_caaqs) %>% 
  filter(ems_id == "E293810",
         date == exclusion_dates) %>% 
  select(ems_id, station_name, date)

# exclusions  <- data.frame(ems_id = "E293810", station_name = "Agassiz Municipal Hall",
#                                 start = as.Date("2015-07-08"), end = as.Date("2015-07-10"))


## Add management Ozone CAAQS analysis to ambient
ozone_caaqs_mgmt <- caaqs_management(ozone_caaqs, exclude_df = exclusions, exclude_df_dt = "date")

ozone_caaqs_mgmt_df <- get_caaqs(ozone_caaqs_mgmt)


## Ozone CAAQS results for 2017 (based on 2015-2017)
ozone_caaqs_df <- ozone_caaqs_mgmt_df %>% 
  group_by(ems_id) %>% 
  filter(caaqs_year == max(caaqs_year),
         n_years > 1) 

#write out csv file
# write.csv(ozone_caaqs_df, "tmp/ozone_caaqs_2016-2017.csv", row.names = FALSE)


## Add info from stations_clean (created in 02.clean.R) & drop some columns
ozone_caaqs_results <- ozone_caaqs_df %>% 
  left_join(stations_clean, by = c("ems_id", "station_name")) %>% 
  select(c(ems_id, station_name, city, longitude, latitude, metric,
           caaqs_year, min_year,  max_year, n_years, ambient_metric_value, 
           ambient_caaqs, excluded, mgmt_metric_value, mgmt_level,
           based_on_incomplete = flag_yearly_incomplete)) 



## Assign airzone for each station

#get airzone map (sf object) from bcmaps package
az <- bcmaps::airzones()

ozone_stn_az <- assign_airzone(ozone_caaqs_results, airzones = az,
                               coords = c("longitude", "latitude"))

## Get airzone caaqs metric
ozone_az <- airzone_metric(ozone_stn_az)


## Save Ozone CAAQS & Management CAAQS objects
save(ozone_caaqs, exclusions, ozone_caaqs_mgmt, ozone_caaqs_results,
     ozone_stn_az, ozone_az, file = "tmp/analysed.RData")


