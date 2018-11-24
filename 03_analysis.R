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


## Get airzone map (sf object) from bcmaps package
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

#add info from stations_clean (created in 02.clean.R) & drop some columns
ozone_caaqs <- ozone_caaqs %>% 
  left_join(stations_clean, by = c("ems_id", "station_name")) %>% 
  select(c(ems_id, station_name, city, longitude, latitude,
           caaqs_year, min_year,  max_year, n_years, metric_value,
           caaqs, mgmt, based_on_incomplete = flag_yearly_incomplete)) 

#assign airzone for each station
ozone_stn_az <- assign_airzone(ozone_caaqs, airzones = az,
                               coords = c("longitude", "latitude"))

#get airzone caaqs metric
ozone_az <- airzone_metric(ozone_stn_az)



## Create Exceptional Evenst (EEs) and Transboundary Flows (TFs) dataframe for 
## determining AQMS Air Zone Management Levels
## Two days in 2015 were flagged as exceptional events: 
## July 8 and 9, 2015 for Agassiz Municipal Hall (E293810)
## These days are removed (as a result of suspected wildfire influence) 
## for determining AQMS Air Zone Management Levels

exclusion_dates <- c("2015-07-08", "2015-07-09")

exclusions <- extract_daily(ozone_caaqs_all) %>% 
  filter(ems_id == "E293810",
         date == exclusion_dates) %>% 
   select(ems_id, station_name, date)

# exclusions  <- data.frame(ems_id = "E293810", station_name = "Agassiz Municipal Hall",
#                                 start = as.Date("2015-07-08"), end = as.Date("2015-07-10"))


## Management Ozone CAAQS Analysis 

#caaqs calculations removing exclusions
mgmt_ozone_caaqs_all <- o3_caaqs(ozone, by = c("ems_id", "station_name"),
                                 exclude_df = exclusions,
                                 exclude_df_dt = "date")

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
  select(c(ems_id, station_name, city, longitude, latitude, caaqs_year,
           min_year,  max_year, n_years, metric_value, caaqs, mgmt,
           based_on_incomplete = flag_yearly_incomplete)) 

#assign airzone for each station
mgmt_ozone_stn_az <- assign_airzone(mgmt_ozone_caaqs, airzones = az,
                                    coords = c("longitude", "latitude"))

#get airzone management caaqs metric
mgmt_ozone_az <- airzone_metric(mgmt_ozone_stn_az)



## Save Ozone CAAQS & Management CAAQS objects
save(ozone_caaqs_all, ozone_caaqs_df, ozone_caaqs, ozone_stn_az, ozone_az,
     exclusions, mgmt_ozone_caaqs_all, mgmt_ozone_caaqs_df, mgmt_ozone_caaqs,
     mgmt_ozone_stn_az, mgmt_ozone_az, file = "tmp/analysed.RData")


