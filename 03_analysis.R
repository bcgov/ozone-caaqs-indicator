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
library(lubridate) #wrangling dates

## Load the tmp file if ozone doesn't exist
if (!exists("ozone_clean_data")) load("tmp/ozone_clean.RData")

## Ambient Ozone CAAQS analysis 
ozone_caaqs <- o3_caaqs(ozone_clean_data, by = c("ems_id", "station_name"))

#look at ambient-only caaqs results
ozone_caaqs_ambient_df <- get_caaqs(ozone_caaqs)
ozone_caaqs_ambient_df

## Create Exceptional Events (EEs) and Transboundary Flows (TFs) dataframe for 
## determining AQMS Air Zone Management Levels

## Agassiz Municipal Hall (E293810): Two days in 2015 were flagged as exceptional 
## events -- July 8 and 9 (2015)
## Hope Airport (E223756): Three days in 2015 were flagged as exceptional 
## events -- July 6, 8 and 9 (2015)
## Hope Airport (E223756): Five days in 2017 were flagged as exceptional 
## events -- August 3, 7, 10, 11, 29 (2017)

## These days are removed (as a result of suspected wildfire influence) 
## for determining AQMS Air Zone Management Levels

exclusion_dates_E293810 <- c("2015-07-08", "2015-07-09")
exclusion_dates_E223756 <- c("2015-07-06", "2015-07-08", "2015-07-09",
                             "2017-08-03", "2017-08-07", "2017-08-10", "2017-08-11", "2017-08-29")

exclusions <- get_daily(ozone_caaqs) %>% 
  filter((ems_id == "E293810" & date %in% as_date(exclusion_dates_E293810)) | 
           (ems_id == "E223756" & date %in% as_date(exclusion_dates_E223756))) %>% 
  select(ems_id, station_name, date)


## Add management Ozone CAAQS analysis to ambient
ozone_caaqs_mgmt <- caaqs_management(ozone_caaqs, exclude_df = exclusions,
                                     exclude_df_dt = "date")

#look at ambient+mgmt caaqs results
ozone_caaqs_mgmt_df <- get_caaqs(ozone_caaqs_mgmt)


## Ozone CAAQS results for 2017 (based on 2015-2017)
ozone_caaqs_df <- ozone_caaqs_mgmt_df %>% 
  group_by(ems_id) %>%
  # unqoute max_year since it is also a var in the data.frame
  filter(caaqs_year == !!max_year, n_years > 1) 



## Add info from stations_az (created in 02.clean.R) & drop some columns
ozone_caaqs_results <- ozone_caaqs_df %>% 
  left_join(stations_az, by = c("ems_id", "station_name")) %>% 
  select(c(ems_id, station_name, city, lat, lon, airzone, metric,
           caaqs_year, min_year,  max_year, n_years, metric_value_ambient, 
           caaqs_ambient, excluded, metric_value_mgmt, mgmt_level,
           based_on_incomplete = flag_yearly_incomplete))  %>% 
  ungroup()

## Get airzone caaqs metric
ozone_az <- airzone_metric(ozone_caaqs_results)


## Save Ozone CAAQS & Management CAAQS objects
save(ozone_caaqs, exclusions, ozone_caaqs_mgmt, ozone_caaqs_results,
     ozone_az, file = "tmp/analysed.RData")




