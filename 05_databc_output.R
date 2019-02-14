# Copyright 2019 Province of British Columbia
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

## Output Resources for the B.C. Data Catalogue

library(readr)
library(dplyr)
library(sf)
library(envreportutils)
library(bcmaps)
library(tidyr)

if (!exists("ozone_caaqs_results")) load("tmp/analysed.RData")
if (!exists("stations_clean")) load("tmp/ozone_clean.RData")
dir.create("out/databc", showWarnings = FALSE)

az_summary <- st_intersection(airzones(), st_geometry(bc_bound())) %>% 
  group_by(Airzone) %>% 
  summarize() %>% 
  rename_all(tolower) %>% 
  left_join(ozone_az, by = "airzone") %>% 
  mutate(caaqs_ambient = replace_na(caaqs_ambient, "Insufficient Data")) %>% 
  rename(n_years = n_years_ambient) %>% 
  select(-n_years_mgmt) %>% 
  st_set_geometry(NULL) %>% 
  mutate(caaqs_year = max(ozone_caaqs_results$max_year))

ozone_caaqs_results <- ozone_caaqs_results %>% 
  rename(latitude = lat, longitude = lon) %>% 
  select(-reporting_name, -bcgov_station_name)

ozone_stations_2013 <- read_csv(soe_path("Operations ORCS/Indicators/air/ozone/2015/ozone_site_summary.csv")) %>% 
  rename_all(tolower) %>% 
  select(-regional_district) %>% 
  rename(min_year = caaq_year_min, max_year = caaq_year_max, n_years = caaq_nyears, 
         metric_value_ambient = caaq_metric, caaqs_ambient = caaq_status, 
         mgmt_level = caaq_mgmt_level) %>% 
  mutate(caaqs_year = 2013L) %>% 
  left_join(select(stations_clean, ems_id, city))


ozone_stations_2016 <- read_csv(soe_path("Operations ORCS/Indicators/air/ozone/2017/ozone_site_summary.csv")) %>% 
  rename_all(tolower) %>% 
  rename(min_year = caaq_year_min, max_year = caaq_year_max, n_years = caaq_nyears, 
         metric_value_ambient = caaq_metric, caaqs_ambient = caaq_status) %>% 
  mutate(caaqs_year = 2016L) %>% 
  left_join(select(stations_clean, ems_id, city))

bind_rows(ozone_stations_2013, ozone_stations_2016, ozone_caaqs_results) %>% 
  replace_na(list(metric = "o3")) %>% 
  select(names(ozone_caaqs_results)) %>% 
  arrange(caaqs_year) %>% 
  write_csv("out/databc/ozonesitesummary.csv", na = "")

az_2013 <- read_csv(soe_path("Operations ORCS/Indicators/air/ozone/2015/caaq_airzone_metrics.csv")) %>% 
  select(-FID, -starts_with("caaq_category")) %>% 
  select(airzone = Airzone, n_years = nyears, metric_value_ambient = caaq_metric, 
         caaqs_ambient = caaq_status, rep_stn_id_ambient = rep_station_id, 
         mgmt_level = caaq_level) %>% 
  mutate(caaqs_year = 2013L)

az_2016_ambient <- read_csv(soe_path("Operations ORCS/Indicators/air/ozone/2017/airzone_ambient_summary.csv")) %>% 
  select(airzone = Airzone, n_years = caaq_nYears, 
         rep_stn_id_ambient = rep_station_id, metric_value_ambient = caaq_metric, 
         caaqs_ambient = caaq_status)

az_2016_mgmt <- read_csv(soe_path("Operations ORCS/Indicators/air/ozone/2017/airzone_management_level_summary.csv")) %>% 
  select(airzone = Airzone, rep_stn_id_mgmt = rep_station_id, 
         metric_value_mgmt = caaq_mgt_level_metric, mgmt_level = caaq_mngt_level)

left_join(az_2016_ambient, az_2016_mgmt, by = "airzone") %>% 
  mutate(caaqs_year = 2016L) %>% 
  bind_rows(az_2013, az_summary) %>% 
  arrange(caaqs_year, airzone) %>% 
  write_csv("out/databc/ozone_caaqs_airzone_summary.csv", na = "")
