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
library(bcdata)

if (!exists("ozone_caaqs_results")) load("tmp/analysed.RData")
dir.create("out/databc", showWarnings = FALSE)


# update station data 

ozone_caaqs_results_old <- bcdc_get_data('9b7a9e74-9274-4f97-be81-ce4ee475077d', 
                                         resource = 'f8923733-6dfe-47b7-bc57-aaa8f176c67c')

ozone_caaqs_results <- ozone_caaqs_results %>% 
  mutate(caaqs_ambient = as.character(caaqs_ambient),
         mgmt_level = as.character(mgmt_level))

bind_rows(ozone_caaqs_results_old, ozone_caaqs_results) %>% 
  replace_na(list(metric = "o3")) %>% 
  select(names(ozone_caaqs_results)) %>% 
  arrange(caaqs_year) %>% 
  write_csv("out/databc/ozonesitesummary.csv", na = "")


# read in old data, match format and merge latest data sets for air zone summaries: 

az_summary_old <- bcdc_get_data('9b7a9e74-9274-4f97-be81-ce4ee475077d', 
                                resource = '00b1af32-499e-49a4-ba91-6b8b331a6629')


az_summary <- st_intersection(airzones(), st_geometry(bc_bound())) %>% 
  group_by(Airzone) %>% 
  summarize() %>% 
  rename_all(tolower) %>% 
  left_join(ozone_az, by = "airzone") %>% 
  mutate(caaqs_ambient = replace_na(caaqs_ambient, "Insufficient Data"), 
         caaqs_ambient = as.character(caaqs_ambient),
         mgmt_level = as.character(mgmt_level)) %>% 
  rename(n_years = n_years_ambient) %>% 
  select(-n_years_mgmt) %>% 
  st_set_geometry(NULL) %>% 
  mutate(caaqs_year = max(ozone_caaqs_results$max_year))


bind_rows(az_summary_old, az_summary) %>%
  arrange(caaqs_year, airzone) %>% 
  write_csv("out/databc/ozone_caaqs_airzone_summary.csv", na = "")
