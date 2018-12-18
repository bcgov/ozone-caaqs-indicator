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

# ## Load the tmp file if ozone doesn't exist
# if (!exists("o3_caaqs_all")) load("tmp/analysed.RData")

library(dplyr) #data munging
names1 <- readr::read_csv("~/Downloads/Ozone station names for reporting.csv") %>% 
  select(ems_id, station_name, reporting_name)
names2 <- readr::read_csv("~/Downloads/PM25 station names for reporting.csv") %>% 
  select(ems_id, station_name, reporting_name)

reporting_conversion <- names1 %>% 
  bind_rows(names2) %>% 
  unique()

write.csv(reporting_conversion, "data/stn_names_reporting.csv", row.names = FALSE)



