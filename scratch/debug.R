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

## Load the tmp file if ozone doesn't exist
if (!exists("o3_caaqs_all")) load("tmp/analysed.RData")

issues <- c("E229797", "E206271")

o3_issues <- extract_yearly(o3_caaqs_all) %>% 
  filter(ems_id %in% issues)


write.csv(o3_caaqs_df, "tmp/ozone_caaqs_2015-2017.csv", row.names = FALSE)

