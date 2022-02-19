# Copyright 2015 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

source("00_setup.R")

library("readr")
library("dplyr")
library("tidyr")
library("purrr")
library("lubridate")
library("assertr")

library("rcaaqs")


# Load Data ----------------------
ozone_clean <- read_rds("data/datasets/ozone_clean.rds")
stations_clean <- read_rds("data/datasets/stations_clean.rds")

# Transboundary Flows and Exceptional Events ------------------------------
tfee_dates <- ozone_clean %>%
  filter(flag_tfee) %>%
  # Ceiling TFEE to capture original dates
  mutate(date = ceiling_date(date_time, unit = "hour"),
         date = as_date(date)) %>%
  select(site, date) %>%
  distinct()

# Calculate CAAQs --------------------------------------

ozone_caaqs <- o3_caaqs(ozone_clean, by = "site")
ozone_mgmt <- caaqs_management(ozone_caaqs, 
                               exclude_df = tfee_dates, 
                               exclude_df_dt = "date")


# Station results -----------------------------------------
# Combine and filter
ozone_results <- get_caaqs(ozone_mgmt) %>%
  filter(caaqs_year == .env$rep_year, n_years > 1) %>% 
  left_join(stations_clean, by = "site") %>% 
  select(airzone, site, region, lat, lon, everything())

# Airzone results ---------------------------------------------------------
az_ambient <- airzone_metric(ozone_results, keep = c("site", "metric"), station_id = "site") %>%
  select(-metric_mgmt, metric = metric_ambient, everything())

az_mgmt <- az_ambient %>% 
  group_by(airzone) %>%
  slice(which.max(mgmt_level)) %>% 
  mutate(caaqs_year = .env$rep_year) %>% 
  ungroup() %>%
  select(caaqs_year, airzone, mgmt_level, rep_metric = metric, 
         metric_value = metric_value_mgmt, 
         rep_stn_id = rep_stn_id_mgmt)

# For print version --------------------------------------------------------
# Get reporting period tfee numbers for print version
print_tfee <- get_daily(ozone_caaqs) %>% 
  ungroup() %>%
  filter(exceed, year(date) >= rep_year - 2) %>% 
  semi_join(tfee_dates, by = c("site", "date")) %>%
  arrange(date) %>%
  group_by(year = year(date)) %>%
  mutate(n_year = n_distinct(date)) %>%
  ungroup() %>%
  mutate(n_tfee_days = n_distinct(date)) %>%
  pivot_wider(names_from = year, values_from = n_year, names_prefix = "n_tfee_days_") %>%
  summarize(n_sites = n_distinct(site),
            tfee_occurred_in_months = paste0(sort(unique(month(date))), collapse = ", "),
            n_tfee_days = unique(n_tfee_days),
            across(starts_with("n_tfee_days_"), .fns = ~na.omit(unique(.))))


# Save next steps ----------------------------------------
write_rds(ozone_results, "data/datasets/ozone_results.rds")
write_rds(az_ambient, "data/datasets/az_ambient.rds")
write_rds(az_mgmt, "data/datasets/az_mgmt.rds")
write_rds(ozone_mgmt, "data/datasets/ozone_mgmt.rds")
write_rds(print_tfee, "data/datasets/print_tfee.rds")

# Save csvs --------------------------------------------------------
write_csv(ozone_results, "out/ozone_caaqs_results.csv", na = "")
write_csv(az_ambient, "out/ozone_airzone_results.csv" , na = "")
write_csv(az_mgmt, "out/ozone_airzone_management_levels.csv", na = "")
