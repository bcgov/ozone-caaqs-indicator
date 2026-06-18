# Copyright 2026 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and 
# limitations under the License.

source("00_setup.R")

library("readr")
library("dplyr")
library("stringr")
library("tidyr")
library("purrr")
library("lubridate")
library("assertr")
library("rcaaqs")
library("bcdata")


# Load Data ----------------------
ozone_mgmt <- read_rds("data/datasets/rep_year_2024/ozone_mgmt.rds")
stations_clean <- read_rds("data/datasets/rep_year_2024/stations_clean.rds")
print_summary <- read_rds("data/datasets/rep_year_2024/print_summary.rds")
ozone_clean <- read_rds("data/datasets/rep_year_2024/ozone_clean.rds")
az_ambient <- read_rds("data/datasets/rep_year_2024/az_ambient.rds")

ozone_results <- get_caaqs(ozone_mgmt) %>%
  left_join(stations_clean, by = "site") %>% 
  select(caaqs_year, airzone, 
         station_name = site, region, 
         latitude = lat, 
         longitude = lon, 
         metric,	
         n_years,	
         min_year,	
         max_year,	
         metric_value_ambient,
         caaqs_ambient,
         excluded,
         metric_value_mgmt,	mgmt_level, everything(), -flag_daily_incomplete, -flag_yearly_incomplete) %>% 
  arrange(airzone, caaqs_year)

# Compare 2021-2023 and 2022-2024 results for consistency
year_prev <- 2023
year_curr <- 2024

yrs_present <- sort(unique(ozone_results$caaqs_year))
summarise_year <- function(df, y) {
  az_achieved <- df %>%
    filter(caaqs_year == y) %>%
    airzone_metric(keep = c("station_name", "metric"), station_id = "station_name") %>%
    select(-metric_mgmt) %>%
    rename(metric = metric_ambient) %>%
    summarise(az_achieved = sum(caaqs_ambient == "Achieved", na.rm = TRUE))
  df %>%
    filter(caaqs_year == y) %>%
    group_by(metric) %>%
    filter(!is.na(metric_value_ambient)) %>%
    summarise(
      year = y,
      n_stations = n(),
      n_achieved = sum(caaqs_ambient == "Achieved", na.rm = TRUE),
      n_not_achieved = sum(caaqs_ambient == "Not Achieved", na.rm = TRUE),
      pct_achieved = round(100 * n_achieved / n_stations),
      min = min(metric_value_ambient, na.rm = TRUE), 
      max = max(metric_value_ambient, na.rm = TRUE), 
      n_lt50 = sum(metric_value_ambient <= 50, na.rm = TRUE),
      p_lt50 = round(n_lt50 / n_stations * 100),
      az_achieved = az_achieved$az_achieved,
      .groups = "drop")
}

summary_prev <- summarise_year(ozone_results, year_prev)
summary_curr <- summarise_year(ozone_results, year_curr)

# Combine for side-by-side comparison
summary_compare <- full_join(
  summary_prev, summary_curr,
  by = "metric",
  suffix = c("_prev", "_curr")
) %>%
  mutate(
    delta_n = n_stations_curr - n_stations_prev,
    delta_achieved = n_achieved_curr - n_achieved_prev,
    delta_pct = pct_achieved_curr - pct_achieved_prev,
    delta_min = min_curr - min_prev,
    delta_max = max_curr - max_prev,
    delta_n_lt50 = n_lt50_curr - n_lt50_prev,
    delta_p_lt50 = p_lt50_curr - p_lt50_prev,
    delta_az_achieved = az_achieved_curr - az_achieved_prev
  )

print(summary_compare, width = Inf)

# diagnose which stations appear/disappear by metric
coverage_by_metric <- function(df, y) {
  df %>%
    filter(caaqs_year == y, !is.na(metric_value_ambient)) %>%
    distinct(metric, station_name)
}

cov_prev <- coverage_by_metric(ozone_results, year_prev)
cov_curr <- coverage_by_metric(ozone_results, year_curr)

added_stations <- anti_join(cov_curr, cov_prev, by = c("metric", "station_name")) %>%
  arrange(metric, station_name)
dropped_stations <- anti_join(cov_prev, cov_curr, by = c("metric", "station_name")) %>%
  arrange(metric, station_name)

print(added_stations)

print(dropped_stations)

# Station-level comparisons for stations present in BOTH years (by metric)
station_compare <- ozone_results %>%
  filter(caaqs_year %in% c(year_prev, year_curr)) %>%
  filter(!is.na(metric_value_ambient)) %>%
  select(caaqs_year, metric, station_name,
         metric_value_ambient, caaqs_ambient,
         metric_value_mgmt, mgmt_level,
         min_year, max_year, n_years) %>%
  mutate(caaqs_ambient = as.character(caaqs_ambient),
         mgmt_level = as.character(mgmt_level)) %>%
  pivot_wider(
    names_from = caaqs_year,
    values_from = c(metric_value_ambient, caaqs_ambient,
                    metric_value_mgmt, mgmt_level,
                    min_year, max_year, n_years),
    names_sep = "_"
  ) %>%
  # keep only those with BOTH years
  filter(
    !is.na(.data[[paste0("metric_value_ambient_", year_prev)]]) &
      !is.na(.data[[paste0("metric_value_ambient_", year_curr)]])
  )


# Flag any changes in achievement status or management level
changes <- station_compare %>%
  mutate(
    achievement_changed =
      .data[[paste0("caaqs_ambient_", year_prev)]] != .data[[paste0("caaqs_ambient_", year_curr)]],
    mgmt_changed =
      .data[[paste0("mgmt_level_", year_prev)]] != .data[[paste0("mgmt_level_", year_curr)]]) %>%
  filter(achievement_changed | mgmt_changed) %>%
  print(n = Inf, width = Inf)


# Transboundary Flows and Exceptional Events: compare previous (2023) vs current (2024) results for consistency
ozone_caaqs <- o3_caaqs(ozone_clean, by = "site")

tfee_dates <- ozone_clean %>%
  filter(flag_tfee) %>%
  # Ceiling TFEE to capture original dates
  mutate(date = ceiling_date(date_time, unit = "hour"),
         date = as_date(date)) %>%
  select(site, date) %>%
  distinct()

summarise_tfee <- function(df, y) {
  get_daily(df) %>% 
   ungroup() %>%
   filter(exceed, year(date) >= y - 2) %>% 
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
}

summarise_tfee_prev <- summarise_tfee(ozone_caaqs, year_prev)
summarise_tfee_curr <- summarise_tfee(ozone_caaqs, year_curr)

summarise_tfee_prev
summarise_tfee_curr