library(magrittr)
library(tidyverse)

old <- read_csv("https://catalogue.data.gov.bc.ca/dataset/9b7a9e74-9274-4f97-be81-ce4ee475077d/resource/f8923733-6dfe-47b7-bc57-aaa8f176c67c/download/ozonesitesummary.csv")
new <- read_csv("out/ozone_site_summary_2016.csv")
stations <- read_csv("data/bc_air_monitoring_stations.csv")

setdiff(names(new), names(old))
setdiff(names(old), names(new))

old2 <- old %>% select(-regional_district, -caaq_mgmt_level) %>% 
  mutate(caaq_year = 2013) %>% 
  rename(min_year = caaq_year_min, max_year = caaq_year_max, n_years = caaq_nYears) %>% 
  left_join(unique(select(stations, EMS_ID, city = CITY)), by = c("ems_id" = "EMS_ID")) 

bind_rows(old2, new) %>% 
  select(ems_id, station_name, Airzone, city, longitude, latitude, caaq_year, everything()) %>% 
  write_csv("out/ozone_site_summary.csv")

