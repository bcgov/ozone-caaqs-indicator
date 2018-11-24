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

library(readr) #read in data

## Download the ground-level ozone and station data from the B.C. Data Catalogue
## Ozone data: https://catalogue.data.gov.bc.ca/dataset/77eeadf4-0c19-48bf-a47a-fa9eef01f409
## Station metadata: https://catalogue.data.gov.bc.ca/dataset/01867404-ba2a-470e-94b7-0604607cfa30

ozone <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2009-LatestVerified/O3.csv"
stations <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Air_Monitoring_Stations/bc_air_monitoring_stations.csv"
path <- "data"
ozone_file <- "O3.csv"
stn_file <- "bc_air_monitoring_stations.csv"

dir.create(path, showWarnings = FALSE)

download.file(ozone, destfile = file.path(path, ozone_file))
download.file(stations, destfile = file.path(path, stn_file))

## Load stations and data from files
stations <- read_csv(file.path(path, stn_file), na = c("", "N/A"))
ozone_all <- read_csv(file.path(path, "O3.csv"))
                  
## Store raw data in local repository
dir.create("tmp", showWarnings = FALSE)
save(ozone_all, stations, file = "tmp/ozone_raw.RData")




############################
## Some Basic Data Checks ##
############################

library("ggplot2") #plotting
library("dplyr") #data munging
library("bcmaps") #quickly plot bc air zones
library("sf") #plot lat-longs in stations file
library("mapview") #interact with station metadata



## Set constants
min_year <- 2015
max_year <- 2017

## Subset for 3-year period of focus
ozone_inspect <- ozone_all %>% 
  mutate(year = as.numeric(format(DATE_PST, "%Y"))) %>% 
  filter(year >= min_year & year <= max_year)

## Check precision
precis <- function(x) nchar(gsub("(.*\\.)|([0]*$)", "", as.character(x)))
table(precis(ozone_inspect$RAW_VALUE)) # 0 to 10 digits

summary(ozone_inspect)

head(ozone_inspect, 50)
tail(ozone_inspect, 50)

## Look at the raw values 
ggplot(ozone_inspect, aes(x = RAW_VALUE))
+ facet_wrap(~ STATION_NAME) + geom_histogram()

## Check for duplicates
duplicates <- ozone_inspect[duplicated(ozone_inspect),]
duplicates # two duplicates found

## Which sites in ozone dataset don't have a corresponding site in ems locations
missing_sites <- unique(ozone_inspect$STATION_NAME)[!unique(ozone_inspect$EMS_ID) %in% stations$EMS_ID]
missing_sites # none

## Look at the station locations
station_points <- stations %>% 
  select(EMS_ID, STATION_NAME, LONGITUDE, LATITUDE) %>% 
  filter(!is.na(LATITUDE) | !is.na(LONGITUDE)) %>% #filter out NAs
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>% # set projection (or WGS84 4326)
  transform_bc_albers() #match projection to bcmap layers

## Plot stations and airzones
ggplot() + 
  geom_sf(data = airzones(), fill = NA) +
  geom_sf(data = bc_bound(), fill = NA) +
  geom_sf(data = station_points)

## Interactive map with station points
mapview(station_points)


