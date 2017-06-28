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

library(readr) # read in data

## Download the ozone and station data from the BC Data Catalogue:
## ozone data from http://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-raw-hourly-data-and-station-data
## station data from https://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-unverified-hourly-air-quality-and-meteorological-data

databc_ozone <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2009-LatestVerified/O3.csv"
databc_stations <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Air_Monitoring_Stations/bc_air_monitoring_stations.csv"
path <- "data"
ozone_file <- "O3.csv"
stn_file <- "bc_air_monitoring_stations.csv"

dir.create(path, showWarnings = FALSE)

download.file(databc_ozone, destfile = file.path(path,ozone_file))
download.file(databc_stations, destfile = file.path(path, stn_file))

## Load stations and data from files
stations <- read_csv(file.path(path, stn_file), na = c("", "N/A"))
ozone_all <- read_csv(file.path(path, "O3.csv"))
                  
## store data in local repository
dir.create("tmp", showWarnings = FALSE)
save(ozone_all, stations, file = "tmp/ozone_raw.RData")


################################
## Some basic data checks
################################

# library("ggplot2") # plotting
# library("dplyr") #filter()
# library("bcmaps") # quickly plot bc air zones
# library("sp") #plot lat-longs in stations file
# 
# ## Set constants
# min_year <- 2014
# max_year <- 2016
# 
# ## subset for 3-year period of focus
# ozonce_all$year <- as.numeric(format(ozonce_all$DATE_PST, "%Y"))
# ozone <- ozonce_all[ozonce_all$year >= min_year & ozonce_all$year <= max_year,]
# 
# ## check precision
# precis <- function(x) nchar(gsub("(.*\\.)|([0]*$)", "", as.character(x)))
# table(precis(ozone$RAW_VALUE)) # 0 to 10 digits
# 
# summary(ozone)
# 
# head(ozone, 50)
# tail(ozone, 50)
# 
# ## look at the values
# ggplot(ozone, aes(x = RAW_VALUE)) + facet_wrap(~ STATION_NAME) + geom_histogram()
# 
# ## Which sites in ozone dataset don't have a corresponding site in ems locations
# missing_sites <- unique(ozone$STATION_NAME)[!unique(ozone$EMS_ID) %in% stations$EMS_ID]
# missing_sites # none
# 
# 
# ## look at the station locations
# station_points <- stations
# station_points$LONGITUDE <- as.double(station_points$LONGITUDE)
# station_points$LATITUDE <- as.double(station_points$LATITUDE)
# station_points <- filter(station_points, station_points$LATITUDE != "NA") #filter out NAs
# coordinates(station_points) <- c("LONGITUDE", "LATITUDE") #use sp to tell R the lat/lon columns
# proj4string(station_points) <- "+init=epsg:4269" # projection of lat/longs
# station_points <- spTransform(station_points, CRSobj = proj4string(airzones)) #match projection to bcmap layers
# 
# ## plot stations and airzones
# plot(airzones) #from bcmaps
# plot(station_points, add=TRUE)

