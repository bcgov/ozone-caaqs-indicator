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

library(readr)
## Download the ozone and station data from BC Data Catalogue:
## http://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-raw-hourly-data-and-station-data
databc_ozone <- "http://pub.data.gov.bc.ca/datasets/77eeadf4-0c19-48bf-a47a-fa9eef01f409/O3_hourly.zip"
databc_stations <- "http://catalogue.data.gov.bc.ca/dataset/77eeadf4-0c19-48bf-a47a-fa9eef01f409/resource/b833311d-4126-4dad-b57e-c9ce2c1133c2/download/bcairmonitoringstations.csv"
path <- "data"
ozone_zip <- "O3_hourly.zip"
stn_file <- "bc_air_monitoring_stations.csv"
dir.create(path, showWarnings = FALSE)

download.file(databc_ozone, destfile = file.path(path,ozone_zip))
download.file(databc_stations, destfile = file.path(path, stn_file))
unzip(file.path(path,ozone_zip), exdir = path)

## Load stations and data from files
stations <- read.csv(file.path(path, stn_file), stringsAsFactors = FALSE)

ozone_all <- read_csv(file.path(path, "O3_hourly.csv"), col_types = "cccccidc")

## store data in local repository
dir.create("tmp", showWarnings = FALSE)
save(ozone_all, stations, file = "tmp/ozone_raw.RData")

#####################################################
# ## Some sanity checks
# 
# library("ggplot2") # plotting
# library("dplyr") #filter()
# library("bcmaps") # quickly plot bc air zones
# library("sp") #plot lat-longs in stations file
# 
# ## Set constants
# min_year <- 2013
# max_year <- 2015
# 
# ## subset for 3-year period of focus
# ozone <- ozone_all[ozone_all$year >= min_year & ozone_all$year <= max_year,]
# 
# ## check precision
# precis <- function(x) nchar(gsub("(.*\\.)|([0]*$)", "", as.character(x)))
# table(precis(ozone$value)) # all 0 to 2 digits
# 
# summary(ozone)
# 
# head(ozone, 50)
# tail(ozone, 50)
# 
# ## look at the values
# ggplot(ozone, aes(x = value)) + facet_wrap(~ site) + geom_histogram()
# 
# ## Which sites in ozone dataset don't have a corresponding site in ems locations
# missing_sites <- unique(ozone$site)[!unique(ozone$ems_id) %in% stations$ems_id]
# missing_sites
# 
# #some of the ozone$ems_id's start with 0, which are missing in the statsions$ems-id versions
# 
# ## look at the station locations
# station_points <- filter(stations, stations$Latitude != "NA", stations$ems_id != "E269223") #filter out NAs & error in ems_id == E269223
# coordinates(station_points) <- c("Longitude", "Latitude") #use sp to tell R the lat/lon columns
# proj4string(station_points) <- "+init=epsg:4269" # projection of lat/longs
# station_points <- spTransform(station_points, CRSobj = proj4string(airzones)) #match projection to bcmap layers
# 
# ## plot stations and airzones
# plot(airzones) #from bcmaps
# plot(station_points, add=TRUE)
# 
# 
# # find_site <- function(re) {
# #    i <- grep(re, ems_sites$MNTRNGLCT1[ems_sites$LCTNTPCD == "01"],
# #              ignore.case = TRUE)
# #    ems_sites[i,]
# #  }
# # 
# # sapply(missing_sites, find_site, USE.NAMES = TRUE, simplify = FALSE)
# # ## Most okay to be missing except for Port Moody Rocky Point Park
# # 
# # find_site("moody")

