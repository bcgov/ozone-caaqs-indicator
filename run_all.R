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

source("01_load.R")
source("02_clean.R")
source("03_analysis.R")
source("04_output.R")
source("05_databc_output.R")

mon_year <- format(Sys.Date(), "%B%Y")
outfile <- paste0("envreportbc_ozone_", mon_year, ".pdf")

rmarkdown::render("print_ver/ozone.Rmd", output_file = outfile)
# extrafont::embed_fonts(file.path("print_ver/", outfile))


###############################################################################################
## copy files to web dev folder
air_indicators_path <- "~/soe_wwwd/indicators/air"
air_viz_path <- file.path(air_indicators_path, "ozone_viz/")
dir.create(air_viz_path, showWarnings = FALSE)
air_indicators_station_plots <- file.path(air_viz_path, "station_plots/")
dir.create(air_indicators_station_plots, showWarnings = FALSE)

web_viz_plots <- list.files("leaflet_map/station_plots", full.names = TRUE)

over_copy <- function(...) {
  file.copy(..., overwrite = TRUE)
}

## Copy print version
over_copy(file.path("print_ver", outfile), 
          file.path(air_indicators_path, "print_ver/"))

## Copy the management viz map and bar chart plots
over_copy("out/ozone_caaqs_mgmt_map.svg", 
          file.path(air_indicators_path, "images/"))

over_copy("out/ozone_caaqs_mgmt_chart.svg", 
          file.path(air_indicators_path, "images/"))

## Copy leaflet map
over_copy("leaflet_map/leaflet_map.html", air_viz_path)

## Copy dataviz plots
lapply(list.files(air_indicators_station_plots, full.names = TRUE), file.remove)
lapply(web_viz_plots, over_copy, to = air_indicators_station_plots)

