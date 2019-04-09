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


library(dplyr) #data munging
library(ggplot2) #for plotting
library(scales) #for date_breaks()
library(envreportutils) #for theme_facet_soe() & svg_px
library(forcats) #wrangling factors
library(rcaaqs) #rcaaqs functions
library(sf) #mapping
library(bcmaps) #airzone map
library(tidyr) # for replace_na
library(geojsonio) # for geojson outputs
library(readr)

## Load data
if (!exists("ozone_caaqs_results")) load("tmp/analysed.RData")
if (!exists("max_year")) load("tmp/ozone_clean.RData")

# Create output directory:
dir.create("out", showWarnings = FALSE)

## WEB & PDF OUTPUTS ##

## Summary Plot of Ambient CAAQ metrics by Station & Airzones (PDF only) -------

ambient_summary_plot <- summary_plot(
  ozone_caaqs_results, 
  metric_val = "metric_value_ambient", 
  airzone = "airzone", station = "station_name", 
  parameter = "metric", pt_size = 2) +
  theme(strip.text.y = element_text(angle = 0))
plot(ambient_summary_plot)


#png of ozone CAAQS achievement by station and air zone summary plot 
png_retina(filename = "out/ozone_station_summary_chart.png",
           width = 836, height = 700)
plot(ambient_summary_plot)
dev.off()

#svg of ozone CAAQS achievement by station and air zone summary plot 
# svg_px("out/ozone_station_summary_chart.svg", width = 836, height = 700) 
# plot(summary_plot)
# dev.off()



## Individual Monitoring Station Line Plots with Daily Maximum Data &
## Ambient CAAQS Metric --------------------------------------------------------

#generate lineplots for each station
ems_ids <- ozone_caaqs_results$ems_id
stn_plots <- vector("list", length(ems_ids))
names(stn_plots) <- ems_ids

for (emsid in ems_ids) {
  
  lineplot <- plot_ts(ozone_caaqs, id = emsid,
                      id_col = "ems_id", rep_yr = 2017, base_size = 14)
  
  stn_plots[[emsid]] <- lineplot
  cat("creating plot for", emsid, "\n")
}


#save svgs of station line plots for web map
svg_dir <- "leaflet_map/station_plots/"
dir.create(svg_dir, showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(stn_plots)) {
  obj <- stn_plots[i]
  name <- names(obj)
  cat("saving svg plots for", name, "\n")
  svg_px(paste0(svg_dir, name, "_lineplot.svg"), width = 778, height = 254)
  plot(obj[[1]])
  dev.off()
}
graphics.off() #STOP any hanging graphics processes

#save pngs of station line plots for proofing
png_dir <- "out/station_plots/"
dir.create(png_dir, showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(stn_plots)) {
  obj <- stn_plots[i]
  name <- names(obj)
  cat("saving png plots for", name, "\n")
  png_retina(filename = paste0(png_dir, name, "_lineplot.png"),
             width = 778, height = 254)
  plot(obj[[1]])
  dev.off()
}
graphics.off() #STOP any hanging graphics processes

## Ambient CAAQS Achievement Map (PDF only) ------------------------------------

#get airzone map (sf object) from bcmaps package
az <- st_intersection(airzones(), st_geometry(bc_bound())) %>% 
  group_by(Airzone) %>% 
  summarize()

achievement_map <- az  %>%
  left_join(ozone_az, by = c("Airzone" = "airzone")) %>% 
  mutate(caaqs_ambient = replace_na(caaqs_ambient, "Insufficient Data")) %>% 
  ggplot() +
  geom_sf(aes(fill = caaqs_ambient), colour = "white", size = .4) +
  geom_sf(data = st_as_sf(ozone_caaqs_results, coords = c("lon", "lat"),
                          crs = 4326),
          aes(colour = metric_value_ambient), size = 4) +
  scale_fill_manual(values = get_colours(type = "achievement", drop_na = FALSE), 
                    drop = FALSE, name = "Airzones:\nOzone Air Quality Standard",
                    guide = guide_legend(order = 1, title.position = "top")) +
  scale_colour_gradient(high = "#252525", low = "#f0f0f0", 
                        name = "Monitoring Stations:\nOzone Metric (ppb)", 
                        guide = guide_colourbar(order = 2,
                                                title.position = "top",
                                                barwidth = 10)) + 
  coord_sf(datum = NA) +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom",
        legend.box.just = "left")
plot(achievement_map)

#png of airzone CAAQS ambient achievement map
png_retina(filename = "out/ozone_caaqs_achievement_map.png",
           width = 836, height = 700)
plot(achievement_map)
dev.off()

##svg of airzone CAAQS ambient achievement map
# svg_px("out/ozone_caaqs_achievement_map.svg", width = 836, height = 700)
# plot(achievement_map)
# dev.off()



##  AQMS Management Levels Map -------------------------------------------------
#get the starting/center coordinates for text labels
# points <- sf::st_point_on_surface(az)

management_map <- az %>%
  left_join(ozone_az, by = c("Airzone" = "airzone")) %>% 
  mutate(mgmt_level = replace_na(mgmt_level, "Insufficient Data")) %>% 
  ggplot() +
  geom_sf(aes(fill = mgmt_level), colour = "white") +
  scale_fill_manual(values = get_colours(type = "management", drop_na = FALSE), 
                    drop = FALSE,
                    name = "Air Zone Management Levels", 
                    guide = guide_legend(reverse = TRUE)) +
  coord_sf(datum = NA) +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0),"mm"))

management_map_web <- management_map +
  annotate("text", x = 757938.691028203, y = 755183.494351726,
           label = "Coastal",colour = "black", size = 5) +
  annotate("text", x = 1237018.05546828, y = 1353777.8838629,
           label = "Northeast", colour = "black", size = 5) +
  annotate("text", x = 825358.624129937, y = 1458245.53295221,
           label = "Northwest", colour = "black", size = 5) +
  annotate("text", x = 1136451.95878554, y = 1008862.625,
           label = "Central\n Interior", colour = "black", size = 5) +
  annotate("text", x = 1477982.32044802, y = 657920.545686275,
           label="Southern\n Interior", colour = "black", size = 5) +
  annotate("text", x = 1191040.32976477, y = 338497.757759471, 
           label = "Georgia Strait", colour = "black", size = 5) +
  annotate("text", x = 1442293.56067042, y = 419268.843749995,
           label= "Lower Fraser Valley", colour = "black", size = 5)
  # geom_sf_text(aes(label = Airzone), colour = "black") # centers not great for many labels
plot(management_map_web)

management_map_pdf <- management_map +
  annotate("text", x = 757938.691028203, y = 755183.494351726,
           label = "Coastal",colour = "black", size = 4) +
  annotate("text", x = 1237018.05546828, y = 1353777.8838629,
           label = "Northeast", colour = "black", size = 4) +
  annotate("text", x = 825358.624129937, y = 1458245.53295221,
           label = "Northwest", colour = "black", size = 4) +
  annotate("text", x = 1136451.95878554, y = 1008862.625,
           label = "Central\n Interior", colour = "black", size = 4) +
  annotate("text", x = 1477982.32044802, y = 657920.545686275,
           label="Southern\n Interior", colour = "black", size = 4) +
  annotate("text", x = 1191040.32976477, y = 338497.757759471, 
           label = "Georgia Strait", colour = "black", size = 4) +
  annotate("text", x = 1442293.56067042, y = 419268.843749995,
           label= "Lower Fraser Valley", colour = "black", size = 4)
# geom_sf_text(aes(label = Airzone), colour = "black") # centers not great for many labels
plot(management_map_pdf)

#svg of airzone CAAQS mgmt levels map
svg_px("out/ozone_caaqs_mgmt_map.svg", width = 500, height = 500)
plot(management_map_web)
dev.off()

#png of airzone CAAQS mgmt levels map
# png_retina(filename = "out/ozone_caaqs_mgmt_map.png",
#            width = 500, height = 500)
# plot(management_map)
# dev.off()


## AQMS Management Levels by Station Bar Chart ---------------------------------
management_chart <- ozone_caaqs_results %>% 
  mutate(mgmt_level = fct_drop(mgmt_level, only = "Insufficient Data")) %>% 
  ggplot(aes(x = airzone, fill = mgmt_level)) + 
  geom_bar(stat = "count", alpha = 1) +
  coord_flip() +
  xlab ("") + ylab ("Number of Reporting Stations") +
  scale_y_continuous(limits = c(0,25), breaks=seq(0, 25, 5),
                     expand=c(0,0)) +
  scale_fill_manual(values = get_colours(type = "management", drop_na = FALSE),
                    drop = FALSE,
                    name = "Air Zone Management Levels",
                    guide = guide_legend(reverse = TRUE)) +
  theme_soe() +
  theme(panel.grid.major.y = (element_blank()),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
   #     legend.margin = unit(15,"mm"),
        plot.margin = unit(c(10,10,0,0),"mm"))
plot(management_chart)

#svg of airzone CAAQS mgmt level bar chart
svg_px("out/ozone_caaqs_mgmt_chart.svg",
       width = 500, height = 500)
plot(management_chart)
dev.off()

#png of airzone CAAQS mgmt level bar chart
# png_retina(filename = "out/ozone_caaqs_mgmt_chart.png",
#            width = 500, height = 500)
# plot(management_chart)
# dev.off()

## Summary Numbers for Use in ozone.Rmd ----------------------------------------

#number of achieved airzones
num_az_achieved <- az %>%
  left_join(ozone_az, by = c("Airzone" = "airzone")) %>% 
  mutate(caaqs_ambient = replace_na(caaqs_ambient, "Insufficient Data")) %>% 
  st_set_geometry(NULL) %>% 
  select(caaqs_ambient) %>% 
  add_count(caaqs_ambient) %>% 
  unique() %>% 
  filter(caaqs_ambient == "Achieved") %>% 
  pull()


#number of stations
num_stns <- ozone_caaqs_results %>% 
  add_count(ems_id) %>% 
  pull(n) %>% 
  sum()

#number stations achieved
num_stns_achieved <- ozone_caaqs_results %>% 
  add_count(ems_id) %>% 
  filter(caaqs_ambient == "Achieved") %>% 
  pull(n) %>% 
  sum()

#low metric value
lowest_value <- ozone_caaqs_results %>%
  pull(metric_value_ambient) %>% 
  min()

#high metric value
highest_value <- ozone_caaqs_results %>%
  pull(metric_value_ambient) %>% 
  max()

#number and % stations equal or less than 50ppb
num_values_50 <- ozone_caaqs_results %>% 
  select(ems_id, metric_value_ambient) %>% 
  add_count(ems_id) %>% 
  filter(metric_value_ambient < 51) %>% 
  pull(n) %>% 
  sum()

perc_values_50 <- round(num_values_50/num_stns*100, digits = 0)

num_values_60 <- ozone_caaqs_results %>% 
  select(ems_id, metric_value_ambient) %>% 
  add_count(ems_id) %>% 
  filter(metric_value_ambient > 60) %>% 
  pull(n) %>% 
  sum()

## Save Objects for Use in ozone.Rmd --------------------------------------
save(ambient_summary_plot, stn_plots, achievement_map,
     management_map, management_map_web,
     management_map_pdf, management_chart,
     num_stns, num_stns_achieved,
     lowest_value, highest_value, num_values_50,
     perc_values_50, num_values_60, 
     num_az_achieved, file = "tmp/plots.RData")


## Output geojson Files for Web Map --------------------------------------------

ozone_caaqs_results %>%
  geojson_write(file = "out/ozone_sites.geojson")

az %>% 
  st_transform(az, crs = 4326) %>% 
  left_join(ozone_az, by = c("Airzone" = "airzone")) %>% 
  mutate(caaqs_ambient = replace_na(caaqs_ambient, "Insufficient Data")) %>% 
  geojson_write(file = "out/airzones.geojson")

#output air zone results as CSV format

az_summary <- az %>% 
  rename_all(tolower) %>% 
  left_join(ozone_az, by = "airzone") %>% 
  mutate(caaqs_ambient = replace_na(caaqs_ambient, "Insufficient Data")) %>% 
  rename(n_years = n_years_ambient) %>% 
  select(-n_years_mgmt) %>% 
  st_set_geometry(NULL) %>% 
  mutate(caaqs_year = max_year) %>% 
  write_csv("out/ozone_airzone_summary_2017.csv", na = "")

#output stations results as CSV format
ozone_caaqs_results %>% 
  rename(latitude = lat, longitude = lon) %>% 
  write_csv("out/ozone_site_summary_2017.csv", na = "")
