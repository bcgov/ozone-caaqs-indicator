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

## Load data
if (!exists("ozone_stn_az")) load("tmp/analysed.RData")


## WEB & PDF OUTPUTS ##

## Summary Plot of Ambient CAAQ metrics by Station & Airzones (PDF only) -------

#set up constants
dir.create("out", showWarnings = FALSE)
o3_standard <- 63

#make standard annotation
annotated_text <- data.frame(station_name = "Victoria Topaz",
                             metric_value = 62.5,
                             airzone = as_factor("Georgia Strait"))
#plot
summary_plot <- ozone_stn_az %>% 
  mutate(airzone = as_factor(airzone),
         airzone = reorder(airzone, metric_value, min, order = TRUE)) %>% 
  ggplot(aes(x = metric_value, 
             y = reorder(station_name, metric_value, sum))) + 
  facet_grid(airzone ~ ., scales = "free_y", space = "free", drop=TRUE, 
             labeller = label_wrap_gen(width = 15, multi_line = TRUE)) + 
  geom_point(size = 2, colour = "#377eb8") + 
  geom_vline(aes(xintercept = o3_standard), linetype = 2, colour = "#e41a1c") + 
  geom_text(data = annotated_text, label = "Ozone Standard (63 ppb)", size = 4,
            hjust = 1, colour = "#e41a1c") +
  labs(x = "Ozone Metric (ppb)", y = "Monitoring Station") + 
  theme_soe_facet() + 
  theme(axis.title.y = element_text(size=rel(1.2)), 
        axis.text.y = element_text(size = rel(0.8)), 
        axis.line.x = element_blank(), 
        strip.text.y = element_text(size = rel(1), angle = 0))
plot(summary_plot)

#png of ozone CAAQS achievement by station and air zone summary plot 
png_retina(filename = "out/ozone_station_summary_chart.png",
           width = 836, height = 700)
plot(summary_plot)
dev.off()

#svg of ozone CAAQS achievement by station and air zone summary plot 
# svg_px("out/ozone_station_summary_chart.svg", width = 836, height = 700) 
# plot(summary_plot)
# dev.off()



## Individual Monitoring Station Line Plots with Daily Maximum Data &
## Ambient CAAQS Metric (for both PDF & Web) -----------------------------------

#generate lineplots for each station
ems_ids <- ozone_stn_az$ems_id
stn_plots <- vector("list", length(ems_ids))
names(stn_plots) <- ems_ids

for (emsid in ems_ids) {
  
  lineplot <- plot_ts(ozone_caaqs_all, id = emsid,
                      id_col = "ems_id", rep_yr = 2017)
  
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

achievement_map <- az  %>% # st_transform(az, crs = "+proj=longlat")
  left_join(ozone_az, by = c("Airzone" = "airzone")) %>% 
  mutate(caaqs = replace_na(caaqs, "Insufficient Data")) %>% 
  ggplot() +
  geom_sf(aes(fill = caaqs), colour = "white") +
  geom_sf(data = st_as_sf(ozone_stn_az, coords = c("lon", "lat"),
                          crs = "+proj=longlat"),
          aes(colour = metric_value), size = 4) +
  geom_sf(data = st_as_sf(ozone_stn_az, coords = c("lon", "lat"),
                          crs = "+proj=longlat"),
          aes(), colour = "grey30", shape = 21, size = 4) +
  # geom_point(data = ozone_stn_az, aes(x = lon, y = lat,
  #                                       colour = metric_value), size = 4) +
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

management_map <- az %>% # st_transform(az, crs = "+proj=longlat")
  left_join(ozone_az, by = c("Airzone" = "airzone")) %>% 
  mutate(mgmt = replace_na(mgmt, "Insufficient Data")) %>% 
  ggplot() +
  geom_sf(aes(fill = mgmt), colour = "white") +
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
        plot.margin = unit(c(0,0,0,0),"mm")) +
  annotate("text", x = 757938.691028203, y = 755183.494351726,
           label = "Coastal",colour = "black", size = 4.5) +
  annotate("text", x = 1237018.05546828, y = 1353777.8838629,
           label = "Northeast", colour = "black", size = 4.5) +
  annotate("text", x = 825358.624129937, y = 1458245.53295221,
           label = "Northwest", colour = "black", size = 4.5) +
  annotate("text", x = 1136451.95878554, y = 1008862.625,
           label = "Central\n Interior", colour = "black", size = 4.5) +
  annotate("text", x = 1477982.32044802, y = 657920.545686275,
           label="Southern\n Interior", colour = "black", size = 4.5) +
  annotate("text", x = 1191040.32976477, y = 338497.757759471, 
           label = "Georgia Strait", colour = "black", size = 4.5) +
  annotate("text", x = 1442293.56067042, y = 419268.843749995,
           label= "Lower Fraser Valley", colour = "black", size = 4.5)
  # geom_sf_text(aes(label = Airzone), colour = "black") # centers not great for many labels
plot(management_map)

#svg of airzone CAAQS mgmt levels map
svg_px("out/ozone_caaqs_mgmt_map.svg", width = 500, height = 500)
plot(management_map)
dev.off()

#png of airzone CAAQS mgmt levels map
# png_retina(filename = "out/ozone_caaqs_mgmt_map.png",
#            width = 500, height = 500)
# plot(management_map)
# dev.off()


## AQMS Management Levels by Station Bar Chart ---------------------------------
management_chart <- ggplot(mgmt_ozone_stn_az,
                              aes(x = airzone, fill = mgmt)) + 
  geom_bar(stat = "count", alpha = 1) +
  coord_flip() +
  xlab ("") + ylab ("Number of Reporting Stations") +
  scale_y_continuous(limits = c(0,25), breaks=seq(0, 25, 5),
                     expand=c(0,0)) +
  scale_fill_manual(values = get_colours(type = "management", drop_na = FALSE),
                    drop = TRUE,
                    name = "Air Zone Management Levels",
                    guide = guide_legend(reverse = TRUE)) +
  theme_soe() +
  theme(panel.grid.major.y = (element_blank()),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
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


## Save Plot Objects for Use in ozone.Rmd --------------------------------------
save(summary_plot, stn_plots, achievement_map,
     management_map, management_chart, file = "tmp/plots.RData")



## Output geojson Files for Web Map --------------------------------------------

# ozone_caaqs_map %>%
#   spTransform(CRSobj = outCRS) %>%
#   as.data.frame() %>%
#   geojson_write(file = "out/ozone_sites.geojson")
# 
# ambient_airzone_map %>%
#   spTransform(CRSobj = outCRS) %>%
#   geojson_write(file = "out/airzones.geojson")

ozone_stn_az %>%
  geojson_write(file = "out/ozone_sites.geojson")

az %>% # st_transform(az, crs = "+proj=longlat")
  left_join(ozone_az, by = c("Airzone" = "airzone")) %>% 
  mutate(caaqs = replace_na(caaqs, "Insufficient Data")) %>% 
  geojson_write(file = "out/airzones.geojson")



## Output Resources for the B.C. Data Catalogue --------------------------------

## Output ozone_caaqs ambient caaqs for stations as CSV format for the BC Data Catalogue
ozone_caaqs_map %>%
  spTransform(CRSobj = outCRS) %>%
  as.data.frame() %>%
  select(ems_id, station_name, longitude, latitude, Airzone,
         min_year = caaq_year_min, max_year = caaq_year_max, n_years = caaq_nYears,
         based_on_incomplete, caaq_metric, caaq_status) %>%
  mutate(caaq_year = rep_yr) %>% 
  left_join(unique(select(stations_clean, ems_id, city)), by = "ems_id") %>% 
  write.csv(paste0("out/ozone_site_summary_", rep_yr, ".csv"), row.names = FALSE)

## Output ozone_caaqs ambient caaqs for air zones as CSV format for the BC Data Catalogue
ambient_airzone_map %>%
  spTransform(CRSobj = outCRS) %>%
  as.data.frame() %>%
  select(Airzone, rep_station_id, rep_station_name, caaq_nYears,
         caaq_metric, caaq_status) %>%
  write.csv("out/airzone_ambient_summary.csv", row.names = FALSE)

## Output ml_airzone air zone management levels as CSV format for the BC Data Catalogue
ml_airzone_map %>%
  spTransform(CRSobj = outCRS) %>%
  as.data.frame() %>%
  select(Airzone, rep_station_id, rep_station_name, caaq_nYears,
         caaq_mgt_level_metric, caaq_mngt_level) %>%
  write.csv("out/airzone_management_level_summary.csv", row.names = FALSE)



