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

## @knitr pre

# library("dplyr")
# library("sp")
# library("rgdal")
# library("rgeos")
library("ggplot2") # for plotting
# library("grid")
# library("scales")
# library("rcaaqs")
# library("geojsonio")
library("envreportutils") # for theme_soe()
# library("bcmaps")

## Load data
if (!exists("three_yr_avg")) load("tmp/analysed.RData")

## Set constants
# min_year <- max_year - 2
# maxdate <- as.Date(paste0(max_year, "-12-31"))
# mindate <- as.Date(paste0(min_year, "-01-01"))
# outCRS <- CRS("+init=epsg:4326")
dir.create("out", showWarnings = FALSE)
o3_standard <- 63


## Subset ozone sites to use only those that have a calculated CAAQS metric
#ozone_sites <- ozone_sites[!is.na(ozone_sites$caaq_metric),]

# Summary graph of airzones -----------------------------

## @knitr summary_plot

sum_dat <- ozone_caaqs_sdf@data
sum_dat$Airzone <- reorder(sum_dat$Airzone, sum_dat$caaq_metric, max, order = TRUE)
sum_dat$Airzone <- factor(sum_dat$Airzone, levels = rev(levels(sum_dat$Airzone)))

summary_plot <- ggplot(sum_dat, 
                       aes(x = caaq_metric, 
                           y = reorder(station_name, caaq_metric, sum))) + 
  facet_grid(Airzone~., scales = "free_y", space = "free_y", drop=TRUE, 
             labeller = label_wrap_gen(15)) + 
  geom_point(size = 4, colour = "#377eb8") + 
  geom_vline(aes(xintercept = o3_standard), linetype = 2, colour = "#e41a1c") + 
  labs(x = "CAAQS Metric", y = "Ozone Monitoring Station") + 
  theme_soe_facet() + 
  theme(axis.title.y = element_text(size=rel(1.2)), 
        axis.text.y = element_text(size = rel(0.8)), 
        axis.line.x = element_blank(), 
        strip.text = element_text(size = rel(0.8)))
plot(summary_plot)

# Individual monitoring station plots -------------------------------------

ems_ids <- ozone_sites@data$ems_id
stn_plots <- vector("list", length(ozone_sites@data$ems_id))
names(stn_plots) <- ems_ids

plot_exceedances <- FALSE

for (emsid in ems_ids) {
  dailydata <- daily_8hr_roll_max[daily_8hr_roll_max$ems_id == emsid &
                                    daily_8hr_roll_max$date <= maxdate,]
  
  if (nrow(dailydata) == 0) next
  
  site <- dailydata$site[1]
  
  caaq_data <- three_yr_avg[three_yr_avg$ems_id == emsid,]
  
  lineplot <- ggplot(dailydata, size = 1) + 
    scale_x_date(expand = c(0,50), limits = c(mindate - 1, maxdate), 
                 breaks = date_breaks(width = "1 year"), labels = date_format("%Y")) + 
    scale_y_continuous(limits = c(0, 102), 
                       breaks = seq(0, 100, by = 20), labels = seq(0, 100, by = 20), 
                       expand = c(0,0)) + 
    geom_line(aes(x = date, y = max8hr), colour = "#9ecae1", size = 0.5) + 
    geom_hline(aes(yintercept = o3_standard), linetype = 2, colour = "#e41a1c") + 
    annotate("text", label = paste0("Ozone Standard (", o3_standard, " ppb)  "), 
             x = maxdate, y = o3_standard + 3, vjust = 0, hjust = 1, 
             size = 3.5, colour = "#e41a1c") + 
    theme_soe(base_size = 10) + 
    theme(axis.title.y = element_text(vjust = 1)) + 
    labs(x = NULL, y = "Daily Maximum Ozone\n(parts per billion)")
  
  if (plot_exceedances) {
    exceedance_data <- filter(dailydata, exceed)
    
    if (nrow(exceedance_data) > 0) {
      lineplot <- lineplot + 
        geom_point(data = exceedance_data, aes(x = date, y = max8hr), 
                   colour = "#e41a1c", size = 2) + 
        annotate("text", x = exceedance_data$date[1] + 20, y = exceedance_data$max8hr[1], 
                 label = "Exceedances", hjust = 0, vjust = 0, colour = "#e41a1c", size = 3)
    }
  }
    
  if (nrow(caaq_data) > 0) {
    lineplot <- lineplot + 
      geom_segment(data = caaq_data, 
                   mapping = aes(x = as.Date(paste0(caaq_year_min, "-01-01")), 
                                 xend = as.Date(paste0(caaq_year_max,"-12-31")), 
                                 y = caaq_metric, yend = caaq_metric, 
                                 colour = factor(caaq_status, 
                                                 levels = c("Achieved", "Not Achieved"))), 
                   size = 1.5) + 
      annotate("text", x = as.Date(paste0(caaq_data$caaq_year_min, "-01-30")), 
               y = 73, label = "2011-2013 Ozone Metric", size = 3.5, hjust=0, 
               colour = "grey50") + 
      geom_segment(data = caaq_data, colour = "grey60",
                   aes(x = as.Date(paste0(caaq_year_min,"-09-15")), y = 69, 
                       xend = as.Date(paste0(caaq_year_min, "-11-01")), 
                       yend = caaq_metric + 1)) +
      scale_colour_manual(values = c("#377eb8", "#e41a1c"), labels = "2011-2013 Ozone Metric", 
                          name = element_blank(), guide = "none") #+ 
#     theme(legend.direction = "horizontal", 
#           legend.key.width = unit(2, "lines"), 
#           legend.position = c(0.02,0.77), 
#           legend.justification = c(0,0), legend.title = element_text(size = 9), 
#           legend.margin = unit(0, "lines"), legend.background = element_rect(fill = "transparent"),
#           legend.key = element_rect(colour="grey80"))
  }
  
  stn_plots[[emsid]] <- lineplot
  
  cat("creating plot for", emsid, site, "\n")
}

# Plot maps ---------------------------------------------------------------

## @knitr achievement_map

airzones.points <- fortify(airzone_map, region = "Airzone") %>%
  left_join(airzone_map@data, by = c("id" = "Airzone"))

stations.points <- as.data.frame(ozone_sites)

achievement_map <- ggplot(airzones.points, aes(long, lat)) + 
  geom_polygon(aes(group=group, fill = caaq_status)) + 
  coord_fixed() + 
  scale_fill_manual(values = c("Achieved" = "#377eb8", "Not Achieved" = "#e41a1c", 
                               "Insufficient Data" ="grey80"), drop = FALSE, 
                    name = "Airzones:\n2011-2013 CAAQS achievement status", 
                    guide = guide_legend(order = 1)) + 
  geom_path(aes(group=group), colour = "white") + 
  geom_point(data = stations.points, aes(x = longitude, y = latitude, 
                                         colour = caaq_metric)) +
  scale_colour_gradient(high = "#252525", low = "#f0f0f0", 
                        name = "Monitoring Stations:\n2011-2013 average ozone concentration", 
                        guide = guide_colourbar(order = 2)) + 
  theme_minimal() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), panel.grid = element_blank(), 
        legend.position = "bottom", legend.box.just = "left")

plot(achievement_map)

## @knitr mgmt_map

colrs <- c("Insufficient Data" = "grey80", 
           "Actions for Keeping Clean Areas Clean" = "#A6D96A", 
           "Actions for Preventing Air Quality Deterioration" = "#FEE08B", 
           "Actions for Preventing CAAQS Exceedance" = "#F46D43", 
           "Actions for Achieving Air Zone CAAQS" = "#A50026")

mgmt_map <- ggplot(airzones.points, aes(long, lat)) +   
  geom_polygon(aes(group=group, fill = caaq_level)) + 
  coord_fixed() + 
  geom_path(aes(group=group), colour = "white") + 
  theme_minimal() + 
  scale_fill_manual(values = colrs, 
                    drop = FALSE, 
                    name = "Air Zone Management Levels", 
                    guide = guide_legend(reverse = TRUE)) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
       legend.position = "none",
       plot.margin = unit(c(0,0,0,0),"mm")) +
  annotate("text", x=680000, y=950000,label="Coastal",colour="black",
           size=4.5, family = "Verdana") +
  annotate("text", x=1150000, y=1550000,label="Northeast",colour="black",
           size=4.5, family = "Verdana") +
  annotate("text", x=780000, y=1500000,label="Northwest",colour="black",
           size=4.5, family = "Verdana") +
  annotate("text", x=1150000, y=950000,label="Central\n Interior",colour="black",
           size=4.5, family = "Verdana") +
  annotate("text", x=1550000, y=600000,label="Southern\n Interior",colour="black",
           size=4.5, family = "Verdana") +
  annotate("text", x=1150000, y=325000,label="Georgia Strait",colour="black",
           size=4.5, family = "Verdana") +
  annotate("text", x=1500000, y=410000,label="Lower Fraser Valley",colour="black",
           size=4.5, family = "Verdana")

plot(mgmt_map)

# Management bar chart----------------------------------------------------

## @knitr mgmt_chart

chart_colrs <- colrs[-1]

mlevels <- c("Actions for Keeping Clean Areas Clean (\u226450 ppb)", 
             "Actions for Preventing Air Quality Deterioration (>50 & \u226456 ppb)", 
             "Actions for Preventing CAAQS Exceedance (>56 & \u226463 ppb)", 
             "Actions for Achieving Air Zone CAAQS (>63 ppb)")

mgmt_chart <- ggplot(data=stations.points,
                              aes(x = Airzone, fill = caaq_level)) + 
  geom_bar(stat = "bin", alpha = 1) +
  xlab ("") + ylab ("Number of Reporting Stations") +
  scale_y_continuous(limits = c(0,20), breaks=seq(0, 20, 4),
                     expand=c(0,0)) +
  scale_fill_manual(values = chart_colrs, 
                    drop = FALSE, 
                    name = "Air Zone Management Levels", 
                    guide = guide_legend(reverse = TRUE),
                    labels = mlevels) + 
  theme_soe() +
  theme(panel.grid.major.y = (element_blank()),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.margin = unit(15,"mm"),
        plot.margin = unit(c(10,0,0,0),"mm")) +
  coord_flip()

plot(mgmt_chart)

## @knitr stop

## Trying with ggvis ... not bad!
# ggvis_map <- airzones.points %>% 
#   group_by(group) %>% 
#   ggvis(~long, ~lat, fill = ~caaq_status) %>% 
#   layer_paths() %>%
#   layer_points(~coords.x1, ~coords.x2, stroke = ~caaq_metric, 
#                data = stations.points)


# Save plots --------------------------------------------------------------

## Summary bar chart
png(filename = paste0("out/airzone_summary_barchart.png"), 
    width = 836, height = 700, units = "px", res = 80) # Match dimensions to invasive species
plot(summary_plot)
dev.off()

## Airzone achievement map
ggsave("out/airzone_map.pdf", achievement_map, width = 8, height = 10, units = "in", scale = 1)

## Combined Management map and barchart with multiplot
png(filename = "./out/mgmt_viz.png", width=836, height=430, units="px")
multiplot(mgmt_chart, mgmt_map, cols=2, widths = c(1, 1.25))
dev.off()

## Line plots
line_dir <- "out/station_plots/"
dir.create(line_dir, showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(stn_plots)) {
  obj <- stn_plots[i]
  name <- names(obj)
  cat("savinging plot for", name, "\n")
  png(filename = paste0(line_dir, name, "_lineplot.png"), 
      width = 778, height = 254, units = "px", res = 90)
  plot(obj[[1]])
  dev.off()
}
graphics.off() # Kill any hanging graphics processes

# Ouput csv files ---------------------------------------------------------

daily_8hr_roll_max$max8hr <- round(daily_8hr_roll_max$max8hr, 1)
write.csv(daily_8hr_roll_max, "out/daily_8r_roll_max.csv", row.names = FALSE)

annual_4th_daily_max$max8hr <- round(annual_4th_daily_max$max8hr, 1)
write.csv(annual_4th_daily_max, "out/annual_4th_daily_max.csv", row.names = FALSE)

write.csv(as.data.frame(airzone_map), "out/caaq_airzone_metrics.csv", 
          row.names = FALSE)

## Output ozone_sites as csv - format for open data catalogue
ozone_sites %>%
  spTransform(CRSobj = outCRS) %>%
  as.data.frame() %>%
  select(ems_id, station_name = display_name, longitude, latitude, Airzone, 
         regional_district, caaq_year_min, caaq_year_max, caaq_nYears, 
         based_on_incomplete, caaq_metric, caaq_status, 
         caaq_mgmt_level = caaq_level) %>% 
  write.csv("out/ozone_site_summary.csv", row.names = FALSE)

# Outpus spatial files as geojson: ----------------------------------------

airzone_map %>%
  spTransform(CRSobj = outCRS) %>%
  geojson_write(file = "out/airzones.geojson")

ozone_sites %>%
  spTransform(CRSobj = outCRS) %>%
  geojson_write(file = "out/ozone_sites.geojson")

regional_districts_disp %>% 
  spTransform(outCRS) %>% 
  geojson_write(file = "out/regional_districts.geojson", precision = 4)
