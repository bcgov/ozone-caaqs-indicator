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


library("dplyr") # data munging
library("ggplot2") # for plotting
library("scales") # for date_breaks()
library("envreportutils") # for theme_facet_soe() & svg_px
library("forcats") # tweak factor levels fct_drop()
library("sp") 
library("geojsonio") # for geojson outputs

## Load data
if (!exists("ml_airzone_map")) load("tmp/analysed.RData")

## @knitr pre

## Set constants
rep_yr <- 2016
min_year <- rep_yr - 2
maxdate <- as.Date(paste0(rep_yr, "-12-31"))
mindate <- as.Date(paste0(min_year, "-01-01"))
outCRS <- CRS("+init=epsg:4326")
dir.create("out", showWarnings = FALSE)
o3_standard <- 63


### Plots for web and print version ##

## @knitr summary_plot 

## Summary graph of ambient CAAQ metrics by station and airzones (print version only)

sum_dat <- ozone_caaqs_map@data
sum_dat$Airzone <- reorder(sum_dat$Airzone, sum_dat$caaq_metric, max, order = TRUE)
sum_dat$Airzone <- factor(sum_dat$Airzone, levels = rev(levels(sum_dat$Airzone)))

ann_text <- data.frame(station_name = "Victoria Topaz", caaq_metric = 62.5,
                       Airzone = factor("Georgia Strait",
                                        levels = levels(sum_dat$Airzone)))

summary_plot <- ggplot(sum_dat, 
                       aes(x = caaq_metric, 
                           y = reorder(station_name, caaq_metric, sum))) + 
  facet_grid(Airzone~., scales = "free_y", space = "free", drop=TRUE, 
             labeller = label_wrap_gen(width = 15, multi_line = TRUE)) + 
  geom_point(size = 2, colour = "#377eb8") + 
  geom_vline(aes(xintercept = o3_standard), linetype = 2, colour = "#e41a1c") + 
  geom_text(data = ann_text, label = "Ozone Standard (63 ppb)", size = 4, 
            hjust = 1, colour = "#e41a1c") + 
  labs(x = "Ozone Metric (ppb)", y = "Monitoring Station") + 
  theme_soe_facet() + 
  theme(axis.title.y = element_text(size=rel(1.2)), 
        axis.text.y = element_text(size = rel(0.8)), 
        axis.line.x = element_blank(), 
        strip.text.y = element_text(size = rel(1), angle = 0))
plot(summary_plot)

## @knitr summary_plot_end


## PNG of summary ozone CAAQS achivement by station and air zone chart
# png_retina(filename = paste0("out/ozone_station_summary_chart.png"),
#     width = 836, height = 700, units = "px", res = 80)
svg_px(paste0("out/ozone_station_summary_chart.svg"), 
        width = 836, height = 700) 

plot(summary_plot)
dev.off()


mid_breaks <- function(width = "1 year") {
  function(x) {
    if (length(x) > 2) stop("x should be a range of length 2")
    sq <- scales::fullseq(x, width)
    diff <- diff(sq)
    sq[-length(sq)] + diff / 2
  }
}


## Individual monitoring station plots with daily maximum data and ambient
## CAAQS metric (for both print & web)


ems_ids <- sum_dat$ems_id
stn_plots <- vector("list", length(sum_dat$ems_id))
names(stn_plots) <- ems_ids

plot_exceedances <- FALSE

for (emsid in ems_ids) {
  dailydata <- daily_max_o3[daily_max_o3$ems_id == emsid &
                              daily_max_o3$date <= maxdate,]
  
  if (nrow(dailydata) == 0) next
  
  site <- dailydata$station_name[1]
  
 caaq_data <- sum_dat[sum_dat$ems_id == emsid,]

# lineplot <- plot_ts(dailydata, caaqs_data = caaq_data,
#                      parameter = "o3", rep_yr = 2016)
# lineplot <- lineplot + coord_cartesian(ylim = c(0, 100))

  lineplot <- ggplot(dailydata, size = 1) +
    scale_x_date(expand = c(0,50), limits = c(mindate - 1, maxdate),
                 breaks = mid_breaks(), labels = date_format("%Y")) +
    scale_y_continuous(limits = c(0, 102),
                       breaks = seq(0, 100, by = 20), labels = seq(0, 100, by = 20),
                       expand = c(0,0)) +
    geom_line(aes(x = date, y = max8hr), colour = "#9ecae1", size = 0.5) +
    geom_hline(aes(yintercept = o3_standard), linetype = 2, colour = "#e41a1c") +
    annotate("text", label = paste0("Ozone Standard (", o3_standard, " ppb)  "),
             x = maxdate, y = o3_standard + 3, vjust = 0, hjust = 1,
             size = 4.5, colour = "#e41a1c") +
    theme_soe(base_size = 14) +
    theme(axis.title.y = element_text(vjust = 1),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_line(colour = "grey85")) +
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
               y = 73, label = "2014-2016 Ozone Metric", size = 4.5, hjust=0,
               colour = "grey50") +
      geom_segment(data = caaq_data, colour = "grey60",
                   aes(x = as.Date(paste0(caaq_year_min,"-09-15")), y = 69,
                       xend = as.Date(paste0(caaq_year_min, "-11-01")),
                       yend = caaq_metric + 1)) +
      scale_colour_manual(values = c("#377eb8", "#e41a1c"), labels = "2014-2016 Ozone Metric",
                          name = element_blank(), guide = "none")

  }

  
  stn_plots[[emsid]] <- lineplot
  
  cat("creating plot for", emsid, site, "\n")
}



## PNGs of CAAQS metrics and raw data station line plots
line_dir <- "out/station_plots/"
dir.create(line_dir, showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(stn_plots)) {
  obj <- stn_plots[i]
  name <- names(obj)
  cat("saving plot for", name, "\n")
  # png_retina(filename = paste0(line_dir, name, "_lineplot.png"), 
  #     width = 778, height = 254, units = "px", res = 90)
  svg_px(paste0(line_dir, name, "_lineplot.svg"), 
          width = 778, height = 254)
  plot(obj[[1]])
  dev.off()
}
graphics.off() # kill any hanging graphics processes


## @knitr achievement_map

## Ambient CAAQS achievement map (print version only)
ach_airzones <- fortify(ambient_airzone_map, region = "Airzone") %>% 
 left_join(ambient_airzone_map@data, by = c("id" = "Airzone"))

station.points <- as.data.frame(ozone_caaqs_map)

caaqs_achievement_map <- ggplot(ach_airzones, aes(long, lat)) + 
  geom_polygon(aes(group=group, fill = caaq_status)) + 
  coord_fixed() + 
  scale_fill_manual(values = c("Achieved" = "#377eb8", "Not Achieved" = "#e41a1c", 
                               "Insufficient Data" ="grey80"), drop = FALSE, 
                    name = "Airzones:\nOzone Air Quality Standard", 
                    guide = guide_legend(order = 1, title.position = "top")) + 
  geom_path(aes(group=group), colour = "white") + 
  geom_point(data = station.points, aes(x = longitude, y = latitude, 
                                         colour = caaq_metric), size = 4) +
  scale_colour_gradient(high = "#252525", low = "#f0f0f0", 
                        name = "Monitoring Stations:\nOzone Metric (ppb)", 
                        guide = guide_colourbar(order = 2, title.position = "top",
                                                barwidth = 10)) + 
  theme_minimal() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), 
        axis.ticks = element_blank(), panel.grid = element_blank(), 
        legend.position = "bottom", legend.box.just = "left")
plot(caaqs_achievement_map)

## @knitr achievement_map_end

## PNG of airzone CAAQS ambient achievement map
#ggsave("out/ozone_caaqs_achievement_map.pdf", caaqs_achievement_map, width = 8, height = 10, units = "in", scale = 1)
# png_retina(filename = paste0("out/ozone_caaqs_achievement_map.png"), 
#     width = 836, height = 700, units = "px", res = 80)
svg_px(paste0("out/ozone_caaqs_achievement_map.svg"), 
        width = 836, height = 700)
plot(caaqs_achievement_map)
dev.off()


## @knitr mgmt_map

##  AQMS Management Levels Map
ml_airzones <- fortify(ml_airzone_map, region = "Airzone") %>% 
  left_join(ml_airzone_map@data, by = c("id" = "Airzone"))

colrs <- c("Insufficient Data" = "grey80", 
           "Actions for Keeping Clean Areas Clean" = "#A6D96A", 
           "Actions for Preventing Air Quality Deterioration" = "#FEE08B", 
           "Actions for Preventing CAAQS Exceedance" = "#F46D43", 
           "Actions for Achieving Air Zone CAAQS" = "#A50026")

mgmt_map <- ggplot(ml_airzones, aes(long, lat)) +   
  geom_polygon(aes(group=group, fill = caaq_mngt_level)) + 
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


## @knitr mgmt_chart

## AQMS Management Levels by station bar chart
ml_station.points <- as.data.frame(ml_ozone_caaqs_map)
ml_station.points$caaq_mgmt_cat <- fct_drop(ml_station.points$caaq_mgmt_cat,
                                            only = "Insufficient Data")

chart_colrs <- colrs[-1]

# mlevels <- c("Actions for Keeping Clean Areas Clean (\u226450 ppb)", 
#              "Actions for Preventing Air Quality Deterioration (>50 & \u226456 ppb)", 
#              "Actions for Preventing CAAQS Exceedance (>56 & \u226463 ppb)", 
#              "Actions for Achieving Air Zone CAAQS (>63 ppb)")

mlevels <- c("Actions for Keeping Clean Areas Clean", 
             "Actions for Preventing Air Quality Deterioration", 
             "Actions for Preventing CAAQS Exceedance", 
             "Actions for Achieving Air Zone CAAQS")

mgmt_chart <- ggplot(data=ml_station.points,
                              aes(x = Airzone, fill = caaq_mgmt_cat)) + 
  geom_bar(stat = "count", alpha = 1) +
  xlab ("") + ylab ("Number of Reporting Stations") +
  scale_y_continuous(limits = c(0,25), breaks=seq(0, 25, 5),
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
   #     legend.margin = unit(15,"mm"),
        plot.margin = unit(c(10,10,0,0),"mm")) +
  coord_flip()

## @knitr stop
plot(mgmt_chart)


## PNG of combined Management map and barchart with multiplot
# png_retina(filename = "./out/mgmt_viz.png", width=836, height=430, units="px")
svg_px("./out/mgmt_viz.svg", width=836, height=430)
multiplot(mgmt_chart, mgmt_map, cols=2, widths = c(1, 1.25))
dev.off()

### BCDC Resources ###

## Output ozone_caaqs ambient caaqs for stations as CSV format for the BC Data Catalogue
ozone_caaqs_map %>%
  spTransform(CRSobj = outCRS) %>%
  as.data.frame() %>%
  select(ems_id, station_name, longitude, latitude, Airzone,
         min_year = caaq_year_min, max_year = caaq_year_max, n_years = caaq_nYears,
         based_on_incomplete, caaq_metric, caaq_status) %>%
  mutate(caaq_year = rep_yr) %>% 
  left_join(unique(select(stations, EMS_ID, city = CITY)), by = c("ems_id" = "EMS_ID")) %>% 
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


### Outputs spatial files as geojson for web ###

ozone_caaqs_map %>%
  spTransform(CRSobj = outCRS) %>%
  as.data.frame() %>%
  geojson_write(file = "out/ozone_sites.geojson")

ambient_airzone_map %>%
  spTransform(CRSobj = outCRS) %>%
  geojson_write(file = "out/airzones.geojson")



