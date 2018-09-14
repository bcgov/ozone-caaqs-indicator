
create_popup <- function(data, caaq = "o3", type = "polygon") {
  
  data %>%
    # Define individual elements
    title_popup(., type) %>%
    metric_popup(., caaq) %>%
    standard_popup(., caaq) %>%
    mutate(popup_svg = paste0("./out/station_plots/", p_station_id, "_lineplot.svg"),
    # Create the rows
    popup_row1 = paste0("<div class = 'popup-row'>\n",
                        "  <div class = 'title'>\n", popup_title, "  </div>\n",
                        "</div>\n"),
    popup_row2 = paste0("<div class = 'popup-row'>\n",
                        "  <div class = 'section-metric'>\n", popup_metric, "  </div>\n",
                        "  <div class = 'section-standard' ",
                        "style = 'background-color: ", popup_standard_col, "'>\n",
                        popup_standard, "  </div>\n",
                        "</div>\n"),
    popup_row3 = paste0("<img src = ", popup_svg, ">"),
    
    # Assemble them all together
    popup = pmap_chr(list(popup_row1, popup_row2, popup_row3),
                     ~HTML(paste0(..1, ..2, ..3))))
  
}

title_popup <- function(data, type) {
  if(type == "polygon") {
    data <- mutate(data, popup_title = paste0("    <h2>Air Zone: ", p_az, "</h2>\n",
                                              "    <h4>Station: ", p_station, "</h4>\n"))
  } else if(type == "markers") {
    data <- mutate(data, popup_title = paste0("    <h2>Station: ", p_station, "</h2>\n",
                                              "    <h4>Air Zone: ", p_az, "</h4>\n"))
  }
  data
}

metric_popup <- function(data, caaq) {
  if(caaq == "o3") {
    m <- "Ozone Metric"
    units <- "ppm"
  }
  
  data <- mutate(data,
                 popup_metric = paste0("    <h4>", m, "</h4>\n",
                                       "    <h3>", caaq_metric, " ", units, "</h3>\n",
                                       "    <span>(", caaq_nYears, " year average)</span>\n"))
}

standard_popup <- function(data, caaq) {
  if(caaq == "o3") {
    s <- "Ozone Air Quality Standard"
  } 
  data <- mutate(data, 
                 popup_standard = paste0("    <h4>", s, "</h4>\n",
                                         "    <h2>", caaq_status, "</h2>\n"),
                 popup_standard_col = case_when(caaq_status == "Achieved" ~ "#377EB8",
                                                caaq_status == "Not Achieved" ~ "#B8373E",
                                                TRUE ~ as.character(NA)))
}

