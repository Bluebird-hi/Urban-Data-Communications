# Load necessary libraries
library(tidyverse)
library(sf)
library(terra)
library(rgeoboundaries)
library(climateR)
library(biscale)
library(cowplot)

# Retrieve Asian population data
tracts22 <- get_acs(geography = "tract",
                    variables = c("B01001D_001E"), 
                    year=2022, 
                    state=42, 
                    county=101, 
                    geometry=TRUE, 
                    output="wide") %>%
  st_transform('EPSG:2272')

# get philly boundary
philly_bound <- tracts22 %>% st_union() 
# Classify data into bivariate categories using 'biscale'
data <- bi_class(tracts22, x = B01001D_001E, y = B01001D_001M, style = "quantile", dim = 4)

# selectCentroids.22 <-
#   st_centroid(data)
# 
# coordinates <- st_coordinates(selectCentroids.22)
# 
# selectCentroids.22$x <- coordinates[,1]
# selectCentroids.22$y <- coordinates[,2]
# 
# selectCentroids.22 <- selectCentroids.22 %>% st_drop_geometry()

# Define the color palette for the bivariate map
pallet <- "DkViolet2"

library(ggtext)
library(glue)

# Define colors for the annotations
blue_color <- "#4367b0"  # Blue for low temperature and high precipitation
red_color <- "#ba0651"  # Orange for high temperature and low precipitation
gray_color <- "lightgray"  # Gray for low temperature and low precipitation
purple_color <- "#5b0f69"  # Green for high temperature and high precipitation

title_text <- glue("<span style='color:{red_color};'>**Estimate**</span> and <span style='color:{blue_color};'>**Margin of Error**</span> of Asian Population")

annotation_text_LL <- glue("Low Estimate \n Low Margin of Error")
annotation_text_LH <- glue("Low Estimate \n High Margin of Error")
annotation_text_HL <- glue("High Estimate \n Low Margin of Error")
annotation_text_HH <- glue("High Estimate \n High Margin of Error")

# Create the map with colored subtitle, annotations, and leader lines
map <- ggplot() +
  # theme_void(base_size = 14) +
  xlim(2660586.2, 2750104) +  # Set x-axis limits for the map (longitude range)
  ylim(184667.4, 324965.3) +  # Set y-axis limits for the map (latitude range)
  geom_sf(data = data, aes(fill = bi_class), color = NA, linewidth = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = pallet, dim = 4, flip_axes = FALSE, rotate_pal = FALSE)+
  
  # Title and subtitle using ggtext for colored styling
  labs(title = "Finding Errors when Counting Asian Population in Philadelphia",
       subtitle = title_text,
       caption = "Source: 2022 ACS Census Data") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(hjust = .6, size = 12),  # Use element_markdown for ggtext
        plot.caption = element_text(size = 10, face = "bold", hjust = 0)) +
  
  # Add the colored annotations with leader lines
  annotate("text", x = -7.5, y = 60, label = annotation_text_LH, color = blue_color, size = 4, fontface = "bold") +
  annotate("text", x =  2660586, y = 202204.6, label = annotation_text_LL, color = gray_color, size = 4, fontface = "bold") +
  annotate("text", x = 4, y = 49, label = annotation_text_HL, color = orange_color, size = 4, fontface = "bold") +
  annotate("text", x = -10, y = 51, label = annotation_text_HH, color = green_color, size = 4, fontface = "bold") +
  
  # Add leader lines to the annotations
  geom_segment(aes(x = -5.5, xend = -7.5, y = 57.5, yend = 59.25), color = blue_color, size = 0.8) +
  geom_segment(aes(x = -2.25, xend = 1.5, y = 57.65, yend = 57.5), color = gray_color, size = 0.8) +
  geom_segment(aes(x = 1, xend = 4, y = 51, yend = 49.75), color = orange_color, size = 0.8) +
  geom_segment(aes(x = -5, xend = -10, y = 50, yend = 50.25), color = green_color, size = 0.8) +
  
  
  # Add point lines to the end of the line
  geom_point(aes(x = -7.5, y = 59.25), color = blue_color, size = 2) +
  geom_point(aes(x = 1.5, y = 57.5), color = gray_color, size = 2) +
  geom_point(aes(x = 4, y = 49.75), color = orange_color, size = 3) +
  geom_point(aes(x = -10, y = 50.25), color = green_color, size = 3)

legend <- bi_legend(pal = pallet,   
                    flip_axes = FALSE,
                    rotate_pal = FALSE,
                    dim = 4,
                    xlab = "Estimate",
                    ylab = "Marginal",
                    size = 10)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.05, 0.28, 0.28)

# Display the final map with text annotations and leader lines
finalPlot