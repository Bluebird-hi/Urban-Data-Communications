# Load necessary libraries
library(tidyverse)
library(sf)
library(terra)
library(rgeoboundaries)
library(climateR)
library(biscale)
library(cowplot)
library(tidycensus)
library(gridGraphics)

census_api_key("e62580f74ef222beadd9dd2fbaf48ff130b31c4a", overwrite = TRUE)
acs_variable_list.2022 <- load_variables(2022, #year
                                         "acs5", #five year ACS estimates
                                         cache = TRUE)
# Retrieve Asian population data
tracts22 <- get_acs(geography = "tract",
                    variables = c("B01001D_001E"), # B01001D_001: Asian population
                    year=2022, 
                    state=42, 
                    county=101, 
                    geometry=TRUE, 
                    output="wide") %>%
  st_transform('EPSG:2272')

# get philly boundary
philly_bound <- tracts22 %>% st_union() 
# log
tracts22 <- tracts22 %>%
  mutate(logE = log(B01001D_001E+1),
         logM = log(B01001D_001M+1))
# Classify data into bivariate categories using 'biscale'
# data <- bi_class(tracts22, x = B01001D_001E, y = B01001D_001M, style = "quantile", dim = 4)
# data <- bi_class(tracts22, x = logE, y = logM, style = "equal", dim = 4)
data <- bi_class(tracts22, x = logE, y = logM, style = "fisher", dim = 4)
# data <- bi_class(tracts22, x = logE, y = logM, style = "quantile", dim = 4)

hist(tracts22$logM)
ggplot(data)+
  geom_point(aes(x = logE, y = logM))

# Define the color palette for the bivariate map
# pallet2 <- "DkViolet2"
# the bi_classes is not complete, it just shows 8 classes! so the remaining colors are useless but to match ggplot
pallet <- c('#A1ADB7', '#00429d', '#73a2c6', '#a5d5d8', '#ffbcaf', '#f4777f', '#cf3759', '#93003a',
            '#00429d', '#4771b2', '#73a2c6', '#a5d5d8', '#ffbcaf', '#f4777f', '#cf3759', '#93003a')


library(ggtext)
library(glue)

# Define colors for the annotations
LH_color <-'#a5d5d8'  # Blue for low temperature and high precipitation
HH_color <- '#93003a'  # Orange for high temperature and low precipitation
LL_color <- '#A1ADB7'  # Gray for low temperature and low precipitation
HL_color <- '#a5d5d8'  # Green for high temperature and high precipitation

title_text <- glue("<span style='color:{HH_color};'>**Estimate**</span> and <span style='color:{LH_color};'>**Margin of Error**</span> of Asian Population")

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
  annotate("text", x = -7.5, y = 60, label = annotation_text_LH, color = LH_color, size = 4, fontface = "bold") +
  annotate("text", x =  2660586, y = 202204.6, label = annotation_text_LL, color = LL_color, size = 4, fontface = "bold") +
  annotate("text", x = 4, y = 49, label = annotation_text_HL, color = HH_color, size = 4, fontface = "bold") +
  annotate("text", x = -10, y = 51, label = annotation_text_HH, color = HL_color, size = 4, fontface = "bold") +
  
  # Add leader lines to the annotations
  geom_segment(aes(x = -5.5, xend = -7.5, y = 57.5, yend = 59.25), color = LH_color, size = 0.8) +
  geom_segment(aes(x = -2.25, xend = 1.5, y = 57.65, yend = 57.5), color = LL_color, size = 0.8) +
  geom_segment(aes(x = 1, xend = 4, y = 51, yend = 49.75), color = HH_color, size = 0.8) +
  geom_segment(aes(x = -5, xend = -10, y = 50, yend = 50.25), color = HL_color, size = 0.8) +
  
  
  # Add point lines to the end of the line
  geom_point(aes(x = -7.5, y = 59.25), color = LH_color, size = 2) +
  geom_point(aes(x = 1.5, y = 57.5), color = LL_color, size = 2) +
  geom_point(aes(x = 4, y = 49.75), color = HH_color, size = 3) +
  geom_point(aes(x = -10, y = 50.25), color = HL_color, size = 3)

# not work now TT
legend <- bi_legend(pal = pallet2,   
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