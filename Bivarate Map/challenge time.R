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
library(ggtext)
library(glue)


census_api_key("e62580f74ef222beadd9dd2fbaf48ff130b31c4a", overwrite = TRUE)
# acs_variable_list.2022 <- load_variables(2022, #year
#                                          "acs5", #five year ACS estimates
#                                          cache = TRUE)

# Retrieve Asian population data
# tracts22_orig <- get_acs(geography = "tract",
#                     variables = c("B01001D_001E"), # B01001D_001: Asian population
#                     year=2022, 
#                     state=42, 
#                     geometry=TRUE, 
#                     output="wide") %>%
#   # st_transform('EPSG:2272') %>%
#   st_transform(crs = 4326)

# including surrounding counties for context
{
  tracts22_pa <- get_acs(geography = "tract",
                         variables = c("B01001D_001E"), # B01001D_001: Asian population
                         year =2022, 
                         state = 42, 
                         county = c(101,17,91,45),
                         geometry = TRUE, 
                         output = "wide") %>%
    st_transform(crs = 4326)
  
  tracts22_nj <- get_acs(geography = "tract",
                         variables = c("B01001D_001E"), # B01001D_001: Asian population
                         year =2022, 
                         state = 34, 
                         county = c(5,7,15),
                         geometry = TRUE, 
                         output = "wide") %>%
    st_transform(crs = 4326)
  
  tracts22_orig <- rbind(tracts22_pa, tracts22_nj)
}

# get philly boundary
philly_bound <- tracts22_orig %>% 
  filter(grepl("42101", GEOID)) %>% 
  st_union() 

whole_bound <- tracts22_orig %>% 
  st_union()

# get max/min lat/longs
# sort(unique(whole_bound[[1]][[1]]))
# lat: -75.69678, -74.38971
# long: 39.51497, 40.60858

# log
tracts22 <- tracts22_orig %>%
  mutate(logE = log(B01001D_001E+1),
         logM = log(B01001D_001M+1))
# Classify data into bivariate categories using 'biscale'
# data <- bi_class(tracts22, x = B01001D_001E, y = B01001D_001M, style = "quantile", dim = 4)
# data <- bi_class(tracts22, x = logE, y = logM, style = "equal", dim = 4)
data <- bi_class(tracts22, x = logE, y = logM, style = "fisher", dim = 4)
# data <- bi_class(tracts22, x = logE, y = logM, style = "quantile", dim = 4)

hist(tracts22$logM)
ggplot(data) +
  geom_point(aes(x = logE, y = logM))

# Define the color palette for the bivariate map
# palette2 <- "DkViolet2"
# the bi_classes is not complete, it just shows 8 classes! so the remaining colors are useless but to match ggplot
# palette <- c('#A1ADB7', '#00429d', '#73a2c6', '#a5d5d8', '#ffbcaf', '#f4777f', '#cf3759', '#93003a',
#             '#00429d', '#4771b2', '#73a2c6', '#a5d5d8', '#ffbcaf', '#f4777f', '#cf3759', '#93003a')
dkblue_colors_16 <- bi_pal(pal = "DkBlue2", dim = 4, preview = FALSE)
# 1-1       2-1       3-1       4-1       1-2       2-2 
# "#d3d3d3" "#accaca" "#81c1c1" "#52b6b6" "#c6acc1" "#a2a5b9" 
# 3-2       4-2       1-3       2-3       3-3       4-3 
# "#799db0" "#4d94a6" "#ba85b0" "#977fa8" "#7279a0" "#487397" 
# 1-4       2-4       3-4       4-4 
# "#ad5b9c" "#8d5796" "#6a538f" "#434e87" 
palette <- "DkBlue2"


# Define colors for the annotations
LH_color <- "#ad5b9c"  # low estimate, high MOE
HH_color <- "#434e87" 
LL_color <- "#d3d3d3"
HL_color <- "#52b6b6" # high estimate, low MOE

title_text <- glue("<span style='color:{HL_color};'>**Estimate**</span> and <span style='color:{LH_color};'>**Margin of Error**</span> of Asian Population")

annotation_text_LL <- glue("Low Estimate \n Low Margin of Error")
annotation_text_LH <- glue("Low Estimate \n High Margin of Error")
annotation_text_HL <- glue("High Estimate \n Low Margin of Error")
annotation_text_HH <- glue("High Estimate \n High Margin of Error")

# define x/y coordinates for annotations
LH_x <- -75.32
LH_y <- 40.47
HH_x <- -75.52
HH_y <- 39.87
LL_x <- -74.8
LL_y <- 39.7
HL_x <- -74.9
HL_y <- 40.25

# Create the map with colored subtitle, annotations, and leader lines
map <- ggplot() +
  # theme_void(base_size = 14) +
  xlim(-75.75, -74.3) +  # Set x-axis limits for the map (longitude range)
  ylim(39.35, 40.7) +  # Set y-axis limits for the map (latitude range)
  geom_sf(data = data, aes(fill = bi_class), color = NA, linewidth = 0.1, show.legend = FALSE) +
  geom_sf(data = philly_bound, fill = "transparent", color = "yellow", size = 10) +
  geom_sf(data = whole_bound, fill = "transparent", color = "black") +
  bi_scale_fill(pal = "DkBlue2", dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  
  # Title and subtitle using ggtext for colored styling
  labs(title = "Asian Population in Philadelphia and Surrounding Counties:\nCount Estimates and Error Margins",
       subtitle = title_text,
       caption = "Source: 2022 ACS Census Data") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
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
  geom_point(aes(x = LH_x, y = LH_y), color = LH_color, size = 2) +
  geom_point(aes(x = LL_x, y = LL_y), color = LL_color, size = 2) +
  geom_point(aes(x = HH_x, y = HH_y), color = HH_color, size = 2) +
  geom_point(aes(x = HL_x, y = HL_y), color = HL_color, size = 2)

# legend
legend <- bi_legend(pal = "DkBlue2",   
                    flip_axes = FALSE,
                    rotate_pal = FALSE,
                    dim = 4,
                    xlab = "Estimate",
                    ylab = "Margin of Error",
                    size = 10)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.05, 0.28, 0.28)

# Display the final map with text annotations and leader lines
finalPlot