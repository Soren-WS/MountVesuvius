
vesuvius <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')


# This script was used to clean data saved as 13 csv files 
# from the Italian Istituto Nazionale di Geofisica e Vulcanologia found
# here https://terremoti.ov.ingv.it/gossip/vesuvio/index.html

# Load packages
library(tidyverse)
library(janitor)
library(dplyr)
library(tidyverse)

# List all vesuvius CSV files in the data_raw folder
file_list <- list.files(path = "data/data_raw", pattern = "^vesuvius_\\d{4}\\.csv$", full.names = TRUE)

# Read and row-bind all CSVs
vesuvius_data <- 
  file_list |> 
  map_dfr(read_csv)

# Clean column names and create year variable from time variable
vesuvius <- 
  vesuvius_data |> 
  clean_names() |> 
  mutate(
    year = year(time)
  )


# Add unit and context to column names
vesuvius <- 
  vesuvius |> 
  rename(duration_magnitude_md = md, 
         depth_km = depth,
         event_id = number_event_id,
         review_level = level)

# Translate some values from Italian to English
# Rivisto is revised (reviewed by humans)
# Bollettino is "bulletin" or the original preliminary source of the data,
# the Italian Seismic Bulletin (Il Bollettino Sismico Italiano)
vesuvius <-
  vesuvius |> 
  mutate(review_level = case_when(
    review_level == "Rivisto" ~ "revised",
    review_level == "Bollettino" ~ "preliminary",
    .default = NA_character_
  ))

# Let's change vesuvio to Mount Vesuvius! (Same value for all rows, but still)
vesuvius <-
  vesuvius |> 
  mutate(area = case_when(
    area == "vesuvio" ~ "Mount Vesuvius",
    .default = NA_character_
  ))

# Make year and event_id into integers, but everything else is a decimal/dbl
vesuvius <-
  vesuvius |> 
  mutate(
    dplyr::across(
      c("event_id", "year"),
      as.integer
    )
  )


theme_set()



ggplot(vesuvius) +
  geom_point(aes(time, duration_magnitude_md)) +
  geom_smooth(aes(time, duration_magnitude_md))

ggplot(vesuvius) +
  geom_point(aes(depth_km, duration_magnitude_md, color = year), alpha = 0.5) +
  geom_smooth(aes(depth_km, duration_magnitude_md)) +
  xlim(NA, 5)



# Load the library
library(leaflet)

# Note: if you do not already installed it, install it with:
# install.packages("leaflet")

# Background 1: NASA
m <- leaflet() %>% 
  addTiles() %>% 
  setView( lng = 2.34, lat = 48.85, zoom = 5 ) %>% 
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
m

# Background 2: World Imagery
m <- leaflet() %>% 
  addTiles() %>% 
  setView( lng = 2.34, lat = 48.85, zoom = 3 ) %>% 
  addProviderTiles("Esri.WorldImagery")
m

# save the widget in a html file if needed.
# library(htmlwidgets)
# saveWidget(m, file=paste0( getwd(), "/HtmlWidget/backgroundMapTile.html", width="1000px"))




#library
library(leaflet)

# Create 20 markers (Random points)
data <- data.frame(
  long=sample(seq(-150,150),20),
  lat=sample(seq(-50,50),20),
  val=round(rnorm(20),2),
  name=paste("point",letters[1:20],sep="_")
) 

# Show a circle at each position
m <- leaflet(data = data) %>%
  addTiles() %>%
  addCircleMarkers(~long, ~lat , popup = ~as.character(name))
m

leaflet(data) %>% 
  addTiles()

?addTiles
library(ggmap)
library(ggplot2)
library(ggspatial)  # for scale bar / north arrow
library(dplyr)

# example seismic points (replace with your real data frame)
seismic <- data.frame(
  lon = c(14.42, 14.43, 14.41, 14.44),
  lat = c(40.82, 40.825, 40.818, 40.83),
  mag = c(1.2, 2.5, 0.8, 1.9),
  time = as.POSIXct(c("2025-01-01","2025-01-02","2025-01-03","2025-01-04"))
)

# bounding box around Vesuvius (center ± delta)
center <- c(lon = 14.4261, lat = 40.8214)
delta <- 0.025  # ~ ~2–3 km, tweak for area
bbox <- c(left = center["lon"] - delta,
          bottom = center["lat"] - delta,
          right = center["lon"] + delta,
          top = center["lat"] + delta)

# get Stamen tile (no API key needed)
map_tile <- get_stadiamap(bbox = bbox, zoom = 13, maptype = "stamen_terrain")

# plot
ggmap(map_tile) +
  geom_point(data = seismic, aes(x = lon, y = lat, size = mag, alpha = mag),
             color = "red") +
  scale_size_continuous(name = "Magnitude") +
  guides(alpha = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.05, "in"), pad_y = unit(0.35, "in")) +
  labs(title = "Seismic events near Mt Vesuvius",
       subtitle = "Points sized by magnitude")
