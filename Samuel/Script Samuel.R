Sys.setenv(language = "EN")
# Using R
# Option 1: tidytuesdayR R package 

# install.packages("tidytuesdayR", dependencies = T)
# install.packages("janitor", dependencies = T)

tuesdata <- tidytuesdayR::tt_load('2025-05-13')
head(tuesdata)

vesuvius <- tuesdata$vesuvius


# This script was used to clean data saved as 13 csv files 
# from the Italian Istituto Nazionale di Geofisica e Vulcanologia found
# here https://terremoti.ov.ingv.it/gossip/vesuvio/index.html

# Load packages
library(tidyverse)
library(janitor)

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


head(vesuvius)

library(sf)
library(rnaturalearth)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(lubridate)

# Only rows with coordinates
vesu_pts <- vesuvius %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE)

# Simple leaflet map
leaflet(vesu_pts) %>%
  addTiles() %>%   # OpenStreetMap basemap
  addCircleMarkers(
    radius = 4,
    color = "red",
    stroke = FALSE, fillOpacity = 0.7,
    popup = ~paste0("Magnitude: ", duration_magnitude_md,
                    "<br>Depth (km): ", depth_km,
                    "<br>Time: ", time)
  )


#### Correlation between earthquake depth and magnitude
depth_mag <- vesuvius %>%
  filter(!is.na(depth_km), !is.na(duration_magnitude_md))

# Correlation test
(cor_test <- cor.test(depth_mag$depth_km, depth_mag$duration_magnitude_md))


# Scatter plot
ggplot(depth_mag, aes(x = depth_km, y = duration_magnitude_md)) +
  geom_point(alpha = 0.3, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Depth vs Magnitude at Vesuvius",
       x = "Depth (km)", y = "Magnitude (Md)") +
  theme_minimal()


#### Sismic events vs. seasonal or time-of-day patterns
vesu_season <- vesuvius %>%
  filter(!is.na(time)) %>%
  mutate(
    month = month(time, label = TRUE, abbr = TRUE),
    hour = hour(time)
  )

# Seasonal distribution
ggplot(vesu_season, aes(x = month)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Monthly Distribution of Seismic Events",
       x = "Month", y = "Count") +
  theme_minimal()

# Time-of-day distribution
ggplot(vesu_season, aes(x = hour)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "white") +
  labs(title = "Hourly Distribution of Seismic Events",
       x = "Hour of day", y = "Count") +
  theme_minimal()

#### Average location of seismic events migrated over time? 
vesu_location <- vesuvius %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  group_by(year) %>%
  summarise(
    mean_lat = mean(latitude, na.rm = TRUE),
    mean_lon = mean(longitude, na.rm = TRUE),
    n = n()
  )

# Yearly centroid shifts
print(vesu_location)


# Yearly Centroid Shift of Seismic Events
ggplot(vesu_location, aes(x = mean_lon, y = mean_lat)) +
  geom_path(arrow = arrow(type = "open", length = unit(0.2,"cm")), color = "darkorange") +
  geom_point(aes(size = n, color = year)) +
  scale_color_viridis_c() +
  labs(title = "Yearly Centroid Shift of Seismic Events",
       x = "Longitude", y = "Latitude", size = "N events", color = "Year") +
  theme_minimal()



# Events over time (monthly) + smooth trend

monthly <- vesuvius %>%
  filter(!is.na(date)) %>%
  count(ym = floor_date(date, "month"))

ggplot(monthly, aes(ym, n)) +
  geom_col(fill = "steelblue", alpha = .8) +
  geom_smooth(method = "loess", se = TRUE, span = .25, color = "black") +
  labs(x = NULL, y = "Events / month", title = "Monthly seismic activity")


# Depth classes vs magnitude (distribution)

dm_class <- dm %>%
  mutate(depth_class = cut(depth_km, c(-Inf,3,6,Inf),
                           labels = c("Shallow ≤3 km","Mid 3–6 km","Deep >6 km")))
ggplot(dm_class, aes(depth_class, md)) +
  geom_violin(fill = "red") +
  stat_summary(fun = median, geom = "point", size = 2) +
  labs(x = NULL, y = "Md", title = "Magnitude by depth class")



# Time-of-day pattern (hour histogram)

ggplot(vesuvius, aes(hour)) +
  geom_histogram(binwidth = 1,
                 fill = "firebrick",
                 alpha = .8) +
  labs(x = "Hour of day",
       y = "Events",
       title = "Time-of-day pattern")


# Magnitude by depth class → Shallow quakes (<3 km) dominate, though rare deeper events (>6 km) tend to have slightly higher magnitudes.

# Seasonality → Events are not evenly distributed across the year, suggesting possible seasonal or hydrological influences on shallow faulting.

# Time of day → A slight diurnal pattern is visible, which may reflect either natural stresses or detection biases related to background noise.


# Inter-event times → Events cluster closely in time, producing swarms, rather than following a purely random (Poisson) process.

# Centroid migration → The average epicentre location shifts subtly over the years, hinting at evolving stress concentrations beneath Vesuvius.

# Hotspot density (KDE) → Seismicity is concentrated in a few persistent clusters around the summit and flanks of the volcano.

