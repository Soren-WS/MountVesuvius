# Load packages and data ----
library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)

vesuvius <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')
str(vesuvius)

# Missingness ----
counts_NA <- colSums(is.na(vesuvius)) 
perc_NA <- colMeans(is.na(vesuvius)) *100

vesuvius_missingness <- data.frame(
  var      = colnames(vesuvius),
  counts_na = as.vector(counts_NA),
  perc_na   = as.vector(round(perc_NA, 1))
) %>%
  filter(perc_na > 0)
print(vesuvius_missingness)

# Plots ----
# Global plot settings
theme_set(theme_minimal())

vesuvius %>%
  filter(!is.na(duration_magnitude_md)) %>%
  ggplot()+
  geom_density(aes(x = time, 
                   fill = cut(duration_magnitude_md, breaks = c(-Inf, 0, 2, 4, Inf),
                              labels = c("Micro", "Unnoticeable", "Barely felt", "Uh Oh!"))), 
                   alpha = 0.5)+ 
  labs(
    x = "Time",
    y = "Earthquake Frequency",
    fill = "Intensity",    # if you map fill = grouping var
    title = "Vesuvius Earthquakes - How often and intense?",
  )+ 
  scale_fill_viridis_d(option = "magma")