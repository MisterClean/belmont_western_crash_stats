# Load necessary libraries
library(dplyr)
library(readr)
library(geosphere)
library(ggplot2)
library(ggmap)
library(magrittr)
library(leaflet)

# Read the data
crashes <- read_csv("traffic_crashes.csv")

# Define the target point
target_point <- c(-87.688037, 41.939453) # longitude first, then latitude

# Function to calculate distance
calculate_distance <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) {
    return(NA)
  } else {
    return(distm(c(lon, lat), target_point, fun = distHaversine))
  }
}

# Vectorize the function
calculate_distance_vectorized <- Vectorize(calculate_distance)

# Add a new column for distance to each crash
crashes <- crashes %>%
  mutate(distance = calculate_distance_vectorized(LONGITUDE, LATITUDE))

# Filter crashes within 400 feet (approximately 122 meters)
crashes_nearby <- crashes %>%
  filter(distance <= 122)

# Add a new column for economic damages
crashes_nearby <- crashes_nearby %>%
  mutate(economic_damages = 11400 + (24000 * INJURIES_TOTAL) + (155000 * INJURIES_INCAPACITATING))

# Create the summary with the sum of economic damages
summary <- crashes_nearby %>%
  summarize(
    total_crashes = n(),
    sum_injuries = sum(INJURIES_TOTAL, na.rm = TRUE),
    sum_injuries_incapacitating = sum(INJURIES_INCAPACITATING, na.rm = TRUE),
    pedestrian_crashes = sum(FIRST_CRASH_TYPE == 'PEDESTRIAN', na.rm = TRUE),
    cyclists_crashes = sum(FIRST_CRASH_TYPE == 'PEDALCYCLIST', na.rm = TRUE),
    hit_and_run_crashes = sum(HIT_AND_RUN_I == 'Y', na.rm = TRUE),
    injuries_in_hit_and_run = sum(INJURIES_TOTAL * (HIT_AND_RUN_I == 'Y'), na.rm = TRUE),
    total_economic_damages = sum(economic_damages, na.rm = TRUE)  # Sum of economic damages
  )

# Print summary
print(summary)

crashes_aggregated <- crashes_nearby %>%
  group_by(LATITUDE, LONGITUDE) %>%
  summarize(count = n()) %>%
  ungroup()

# Define a function to scale the radius
scale_radius <- function(count) {
  sqrt(count) * 3  # Adjust this formula as needed
}

# Create the leaflet map
map <- leaflet(crashes_aggregated) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(
    ~LONGITUDE, 
    ~LATITUDE, 
    radius = ~scale_radius(count), 
    color = "#FF0000",  # Red color for the circles
    stroke = TRUE, 
    weight = 1,  # Adjust the stroke weight as needed
    fillColor = "#FF0000",
    fillOpacity = 0.6,
    colorOpacity = 0.8,  # Adjust the opacity of the stroke
    strokeColor = "#CC0000"
  ) %>%
  setView(lng = -87.688037, lat = 41.939453, zoom = 15)

# Print the map
map

