#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gather Weather Data: Code from Dan Watson
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(riem)
library(tidyverse)
library(geosphere)


# Load coordinates 
coords <- read_csv("Data/ARU_METADATA/aru_site_coordinates.csv")

# Look at networks in GA
stations <- riem_stations(network = "GA_ASOS")

# Function to find the closest station
match_coords_to_stations <- function(site_lat, site_lon, stations) {
  distances <- distHaversine(matrix(c(site_lon, site_lat), nrow = 1), 
                             matrix(c(stations$lon, stations$lat), ncol = 2))
  closest_station <- stations[which.min(distances), ]
  return(closest_station)
}

# Apply the function to each site in the "coords" dataframe
matched_stations <- coords %>%
  rowwise() %>%
  mutate(closest_station = list(match_coords_to_stations(lat, lon, stations))) %>%
  unnest_wider(closest_station, names_sep = "_")

### Getting daily weather data for each site ###

# Retrieve weather data for each station and filter by time range
get_weather_data <- function(station_id) {
  riem_measures(station = station_id, date_start = "2023-05-01", date_end = "2023-07-31") %>%
    mutate(time = hms::as_hms(valid),
           hour = hour(time)) %>%
    filter(hour >= 5 & hour <= 11) # Filter for 5:00 AM to 11:00 AM
}

weather_data <- matched_stations %>%
  distinct(site, closest_station_id) %>%
  rowwise() %>%
  mutate(weather = list(get_weather_data(closest_station_id))) %>%
  unnest(weather)

# Calculate daily averages after filtering
weather_data_summary <- weather_data %>%
  mutate(date = as.Date(valid)) %>%
  group_by(site, date) %>%
  summarise(
    avg_temp_f = mean(tmpf, na.rm = TRUE),
    avg_wind_speed_mph = mean(sknt * 1.15078, na.rm = TRUE), # Convert from knots to mph
    avg_wind_dir = mean(drct, na.rm = TRUE),
    total_precipitation_in = sum(p01i, na.rm = TRUE),
    avg_rel_humidity = mean(relh, na.rm = TRUE),
    avg_vis = mean(vsby, na.rm = TRUE),
    avg_altimeter = mean(alti, na.rm = TRUE)) %>%
  ungroup()

# Save the data to a CSV file
write_csv(weather_data_summary, "Data/CSVs/weather_station_daily_2023.csv")
