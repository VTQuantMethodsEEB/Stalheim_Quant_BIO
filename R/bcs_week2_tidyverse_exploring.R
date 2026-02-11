#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Title: This script explores my dataset using Tidyverse packages
#  Author: Ben Stalheim
#  Date: February, 4, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Required Packages
library(tidyverse)

# Load the R data file (it automatically assigns the name as bn_dat_allyears)
load("Data/RDS/bn_dat_allyears.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Tidyverse Exploration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filter ~~~~~~~~~~~~~~~~~~~~~

sparrow <- bn_dat_allyears |> 
  filter(common_name == "Bachman's Sparrow")
# This filters the dataset to only include detections of Bachman's Sparrow.
# But not all detections are correct, or of interest. I know that a confidence
# score threshold of 0.996 has to be applied.

sparrow_high_conf <- sparrow |> 
  filter(confidence >= 0.996)
# Now I have a dataset of only high-confidence Bachman's Sparrow detections.

# I could also set a universal confidence score threshold of 0.8 for all species.
bn_dat_filtered <- bn_dat_allyears |> 
  filter(confidence >= 0.8)

# Find the data where site is NA
missing_site_data <- bn_dat_filtered |> 
  filter(is.na(site))
# No missing site data in this dataset

# Summarize ~~~~~~~~~~~~~~~~~~~~~

# I might want to know how many species were detected at each site.
species_per_site <- bn_dat_filtered |> 
  group_by(site) |> 
  summarize(num_species = n_distinct(common_name)) |> 
  arrange(desc(num_species)) |> 
  print()

species_per_year <- bn_dat_filtered |> 
  group_by(year) |> 
  summarize(num_species = n_distinct(common_name)) |> 
  arrange(desc(num_species)) |> 
  print()


# Mutate ~~~~~~~~~~~~~~~~~~~~~

# Add an hour column based off the time column for cruder time-series analysis
bn_dat_filtered <- bn_dat_filtered |> 
  mutate(hour = lubridate::hour(time))

# Add Julian Day as a column
bn_dat_filtered <- bn_dat_filtered |> 
  mutate(julian_day = lubridate::yday(date))

# Pivot ~~~~~~~~~~~~~~~~~~~~~~~

# Turn the dataset into a wide dataset, first making a matrix
species_matrix <- bn_dat_filtered |> 
  group_by(common_name, site, date) |> 
  summarise(count = n()) |> 
  arrange(date) |> 
  print()

species_matrix_wide <- species_matrix |> 
  pivot_wider(names_from = common_name,
              values_from = count) |> 
  print()

# Shows raw counts by date (note that 2023 did not have as many sites, so less total count)
ggplot(species_matrix, aes(x = date, y = count)) +
  geom_col()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Some Updated Exploration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("Data/RDS/bn_data_thresholded.rds")

# There is no data from the Okefenokee for 2023, so I am removing it here for community analysis comparison
bn_data <- bn_data |> 
  filter(year != "2023")

# How many species are represented in the dataset
n_distinct(bn_data$common_name)

# How many species were detected at each location over all years
bn_data |> 
  group_by(location) |> 
  summarise(species_count = n_distinct(common_name)) |> 
  arrange(desc(species_count))

# How many species were detected at each location, broken down by years
bn_data |> 
  group_by(location, year) |> 
  summarise(species_count = n_distinct(common_name)) |> 
  arrange(desc(species_count))

# Species Matrices ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
species_matrix <- bn_data |> 
  group_by(common_name, date, site) |> 
  summarise(count = n()) |> 
  arrange(date) |> 
  print()

species_matrix_wide <- species_matrix |> 
  pivot_wider(names_from = common_name,
              values_from = count) |> 
  print()

# Species Summaries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
species_summary <- bn_data |> 
  group_by(common_name) |> 
  summarise(raw_detections = n(),
            days_detected = n_distinct(date),
            most_common_location = names(which.max(table(location))),
            most_common_year = names(which.max(table(year))),
            .groups = "drop") |> 
  arrange(desc(raw_detections)) |> 
  print()

species <- bn_data |> 
  filter(sp_code == "BACS") |> 
  group_by(location, year) |> 
  summarise(detection = n()) |> 
  print()

# Change as needed
abundant_species <- bn_data |> 
  filter(sp_code %in% c("CWWI", "EATO", "CONI")) |> 
  group_by(location, year) |> 
  summarise(detection = n()) |> 
  print()

# ~ 25% of my data are just Eastern Towhee detections
# And another 25% are just Common Nighthawk detections
# 65% of my data are CWWI, EATO, and CONI detections!!