#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Title: This script explores my dataset using Tidyverse packages
#  Author: Ben Stalheim
#  Date: February, 4, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Required Packages
library(tidyverse)

# Load the R data file
bn_dat <- readRDS("data/birdnet_data.rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Tidyverse Exploration
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Filter ~~~~~~~~~~~~~~~~~~~~~

sparrow <- bn_dat |> 
  filter(common_name == "Bachman's Sparrow")
# This filters the dataset to only include detections of Bachman's Sparrow.
# But not all detections are correct, or of interest. I know that a confidence
# score threshold of 0.996 has to be applied.

sparrow_high_conf <- sparrow |> 
  filter(confidence >= 0.996)
# Now I have a dataset of only high-confidence Bachman's Sparrow detections.

# I could also set a universal confidence score threshold of 0.8 for all species.
bn_dat_filtered <- bn_dat |> 
  filter(confidence >= 0.8)

# Find the data where site is NA
missing_site_data <- bn_dat_filtered |> 
  filter(is.na(site))
# I actually need to figure out where this data came from eventually...

# Summarize ~~~~~~~~~~~~~~~~~~~~~

# I might want to know how many species were detected at each site.
species_per_site <- bn_dat_filtered |> 
  group_by(site) |> 
  summarize(num_species = n_distinct(common_name)) |> 
  arrange(desc(num_species))
species_per_site

species_per_year <- bn_dat_filtered |> 
  group_by(year) |> 
  summarize(num_species = n_distinct(common_name)) |> 
  arrange(desc(num_species))
species_per_year

# Mutate ~~~~~~~~~~~~~~~~~~~~~

# Add an hour column based off the time column for cruder time-series analysis
bn_dat_filtered <- bn_dat_filtered |> 
  mutate(hour = lubridate::hour(time))

# Add Julian Day as a column
bn_dat_filtered <- bn_dat_filtered |> 
  mutate(julian_day = lubridate::yday(date))
