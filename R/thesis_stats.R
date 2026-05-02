#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Thesis and other random stats to calculate to 
#     help with writing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages
library(tidyverse)
library(MASS)
library(lme4)

load("Data/RDS/bn_dat_filtered_95.rds")

bn_data <- bn_dat_filtered_95 |> 
  mutate(site_year = paste(site, year, sep = "_"))
# This is my dataset. Currently it only has bird detections made during June
# in 2023, 2024, and 2025 (2024 and 2025 only at Okefenokee). 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Birds ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# **Number of detections (of species kept)**: 670,982 

# **Most commonly detected species at each location**
bn_data |>
  group_by(location, year, common_name) |>
  summarise(detections = n(), .groups = "drop_last") |>
  slice_max(order_by = detections, n = 2) |>
  arrange(location, year, desc(detections))
# At the Mine, Common Nighthawk, Northern Bobwhite, and Chuck-will's-widow are the 
# most commonly detected species. At Okefenokee, Common Nighthawk, Eastern Towhee,
# and Mourning Dove are. And at Sansavilla, Common Nighthawk, Eastern Towhee, and Chuck-
# will's-widow are most detected.

detections <- bn_data |> 
  group_by(common_name) |> 
  summarise(detections = n())
# Species with the fewest detections and dates, problems for modeling....
bn_data |> 
  group_by(location, year, common_name) |>
  summarise(detections = n(), 
            n_days_detected = n_distinct(date),
            .groups = "drop") |> 
  arrange(year, detections) |> 
  print(n = 30)

# **Number of species detected during study**: 44 
bn_data |> 
  summarise(n_species = n_distinct(common_name))

# **Number of species detected at each location during study**
bn_data |> 
  group_by(location) |> 
  summarise(n_species = n_distinct(common_name)) |> 
  print(n = Inf)
# Mine = 31
# Okefenokee = 35
# Sansavilla = 40

# Number of species by year and location (Gamma diversity)
bn_data |> 
  group_by(location, year) |> 
  summarise(n_species = n_distinct(common_name)) 

# mine        2023        27
# mine        2024        27
# mine        2025        30
# okefenokee  2024        33
# okefenokee  2025        33
# sansavilla  2023        40
# sansavilla  2024        33
# sansavilla  2025        34

# The Mine picked up a few species in 2025. Okefenokee stayed the same over 2 years. 
# And all of the species were detected at Sansavilla in 2023, which those numbers
# settling down the following years. 

# What species had the lowest confidence score thresholds?
bn_data |> 
  group_by(common_name) |> 
  summarise(min_conf_score = min(confidence)) |> 
  arrange(min_conf_score)
# Carolina Wren, Common Nighthawk, Mourning Dove, Blue-gray Gnatcatcher, Eastern Towhee


# What species had the highest min confidence score thresholds?
bn_data |> 
  group_by(common_name) |> 
  summarise(min_conf_score = min(confidence)) |> 
  arrange(desc(min_conf_score))
# Bachman's Sparrow, Red-cockaded Woodpecker, Barred Owl, Purple Martin, Eastern Screech-Owl

# What foraging guilds were detected the most?
guilds <- bn_data |>  
  group_by(foraging_a, location) |> 
  summarise(n_species = n_distinct(common_name)) |> 
  arrange(desc(n_species)) |> 
  print()
# Insectivores were the most common at all locations, then omnivores, carnivores, and granivores.

# Foraging strategies? E.g., ground forager, etc...
guilds_b <- bn_data |>  
  group_by(foraging_b, location) |> 
  summarise(n_species = n_distinct(common_name)) |> 
  arrange(foraging_b, desc(n_species)) |> 
  print()
# The one that stands out here is that Sansavilla has quite a few more lower canopy
# gleaner's compared to other locations. Okefenokee has more bark gleaners


# How does each species respond do disturbance at the different sites?
# Now I first need to build a complete detection history for all site years that does
# not include only detections, but site-years where species were not detected too.
all_species <- bn_data |> distinct(common_name)
all_site_years <- bn_data |> 
  distinct(site, year, location, yrs_since_disturbance)
all_combos <- crossing(all_species, all_site_years)
days_surveyed <- bn_data |>
  distinct(site, year, date) |>
  group_by(site, year) |>
  summarise(days_surveyed = n(), .groups = "drop") |> 
  filter(days_surveyed >= 28) # Removes O-4 in 2025(7 days) and M-2 in 2023 (21 days)
det_summary <- bn_data |>
  group_by(common_name, site, year, location, yrs_since_disturbance) |>
  summarise(detections = n(),
    n_days_detected = n_distinct(date),
    .groups = "drop")
species_dist_history <- all_combos |>
  left_join(det_summary,
            by = c("common_name", "site", "year", "location", "yrs_since_disturbance")) |>
  mutate(detections = replace_na(detections, 0),
    n_days_detected = replace_na(n_days_detected, 0)) |>
  arrange(common_name, site, year) 
species_dist_history <- species_dist_history |>
  left_join(days_surveyed, by = c("site", "year")) |> 
  filter(!is.na(days_surveyed)) |>
  mutate(detections_day = detections/days_surveyed) |> 
  print()
# There is a lot of variation and a lot of information here. This is the crux of my 
# thesis and what my hypotheses are going to center around. 

# Just to explore certain species relationships
sp_history <- species_dist_history |> 
  filter(common_name == "Eastern Towhee")

ggplot(sp_history, aes(x = yrs_since_disturbance, y = detections_day, color = location)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = T) +
  facet_wrap(~ location) +
  theme_bw()

sp_mod <- lmer(detections_day ~ yrs_since_disturbance*location + (1|site),
               data = sp_history)
summary(sp_mod)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        ARUs and Effort ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# **Total recording hours of the study**: 2,392 hours
bn_data |> 
  distinct(site_year, date, daily_effort_min) |>
  summarise(total_recording_hours = sum(daily_effort_min) / 60)

# Effort by site-year
site_year_effort <- bn_data |>
  distinct(site_year, date, daily_effort_min) |>
  group_by(site_year) |>
  summarise(total_recording_hours = sum(daily_effort_min) / 60) |> 
  print()

ggplot(site_year_effort, aes(x = site_year, y = total_recording_hours)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Site-Year",
    y = "Total Recording Hours",
    title = "Total ARU Recording Hours per Site-Year") +
  theme_minimal()
# Most of my sites had the same ammount of aru recording effort. Two sites stand out
# as problematic: O-4 in 2025 (7 hours) and M-2 in 2023 (23.7 hours). All other site-years
# were over at least 30 hours of effort during the month of June.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Environment ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mean temperature during our study in month of June?# **Mean temp in June**: 71.9°F
bn_data |> 
  group_by(year, location) |> 
  summarise(mean_temp = mean(avg_temp_f, na.rm = T))
# Temperature is about the same everywhere each year. 


# Wind Speed: Not a lot of wind either, 0 - 3.3 mph on average
bn_data |> 
  group_by(location) |> 
  summarise(mean_wind = mean(avg_wind_speed_mph, na.rm = T))

# Relative Humidity
bn_data |> 
  #group_by(location) |> 
  summarise(mean_humidity = mean(avg_rel_humidity, na.rm = T))
# Humidity at Mine and Okefenokee averages about 95%. Slightly lower at Sansavilla (90%)


# Disturbance histories
bn_data |> 
  group_by(location, site) |> 
  summarise(min_dist_age = min(yrs_since_disturbance),
            max_dist_age = max(yrs_since_disturbance),
            mean_dist_age = mean(yrs_since_disturbance)) |> 
  arrange(location, mean_dist_age) |> 
  print(n = Inf)
# The Mine has the oldest dist age points. Okefenokee has youngest. Sansavilla has
# a wide range, but still skews on younger side. 
