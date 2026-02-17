#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Week 3 Assignment
# Author: Ben Stalheim
# Date: February, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages
library(tidyverse)
library(viridis) # For some fun colors
library(vegan)
library(ggthemes)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Load Necessary Dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data (This is my birdnet data that I filtered by applying species-specific
# confidence or logit score thresholds to).
load("Data/RDS/bn_data_thresholded.rds")

# Dataset containing recorded sound levels (dBFS) and known distances to singing Bachman's Sparrows
bacs_master_temp <- read_csv("Data/CSVs/bacs_master_temp.csv")

# 2025 Dataset containing all of the detected Bachman's Sparrows above the 95% threshold
bacs_2025_whole_year <- read_csv("Data/CSVs/bacs_2025_log_distance_predictions.csv")

# This contains my ARU effort for 2025. If a cell has an ARU name, that means that I 
# was able to extract data and it was confirmed to have been recording at the site. NA values
# indicate days where the ARU was not recording.
aru_effort <- read_csv("Data/ARU_METADATA/ARU_timeline_2025.csv") |>
  select(-"U-1") |>
  mutate(date = mdy(date)) |>
  pivot_longer(cols = -date, names_to = "aru_name", values_to = "surveyed") |>
  filter(!is.na(surveyed)) |>
  group_by(date) |>
  summarise(num_arus = n(), .groups = "drop")
# I am also prepping it for a plot here just to save some clutter below

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Plotting Exercies
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# One of my focuses is determining the vocalization patterns of the Bachman's Sparrow 
# and when they are detected the most. This is a monthly plot that shows the number
# of raw detections, but also gives some context with actual effort as the same number
# ARUs were not always recording the birds throughout the whole timeframe

# Bachman's Sparrow Detections 2025 ~~~~~~~~~~~~~~~
# Prepare detection data
bacs_2025_whole_year <- bacs_2025_whole_year |>
  mutate(month = month(date, label = TRUE)) |>
  filter(month != "Sep")

# Calculate monthly summaries
monthly_summary <- bacs_2025_whole_year |>
  left_join(aru_effort, by = "date") |>
  mutate(num_arus = replace_na(num_arus, 0)) |>
  # Get unique ARU count per date
  distinct(date, month, num_arus) |>
  group_by(month) |>
  summarise(
    total_aru_days = sum(num_arus),  # Sum of ARUs across all days in month
    .groups = "drop") |>
  # Now join back to get total detections
  left_join(
    bacs_2025_whole_year |> 
      group_by(month) |> 
      summarise(total_detections = n(), .groups = "drop"),
    by = "month") |>
  mutate(
    detections_per_aru_day = total_detections / total_aru_days,
    date = case_when(
      month == "Mar" ~ as.Date("2025-03-12"),
      month == "Apr" ~ as.Date("2025-04-01"),
      month == "May" ~ as.Date("2025-05-01"),
      month == "Jun" ~ as.Date("2025-06-01"),
      month == "Jul" ~ as.Date("2025-07-01"),
      month == "Aug" ~ as.Date("2025-08-01")),
    label = paste0("Total ARU-days: ", total_aru_days, "\n",
                   "Det/ARU-day: ", round(detections_per_aru_day, 3))) |> 
  print()

# Plot with annotations
ggplot(bacs_2025_whole_year, aes(x = date, fill = month)) +
  geom_histogram(color = "black", binwidth = 1, alpha = 0.9) +
  scale_fill_wsj() +
  geom_text(data = monthly_summary, 
            aes(x = date, y = Inf, label = label),
            vjust = 1.5, hjust = 0, size = 3.5, inherit.aes = FALSE) +
  facet_wrap(~month, scales = "free_x", ncol = 3, strip.position = "top") +
  scale_y_continuous(breaks = seq(0, 250, by = 50),
                     expand = expansion(mult = c(0, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Bachman's Sparrow Detections by Month in 2025",
    x = NULL,
    y = "Number of Detections",
    fill = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 12, vjust = -0.75),
        legend.position = "none",
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10))

# This figure shows how Bachman's Sparrows vary across months in the rate with which I
# detected them. They appear to vocalize most frequently during the spring (especially April)
# before quieting down in May and June. They then pick back up a little in July. This
# is really important information to help learn about their patterns and when to survey
# for the species.

# Species Richness by site and year ~~~~~~~~~~~~~~~~~~~~~
bn_data |>
  group_by(year, site) |>
  summarize(n_species = n_distinct(common_name), .groups = 'drop') |>
  ggplot(aes(x = site, y = n_species, fill = factor(year))) +
  geom_col(position = "dodge") +
  labs(x = "Site", y = "Number of Species", fill = "Year", title = "Observed Species Richness by Site and Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# This is a basic barplot showing how observed species richness changes over years
# of monitoring at all of my sampling locations. Site at the Okefenokee (O-sites) were
# not sampled in 2023, so they only have data from 2024 and 2025. I like looking at 
# this basic plot to get a feel for differences across locations and volatility over years.

# I need to convert my categorical variables to factors to get this theme to work
# This graph shows the relationship between recorded sound level using an ARU and 
# distance. I recorded Bachman's Sparrows of known distance and extracted their sound
# levels, which is present in this dataset under the dbfs column.
bacs_master_temp <- bacs_master_temp |> 
  mutate(treatment = factor(treatment, 
                            levels = c("okefenokee", "mine", "sansavilla")))

ggplot(data = bacs_master_temp, aes(x = distance, y = dbfs, color = treatment)) +
  geom_point(size = 2.5) +
  scale_color_few(labels = c("Okefenokee NWR", "Mission Mine", "Sansavilla WMA")) +  
  labs(x = "Distance (m)", y = "Decibels at Full Scale (dBFS)", 
       color = "Recording Locations",
       title = "Relationship Between Recorded Sound Level and Distance for Bachman's Sparrows") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray90", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = "lightgray"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.85),
        legend.justification = c(0.7, 0.75),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20))

# Same plot but with a line fit to the data points
ggplot(data = bacs_master_temp, aes(x = distance, y = dbfs)) +
  geom_point(aes(color = treatment), size = 2.5) +
  geom_smooth(method = "gam", se = T, color = "black") +
  scale_color_few(labels = c("Okefenokee NWR", "Mission Mine", "Sansavilla WMA")) +  
  labs(x = "Distance (m)", y = "Decibels at Full Scale (dBFS)", 
       color = "Recording Locations",
       title = "Relationship Between Recorded Sound Level and Distance for Bachman's Sparrows") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray90", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        panel.grid.minor = element_line(color = "lightgray"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.85),
        legend.justification = c(0.7, 0.75),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20))


# NMDS Plot (need to prep first) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
species_matrix_2 <- bn_data |> 
  group_by(site, sp_code) |> 
  summarise(count = n(), .groups = 'drop') |> 
  pivot_wider(names_from = sp_code, 
              values_from = count, 
              values_fill = 0)

species_mat <- species_matrix_2 |> 
  column_to_rownames("site") |> 
  as.matrix()

jaccard_dist <- vegdist(species_mat, method = "jaccard", binary = TRUE)

# NMDS using Jaccard
nmds_jaccard <- metaMDS(species_mat, distance = "jaccard", binary = TRUE, k = 2)
nmds_jaccard$stress  # Check stress

# Extract NMDS scores
nmds_scores <- as.data.frame(nmds_jaccard$points)
nmds_scores$site <- rownames(nmds_scores)
nmds_scores$location <- case_when(
  substr(nmds_scores$site, 1, 1) == "R" ~ "sansavilla",
  substr(nmds_scores$site, 1, 1) == "M" ~ "mine",
  substr(nmds_scores$site, 1, 1) == "O" ~ "okefenokee")

# Plot Jaccard NMDS 
ggplot(nmds_scores, aes(x = MDS1, y = MDS2, label = site, color = location)) +
  geom_point(size = 3) +
  geom_text(hjust = -0.2, vjust = 0.5) +
  stat_ellipse(aes(group = location), level = 0.99) +
  theme_minimal() +
  labs(title = "NMDS of Acoustic Communities (Jaccard)",
       subtitle = paste("Stress =", round(nmds_jaccard$stress, 3)),
       color = "Location")

# I like this plot because it shows the sites congregate in ordinal space. I honestly 
# need to learn more about the full meaning and way to interpret this plot, but I like
# how it clearly shows the differences in the bird communities from the various locations.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     More Plotting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What species were detected the most?
bn_data |> 
  group_by(common_name) |> 
  summarise(n_detections = n(), .groups = 'drop') |> 
  arrange(desc(n_detections)) |> 
  head(10) |> 
  ggplot(aes(x = reorder(common_name, n_detections), y = n_detections)) +
  geom_col(fill = "peachpuff4", color = "gray2") +
  coord_flip() +
  labs(x = "Species", y = "Number of Detections", title = "Top 10 Most Detected Species and Raw Detection Counts") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 11, color = "salmon3", angle = 0, hjust = 1.1))

# This is another plot that helps me quickly see what were the most commonly detected
# species. It also helps me see what species I should focus on as core species in the community.

# Bad Version of the Plot
bn_data |> 
  group_by(common_name) |> 
  summarise(n_detections = n(), .groups = 'drop') |> 
  arrange(desc(n_detections)) |> 
  head(10) |> 
  ggplot(aes(x = reorder(common_name, n_detections), y = n_detections)) +
  geom_col(fill = "pink3", color = "green") +
  coord_flip() +
  geom_line() +
  labs(x = "Species", y = "Number of Detections", title = "Top 10 Most Detected Species and Raw Detection Counts") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10, color = "salmon3", angle = 0, hjust = 0.5))  

# Histogram ~~~~~~~~~~~~~~~~~~~~~~~
# I care about Bachman's Sparrows and when they are most active...
sparrows <- bn_data |> 
  filter(sp_code == "BACS",
         year == 2025) |> 
  mutate(month = month(date, label = TRUE))

ggplot(sparrows, aes(x = date)) +
  geom_histogram(fill = "gray", color = "black") +
  facet_wrap(~month, scales = "free_x", ncol = 3) +
  scale_y_continuous(breaks = seq(0, 250, by = 50),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Bachman's Sparrow Detections by Month in 2025",
    x = NULL,
    y = "Detection Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 14))
# This shows me when they were detected the most. It shows that they were detected
# more in July than in June (mostly because an outlier day), which is really interesting actually!

# Heatmap Style:
sparrows <- bn_data |> 
  filter(sp_code == "BACS", year == 2025) |>
  mutate(hour = hour(time),  # extract hour from datetime
         date_only = date(date))

ggplot(sparrows, aes(x = date_only, y = hour, fill = ..count..)) +
  geom_bin2d(binwidth = c(1, 1)) +
  scale_fill_viridis_c(option = "plasma", name = "Detections") +
  labs(
    title = "Bachman's Sparrow Vocalizations by Time of Day (2025)",
    x = "Date",
    y = "Hour of Day (24-hour)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#      Turnover (Needs a little Prep)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# There is no data from the Okefenokee for 2023, so I am removing it here for community analysis comparison
bn_data2 <- bn_data |> 
  filter(year != "2023")

# Create presence/absence data by site, year, and species
species_presence <- bn_data2 |>
  group_by(site, year, sp_code, location) |>
  summarize(present = 1, .groups = 'drop') |>
  distinct()

sites_two_years <- species_presence |>
  group_by(site) |>
  summarize(n_years = n_distinct(year), .groups = 'drop') |>
  filter(n_years == 2) |>
  pull(site)

# Function to calculate Jaccard dissimilarity and Sorenson index for sites with two years (Okefenokee)
calc_jaccard_two_years <- function(site_name, data) {
  sp_2024 <- data |>
    filter(site == site_name, year == 2024) |>
    pull(sp_code) |>
    unique()
  
  sp_2025 <- data |>
    filter(site == site_name, year == 2025) |>
    pull(sp_code) |>
    unique()
  
  shared_24_25 <- length(intersect(sp_2024, sp_2025))
  total_unique_24_25 <- length(union(sp_2024, sp_2025))
  jaccard_sim_24_25 <- shared_24_25 / total_unique_24_25
  sorenson_sim_24_25 <- (2 * shared_24_25) / (length(sp_2024) + length(sp_2025))
  
  results <- data.frame(
    site = site_name,
    year_pair = "2024_to_2025",
    n_yr1 = length(sp_2024),
    n_yr2 = length(sp_2025),
    shared = shared_24_25,
    total_unique = total_unique_24_25,
    jaccard_similarity = jaccard_sim_24_25,
    jaccard_dissimilarity = 1 - jaccard_sim_24_25,
    sorenson_similarity = sorenson_sim_24_25,
    sorenson_dissimilarity = 1 - sorenson_sim_24_25)
  
  return(results)
}

# Function to calculate Nichols turnover for sites with two years
calc_nichols_two_years <- function(site_name, data) {
  sp_2024 <- data |>
    filter(site == site_name, year == 2024) |>
    pull(sp_code) |>
    unique()
  
  sp_2025 <- data |>
    filter(site == site_name, year == 2025) |>
    pull(sp_code) |>
    unique()
  
  shared_24_25 <- length(intersect(sp_2024, sp_2025))
  turnover_24_25 <- 1 - (shared_24_25 / length(sp_2025))
  
  results <- data.frame(
    site = site_name,
    year_pair = "2024_to_2025",
    nichols_turnover = turnover_24_25,
    shared = shared_24_25,
    n_later_year = length(sp_2025))
  
  return(results)
}

# Calculate metrics for sites with two years of data (Okefenokee)
jaccard_results <- map_df(sites_two_years, ~calc_jaccard_two_years(.x, species_presence))
nichols_results <- map_df(sites_two_years, ~calc_nichols_two_years(.x, species_presence))

# Combine all results into one table
turnover_table <- jaccard_results |>
  left_join(nichols_results |> select(site, year_pair, nichols_turnover), by = c("site", "year_pair")) |>
  left_join(species_presence |> select(site, location) |> distinct(), by = "site") |>
  arrange(year_pair, desc(nichols_turnover)) |> 
  print()

# Summary statistics by location and year pair
turnover_summary <- turnover_table |>
  group_by(location, year_pair) |>
  summarize(
    n_sites = n(),
    mean_jaccard_dissim = mean(jaccard_dissimilarity),
    sd_jaccard_dissim = sd(jaccard_dissimilarity),
    mean_sorenson_dissim = mean(sorenson_dissimilarity),
    sd_sorenson_dissim = sd(sorenson_dissimilarity),
    mean_nichols_turnover = mean(nichols_turnover),
    sd_nichols_turnover = sd(nichols_turnover),
    .groups = 'drop') |> 
  print()

# Visualizations of Turnover ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Jaccard vs Nichols turnover scatter
ggplot(turnover_table, aes(x = jaccard_dissimilarity, y = nichols_turnover)) +
  geom_point(aes(color = location), size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", alpha = 0.4) +
  scale_color_viridis(discrete = T, option = "viridis") +
  labs(x = "Jaccard Dissimilarity", y = "Nichols Turnover", color = "Location") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Boxplot of Turnover by Location (with raw data points)
ggplot(turnover_table, aes(x = location, y = nichols_turnover, fill = location)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, size = 2, alpha = 0.3) +
    scale_fill_manual(values = c("brown3", "skyblue", "lightgreen")) +
    labs(title = "Avian Community Turnover Among Locations Between 2024 and 2025", x = NULL, y = "Turnover") +
    theme(legend.position = "none",
          plot.title = element_text(size = 14, hjust = 0.5),
          axis.title = element_text(size = 14), 
          axis.text = element_text(size = 12, color = "black"))

# Boxplot of Jaccard Dissimilarity by Location (with raw data points)
ggplot(turnover_table, aes(x = location, y = jaccard_dissimilarity)) +
  geom_boxplot(aes(fill = location), alpha = 0.7) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.3) +
  scale_fill_manual(values = c("brown3", "skyblue", "lightgreen")) +
  labs(title = "Jaccard Dissimilarity Index Among Locations Between 2024 and 2025", x = NULL, y = "Turnover") +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12, color = "black"))

# These Box Plots show how Turnover and Jaccard Dissimilarity (Index for Turnover) 
# vary among sites and locations. My hypothesis was that mine sites would have higher 
# turnover rates than the other locations, because the community is less stable as vegetation
# regrows post-mining. These two approaches measure communities slightly differently, which is
# why there are some differences, but it clearly shows that mine sites have elevated 
# turnover rates. 
