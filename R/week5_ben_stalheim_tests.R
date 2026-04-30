#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Week 5 Script
# Author: Ben Stalheim
# Date: February, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(wesanderson) # Fun Colors
#devtools::install_github("G-Thomson/Manu")# To download the NZ bird color scheme
library(Manu) # Even More Fun Colors (NZ Birds), so cool, thanks Kate!
library(ggthemes)

# Load R data file
load("Data/RDS/bn_dat_filtered_95.rds") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Hypotheses ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Permutation: Mean species richness per monitoring point is greater at 
#              Sansavilla WMA (early successional longleaf pine forest) than at 
#              Mission Mine (reclaimed heavy mineral surface mine).
# 
# Classic Test: There is a negative correlation between the number of detections/day
#               of Northern Bobwhite and years since disturbance (reclamation) at the Mission Mine.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Permutation Test ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create a dataframe that stores species richness per point (n=10 @ Sansavilla, n=9 @ Mine)
richness_point <- bn_dat_filtered_95 |>
  filter(location %in% c("sansavilla", "mine")) |>
  group_by(location, site) |>
  summarise(richness = n_distinct(sp_code), .groups = "drop") |> 
  print()

# Store the values
san  <- richness_point$richness[richness_point$location == "sansavilla"]
mine <- richness_point$richness[richness_point$location == "mine"]

# Calculate the observed mean difference per point
obs <- mean(san) - mean(mine)
obs

# Run the Permutations:
set.seed(3)
# We need the lengths:
n_san  <- length(san)
n_mine <- length(mine)
# Combine it all together:
combined <- c(san, mine)
# To store the results:
res <- numeric(10000)

# For Loop: Structured borrowed from class, adapted to my data
for (i in 1:10000) {
  boot      <- sample(combined)
  sansaboot <- boot[1:n_san]
  mineboot  <- boot[(n_san + 1):length(combined)]
  res[i]    <- mean(sansaboot) - mean(mineboot)
}

# Results ~~~~~~~~~~~~~~~~~~~~~~~~~

# Setting Colors
col <- wes_palette("IsleofDogs1")

# Store the residuals in a dataframe
res_df <- data.frame(difference = res)

# Plot:
ggplot(res_df, aes(x = difference)) +
  geom_histogram(fill = col[1], color = col[5], bins = 50) +
  geom_vline(xintercept = obs, color = "red", linewidth = 1) +
  labs(
    title = NULL,
    x = "Residuals (Sansavilla - Mine)",
    y = "Count") +
  theme_classic()

# Calculating the P-Value:
mean(res>=obs)   

# This shows a p-value of 0.0013, a significant result. This shows that observed mean difference
# in species per point between Sansavilla WMA and the Mission Mine is unlikely to have resulted
# by chance.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Classic Test ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Setting Color Scheme
names(manu_palettes)
pal <- get_pal("Kereru")
print_pal(pal)

# Produce color gradient for 9 sites:
selected_colours <- get_pal("Kaka")[c(1, 3, 5)]
site_pal <- colorRampPalette(selected_colours)(9)

# Summarise: mean NOBO detections per point per survey day at Mine only ~~~~~

# Firsy, filter and get detection data
daily_counts <- bn_dat_filtered_95 |>
  filter(location == "mine", sp_code == "NOBO") |>
  group_by(site, yrs_since_disturbance, date) |>
  summarise(daily_detections = n(), .groups = "drop") |> 
  print()

# Run some calculations, grouping by site to account for variability
nobo_mine <- bn_dat_filtered_95 |>
  filter(location == "mine", sp_code == "NOBO") |>
  group_by(site, yrs_since_disturbance) |>
  summarise(
    total_detections = n(),
    mean_detections_day = total_detections / 61,
    .groups = "drop") |>
  arrange(desc(mean_detections_day)) |> 
  print()

# Visualize to get an idea of the relationship:
ggplot(nobo_mine, aes(x = yrs_since_disturbance, y = mean_detections_day, color = site)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = pal[3], fill = pal[5]) +
  scale_color_manual(values = site_pal) +
  labs(
    x = "Years Since Mine Reclamation",
    y = "Mean Detections/Day of NOBO") +
  theme_solarized(light=F)

ggplot(nobo_mine, aes(x = yrs_since_disturbance, y = mean_detections_day, color = site)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = F, color = pal[3], fill = pal[5]) +
  scale_color_brewer(palette = "Paired") +
  labs(
    x = "Years Since Mine Reclamation",
    y = "Mean Detections/Day of NOBO") +
  theme_solarized(light=T)

# The plot shows a negative association between the mean detections/day for Bobwhite
# and years since disturbance (reclamation). This aligns with my hypothesis,
# but still needs to be tested.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Correlation Test
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check normality before choosing test
shapiro.test(nobo_mine$mean_detections_day)
shapiro.test(nobo_mine$yrs_since_disturbance)

# Some of my data appears to be non-normally distributed, so I am using a Spearman
# Rank Correlation Test instead of a Pearson Correlation Test.

sp <- cor.test(
  nobo_mine$yrs_since_disturbance,
  nobo_mine$mean_detections_day,
  method = "spearman")

sp

# The Spearman test showed a significant negative relationship (rho = –0.449, p = 0.0189), 
# meaning detections tended to decrease as years since disturbance increased.
# Northern Bobwhite appear to prefer areas shortly after reclamation.

