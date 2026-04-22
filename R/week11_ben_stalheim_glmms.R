#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Title: GLMMs: Daily Species Richness ~ Ecosystem and Disturbance Age 
#  Author: Ben Stalheim
#  Date: April, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages
library(tidyverse)
library(glmmTMB)
library(lme4)
library(ggeffects)
library(emmeans)
library(DHARMa)
library(ggthemes)

# Load data
load("Data/RDS/bn_dat_filtered_95.rds")

# ~~~~~~~~~~~~~~~~~~~~~~ Hypothesis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# **My hypothesis** is that daily species richness of birds varies between my sampling
# locations, and is also affected by disturbance age (years since a disturbance event).
# I predict that years since a disturbance will have a negative effect on daily species
# richness across locations. I also predict daily species richness to be lowest at
# the mining location. 

# I will be using GLMMs to model the number of species detected each day as a function
# of the sampling location and disturbance age. 

# **Note:** I am using naive daily species richness counts for these models. I am not
# using estimators or modeling for imperfect detection to obtain estimated counts for
# each day. For this, I am simply using the observed daily richness as detected using
# acoustic surveys. For all daily surveys, species were surveyed an estimated 67 minutes
# each day at each site. 

# **Note:** For days where sampling did not occur (ARU was not active), these days
# are omitted from the data and are not part of the modeling process.

# ~~~~~~~~~~~~~~~~~~ Diagnostic prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# How many unique sites per location?
bn_dat_filtered_95 |> 
  distinct(site, location) |> 
  count(location)

# How does yrs_since_disturbance vary across sites and years?
bn_dat_filtered_95 |>
  distinct(site, location, year, yrs_since_disturbance) |>
  arrange(location, site, year) |>
  print(n = 50)

# Check the range of disturbance ages
bn_dat_filtered_95 |>
  group_by(location) |>
  summarise(
    min_yrs = min(yrs_since_disturbance),
    max_yrs = max(yrs_since_disturbance),
    n_sites = n_distinct(site),
    n_years = n_distinct(year))

# ~~~~~~~~~~~~~~~~~~ Create daily species richness summary ~~~~~~~~~~~~~~~~~~~~~

# Create mean daily species richness summary
daily_rich_summary <- bn_dat_filtered_95 |> 
  group_by(site, location, date, yrs_since_disturbance) |> 
  summarise(n_species = n_distinct(common_name),
            n_detections = n(),
            .groups = "drop_last") |> 
  mutate(location = as_factor(location)) |> 
  print()

location_rich_summary <- bn_dat_filtered_95 |> 
  group_by(location, date) |> 
  summarise(n_species = n_distinct(common_name),
            n_detections = n(),
            .groups = "drop_last") |> 
  mutate(location = as_factor(location)) |> 
  print()

# Adding in year and julian day
daily_rich_summary <- daily_rich_summary |> 
  mutate(julian_day = yday(date),
         year = as_factor(year(date)))

location_rich_summary <- location_rich_summary |> 
  mutate(julian_day = yday(date),
         year = as_factor(year(date)))

mean_summary <- daily_rich_summary |> 
  group_by(location) |> 
  summarise(mean_daily_richness = mean(n_species)) |> 
  print()

# ~~~~~~~~~~~~~~~~~~~~~~ Explore with some plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Disturbance Age
ggplot(daily_rich_summary, aes(x = yrs_since_disturbance, y = n_species, color = location)) +
  geom_jitter() +
  geom_smooth(method = "glm", color = "red", fill = "purple", alpha = 1) +
  facet_grid(~ location) +
  scale_color_colorblind() +
  labs(title = "Effect of Disturbance Age on Daily Species Richness",
       y = "Observed Daily Species Richness",
       x = "Years Since Disturance Event") +
  theme_bw() +
theme(legend.position = "none")
# Shows signs of having an effect, especially varying by location

# Julian Day
ggplot(daily_rich_summary, aes(x = julian_day, y = n_species, color = location)) +
  geom_jitter() +
  geom_smooth(method = "glm", color = "red", fill = "purple", alpha = 1) +
  facet_grid(~ location) +
  scale_color_colorblind() +
  labs(title = "Effect of Julian Day on Daily Species Richness",
       y = "Observed Daily Species Richness",
       x = "Julian Day") +
  theme_bw() +
  theme(legend.position = "none")
# Julian day doesn't appear to have a major effect. This is good, because I have
# assumed that my sampling window (month of June) is a closed season and should not
# impact my detection probability or estimates of species.

# Faceted by location and effect of year (grouping estimates at survey site to location level)
ggplot(location_rich_summary, aes(x = 1, y = n_species, color = factor(year))) +
  geom_jitter(width = 0.1, alpha = 0.7) +
  facet_grid(~ location) +
  scale_color_colorblind() +
  labs(
    title = "Daily Species Richness by Location",
    x = NULL,
    y = "Observed Daily Species Richness",
    color = "Year") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
# This shows that daily species richness appears generally highest at Okefenokee,
# followed by Sansavilla (with a wide spread) and last is the Mine. The good thing
# is that daily species richness does not appear to vary much between years, so this
# doesn't appear to be driving changes in daily species richness.

# Does the number of daily species detected influence the number of daily detections total?
ggplot(daily_rich_summary, aes(x = n_species, y = n_detections, color = location)) +
  geom_jitter() +
  geom_smooth(method = "glm", color = "red", fill = "purple", alpha = 1) +
  facet_grid(~ location) +
  scale_color_colorblind() +
  labs(title = "Relationship Between Number of Species and Detections",
       y = "Total Number of Detections",
       x = "Observed Daily Species Richness") +
  theme_bw() +
  theme(legend.position = "none")

# ~~~~~~~~~~~~~~~~~ Model effects ~~~~~~~~~~~~~~~~~~~~~~~~~
# Modeling number of species detected each day ~ location AND yrs_since_disturbance

# NUll model
m0 <- glmmTMB(n_species ~ 1 + (1|site),
              data = daily_rich_summary, family = compois)
summary(m0)

# Location as only predictor
m1 <- glmmTMB(n_species ~ location + (1|site),
              data = daily_rich_summary, family = compois)
summary(m1)
plot(ggpredict(m1, terms = c("location"), bias_correction = TRUE))

# M2: Location + disturbance age (additive model)
m2 <- glmmTMB(n_species ~ location + yrs_since_disturbance + (1|site),
              data = daily_rich_summary, family = compois)
summary(m2)
plot(ggpredict(m2, terms = c("yrs_since_disturbance", "location")))

# Location * disturbance age (interactive model)
m3 <- glmmTMB(n_species ~ location * yrs_since_disturbance + (1 | site),
              data = daily_rich_summary, family = compois)
summary(m3)
plot(ggpredict(m3, terms = c("yrs_since_disturbance", "location")))

# Interactive model (but including random slope with disturbance age)
m4 <- glmmTMB(n_species ~ location * yrs_since_disturbance + 
                (yrs_since_disturbance | site),
              data = daily_rich_summary, family = compois)
summary(m4)
plot(ggpredict(m4, terms = c("yrs_since_disturbance", "location")))

emmeans(m4, pairwise ~ location, type = "response")
emtrends(m4, specs = ~ location ,var = "yrs_since_disturbance",
         type = "response")

# Compare with AIC 
AIC(m0, m1, m2, m3, m4) |> 
  arrange(AIC)
# Based on AIC output, m4 is my best model. I need to run diagnostic checks on this
# model to make sure it passes basic tests.

testDispersion(m4)
# It still appears to be slightly underdispersed, but still better than when using
# just a poisson distribution or negative binomial (nbinom2).

simulationOutput <- simulateResiduals(fittedModel = m4, plot = T)

