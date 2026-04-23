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
library(lubridate)

# Load data
load("Data/RDS/bn_dat_filtered_95.rds")

# ~~~~~~~~~~~~~~~~~~~~~~ Hypothesis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# **My hypothesis** is that alpha diversity (i.e., species richness at local, or survey point
# level) of birds varies between my sampling locations, and is also affected by 
# disturbance age (years since a disturbance event). I predict that years since 
# a disturbance will have a negative effect on daily species richness across locations. 
# I also predict daily species richness to be lowest at the mining location. 

# I will be using GLMMs to model alpha diversity as a function of the sampling 
# location and disturbance age. 

# **Note:** I am using naive alpha diversity counts for these models. I am not
# using estimators or modeling for imperfect detection to obtain estimated counts.
# For this, I am simply using the observed richness at each survey point over the sampling
# season (30 days in June for all years) 

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
alpha_rich_summary <- bn_dat_filtered_95 |> 
  group_by(site, location, year, yrs_since_disturbance) |> 
  summarise(n_species = n_distinct(common_name),
            n_detections = n(),
            .groups = "drop_last") |> 
  mutate(location = as_factor(location)) |> 
  filter(!(site == "O-4" & year == 2025)) |>  # removing O-4 from 2025 because it had so few survey days
  print()

daily_rich_summary <- bn_dat_filtered_95 |> 
  group_by(site, location, date, yrs_since_disturbance) |> 
  summarise(n_species = n_distinct(common_name),
            n_detections = n(),
            .groups = "drop_last") |> 
  mutate(location = as_factor(location)) |> 
  print()

location_rich_summary <- bn_dat_filtered_95 |> 
  group_by(location, year) |> 
  summarise(n_species = n_distinct(common_name),
            n_detections = n(),
            .groups = "drop_last") |> 
  mutate(location = as_factor(location)) |> 
  print()

# Adding in year and julian day
daily_rich_summary <- daily_rich_summary |> 
  mutate(julian_day = yday(date),
         year = as_factor(year(date)))

mean_summary <- daily_rich_summary |> 
  group_by(location) |> 
  summarise(mean_daily_richness = mean(n_species)) |> 
  print()

# ~~~~~~~~~~~~~~~~~~~~~~ Explore with some plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Disturbance Age
ggplot(alpha_rich_summary, aes(x = yrs_since_disturbance, y = n_species, color = location)) +
  geom_jitter() +
  geom_smooth(method = "glm", color = "red", fill = "purple", alpha = 0.2) +
  facet_grid(~ location) +
  scale_color_colorblind() +
  labs(title = "Effect of Disturbance Age on Alpha Diversity",
       y = "Observed Species Richness at Survey Point",
       x = "Years Since Disturance Event") +
  theme_bw() +
theme(legend.position = "none")
# Alpha diversity appears to increase slightly as disturbance age increases at the 
# Mission Mine. Meanwhile, alpha diversity decreases with disturbance age at the two
# non-mining locations.

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
# This was for a different analysis, but just keeping it in here. This is daily species
# richness at each survey point (finer scale than alpha diversity) to see if Julian
# Day has any effect on detected species counts. It appears to not.

# Faceted by location and effect of year (grouping estimates at survey site to location level)
ggplot(location_rich_summary, aes(x = 1, y = n_species, color = factor(year))) +
  geom_jitter(size = 4, width = 0, height = 0.4, alpha = 0.8) +
  facet_grid(~ location) +
  scale_color_colorblind() +
  labs(
    title = "Gamma Diversity by Location and Year",
    x = NULL,
    y = "Gamma Diversity (Species Richness over Sampling Season)",
    color = "Year") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
# Gamma diversity (i.e., species richness estimates for each location, adding up each
# survey point). Gamma diversity is lowest at the mine, intermediate at Okefenokee (interesting
# because alpha diversity is highest here), while gamma diversity is highest at Sansavilla.

# Does alpha diversity influence the number of daily detections total?
ggplot(alpha_rich_summary, aes(x = n_species, y = n_detections, color = location)) +
  geom_jitter() +
  geom_smooth(method = "glm", color = "red", fill = "purple", alpha = 0.4) +
  facet_grid(~ location) +
  scale_color_colorblind() +
  labs(title = "Effect of Alpha Diversity on Raw Detection Data",
       y = "Total Number of Detections",
       x = "Observed Species Richness at Survey Point") +
  theme_bw() +
  theme(legend.position = "none")
# The more species detected, the more number of detections...makes sense.

# ~~~~~~~~~~~~~~~~~ Model effects ~~~~~~~~~~~~~~~~~~~~~~~~~
# Modeling alpha diversity ~ location AND yrs_since_disturbance

# M0: NUll model
m0 <- glmmTMB(n_species ~ 1 + (1|site),
              data = alpha_rich_summary, family = compois)
summary(m0)

# M1: Location as only predictor
m1 <- glmmTMB(n_species ~ location + (1|site),
              data = alpha_rich_summary, family = compois)
summary(m1)
plot(ggpredict(m1, terms = c("location"), bias_correction = TRUE))
emmeans(m1, pairwise ~ location, type = "response", re.form = NA)

# This model is showing that alpha diversity is significantly lower at the Mission
# Mine than Sansavilla or Okefenokee. There is no significant difference in alpha
# diversity between the Okefenokee and Sansavilla.


# M2: Location + disturbance age (additive model)
m2 <- glmmTMB(n_species ~ location + yrs_since_disturbance + (1|site),
              data = alpha_rich_summary, family = compois)
summary(m2)
plot(ggpredict(m2, terms = c("yrs_since_disturbance", "location")))
emmeans(m2, pairwise ~ location, type = "response")


# M3: Location * disturbance age (interactive model)
m3 <- glmmTMB(n_species ~ location * yrs_since_disturbance + (1 | site),
              data = alpha_rich_summary, family = compois)
summary(m3)
plot(ggpredict(m3, terms = c("yrs_since_disturbance", "location")))
emmeans(m3, pairwise ~ location, type = "response")
emtrends(m3, specs = ~ location ,var = "yrs_since_disturbance",
         type = "response")
emtrends(m3, pairwise ~ location, var = "yrs_since_disturbance", type = "response")

# This interactive model shows that there is still significantly lower alpha diversity
# at the Mission Mine than Okefenokee and Sansavilla. There is still no difference
# between those two locations. There is also a significant difference in the slope
# of line between Sansavilla and the Mission Mine. Sansavilla and Okefenokee both
# have negative relationship between alpha diversity and increasing disturbance age
# while Mission Mine has positive. Disturbance age has a significantly different impact
# on alpha diversity at Sansavilla than the Mine. While at Okefenokee, this result is not
# significant, likely due to large error surround later disturbance ages (data doesn't exist).


# M4: Interactive model (but including random slope with disturbance age)
m4 <- glmmTMB(n_species ~ location * yrs_since_disturbance + 
                (yrs_since_disturbance | site),
              data = alpha_rich_summary, family = compois)
summary(m4)
plot(ggpredict(m4, terms = c("yrs_since_disturbance", "location")))
emmeans(m4, pairwise ~ location, type = "response")
emtrends(m4, specs = ~ location ,var = "yrs_since_disturbance",
         type = "response")
# This allows for a random slope, but I don't think this really helps that much

# Compare with AIC 
AIC(m0, m1, m2, m3, m4) |> 
  arrange(AIC)
# Based on AIC output, m1 is my best model. This is the simplest, saying that alpha
# diversity is best explained by the location. M3 is also a reasonable model based on AIC,
# this model allows for an interaction term of disturbance age

testDispersion(m1)
# This looks pretty good

simulationOutput <- simulateResiduals(fittedModel = m1, plot = T)
# This also looks ok

# ~~~~~~~~~~~ Plotting my best model with my raw data ~~~~~~~~~~~~

# M1:
pred_data <- data.frame(location = unique(alpha_rich_summary$location))
pred_data$fit <- predict(m1, newdata = pred_data, re.form = NA, type = "response")

# Plot
ggplot() +
  geom_jitter(data = alpha_rich_summary, 
              aes(x = location, y = n_species, color = location),
              width = 0.15, alpha = 0.5) +
  geom_point(data = pred_data,
             aes(x = location, y = fit),
             size = 4, shape = 16, color = "brown", alpha = 0.7) +
  geom_errorbar(data = as.data.frame(emmeans(m1, ~location, type = "response")),
                aes(x = location, ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, linewidth = 0.8, color = "purple", alpha = 0.6) +
  scale_color_few() + 
  labs(y = "Alpha Diversity", 
       x = NULL,) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# M3:
pred_grid <- expand.grid(
  location = unique(alpha_rich_summary$location),
  yrs_since_disturbance = seq(min(alpha_rich_summary$yrs_since_disturbance),
                              max(alpha_rich_summary$yrs_since_disturbance),
                              length.out = 100))

emm <- emmeans(m3, ~ location * yrs_since_disturbance,
               at = list(yrs_since_disturbance = seq(
                 min(alpha_rich_summary$yrs_since_disturbance),
                 max(alpha_rich_summary$yrs_since_disturbance),
                 length.out = 100)), type = "response")

pred_data <- as.data.frame(emm)

ggplot() +
  geom_ribbon(data = pred_data,
              aes(x = yrs_since_disturbance, ymin = asymp.LCL, ymax = asymp.UCL,
                  fill = location),
              alpha = 0.2) +
  geom_point(data = alpha_rich_summary,
             aes(x = yrs_since_disturbance, y = n_species, color = location),
             alpha = 0.7, size = 2) +
  geom_line(data = pred_data,
            aes(x = yrs_since_disturbance, y = response, color = location),
            linewidth = 1) +
  scale_color_few() +
  scale_fill_few() +
  labs(x = "Years Since Disturbance",
       y = "Alpha Diversity",
       color = "Location",
       fill = "Location") +
  theme_bw() +
  theme(legend.position = "right")
