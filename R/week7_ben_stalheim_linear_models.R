#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Linear Models... WOO!
# Author: Ben Stalheim
# Date: March, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages
library(tidyverse)
library(performance)
library(emmeans)
library(lme4)
library(corrplot)
library(ggthemes)
library(Manu)
library(effects)
library(ggeffects)

# Load data
load("Data/RDS/bn_dat_filtered_95.rds")

# Prep ARU effort ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aru_effort_short <- read_csv("Data/CSVs/aru_effort_short.csv")

aru_effort_long  <- read_csv("Data/CSVs/aru_effort_long.csv") 

# Setting color and themes 
manu_col_1 <- get_pal("Kaka")[1]
manu_col_2 <- get_pal("Takahe")[4]
manu_col_3 <- get_pal("Putangitangi")[2]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               PART ONE ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Hypothesis: Northern Bobwhite detection rate declines with reclamation age 
# (time since a mining site was reclaimed).

# Note: This is count data, so I assume it should be modeled using a Poisson distribution.
# But to start, I will go through the normal distribution.

# Limit the data to Northern Bobwhite at the mine
nobo_mine <- bn_dat_filtered_95 |> 
  filter(common_name == "Northern Bobwhite",
         location == "mine")

# Get daily detections
nobo_daily <- nobo_mine |>
  group_by(site, date) |>
  summarise(detections = n(), .groups = "drop") |> 
  print()

# I have to account for effort (not all sites get the same amount of effort b/c ARUs
# are silly and cut out)
nobo_daily_effort <- nobo_daily |>
  left_join(aru_effort_long |>
  select(site, date, effort_s), by = c("site", "date")) |>
  mutate(effort_hr = (effort_s / 60)/60)

# Get unique site, year and disturbance data
site_disturbance <- nobo_mine |>
  distinct(site, year, yrs_since_disturbance)

# Join all of this information into one dataframe and calculate detections/min (only for 
# times when a site was actively being surveyed)
nobo_daily_joined <- nobo_daily_effort |>
  mutate(year = year(date)) |>
  left_join(site_disturbance, by = c("site", "year")) |> 
  mutate(det_per_hr = detections / effort_hr) |> 
  print()

# Model the relationship
bob_mod1 <- lm(det_per_hr~yrs_since_disturbance, data = nobo_daily_joined)

lm(det_per_hr~yrs_since_disturbance, data = nobo_daily_joined)

summary(bob_mod1)

# This summary is showing me that the detection rate for Northern Bobwhite does
# decline with years since disturbance. The p-value is < 0.05, a significant result.
# The coefficient is -0.11, and so the model is saying that as years since disturbance
# increases by 1 unit (i.e., 1 year), Bobwhite detection rate drops by by 0.11 detections/min.
plot(allEffects(bob_mod1))

# Base R plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bob_mod1 <- lm(detections~yrs_since_disturbance, data = nobo_daily_joined) 

plot(bob_mod1, 1)
# This plot shows that my residuals don't fall evenly along the line and their ranges vary
# across the fitted values.
plot(bob_mod1, 2)
# The QQ plot shows some clear deviations from normality as the quantiles do not follow
# the 1:1 line.
plot(bob_mod1, 3)
# Similar issue to the first plot, but range of square-root of residuals still uneven
plot(bob_mod1, 4)
# This plot is pointing out the outliers, I believe? Which there are a few of
plot(bob_mod1, 5)
# Not sure here, I just liked going through all of the plots. Looks bad I suppose
plot(bob_mod1, 6)
# I have no idea what this means, but it does look cool

resid(bob_mod1)
# Residuals from my model

hist(resid(bob_mod1))
# Histogram is also showing a clear non-normal distribution

shapiro.test(resid(bob_mod1))
# The Shapiro test and the histogram show that my residuals are significantly non-normally
# distributed and that I should probably switch to a different distribution for modeling.

# Performance package check
check_model(bob_mod1)
# This does not appear to work for me

# Plotting the relationship ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(nobo_daily_joined, aes(x = yrs_since_disturbance, y = det_per_hr)) +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.6, size = 2, color = manu_col_3) +
  geom_smooth(method = "lm", se = TRUE, color = manu_col_2, fill = manu_col_1) +
  labs(title = "Relationship Between Years Since Mine Reclamation\nand Detection per Hour of Northern Bobwhite",
    x = "Years Since Disturbance",
    y = "Detections per Hour") +
  scale_x_continuous(breaks = 1:7) +
  theme_calc() +
  theme(plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))

# This graph shows visually what the model output was saying, which is that there is a negative
# relationship between years since disturbance (mining) and detections/day for Northern Bobwhite. 
# This species requires disturbed, open environments, and so as the trees regrow, the
# Bobwhites are less likely to use the space in the same way.

#ggsave("Figures/bobwhite_disturbance.png",
 #     width = 8, height = 4, dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  PART TWO ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I think I went a little crazy in part two (mainly because I finally got my hands 
# on some environmental data). But I also found it really fun making these models and
# had lots of hypotheses I wanted to test using them. I am still collecting data, so I
# don't expect to find results that support all of my hypotheses yet, but it is good to get
# all of these set up and ready to go!

sites <- sort(unique(bn_dat_filtered_95$site))
all_species_list <- sort(unique(bn_dat_filtered_95$common_name))

all_combos_full <- expand_grid(
  site    = sites,
  date    = unique(aru_effort_long$date),
  species = all_species_list)

# Build detection history for all species
det_long_full <- all_combos_full |>
  left_join(
    aru_effort_long |> select(site, date, effort_s),
    by = c("site", "date")) |>
  left_join(
    bn_dat_filtered_95 |>
      group_by(site, date, common_name) |>
      summarise(count = n(), .groups = "drop"),
    by = c("site", "date", "species" = "common_name")) |>
  mutate(
    count = case_when(
      is.na(effort_s) ~ NA_real_,
      is.na(count)    ~ 0,
      TRUE            ~ count),
    detected = ifelse(count > 0, 1, 0),
    year = year(date)) |>
  group_by(year) |>
  arrange(date) |>
  mutate(day_within_year = dense_rank(date)) |>
  ungroup()

# Summarise to species x site x year
species_site_year <- det_long_full |>
  group_by(species, site, year) |>
  summarise(
    n_detections = sum(detected, na.rm = TRUE),
    n_surveys    = sum(!is.na(detected)),
    naive_occ    = max(detected, na.rm = TRUE),
    total_effort_hr  = round((sum(effort_s, na.rm = TRUE)/60)/60,1),
    relative_activity = n_detections / total_effort_hr,
    .groups = "drop") |>
  filter(n_surveys > 0) |>
  left_join(
    bn_dat_filtered_95 |> distinct(site, year, yrs_since_disturbance, location),
    by = c("site", "year")) |>
  left_join(
    bn_dat_filtered_95 |> distinct(common_name, disturbance_dependent) |>
      rename(species = common_name),
    by = "species") |>
  mutate(
    disturbance_dependent = factor(disturbance_dependent),
    yrs_sc = as.numeric(scale(yrs_since_disturbance))) |> 
  print()

# The warning is because I only have Okefenokee data for 2 years (whereas the other locations
# have data from 3 years). 

# Ok nvm, I think all I want is this, where I have just species richness grouped
# by site and years since disturbance
spec_summary <- bn_dat_filtered_95 |> 
  group_by(site, location, yrs_since_disturbance) |> 
  summarise(species_count = n_distinct(common_name)) |> 
  arrange(desc(species_count))
  
  
# Models    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fit an interactive model
model_a <- lmer(
  species_count ~ yrs_since_disturbance *location + (1|site),
  data   = spec_summary)

summary(model_a)

ggplot(spec_summary, aes(x = yrs_since_disturbance, y = species_count, 
                          color = location)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship Disturbance and Species Richness",
       x = "Years Since Disturbance",
       y = "Species Richness") +
  scale_x_continuous(breaks = 1:7) +
  scale_color_colorblind() +
  theme_calc() +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# This is an interesting result. My hypothesis was that species richness would decline
# as years since disturbance increased. However, at the mine, this does make some sense
# that species richness accumulate, because the vegetation does not regrow in the same way
# as the other locations. Succession occurs slower, because the soil is compacted and 
# native vegetation is not encouraged to regrow, so the environment does not become 
# overgrown in a short amount of time like we would expect in longleaf pine forests. 

# The summary of the model shows this result, where the coefficient for yrs_since_disturbance
# at the mine is positive, but the coefficients are negative at the other 2 locations. 
# Okefenokee shows the stronger negative trend compared to Sansavilla. The Okefenokee is 
# a mature longleaf pine forest, and species might be more closely tied to disturbance 
# there than the other locations.


plot(allEffects(model_a))
# This plot shows the same trends, just broken down more succinctly.

emmeans(model_a,specs=~ yrs_since_disturbance*location)
all_comparisons <- emmeans(model_a, pairwise ~ yrs_since_disturbance*location)
all_comparisons$contrasts
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modeling weather variables (I finally figured out where to source some)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Does average daily wind speed or average wind speed effect count of species per day?
# I am filtering to just 2025 for now, to not have all these various years make thing confusing.

daily_richness <- bn_dat_filtered_95 |>
  filter(year == "2025") |> 
  group_by(site, date, avg_temp_f, avg_wind_speed_mph, julian_day, avg_altimeter,
           avg_vis, avg_rel_humidity, total_precipitation_in, avg_wind_dir) |>
  summarise(n_species = n_distinct(sp_code), .groups = "drop") |> 
  mutate( # I need to scale all of my variables so that I can include them together in future models
    avg_temp_scaled        = scale(avg_temp_f),
    avg_wind_scaled        = scale(avg_wind_speed_mph),
    julian_day_scaled      = scale(julian_day),
    avg_altimeter_scaled   = scale(avg_altimeter),
    avg_vis_scaled         = scale(avg_vis),
    avg_humidity_scaled    = scale(avg_rel_humidity),
    total_precip_scaled    = scale(total_precipitation_in),
    avg_wind_dir_scaled    = scale(avg_wind_dir))

# Just very basic plots to see how some of the variables interacti with one another
ggplot(daily_richness, aes(julian_day, avg_altimeter)) +
  geom_point()

ggplot(daily_richness, aes(julian_day, avg_temp_f)) +
  geom_point()

ggplot(daily_richness, aes(julian_day, avg_vis)) +
  geom_point()

ggplot(daily_richness, aes(julian_day, avg_rel_humidity)) +
  geom_point()

# I have to account for random effects at the site level, because I have many repeated measures
# at the sites level.
wind_mod <- glmer(
  n_species ~ avg_wind_scaled + (1|site), data = daily_richness,
  family = poisson, na.action = na.omit)

summary(wind_mod)
plot(ggpredict(wind_mod, terms = "avg_wind_scaled", bias_correction = TRUE))
# Based on the results of the wind model, wind does have appear to have a slight negative effect
# on count of daily species richness, but this is not significant
ggplot(daily_richness, aes(x = avg_wind_speed_mph, y = n_species)) + 
  geom_point() +
  geom_smooth(method = "glm")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

temp_mod <- glmer(
  n_species ~ avg_temp_scaled + (1|site), data = daily_richness,
  family = poisson, na.action = na.omit)

summary(temp_mod)
plot(ggpredict(temp_mod, terms = "avg_temp_scaled", bias_correction = TRUE))
# Based on the results of the temp model, temperature appears to have a signifcant negative
# effect on count of daily species richness.
ggplot(daily_richness, aes(x = avg_temp_f, y = n_species)) + 
  geom_point() +
  geom_smooth(method = "glm")

# Additive model with temp and wind now ~~~~~~~~~~~~~~~~~~~~~
temp_wind_mod <- glmer(
  n_species ~ avg_temp_scaled + avg_wind_scaled + (1|site), 
  data = daily_richness, family = poisson, na.action = na.omit)

summary(temp_wind_mod)
# This model really did not improve the fit by much, as AIC did not improve and includes more 
# paramers. Temperate alone is doing good job.

# Interactive models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I would think wind speed might be more impactful coming from certain directions, so...
interactive_wind_mod <- glmer(
  n_species ~ avg_wind_dir_scaled * avg_wind_scaled + (1|site), data = daily_richness,
  family = poisson, na.action = na.omit)

summary(interactive_wind_mod)
# Wind does not appear to have much effect on detecting species richness during an individual day.

# One more interactive model
interactive_day_alt_mod <- glmer(
  n_species ~ julian_day_scaled * avg_altimeter_scaled + (1|site), data = daily_richness,
  family = poisson, na.action = na.omit)

summary(interactive_day_alt_mod)
plot(ggpredict(interactive_day_alt_mod, terms = "avg_altimeter_scaled"))
plot(ggpredict(interactive_day_alt_mod, terms = "julian_day_scaled"))
# This shows the interaction
plot(ggpredict(interactive_day_alt_mod, 
               terms = c("avg_altimeter_scaled", 
                         "julian_day_scaled [-1, 0, 1]")))

# This model has some interesting results! Where avg daily altimeter (pressure) 
# appears to have a significant positive effect on the number of bird species detected. 
# Meanwhile, Julian Day has a significant negative effect on the bird of species detected.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Here, I just started testing hypotheses on groups of bird species. Specifically, I
# have 2 main groups: birds that are known to rely on disturbance events (disturbance-dependent) 
# and birds that are not (non disturbance-dependent). I specifically want to know if 
# disturbance-dependent bird species experience greater declines (or impacts) as 
# years since disturbance increase at these different locations. At the mine specifically, 
# we don't really know the importance the environment can serve these kinds of species. 
# If disturbance-dependent bird species can persist, then that is an interesting thing to know.
# However, I predict that disturbance-dependent birds will experience greater declines
# at the mine than these other environments as disturbance increases. I would like to model
# species' occupancy, colonization, and extinction rates, but here I am just continuing
# the basic species richness estimates, but separating the groups and computing the same model
# as above.

dd_birds <- bn_dat_filtered_95 |> 
  filter(disturbance_dependent == 1)

sites <- sort(unique(dd_birds$site))
all_species_list <- sort(unique(dd_birds$common_name))

all_combos_full <- expand_grid(
  site    = sites,
  date    = unique(aru_effort_long$date),
  species = all_species_list)

# Build detection history for all species
det_long_full <- all_combos_full |>
  left_join(
    aru_effort_long |> select(site, date, effort_s),
    by = c("site", "date")) |>
  left_join(
    dd_birds |>
      group_by(site, date, common_name) |>
      summarise(count = n(), .groups = "drop"),
    by = c("site", "date", "species" = "common_name")) |>
  mutate(
    count = case_when(
      is.na(effort_s) ~ NA_real_,
      is.na(count)    ~ 0,
      TRUE            ~ count),
    detected = ifelse(count > 0, 1, 0),
    year = year(date)) |>
  group_by(year) |>
  arrange(date) |>
  mutate(day_within_year = dense_rank(date)) |>
  ungroup()

# Summarise to species x site x year
species_site_year <- det_long_full |>
  group_by(species, site, year) |>
  summarise(
    n_detections = sum(detected, na.rm = TRUE),
    n_surveys    = sum(!is.na(detected)),
    naive_occ    = max(detected, na.rm = TRUE),
    total_effort_hr  = round((sum(effort_s, na.rm = TRUE)/60)/60,1),
    relative_activity = n_detections / total_effort_hr,
    .groups = "drop") |>
  filter(n_surveys > 0) |>
  left_join(
    dd_birds |> distinct(site, year, yrs_since_disturbance, location),
    by = c("site", "year")) |>
  left_join(
    dd_birds |> distinct(common_name, disturbance_dependent) |>
      rename(species = common_name),
    by = "species") |>
  mutate(
    disturbance_dependent = factor(disturbance_dependent),
    yrs_sc = as.numeric(scale(yrs_since_disturbance))) |> 
  print()

# Ok nvm, I think all I want is this, where I have just species richness grouped
# by site and years since disturbance
dd_summary <- dd_birds |> 
  group_by(site, location, yrs_since_disturbance) |> 
  summarise(species_count = n_distinct(common_name)) |> 
  arrange(desc(species_count))


# Models    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fit an interactive model
model_a <- lmer(
  species_count ~ yrs_since_disturbance *location + (1|site),
  data   = dd_summary)

summary(model_a)

ggplot(dd_summary, aes(x = yrs_since_disturbance, y = species_count, 
                         color = location)) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_smooth(method = "lm") +
  labs(title = "Disturbance-Dependent Bird Species and Disturbance",
       x = "Years Since Disturbance",
       y = "Richness of Disturbance-Dependent Species") +
  scale_x_continuous(breaks = 1:7) +
  scale_color_colorblind() +
  theme_calc() +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

#ggsave("Figures/dd_richness.png",
 #      width = 8, height = 5, dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Non Disturbance-Dependent Species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ndd_birds <- bn_dat_filtered_95 |> 
  filter(disturbance_dependent == 0)

sites <- sort(unique(ndd_birds$site))
all_species_list <- sort(unique(ndd_birds$common_name))

all_combos_full <- expand_grid(
  site    = sites,
  date    = unique(aru_effort_long$date),
  species = all_species_list)

# Build detection history for all species
det_long_full <- all_combos_full |>
  left_join(
    aru_effort_long |> select(site, date, effort_s),
    by = c("site", "date")) |>
  left_join(
    ndd_birds |>
      group_by(site, date, common_name) |>
      summarise(count = n(), .groups = "drop"),
    by = c("site", "date", "species" = "common_name")) |>
  mutate(
    count = case_when(
      is.na(effort_s) ~ NA_real_,
      is.na(count)    ~ 0,
      TRUE            ~ count),
    detected = ifelse(count > 0, 1, 0),
    year = year(date)) |>
  group_by(year) |>
  arrange(date) |>
  mutate(day_within_year = dense_rank(date)) |>
  ungroup()

# Summarise to species x site x year
species_site_year <- det_long_full |>
  group_by(species, site, year) |>
  summarise(
    n_detections = sum(detected, na.rm = TRUE),
    n_surveys    = sum(!is.na(detected)),
    naive_occ    = max(detected, na.rm = TRUE),
    total_effort_hr  = round((sum(effort_s, na.rm = TRUE)/60)/60,1),
    relative_activity = n_detections / total_effort_hr,
    .groups = "drop") |>
  filter(n_surveys > 0) |>
  left_join(
    ndd_birds |> distinct(site, year, yrs_since_disturbance, location),
    by = c("site", "year")) |>
  left_join(
    ndd_birds |> distinct(common_name, disturbance_dependent) |>
      rename(species = common_name),
    by = "species") |>
  mutate(
    disturbance_dependent = factor(disturbance_dependent),
    yrs_sc = as.numeric(scale(yrs_since_disturbance))) |> 
  print()

# Ok nvm, I think all I want is this, where I have just species richness grouped
# by site and years since disturbance
ndd_summary <- ndd_birds |> 
  group_by(site, location, yrs_since_disturbance) |> 
  summarise(species_count = n_distinct(common_name)) |> 
  arrange(desc(species_count))

# Fit an interactive model
model_ndd <- lmer(
  species_count ~ yrs_since_disturbance *location + (1|site),
  data   = ndd_summary)

summary(model_ndd)

ggplot(ndd_summary, aes(x = yrs_since_disturbance, y = species_count, 
                       color = location)) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_smooth(method = "lm") +
  labs(title = "Non Disturbance-Dependent Bird Species and Disturbance",
       x = "Years Since Disturbance",
       y = "Richness of Non Disturbance-Dependent Species") +
  scale_x_continuous(breaks = 1:7) +
  scale_color_colorblind() +
  theme_calc() +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


#ggsave("Figures/ndd_richness.png",
 #      width = 8, height = 5, dpi = 300)


# *Note:* I added some stuff to the very end of my script that I am working on 
#for my research project and not really sure if I am thinking about it the right
#way. I am trying to model the effects of disturbance on latent abundance of various
#bird species. I used Royle-Nichols models in the package unmarked with the
#function occuRN() to generate latent abundance estimates based on repeated survey 
#data. I then extracted the empirical Bayes estimates for each site and used these
#"most likely" estimates (I think that's how they worded it in their documentation).
#The problem is that I have 3 different study locations, each with different monitoring
#points within each location, and the data is collected over 3 years. When I try and
#fit an interactive model where years_since_disturbance*location, I often get errors 
#where the model cannot fit all these parameters, so I am left grouping and modeling 
#Abundance ~ Disturbance Age only. But this feels terribly wrong, because my 3 locations
#are very different. Disturbance type is different (fire vs mining), intensity is different
#(growing vs nongrowing season burns), and the interactive effect makes the most sense 
#in my head. It seems my best options might be to just model each location on its own and 
#remove the interactive effect. But now I have the issue of multiple years, where each year 
#is independent from one another supposedly, but many species do occupy the same territory
#over and over. Also, some years are just better for certain bird species, so the year 
#2025 could have elevated numbers all across one study location, and this throws off
#the relationship with the disturbance gradient. Anyways, that's where I'm at... not totally sure.
#I know that I need to use a mixed-effects model to account for the psuedoreplication at 
#my monitoring points (in code, they are labeled "site"). But I also think I need to
#use a poisson distribution, because my latent abundance estimates, come as counts... 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#           Research Project Modeling ---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the relative abundance dataset (generated using occuRN() looped over each year
# and species)
community_df_all <- read_csv("Data/CSVs/relative_abundance_estimates.csv") |> 
  mutate(year = as_factor(year))

# Subset to a given species (I am also adding scaled predictors here in case)
species_df_all <- community_df_all |>
  filter(species == "Northern Bobwhite")|> 
  mutate(yrs_since_disturbance_scaled = scale(yrs_since_disturbance))

# Plots to explore:
ggplot(species_df_all, aes(x = site, y = N_mean, color = year)) +
  geom_point(size = 1.5) +
  geom_segment(aes(xend = site, yend = 0), alpha = 0.3) +
  scale_color_colorblind() +
  labs(
    title = paste("Relative Abundance of", unique(species_df_all$species),"Across Sites"),
    x = "Site",
    y = "Empirical Bayes Mean of Latent Abundance") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(species_df_all, aes(x = yrs_since_disturbance, y = N_mean, color = year)) +
  geom_jitter(width = 0.05, height = 0.15, size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  scale_color_colorblind() +
  labs(
    title = paste("Relative Abundance of", unique(species_df_all$species),"vs Disturbance Age"),
    x = "Years Since Disturbance",
    y = "Relative Abundance (N_mean)") +
  theme_bw()

ggplot(species_df_all, aes(yrs_since_disturbance, N_mean)) +
  geom_jitter(alpha = 0.6, width = 0.05, height = 0.15, color = "magenta4") +
  geom_smooth(method = "lm", se = T, color = "salmon", fill = "salmon4", alpha = 0.2) +
  facet_wrap(~ location) +
  theme_bw() +
  labs(
    title = paste("Relative Abundance of", unique(species_df_all$species), "Across Disturbance Ages"),
    x = "Years Since Disturbance",
    y = "Relative Abundance (N_mean)")

# Mixed Effects ~~~~~~~~~~~~~~~~~~~~
dist_year_mod <- lmer(N_mean ~ yrs_since_disturbance*location + 
                        (1|site), data = species_df_all)

summary(dist_year_mod)


# GLM?
glm_mod <- glmer(N_mode ~ yrs_since_disturbance*location + (1|site), 
                 family = poisson,
                 data = species_df_all)

summary(glm_mod)

# Subset to mine only ~~~~~~~~~
mine_species <- species_df_all |> 
  filter(location == "Mine")

mine_mod <- glmer(N_mode ~ yrs_since_disturbance + (1|site), 
                  family = poisson,
                  data = mine_species)

summary(mine_mod)

# And then, I go through each location for each species? This is where I'm really not sure...





