#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Final Project
# Author: Ben Stalheim
# Date: April/May, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This R script calculates all of the data used for my final project paper.
# The main pieces are that I model bird community turnover, alpha diversity,
# and gamma diversity as a function of ecosystem type and disturbance age.

# All of my data was collected using autonomous recording units. The main dataset
# containing these raw detections is loaded as an R data file (bn_dat_filtered_95.rds).
# Hypotheses are written below. Code is annotated. And hopefully it all runs...
# There should be no other scripts needed to produce the main results section.
# However, I did use a script called thesis_stats.R to produce some values.
# This was stuff like mean annual temperature data, detection count stats, and
# the max number of species detected across all seasons and locations.

# The different sections can be skipped to using the outline tab.

# Load thee packages
library(vegan)
library(knitr)
library(ggthemes)
library(ggeffects)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(betapart)
library(purrr)
library(ggrepel)
library(tidyverse)

# ~~~~~~~~~~~~~~~~ Load Data ~~~~~~~~~~~~~~~~~~~~~
load("Data/RDS/bn_dat_filtered_95.rds")

# Rename (RDS files are odd)
bn_data <- bn_dat_filtered_95

# ARU effort data 
aru_effort_short <- read_csv("Data/CSVs/aru_effort_short.csv")
aru_effort_long  <- read_csv("Data/CSVs/aru_effort_long.csv") |> 
  mutate(year = year(date))

aru_surveys <- aru_effort_long |> 
  filter(effort_s > 0) |> 
  distinct(site, date)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Prep and exploring ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~ Species Matrices and Summaries ~~~~~~~~~~~~

all_species   <- unique(bn_data$common_name)
all_sites     <- unique(bn_data$site)
all_dates     <- aru_effort_long |> distinct(date)

detections <- bn_data |>
  group_by(common_name, site, date) |>
  summarise(count = n(), .groups = "drop")

species_matrix <- tidyr::crossing(
  common_name = all_species,
  site        = all_sites,
  all_dates) |>
  left_join(
    aru_effort_long |> dplyr::select(site, date) |> 
      mutate(active = TRUE),
    by = c("site", "date")) |>
  left_join(detections, by = c("common_name", "site", "date")) |>
  mutate(
    count = case_when(
      is.na(active) ~ NA_integer_,  
      is.na(count)  ~ 0L,          
      TRUE          ~ count)) |> 
  print()

species_matrix_wide <- species_matrix |>
  dplyr::select(site, date, common_name, count) |>
  pivot_wider(
    names_from  = common_name,
    values_from = count) |>
  arrange(site, date) |> 
  print()

species_summary <- bn_data |> 
  group_by(common_name) |> 
  summarise(raw_detections = n(),
            days_detected = n_distinct(date),
            most_common_location = names(which.max(table(location))),
            most_common_year = names(which.max(table(year))),
            .groups = "drop") |> 
  arrange(desc(raw_detections)) |> 
  print()
# I just really enjoing looking through all of this data. This is the interesting parts
# because it dives into specific species, whereas all of the modeling groups the community
# together. I find it fun and cool!

# ~~~~~~~~~~~~~~~~~~~ Exploring through figure ~~~~~~~~~~~~~~~~~~~~~

# Species richness by year and site
bn_data |>
  group_by(year, site) |>
  summarize(n_species = n_distinct(common_name), .groups = 'drop') |>
  ggplot(aes(x = site, y = n_species, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_few() +
  labs(x = "Site", y = "Number of Species", fill = "Year", title = "Observed Species Richness by Site and Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Here, you can clearly see the reduced sample size at O-4 in 2025. This is also
# a plot of alpha diversity. It shows that there is very little difference between
# survey points at Okefenokee, and quite a bit more variation at Mine and Sansavilla.
# Mine has clearly less species at each survey point than the other sampling locations.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Calculating Turnover (Beta Diversity) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# How different is community composition between year t and t+1 at each site?
species_pa <- bn_data |>
  distinct(site, year, sp_code) |>
  mutate(present = 1) |>
  pivot_wider(
    names_from = sp_code,
    values_from = present,
    values_fill = list(present = 0)) |>
  arrange(site, year) |> 
  print()

# Get all consecutive year pairs present in the data
year_pairs <- bn_data |>
  distinct(year) |>
  arrange(year) |>
  pull(year)

year_transitions <- tibble(
  yr1 = year_pairs[-length(year_pairs)],
  yr2 = year_pairs[-1])

# ~~~~ Function to calculate turnover for each survey site transition year ~~~~

calc_betatrans_site <- function(site_name, y1, y2, data) {
  
  # Get species for year 1
  pa_y1 <- data |>
    filter(site == site_name, year == y1) |>
    dplyr::select(-site, -year)
  
  # Get species for year 2
  pa_y2 <- data |>
    filter(site == site_name, year == y2) |>
    dplyr::select(-site, -year)
  
  # Site needs data in both years
  if (nrow(pa_y1) != 1 | nrow(pa_y2) != 1) return(NULL)
  
  # Align columns 
  all_sp <- union(colnames(pa_y1), colnames(pa_y2))
  
  pa_y1 <- pa_y1 |>
    add_column(!!!setNames(
      as.list(rep(0L, length(setdiff(all_sp, colnames(pa_y1))))),
      setdiff(all_sp, colnames(pa_y1)))) |>
    dplyr::select(all_of(sort(all_sp)))
  
  pa_y2 <- pa_y2 |>
    add_column(!!!setNames(
      as.list(rep(0L, length(setdiff(all_sp, colnames(pa_y2))))),
      setdiff(all_sp, colnames(pa_y2)))) |>
    dplyr::select(all_of(sort(all_sp)))
  
  mat <- rbind(as.matrix(pa_y1), as.matrix(pa_y2))
  
  beta <- beta.pair(mat, index.family = "jaccard")
  
  tibble(
    site            = site_name,
    yr1             = y1,
    yr2             = y2,
    beta_total      = as.numeric(beta$beta.jac),
    beta_turnover   = as.numeric(beta$beta.jtu),
    beta_nestedness = as.numeric(beta$beta.jne))
}

# Run across all sites x all year transitions
turnover_results <- pmap_df(
  year_transitions,
  function(yr1, yr2) {
    map_df(all_sites,
           ~calc_betatrans_site(.x, yr1, yr2, species_pa),
           .progress = TRUE)
  }) |>
  left_join(bn_data |> distinct(site, location), by = "site") |>
  arrange(site, yr1)
# View results
turnover_results

disturbance <- bn_data |>
  distinct(site, year, yrs_since_disturbance)

turnover <- turnover_results |> 
  left_join(disturbance, by = c("site", "yr2" = "year")) |> 
  rename(prev_year = yr1,
         turnover_year = yr2,
         total_turnover = beta_total,
         species_replacement = beta_turnover,
         species_gain_lost = beta_nestedness) |> 
  print()

# This is writing the csv, which I normally would just load, but I wanted to be
# transparent about how I calculated all of my turnover data (see above).

# ~~~~~~~~~~~~~~~~~~~~~~~ Write csv file for turnover ~~~~~~~~~~~~~~~~~~~~~~~
#write_csv(turnover, "Data/CSVs/turnover_data.csv")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Quick Visualize
ggplot(turnover, aes(x = yrs_since_disturbance, y = total_turnover)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "purple4") +
  facet_grid(~ location) +
  theme_bw() +
  labs(x = "Years Since Disturbance",
       y = "Jaccard Turnover (multi-year)",
       title = "Does turnover accelerate with time since disturbance?")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~ Modeling ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove low sampling site and make locations a factor
turnover <- turnover |>
  mutate(location = factor(location)) |> 
  filter(site != "O-4") # O-4 had only 7 days in 2025, so I am removing it for now

turnover_mod <- glmmTMB(total_turnover ~ location + (1 | site), data = turnover,
                        family = beta_family(link = "logit"))

summary(turnover_mod)
plot(ggpredict(turnover_mod, terms = c("location")))
# emmeans
emmeans(turnover_mod, pairwise ~ location, type = "response", re.form = NA)
# This is showing that Mission Mine and Okefenokee are significantly different,
# Sansavilla and Okefenokee are significantly different, and there is no difference
# between Sansavilla and the Mine. Bird communities appear to be changing, or turning
# over at these younger sites in similar ways. While the community really isn't changing
# much at the mature, established site. I used these pairwise comparisons for my 
# p-values, estimates, and SEs.

# Saving emmeans to use as SE in plot below
emm <- emmeans(turnover_mod, ~ location, type = "response") |>
  as.data.frame() |> 
  print()

# ~~~~~~~~~~~~~~ Model Diagnostics ~~~~~~~~~~~~~~~~~
# Test dispersion
testDispersion(turnover_mod) # Looks good
# Sim residuals
sim_res <- simulateResiduals(turnover_mod, plot = TRUE) # Looks good

# Plotting the raw data with the predicted data 
dat.new <- expand.grid(
  location = unique(turnover$location))

# Predict on the response scale
dat.new$yhat <- predict(turnover_mod, 
                        newdata = dat.new, 
                        type = "response",
                        re.form = NA)  

# Plot raw data points and model predictions
ggplot(turnover, aes(x = location, y = total_turnover, color = location)) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.4) +
  geom_point(data = dat.new, aes(x = location, y = yhat),
             size = 3, shape = 16, color = "brown") + 
  geom_errorbar(data = emm, aes(x = location, y = response,
                                ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, linewidth = 0.8, color = "purple4") +
  scale_color_few() +
  scale_x_discrete(labels = c(
    "mine"     = "Mission Mine",
    "sansavilla"  = "Sansavilla WMA",
    "okefenokee"  = "Okefenokee NWR")) +
  theme_bw() +
  labs(title = "Predicted Total Turnover by Location",
       x = NULL,
       y = "Total Turnover (Jaccard)") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 13, color = "black"))

# ~~~~~~~~~~~~~~~~~~ Save plot~~~~~~~~~~~~~~~~~
#ggsave("Figures/turnover_plot.png",
#       width = 7, height = 4, dpi = 300)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Turnover ~ Disturbance Age
turnover_mod2 <- glmmTMB(total_turnover ~ yrs_since_disturbance + (1 | site), data = turnover,
                         family = beta_family(link = "logit"))

summary(turnover_mod2)
plot(ggpredict(turnover_mod2, terms = c("yrs_since_disturbance")))

# Get predicted values across range of disturbance age
dat.new2 <- data.frame(
  yrs_since_disturbance = seq(min(turnover$yrs_since_disturbance, na.rm = TRUE),
                              max(turnover$yrs_since_disturbance, na.rm = TRUE),
                              length.out = 100))

# Predict on response scale (no random effects)
pred2 <- predict(turnover_mod2,
                 newdata = dat.new2,
                 type = "response",
                 re.form = NA,
                 se.fit = TRUE)

dat.new2$yhat  <- pred2$fit
dat.new2$se    <- pred2$se.fit
dat.new2$lower <- pred2$fit - 1.96 * pred2$se.fit
dat.new2$upper <- pred2$fit + 1.96 * pred2$se.fit

# Plot
ggplot(turnover, aes(x = yrs_since_disturbance, y = total_turnover)) +
  geom_point(aes(color = location), size = 2, alpha = 0.5) +
  geom_line(data = dat.new2, aes(x = yrs_since_disturbance, y = yhat),
            color = "purple4", linewidth = 1) +
  geom_ribbon(data = dat.new2, aes(x = yrs_since_disturbance,
                                   ymin = lower, ymax = upper),
              inherit.aes = FALSE,
              fill = "purple4", alpha = 0.15) +
  scale_color_few(labels = c("mine"       = "Mission Mine",
                             "okefenokee" = "Okefenokee NWR",
                             "sansavilla" = "Sansavilla WMA")) +
  theme_bw() +
  labs(x = "Years Since Disturbance",
       y = "Total Turnover (Jaccard)",
       color = NULL) +
  theme(legend.position = "bottom",
        axis.text  = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 13, color = "black"))


# Turnover ~ Disturbance Age + Location
turnover_mod3 <- glmmTMB(total_turnover ~ yrs_since_disturbance + location + 
                           (1 | site), data = turnover, family = beta_family(link = "logit"))

summary(turnover_mod3)
plot(ggpredict(turnover_mod3, terms = c("yrs_since_disturbance", "location")))
emmeans(turnover_mod3, pairwise ~ location, type = "response", re.form = NA)
emtrends(turnover_mod3, specs = ~ location ,var = "yrs_since_disturbance",
         type = "response")

# Turnover ~ Dist Age * Location
turnover_mod4 <- glmmTMB(total_turnover ~ yrs_since_disturbance * location + 
                           (1 | site), data = turnover, family = beta_family(link = "logit"))
summary(turnover_mod4)
plot(ggpredict(turnover_mod4, terms = c("yrs_since_disturbance", "location")))
emmeans(turnover_mod4, pairwise ~ location, type = "response", re.form = NA)
emtrends(turnover_mod4, specs = ~ location ,var = "yrs_since_disturbance",
         type = "response")
emtrends(turnover_mod4, pairwise ~ location ,var = "yrs_since_disturbance",
         type = "response")

# Predict across disturbance age range for each location
dat.new4 <- expand.grid(
  yrs_since_disturbance = seq(min(turnover$yrs_since_disturbance, na.rm = TRUE),
                              max(turnover$yrs_since_disturbance, na.rm = TRUE),
                              length.out = 100),
  location = unique(turnover$location))

pred4 <- predict(turnover_mod4,
                      newdata = dat.new4,
                      type = "link",       
                      re.form = NA,
                      se.fit = TRUE)

# Transform predicts back into response scale (this way they cannot be negative)
dat.new4$yhat  <- plogis(pred4$fit)
dat.new4$lower <- plogis(pred4$fit - 1.96 * pred4$se.fit)
dat.new4$upper <- plogis(pred4$fit + 1.96 * pred4$se.fit)

# Plot
ggplot(turnover, aes(x = yrs_since_disturbance, y = total_turnover, color = location)) +
  geom_point(size = 2, alpha = 0.35) +
  geom_line(data = dat.new4, aes(x = yrs_since_disturbance, y = yhat, color = location),
            linewidth = 1) +
  geom_ribbon(data = dat.new4, aes(x = yrs_since_disturbance,
                                   ymin = lower, ymax = upper,
                                   fill = location),
              inherit.aes = FALSE,
              alpha = 0.2) +
  scale_color_few(labels = c("mine"       = "Mission Mine",
                             "okefenokee" = "Okefenokee NWR",
                             "sansavilla" = "Sansavilla WMA")) +
  scale_fill_few(labels  = c("mine"       = "Mission Mine",
                             "okefenokee" = "Okefenokee NWR",
                             "sansavilla" = "Sansavilla WMA")) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_bw() +
  labs(title = "Effect of Disturbance Age on Turnover in Various Ecosystems",
       x = "Years Since Disturbance",
       y = "Total Turnover (Jaccard)",
       color = NULL,
       fill = NULL) +
  theme(legend.position = "bottom",
        axis.text  = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 13, color = "black"))

# ~~~~~~~~~~~~~~~~~~ Save plot~~~~~~~~~~~~~~~~~
#ggsave("Figures/disturbance_eco_turnover.png",
#       width = 7, height = 4, dpi = 300)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Null model
null_mod <- glmmTMB(total_turnover ~ 1, data = turnover,
                    family = beta_family(link = "logit"))
summary(null_mod)

# ~~~~~~~~~~~~ Model selection and comparison ~~~~~~~~~~~~~~~~

anova(turnover_mod, turnover_mod2, turnover_mod3, turnover_mod4, null_mod)
AIC(turnover_mod,turnover_mod2, turnover_mod3, turnover_mod4, null_mod) |> 
  arrange(AIC)
# According to this, the top model is turnover_mod3 (additive model). The problem
# here is that this isn't at all what my hypotheses are, I don't believe disturbance
# age should or does have an equal effect across the ecosystem types. I mostly included
# this model as an example of an additive model. The other top model, within 2 AIC
# is the ecosystem only model, which is what my hypothesis was. I am choosing this model
# because there is still significant support for it. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Alpha Diversity ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  facet_grid(~ location,
             labeller = labeller(location = c(
               mine     = "Mission Mine",
               sansavilla  = "Sansavilla WMA",
               okefenokee  = "Okefenokee NWR"))) +
  scale_color_colorblind() +
  labs(
    title = "Gamma Diversity by Location and Year",
    x = NULL,
    y = "Species Richness over Sampling Season",
    color = "Year") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 13, color = "black", hjust = -0))
# Gamma diversity (i.e., species richness estimates for each location, adding up each
# survey point). Gamma diversity is lowest at the mine, intermediate at Okefenokee (interesting
# because alpha diversity is highest here), while gamma diversity is highest at Sansavilla.

# ~~~~~~~~~~~~~ Save plot~~~~~~~~~~~~~~~~
#ggsave("Figures/gamma_diversity_plot.png",
#       width = 6, height = 3.5, dpi = 300)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  scale_x_discrete(labels = c(
    "mine"     = "Mission Mine",
    "sansavilla"  = "Sansavilla WMA",
    "okefenokee"  = "Okefenokee NWR")) +
  labs(y = "Alpha Diversity", 
       x = NULL,) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"))

# ~~~~~~~~~~~~~ Save plot~~~~~~~~~~~~~~~~
#ggsave("Figures/alpha_diversity_plot.png",
#       width = 7, height = 3.5, dpi = 300)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       Gamma Diversity ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~ Hypothesis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# **My hypothesis** is that gamma diversity (i.e., species richness at broad scale, or 
# sampling location) varies between my sampling locations. I also want to account
# for sampling year and will include that as a fixed effect.

# I will be using GLMs to model alpha diversity as a function of the sampling 
# location and year. No need for random effects.

# ~~~~~~~~~~~~~~~~ Modeling ~~~~~~~~~~~~~~~~~~~~

# Use the location richness summary, which takes detection and species richness over
# the entire year at each sampling location. 
location_rich_summary <- location_rich_summary |> # turn year into a factor
  mutate(year = as_factor(year))

# Location only
gamma_mod1 <- glmmTMB(n_species ~ location, data = location_rich_summary,
                      family = compois(link = "log"))
summary(gamma_mod1)
plot(ggpredict(gamma_mod1, terms = c("location")))
emmeans(gamma_mod1, pairwise ~ location, type = "response")
# Using the comparison of means, there appears to be significant differences in
# mean gamma diversity across locations. The Mission Mine has lower gamma diversity
# compared to Okefenokee and Sansavilla, while those two do not significantly differ.
testDispersion(gamma_mod1) # looks fine

# Plot the raw data ~~~~~
pred_gamma <- data.frame(location = unique(location_rich_summary$location))
pred_gamma$fit <- predict(gamma_mod1, newdata = pred_gamma, 
                          re.form = NA, type = "response")
# emmeans for confidence intervals
emm_gamma <- emmeans(gamma_mod1, ~ location, type = "response") |>
  as.data.frame()
# Plot
ggplot(location_rich_summary, aes(x = location, y = n_species, color = location)) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.5) +
  geom_point(data = pred_gamma, aes(x = location, y = fit),
             size = 3, shape = 16, color = "brown") +
  geom_errorbar(data = emm_gamma, aes(x = location, y = response,
                                      ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, linewidth = 0.8, color = "purple4") +
  scale_color_few() +
  scale_x_discrete(labels = c(
    "mine"       = "Mission Mine",
    "sansavilla" = "Sansavilla WMA",
    "okefenokee" = "Okefenokee NWR")) +
  theme_bw() +
  labs(title = "Predicted Gamma Diversity Across Ecosystems",
       x = NULL,
       y = "Gamma Diversity (Annual Species Richness)") +
  theme(legend.position = "none",
        axis.text.x  = element_text(size = 12, color = "black"),
        axis.text.y  = element_text(size = 10, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"))

# ~~~~~~~~~~~~~ Save plot~~~~~~~~~~~~~~~~
#ggsave("Figures/gamma_diversity_plot.png",
#       width = 7, height = 3.5, dpi = 300)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Location + Year
gamma_mod2 <- glmmTMB(n_species ~ location + year, data = location_rich_summary,
                      family = compois)
summary(gamma_mod2)
plot(ggpredict(gamma_mod2, terms = c("location")))
plot(ggpredict(gamma_mod2, terms = c("year")))
plot(ggpredict(gamma_mod2, terms = c("location", "year")))
emmeans(gamma_mod2, pairwise ~ location, type = "response")
testDispersion(gamma_mod2)
# This model is fine too, but doesn't improve upon the first model.

# Null model
gamma_null_mod <- glmmTMB(n_species ~ 1, data = location_rich_summary,
                          family = compois)
summary(gamma_null_mod)
testDispersion(gamma_null_mod)

# ~~~~~~~~~~ Model selection ~~~~~~~~~~~~~

AIC(gamma_mod1, gamma_mod2, gamma_null_mod) |> 
  arrange(AIC)
# Based on this, the model with ecosystem type best explains gamma diversity. Which
# makes sense... And both are preferred over the null model. Yay!