#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title:
# Author:
# Date: 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages
library(tidyverse)
library(glmmTMB)
library(lme4)
library(emmeans)
library(ggeffects)
library(DHARMa)
library(ggthemes)
library(mgcv)
library(gratia)

# Load data
abundance_data <- read_csv("Data/CSVs/relative_abundance_estimates.csv") |> 
  mutate(year = as_factor(year),
         location = as_factor(location),
         site = as_factor(site))

# ~~~~~~~~~~~~~~~~~~~~~~ Hypothesis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~ Diagnostic prep ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~ Explore with some plots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~ N_mean (survey-site level estimates) ~~~~~~~~~
# Did the range and total relative abundance of all species vary by year?
ggplot(abundance_data, aes(x = year, y = N_mean, color = year)) +
  geom_jitter(width = 0.15, height = 0.05, shape = 20, size = 4) +
  facet_grid(~ location) +
  scale_color_economist() +
  labs(
    title = paste("N_mean Relative Abundance of all Species"),
    x = NULL,
    y = "Survey-site Estimated Latent Abundance",
    color = "Year") +
  theme_bw() +
  theme(legend.position = "none")
# Not really, this shows that relative abundance totals of all species stayed somewhat
# consistent across all years within each location

# Did the range and total relative abundance of all species vary by disturbance age?
ggplot(abundance_data, aes(x = yrs_since_disturbance, y = N_mean, color = location)) +
  geom_jitter(width = 0.15, height = 0.05, shape = 20, size = 4) +
  facet_grid(~ location) +
  scale_color_economist() +
  labs(
    title = paste("N_mean Relative Abundance of all Species"),
    x = NULL,
    y = "Survey-site Estimated Latent Abundance") +
  theme_bw() +
  theme(legend.position = "none")
# Again, not really, although Sansavilla shows a decline in range and total number of
# estimates as disturbance age increases.

# ~~~~~~ Detection Probability ~~~~~~~~~~~
# Did detection probability vary by year?
ggplot(abundance_data, aes(x = year, y = p_mean, color = year)) +
  geom_jitter(width = 0.15, height = 0.05, shape = 20, size = 4) +
  facet_grid(~ location) +
  scale_color_economist() +
  labs(
    title = paste("N_mean Relative Abundance of all Species"),
    x = NULL,
    y = "Survey-site Estimated Latent Abundance",
    color = "Year") +
  theme_bw() +
  theme(legend.position = "none")
# 2025 shows a cleaar grouping of species that had higher detection probabilities
# than in previous years. But mostly everything else seems fine...

# Did detection probability vary by disturbance age?
ggplot(abundance_data, aes(x = yrs_since_disturbance, y = p_mean, color = location)) +
  geom_jitter(width = 0.15, height = 0.05, shape = 20, size = 4) +
  facet_grid(~ location) +
  scale_color_economist() +
  labs(
    title = paste("N_mean Relative Abundance of all Species"),
    x = NULL,
    y = "Survey-site Estimated Latent Abundance") +
  theme_bw() +
  theme(legend.position = "none")
# I don't see anything here that is too troubling. It appears detection probability
# stayed about the same, no matter the age of the site. Which is good, because this
# could skew my relative abundance, occupancy, and many other estimates...



# ~~~~~ Lambda (location-level estimates) ~~~~~~~~~~~
sp <- "Eastern Towhee" # Change this species name to test/plot new species

lambda_df <- abundance_data |> 
  group_by(location, species, year) |> 
  summarise(lambda = mean(lambda),
            .groups = "drop_last") |> 
  filter(species == sp)

ggplot(lambda_df, aes(x = year, y = lambda, color = year)) +
  geom_point(shape = 8, size = 4) +
  facet_grid(~ location) +
  scale_color_economist() +
  labs(
    title = paste("Lambda Relative Abundance of", unique(lambda_df$species),"Across Locations"),,
    x = NULL,
    y = "Estimated Latent Abundance",
    color = "Year") +
  theme_bw() +
  theme(legend.position = "none")

# ~~~~~~~ Back to species level plots ~~~~~~~~~~~
bayes_df <- abundance_data |> 
  filter(species == sp)

# N_mean vs year
ggplot(bayes_df, aes(x = year, y = N_mean, color = year)) +
  geom_jitter(width = 0.15, height = 0.05, shape = 20, size = 4) +
  facet_grid(~ location) +
  scale_color_economist() +
  labs(
    title = paste("Year and N_mean Relative Abundance of", unique(bayes_df$species),"Across Sites"),,
    x = NULL,
    y = "Estimated Latent Abundance",
    color = "Year") +
  theme_bw() +
  theme(legend.position = "none")

# N_mean vs years since disturbance
ggplot(bayes_df, aes(x = yrs_since_disturbance, y = N_mean, color = location)) +
  geom_jitter(width = 0.15, height = 0.05, shape = 20, size = 4) +
  facet_grid(~ location) +
  scale_color_economist() +
  labs(
    title = paste("Disturbance Age and N_mean Relative Abundance of", 
                  unique(lambda_df$species),"Across Sites"),,
    x = NULL,
    y = "Survey-site Estimated Latent Abundance") +
  theme_bw() +
  theme(legend.position = "none")


# ~~~~~~~~~~~~~~~~~~~~~~~~ Modeling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Make a null model
null_mod <- m1 <- glmmTMB(N_mean ~ 1 + (1|site),
                          data = bayes_df, family = compois(link = "log"))
summary(null_mod)

# Simple model (disturbance age only)
m1 <- glmmTMB(N_mean ~ yrs_since_disturbance + (1|site),
              data = bayes_df, family = Gamma(link = "log")) 
summary(m1)

# Additive model
m2 <- glmmTMB(N_mean ~ yrs_since_disturbance + location + (1|site),
              data = bayes_df, family = Gamma(link = "log")) 
summary(m2)

# Interactive model
m3 <- glmmTMB(N_mean ~ yrs_since_disturbance*location + (1|site),
              data = bayes_df, family = Gamma(link = "log")) 
summary(m3)

# Add year as a predictor (year is a factor)
m4 <- glmmTMB(N_mean ~ yrs_since_disturbance*location + year + (1|site),
                    data = bayes_df, family = Gamma(link = "log"))                               

summary(m4)

# Check AIC
AIC(null_mod, m1, m2, m3, m4) |> 
  arrange(AIC)
# Based on AIC, my interactive model with year as a predictor is best.

# Plotting and tests
emtrends(m4, pairwise ~ location, var = "yrs_since_disturbance")
plot(ggpredict(m4, terms = c("yrs_since_disturbance", "location")))
testDispersion(m4)

pred <- ggpredict(m4, terms = c("yrs_since_disturbance [all]", "location"))

ggplot() +
  geom_point(data  = bayes_df,
             aes(x = yrs_since_disturbance, y = N_mean, color = location),
             alpha = 0.5, size = 2.5, shape = 20) +
  geom_ribbon(data = as.data.frame(pred),
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, color = NA) +
  geom_line(data = as.data.frame(pred),
            aes(x = x, y = predicted, color = group),
            linewidth = 1.2) +
  scale_color_few() +
  scale_fill_few() +
  labs(title    = paste("Model fit with raw data for", unique(bayes_df$species)),
    x        = "Years Since Disturbance",
    y        = "N_mean",
    color    = "Location",
    fill     = "Location") +
  theme_bw() 

# Instead of modeling one species, I model all (change in total bird abundance?)
# ~~~~~~~~~~~~~~~~~~~~~~~~ All Birds Modeling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Make a null model
x <- glmmTMB(N_mean ~ 1 + (1|site),
                          data = abundance_data, family = compois(link = "log"))
summary(null_mod)

# Simple model (disturbance age only)
a <- glmmTMB(N_mean ~ yrs_since_disturbance + (1|site),
              data = abundance_data, family = compois(link = "log")) 
summary(a)

# Additive model
b <- glmmTMB(N_mean ~ yrs_since_disturbance + location + (1|site),
              data = abundance_data, family = compois(link = "log")) 
summary(b)

# Interactive model
c <- glmmTMB(N_mean ~ yrs_since_disturbance*location + (1|site),
              data = abundance_data, family = compois(link = "log")) 
summary(c)

# Add year as a predictor (year is a factor)
d <- glmmTMB(N_mean ~ yrs_since_disturbance*location + year + (1|site),
              data = abundance_data, family = compois(link = "log"))                               

summary(d)

# Check AIC
AIC(x, a, b, c, d) |> 
  arrange(AIC)
# Based on AIC, my interactive model with year as a predictor is best.

# Plotting and tests
emtrends(m4, pairwise ~ location, var = "yrs_since_disturbance")
plot(ggpredict(m4, terms = c("yrs_since_disturbance", "location")))
testDispersion(m4)

pred <- ggpredict(m4, terms = c("yrs_since_disturbance [all]", "location"))

ggplot() +
  geom_point(data  = bayes_df,
             aes(x = yrs_since_disturbance, y = N_mean, color = location),
             alpha = 0.5, size = 2.5, shape = 20) +
  geom_ribbon(data = as.data.frame(pred),
              aes(x = x, ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, color = NA) +
  geom_line(data = as.data.frame(pred),
            aes(x = x, y = predicted, color = group),
            linewidth = 1.2) +
  scale_color_few() +
  scale_fill_few() +
  labs(title    = paste("Model fit with raw data for", unique(bayes_df$species)),
       x        = "Years Since Disturbance",
       y        = "N_mean",
       color    = "Location",
       fill     = "Location") +
  theme_bw() 