#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Generalized Linear Models... Still WOO!
# Author: Ben Stalheim
# Date: April, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages
library(tidyverse)
library(emmeans)
library(lme4)
library(ggthemes)
library(effects)
library(ggeffects)
library(MASS)
library(DHARMa)

# Load relative abundance data
community_df <- read_csv("Data/CSVs/relative_abundance_estimates.csv") |> 
  mutate(year = as_factor(year))
# This dataframe stores all of my relative abundance estimates made using 
# Royle-Nichols models from the unmarked :: occuRN() function.

# Create some other dataframes to model from ~~~~~~~~~~~~~~~~~~

# I need only one year of data to avoid pseudoreplication issues and use glm vs glmm, so...
community_2025 <- community_df |> 
  filter(year == 2025)

# This calculates species richness of all species and by guild
richness_df <- community_df |> 
  filter(N_mode > 0) |> 
  group_by(site, year, yrs_since_disturbance, location) |> 
  summarise(richness = n_distinct(species),
            .groups = "drop")

richness_df_2025 <- richness_df |> 
  filter(year == 2025)

# Analyze the effects on a single species at a time (adjust the filter as needed)
sp <- "Bachman's Sparrow"

species_df <- community_df |> 
  filter(species == sp)

species_df_2025 <- species_df |> 
  filter(year== 2025)

# Same as above, but only look at mining points
mine_species_df <- community_df |> 
  filter(location == "Mine",
         species == sp)

# Lambda estimates for entire location rather than by survey point
sp_lambda <- species_df |> 
  group_by(year, location) |> 
  summarise(lambda = mean(lambda), .groups = "drop")

ggplot(sp_lambda, aes(x = year, y = lambda, color = location, group = location)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_colorblind() +
  labs(
    title = paste("Change in λ of", unique(species_df$species), "Across Years by Location"),
    x = "Year",
    y = "λ (Mean Relative Abundance)") +
  theme_bw()

ggplot(species_df, aes(x = year, y = N_mean , group = site , color = location
                       )) +
  #geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_colorblind() +
  labs(
    title = paste("N_mean Across Years for", unique(species_df$species)),
    x = "Year",
    y = "N_mean") +
  theme_bw()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Generalized Linear Models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Does relative abundance of modeled species in 2025 vary by disturbance age at
# the different study locations?
glm1 <- glm(N_mean ~ yrs_since_disturbance*location, data = species_df_2025,
            family = Gamma(link = "log"))
summary(glm1)
plot(allEffects(glm1))

# Does species richness vary depending on location in 2025?
glm2 <- glm(richness ~ location, data = richness_df_2025, family = poisson)

summary(glm2)

plot(allEffects(glm2))

testDispersion(glm2) # Well that don't seem right
simulationOutput <- simulateResiduals(fittedModel = glm2, plot = T)

# For disturbance-dependent bird species richness
glm_dd_species_richness_mod <- glm(dd_richness ~ yrs_since_disturbance*location,
                                data = richness_df, family = poisson)

summary(glm_dd_species_richness_mod)

plot(allEffects(glm_dd_species_richness_mod))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Generalized Linear Mixed Models
# I am going to use Negative Binomial Models because all
# of my poisson models looked real bad
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
single_species <- glmer.nb(N_mode ~ yrs_since_disturbance*location + (1|site),
                                             data = species_df)

summary(single_species)

plot(allEffects(single_species))

testDispersion(single_species_nb) # Well that does seem better
simulationOutput <- simulateResiduals(fittedModel = single_species_nb, plot = T)

# Right now, I have the species set to Northern Bobwhite. This model is showing me
# the effects of disturbance age on relative abundance of this species. This model is showing
# negative effects at all locations, but the effect of disturbance age on relative abundance
# is significant at the mine. A thing to note, is that disturbance ages only reach 2 years
# at the Okefenokee, so that is why those error bars get much wider... I think.

# What happens if I separate out the mine before modeling and remove the interactive effect?
mine_single_species <- glmer.nb(N_mode ~ yrs_since_disturbance + (1|site),
                        data = mine_species_df)

summary(mine_single_species)

plot(allEffects(mine_single_species))

# Plot the model with my raw data
ggplot(mine_species_df, aes(x = yrs_since_disturbance, y = N_mode)) +
  geom_point() +
  geom_smooth(color = "green4", fill = "grey60", method = "glm", 
              method.args = list(family = "poisson")) +
  theme_classic() +
  labs(x = "Years Since Disturbance", y = "Relative Abundance") +
  ggtitle("Effect of Disturbance Age on Relative Abundance of Northern Bobwhite at the Mine")




