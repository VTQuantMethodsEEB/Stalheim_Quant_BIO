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

# Create some other dataframes to model from ~~~~~~~~~~~~~~~~~~

# This creates total rel abund and ratios for my 2 guilds of species (disturbance-dependent or not)
guild_ratio <- community_df |>  
  group_by(site, year, yrs_since_disturbance, location) |> 
  summarise(
    dd_abund  = sum(N_mean[disturbance_dependent == 1]),
    ndd_abund = sum(N_mean[disturbance_dependent == 0]),
    dd_ratio  = dd_abund / (dd_abund + ndd_abund),
    .groups = "drop")

# This calculates species richness of all species and by guild
richness_df <- community_df |> 
  filter(N_mode > 0) |> 
  group_by(site, year, yrs_since_disturbance, location) |> 
  summarise(richness = n_distinct(species), 
            dd_richness  = n_distinct(species[disturbance_dependent == 1]),
            ndd_richness  = n_distinct(species[disturbance_dependent == 0]),
            .groups = "drop")

# Analyze the effects on a single species at a time (adjust the filter as needed)
species_df <- community_df |> 
  filter(species == "Northern Bobwhite")

# Same as above, but only look at mining points

mine_species_df <- community_df |> 
  filter(location == "Mine",
         species == "Northern Bobwhite")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Generalized Linear Models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GLM looking at species richness as a response to disturbance age
glm_species_richness_mod <- glm(richness ~ yrs_since_disturbance*location,
                                  data = richness_df, family = Gamma)

summary(glm_species_richness_mod)

plot(allEffects(glm_species_richness_mod))

testDispersion(glm_species_richness_mod) # Well that don't seem right
simulationOutput <- simulateResiduals(fittedModel = glm_species_richness_mod, plot = T)

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




