#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Linear Models... WOO!
# Author: Ben Stalheim
# Date: March, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Getting Started
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages
library(tidyverse)
library(performance)
library(lme4)
library(corrplot)

# Load data
load("Data/RDS/bn_dat_filtered_95.rds")

# Hypothesis: Northern Bobwhite detection rate (and/or relative abundance or occupancy)
# declines with reclamation age (time since a mining site was reclaimed).

# Note: This is count data, so I assume it should be modeled using a Poisson distribution.
# But to start, I will go through the normal distribution.

# Limit the data to Northern Bobwhite at the mine
nobo_mine <- bn_dat_filtered_95 |> 
  filter(common_name == "Northern Bobwhite",
         location == "mine")

# Summarise the data to make it easier for the model
nobo_sum <- nobo_mine |> 
  group_by(site, yrs_since_disturbance) |> 
  summarise(detections = n(),
            detections_day = detections/n_distinct(date))

# Model the relationship
bob_mod1 <- lm(detections_day~yrs_since_disturbance, data = nobo_sum)
lm(detections_day~yrs_since_disturbance, data = nobo_sum)
summary(bob_mod1)
# This summary is showing me that the number of Northern Bobwhite detections does
# decline with years since disturbance. The p-value is 0.00207, a significant result.
# 

# Base R plots:
par(mfrow=c(2,2)) 
bob_mod1 <- lm(detections~yrs_since_disturbance, data = nobo_sum) 
plot(bob_mod1)
# The QQ plot shows some clear deviations from normality as the quantiles do not follow
# the 1:1 line.

resid(bob_mod1)
hist(resid(bob_mod1))
shapiro.test(resid(bob_mod1))
# The Shapiro test and the histogram show that my residuals are somewhat normally distributed.
# And therefore, the normal distribution could be used.

# Performance package check
check_model(bob_mod1)
# This does not appear to work for me


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    2nd Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Eastern Screech-Owl
easo_mine <- bn_dat_filtered_95 |> 
  filter(common_name == "Eastern Screech-Owl",
         location == "mine")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Correlations 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
M <- cor(nobo_sum)

