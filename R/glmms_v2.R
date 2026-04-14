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
library(glmmTMB)

# Load Data
turnover_data <- read_csv("Data/CSVs/turnover_data.csv")

#~~~~~~~~~~~~~~~~~ Hypothesis ~~~~~~~~~~~~~~~~~~~~~~~~

# My main hypothesis is that turnover (measured using Jaccard Dissimilarity Index) is
# higher at reclaimed mining location compared to mature longleaf pine (Okefenokee) 
# and young longleaf pine (Sansavilla) ecosystems. 

# My other hypothesis is that turnover decreases with disturbance age at mine reclamation
# sites. I predict that the bird communities will stabilize after the disturbance event
# and turnover will decrease. 

# ~~~~~~~~~~~~~~~~~~~~~~~ Model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I first need to set location as a factor (I like the levels, because Mine is first)
turnover_data <- turnover_data |>
  mutate(location = factor(location))

# I am using a beta distribution for all models because my response variable is bounded
# between 0 and 1 as a beta diversity metric.
turnover_mod <- glmmTMB(total_turnover ~ location + (1 | site), data = turnover_data,
                        family = beta_family(link = "logit"))
summary(turnover_mod)
# This summary shows that the model is predicting Okefenokee has a significantly
# different mean turnover value than the Mine. While Sansavilla is not significantly
# different, the model shows there are expected differences between this location.

plot(ggpredict(turnover_mod, terms = c("location")))
# This plot shows them predicted means with their standard errors for each location

emmeans(turnover_mod, pairwise~location, type="response")
# When adjusting for pairwise comparisons, there are now no significant differences
# between the pairs. However, the Mine and Okefenokee still strong signs of being
# different. 

# Test Dispersion
testDispersion(turnover_mod)
# That doesn't look too bad!

# Plotting the raw data witht the predicted data ~~~~~~~~~
dat.new <- expand.grid(
  location = unique(turnover_data$location))

# Predict on the response scale
dat.new$yhat <- predict(turnover_mod, 
                        newdata = dat.new, 
                        type = "response",
                        re.form = NA)  

# Plot raw data points and model predictions
ggplot(turnover_data, aes(x = location, y = total_turnover, color = location)) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.4) +
  geom_point(data = dat.new, aes(x = location, y = yhat),
             size = 4, shape = 18) + 
  scale_color_few() +
  theme_bw() +
  labs(title = "Predicted Total Turnover by Location",
       x = NULL,
       y = "Total Turnover (Jaccard)") +
  theme(legend.position = "none")

#~~~~~~~~~~~~~~~~~~~ Making more models ~~~~~~~~~~~~~~~~~~~~~~~~

# This model tests my other predictor variable (disturbance age)
turnover_mod2 <- glmmTMB(total_turnover ~ yrs_since_disturbance + (1 | site), data = turnover_data,
                         family = beta_family(link = "logit"))
summary(turnover_mod2)
# Based on this summary output, disturbance age has a positive effect on turnover,
# meaning older sites have higher turnover. But this is likely conflated with the 
# fact that the mine has the oldest sites and we already saw that it has higher turnover.
plot(ggpredict(turnover_mod2, terms = c("yrs_since_disturbance")))


# Additive Model of Disturbance Age and Location
turnover_mod3 <- glmmTMB(total_turnover ~ yrs_since_disturbance + location + (1 | site), data = turnover_data,
                         family = beta_family(link = "logit"))
summary(turnover_mod3)
# Even with the extra parameters added, the Okefenokee is still showing near-significant
# difference from the Mine in terms of mean turnover rate.
plot(ggpredict(turnover_mod3, terms = c("yrs_since_disturbance", "location")))

# Interactive Model of Disturbance Age and Location
turnover_mod4 <- glmmTMB(total_turnover ~ yrs_since_disturbance * location + (1 | site), data = turnover_data,
                         family = beta_family(link = "logit"))
summary(turnover_mod4)
# This shows that both Sansavilla and Okefenokee show increasing turnover rate as disturbance
# age increases. But that the Mine shows a decreasing turnover rate as disturbance age 
# increases, in line with my hypothesis. While there are no significant results between
# the effect sizes between locations, turnover does appear to change differently across
# ecosystems. 
plot(ggpredict(turnover_mod4, terms = c("yrs_since_disturbance", "location")))
emtrends(turnover_mod4, specs = ~ location ,var = "yrs_since_disturbance",
         type = "response")

# Null Model
null_mod <- glmmTMB(total_turnover ~ 1, data = turnover_data,
                    family = beta_family(link = "logit"))
summary(null_mod)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Model Selection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I am just arranging the models by AIC using anova()
anova(turnover_mod, turnover_mod2, turnover_mod3, turnover_mod4, null_mod) |> 
  arrange((AIC))

# And now by AICc
library(MuMIn)
AICc(turnover_mod, turnover_mod2, turnover_mod3, turnover_mod4, null_mod)

# This shows that turnover_mod with the single parameter is preferred by AIC and AICc.
# This falls mostly in line with my main hypothesis that the locations (i.e., ecosystem)
# has the greatest effect on bird community turnover in my study area.

#~~~~~~~~~~~~~~~~~~~~~~ More Plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Turnover ~ Disturbance Age Faceted by location
ggplot(turnover_data, aes(x = yrs_since_disturbance, y = total_turnover, color = location)) +
  geom_point(size = 3, shape = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "purple4", alpha = 0.2) +
  facet_grid(~ location) +
  theme_bw() +
  labs(x = "Years Since Disturbance",
       y = "Jaccard Turnover",
       title = "Does turnover accelerate with time since disturbance?")

# Turnover ~ Disturbance W/O Faceting
ggplot(turnover_data, aes(x = yrs_since_disturbance, y = total_turnover, color = location)) +
  geom_point(size = 3, shape = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "purple4", alpha = 0.2) +
  theme_bw() +
  labs(x = "Years Since Disturbance",
       y = "Jaccard Turnover",
       title = "Does turnover accelerate with time since disturbance?")
