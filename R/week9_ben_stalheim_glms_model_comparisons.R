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

turnover_data <- turnover_data |> # I am removing this site due to low sample coverage in 2025 (only 7 days)
  filter(site != "O-4")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#      Hypotheses ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# My main hypothesis is that land use type influences bird community turnover in
# the southeastern United States. I predict that reclaimed mining land will have
# higher turnover compared to mature and young longleaf pine forests.

# My other hypothesis is that time since a disturbance event effects bird community
# turnover rates. In these disturbance-mediated environments, I predict that turnover
# will increase shortly after a disturbance event as species move in and out, and that
# turnover will eventually decline and stabilize after some time.

# At reclaimed mining lands, where the only disturbance event is mine reclamation
# (they don't introduce fire or thinning), I expect turnover to follow this pattern.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Modeling ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

plot(ggpredict(turnover_mod, terms = c("location"), bias_correction = TRUE))
# This plot shows them predicted means with their standard errors for each location

# Pairwise comparisons
emmeans(turnover_mod, pairwise ~ location, type = "response")
# After this pairwise comparison and p-value adjustment, there are no significant
# differences between the means of the land-use types.

# Using emmeans to get SE for eventual plotting
emm <- emmeans(turnover_mod, ~ location, type = "response") |>
  as.data.frame() |> 
  print()
# When adjusting for pairwise comparisons, there are now no significant differences
# between the pairs. However, the Mine and Okefenokee still show strong signs of being
# different. 

# Test Dispersion
testDispersion(turnover_mod)
# That doesn't look too bad!

# ~~~~~~~~~~~~~~~~~~~~~ Plot raw data and model ~~~~~~~~~~~~~~~~~~~~~~~
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
  geom_errorbar(data = emm, aes(x = location, y = response,
                                ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.1, linewidth = 0.8) +
  scale_color_few() +
  theme_bw() +
  labs(title = "Predicted Total Turnover by Location",
       x = NULL,
       y = "Total Turnover (Jaccard)") +
  theme(legend.position = "none")

# I have never written a results statement... but here goes!!

# ~~~~~~~~~~~~~~~ Results Statement ~~~~~~~~~~~~~~~~~~~

# Bird community turnover, measured using the Jaccard dissimilarity index differed 
# among land-use types (). The model including only land-use as a fixed effect and site 
# as a random effect was preferred by AIC over the null model and others (Delta_AIC >2). 
# Mean turnover at Okefenokee (0.178, mature longleaf pine) was estimated to be 
# significantly lower compared to reclaimed mining mining area (0.261, p-value = 0.0145).
# Mean turnover at Sansavilla (0.212, young longleaf pine) did not significantly differ from
# the reclaimed mining area (p-value = 0.0939), but the model did estimate lower mean
# turnover (0.212 at Sansavilla vs 0.261 at Mission Mine). When comparing between
# locations using emmeans and implementing Tukey-adjusted p-values, there was still
# a significant difference in estimated mean turnover between the Mission Mine and 
# Okefenokee. 

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
#     Model Selection ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~ Likelihood Ratio Test ~~~~~~~~~~~~~~~~~~~~~~~~

# I am still going to use the same models as before (4 turnover models and null model)
anova(turnover_mod, turnover_mod2, turnover_mod3, turnover_mod4, null_mod, test = "LRT") |> 
  arrange(AIC)
# According to the LRT, the turnover model 4 (interactive model) is preferred. However,
# this model gets penalized by AIC because it has more parameters and more Df than
# the other models. 

# ~~~~~~~~~~~~~~~~~~~~~~ AIC Selection ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
AIC(turnover_mod, turnover_mod2, turnover_mod3, turnover_mod4, null_mod) |> 
  arrange(AIC)

# And now by AICc
library(MuMIn)
AICc(turnover_mod, turnover_mod2, turnover_mod3, turnover_mod4, null_mod)|> 
  arrange(AICc)

# This shows that turnover_mod with the single parameter is preferred by AIC and AICc.
# This falls mostly in line with my main hypothesis that the locations (i.e., ecosystem)
# has the greatest effect on bird community turnover in my study area.

# Alternate formatting
library(AICcmodavg)
aictab(cand.set=list(turnover_mod, turnover_mod2, turnover_mod3, turnover_mod4, null_mod),
       modnames=c("tmod1","tmod2","tmod3","tmod4","nullmod")) 

aictab(cand.set=list(turnover_mod, turnover_mod2, turnover_mod3, turnover_mod4, null_mod),
       modnames=c("tmod1","tmod2","tmod3","tmod4","nullmod"), second.ord = F)

# ~~~~~~~~~~~~~~~~~~~~~~ Model Selection Thoughts ~~~~~~~~~~~~~~~~~~~~~~~~~~

# The results of model selection are similar, but some methods penalize models for
# having many parameters (AIC) while log likelihood appears less sensitive to this.
# In both scenarios, I think I would choose the same model, because it is the simplest
# model, preferred by AIC, and very similar in log likelihood to all of the others. 

#~~~~~~~~~~~~~~~~~~~~~~ Some Plotting ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Turnover ~ Disturbance Age Faceted by location
ggplot(turnover_data, aes(x = yrs_since_disturbance, y = total_turnover, color = location)) +
  geom_point(size = 3, shape = 1) +
  geom_smooth(method = "glm", se = TRUE, color = "purple4", alpha = 0.2) +
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
