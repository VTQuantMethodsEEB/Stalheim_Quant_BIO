#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: spOccupancy - Multi-Species Occupancy Model
# Author: Ben Stalheim
# Date: April, 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(lubridate)
library(spOccupancy)

# ~~~~~~~~~~~~ Load data ~~~~~~~~~~~~

load("Data/RDS/bn_dat_filtered_95.rds")
aru_effort <- read_csv("Data/CSVs/aru_effort_long.csv")

# ~~~~~~~~~~~~ Summarise ARU effort to site-year level ~~~~~~~~~~~~
effort <- aru_effort |>
  mutate(
    date      = as.Date(date),
    year      = year(date),
    site_year = paste(site, year, sep = "_")) |>
  filter(year == 2025) |>
  group_by(site_year) |>
  mutate(survey = dense_rank(date)) |>  
  ungroup()

# ~~~~~~~~~~~~ Prepare detection data ~~~~~~~~~~~~

# One row per detection event; survey = rank of recording file within site-year
bn <- bn_dat_filtered_95 |>
  mutate(
    site_year = paste(site, year, sep = "_"),
    date = as.Date(date)) |>
  filter(year == 2025) |>
  group_by(site_year) |>
  mutate(survey = dense_rank(date)) |>   # survey = day
  ungroup() |>
  mutate(detection = 1)

# Collapse to one row per species x site_year x survey (presence/absence)
bn_pa <- bn |>
  select(common_name, site_year, survey, detection) |>
  summarise(detection = max(detection),
            .by = c(common_name, site_year, survey))

survey_lookup <- bn |>
  distinct(site_year, survey)

# ~~~~~~~~~~~~ Define dimensions ~~~~~~~~~~~~
sp.codes   <- sort(unique(bn_pa$common_name))
site.codes <- sort(unique(bn_pa$site_year))
N  <- length(sp.codes)    # number of species
J  <- length(site.codes)  # number of sites
K  <- max(bn_pa$survey)   # maximum number of surveys

# ~~~~~~~~~~~~ Build 3D detection array ~~~~~~~~~~~~

bn_split <- split(bn_pa, bn_pa$common_name)
survey_list <- split(survey_lookup$survey, survey_lookup$site_year)

# Initialize array
y <- array(NA, dim = c(N, J, K))
dimnames(y)[[1]] <- sp.codes
dimnames(y)[[2]] <- site.codes

for (i in seq_along(sp.codes)) {
  
  sp_data <- bn_split[[sp.codes[i]]]
  
  for (j in seq_along(site.codes)) {
    
    surveys_here <- survey_list[[site.codes[j]]]

    site_data <- sp_data[sp_data$site_year == site.codes[j], ]
    
    if (length(surveys_here) == 0) next
    
    for (k in surveys_here) {
      
      if (k %in% site_data$survey) {
        y[i, j, k] <- 1
      } else {
        y[i, j, k] <- 0
      }
    }
    
  }
}

# Check to make sure NAs remain
table(y, useNA = "always")

# ~~~~~~~~~~~~ Build occupancy covariates ~~~~~~~~~~~~

occ.covs <- bn |>
  select(site_year, yrs_since_disturbance, location, year) |>
  distinct() |>
  arrange(match(site_year, site.codes)) |>
  as.data.frame()

# ~~~~~~~~~~~~ Build detection covariates ~~~~~~~~~~~~

effort_mat <- matrix(NA, nrow = J, ncol = K)
rownames(effort_mat) <- site.codes

for (j in seq_along(site.codes)) {
  
  site_eff <- effort |> filter(site_year == site.codes[j])
  
  if (nrow(site_eff) == 0) next
  
  for (k in unique(site_eff$survey)) {
    effort_mat[j, k] <- sum(site_eff$effort_s[site_eff$survey == k]) / 3600
  }
}

effort_mat_scaled <- scale(effort_mat)
det.covs <- list(effort = effort_mat_scaled)

# ~~~~~~~~~~~~ Fit multi-species occupancy model ~~~~~~~~~~~~
fit <- msPGOcc(
  occ.formula = ~ scale(yrs_since_disturbance)*location,
  det.formula  = ~ effort,
  data = list(
    y        = y,
    occ.covs = occ.covs,
    det.covs = det.covs),
  n.samples = 5000,
  n.burn    = 1000,
  n.thin    = 5,
  n.chains  = 3,
  verbose   = TRUE)

# ~~~~~~~~~~~~ Model check ~~~~~~~~~~~~
summary(fit)
ppc.fit <- ppcOcc(fit, fit.stat = "freeman-tukey", group = 1)
summary(ppc.fit)  # Bayesian p-value ~0.5 = good fit

# ~~~~~~~~~~~~ Extract species richness ~~~~~~~~~~~~
z <- fit$z.samples

richness_samples <- apply(z, c(1, 3), sum)

richness_df <- tibble(
  site_year      = site.codes,
  richness_mean  = apply(richness_samples, 2, mean),
  richness_lower = apply(richness_samples, 2, quantile, probs = 0.025),
  richness_upper = apply(richness_samples, 2, quantile, probs = 0.975)) |>
  left_join(
    bn |> select(site_year, yrs_since_disturbance, location, year) |> distinct(),
    by = "site_year")

# Quick plot
ggplot(richness_df, aes(x = yrs_since_disturbance, y = richness_mean,
                        ymin = richness_lower, ymax = richness_upper,
                        color = location, fill = location)) +
  geom_ribbon(alpha = 0.2, color = NA) +
  geom_line() +
  geom_point() +
  labs(x = "Years Since Disturbance", y = "Estimated Species Richness",
       title = "Detection-corrected Species Richness") +
  theme_bw()

# ~~~~~~~~~~~~ Detection probability ~~~~~~~~~~~~
alpha_intercepts <- fit$alpha.samples[, 1:43]
det_prob_mean_effort <- apply(plogis(alpha_intercepts), 2, mean)
det_prob_lower <- apply(plogis(alpha_intercepts), 2, quantile, 0.025)
det_prob_upper <- apply(plogis(alpha_intercepts), 2, quantile, 0.975)

det_prob_df <- tibble(
  common_name = sp.codes,
  det_prob_mean = det_prob_mean_effort,
  det_prob_lower, det_prob_upper) |> arrange(desc(det_prob_mean)) |> 
  print()


# ~~~~~~~~~~~~ Occupancy probability ~~~~~~~~~~~~
dim(fit$psi.samples)

psi_df <- tibble(
  site_year    = rep(site.codes, each = N),
  common_name  = rep(sp.codes, times = J),
  psi_mean     = as.vector(t(apply(fit$psi.samples, c(2, 3), mean))),
  psi_lower    = as.vector(t(apply(fit$psi.samples, c(2, 3), quantile, probs = 0.025))),
  psi_upper    = as.vector(t(apply(fit$psi.samples, c(2, 3), quantile, probs = 0.975))))

