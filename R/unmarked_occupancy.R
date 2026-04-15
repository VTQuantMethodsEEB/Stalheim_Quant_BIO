#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: Community Occupancy Model with occuComm
# Author: Ben Stalheim 
# Date: April 2026
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(lubridate)
library(unmarked)

# Load data 
load("Data/RDS/bn_dat_filtered_95.rds")
aru_effort <- read_csv("Data/CSVs/aru_effort_long.csv")

sp_richness <- bn_dat_filtered_95 |> 
  group_by(site, year) |> 
  summarise(richness = n_distinct(common_name)) |> 
  filter(year == 2025) |> 
  print(n = Inf)

# Prepare detection data (presence/absence per day)
bn_pa <- bn_dat_filtered_95 |>
  mutate(
    site_year = paste(site, year, sep = "_"),
    date = as.Date(date)) |>
  filter(year == 2025) |>
  group_by(site_year, date, common_name) |>  
  summarise(detection = TRUE, .groups = "drop") |>  
  ungroup()

# Define dimensions
sp.codes <- sort(unique(bn_pa$common_name))  # S 
site.codes <- sort(unique(bn_pa$site_year))  # M 
all_dates <- sort(unique(aru_effort$date))       # J 

# Build 3D array: sites × surveys × species (unmarked format)
y <- array(0, dim = c(length(site.codes), length(all_dates), length(sp.codes)))

dimnames(y) <- list(site = site.codes, survey = as.character(all_dates), species = sp.codes)

# Fill array
for(i in seq_along(site.codes)) {
  for(k in seq_along(all_dates)) {
    site_data <- bn_pa |> 
      filter(site_year == site.codes[i], date == all_dates[k])
    if(nrow(site_data) > 0) {
      y[i, k, match(site_data$common_name, sp.codes)] <- site_data$detection
    }
  }
}

# Site covariates
sitecov <- bn_dat_filtered_95 |>
  mutate(site_year = paste(site, year, sep = "_")) |>
  filter(year == 2025) |>
  select(site_year, yrs_since_disturbance, location) |>
  distinct() |>
  slice(match(site.codes, site_year)) |>
  column_to_rownames("site_year") |>
  as.data.frame()

# Observation covariates - effort by site and date
effort_df <- aru_effort |>
  mutate(
    date = as.Date(date),
    site_year = paste(site, year(date), sep = "_"))|>
  filter(year(date) == 2025) |>
  group_by(site_year, date) |>
  summarise(effort_hrs = sum(effort_s) / 3600, .groups = "drop")

# Build effort array (sites × surveys)
effort_array <- array(NA, dim = c(length(site.codes), length(all_dates)))

dimnames(effort_array) <- dimnames(y)[1:2]

for(i in seq_along(site.codes)) {
  site_eff <- effort_df |> filter(site_year == site.codes[i])
  if(nrow(site_eff) > 0) {
    date_idx <- match(site_eff$date, all_dates)
    valid <- !is.na(date_idx)
    effort_array[i, date_idx[valid]] <- site_eff$effort_hrs[valid]
  }
}

# Scale for modeling
effort_array <- scale(effort_array)
obscov <- list(effort = effort_array)

# Species covariates (simple for now)
spcov <- list(species_id = 1:length(sp.codes))
names(spcov$species_id) <- sp.codes

# Create unmarkedFrameOccuComm ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
umf <- unmarkedFrameOccuComm(
  y = y,
  siteCovs = sitecov,
  obsCovs = obscov,
  speciesCovs = spcov)

summary(umf)

# Fit community occupancy model ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit_comm <- occuComm(
  ~ 1 ~ yrs_since_disturbance*location,  # detection (first) ~ occupancy (second)
  data = umf)

summary(fit_comm)

obs_rich<- rowSums(apply(y, c(1,3), max) > 0)

print(data.frame(site_year = site.codes, obs_rich = obs_rich_correct))

rich_est <- richness(fit_comm)
rich_post <- richness(fit_comm, posterior = TRUE)

rich_df <- tibble(
  site_year = site.codes,
  rich_mean = apply(rich_post@samples, 1, mean), 
  rich_lower = apply(rich_post@samples, 1, quantile, 0.025),
  rich_upper = apply(rich_post@samples, 1, quantile, 0.975)) |>
  left_join(sitecov |> rownames_to_column("site_year"), by = "site_year") |> 
  mutate(obs_rich = obs_rich_correct) |>  
  print()

rich_df |> select(site_year, obs_rich, rich_mean) |> 
  mutate(model_higher = rich_mean > obs_rich) |> print()

ggplot(rich_df, aes(yrs_since_disturbance, rich_mean, 
                    ymin = rich_lower, ymax = rich_upper, color = location)) +
  geom_ribbon(alpha = 0.3) +
  geom_line() + geom_point() +
  geom_point(aes(y = obs_rich), shape = 1, size = 3, color = "black") +  
  labs(title = "Community Richness - occuComm", 
       subtitle = "Closed = Model | Open = Observed",
       y = "Species Richness") +
  theme_bw()

# Species detection intercepts (at mean effort)
det_rand <- randomTerms(fit_comm, type = "det", addMean = TRUE)

det_df <- det_rand |>
  filter(Name == "(Intercept)") |>
  mutate(
    p_mean = plogis(Estimate),
    p_lower = plogis(lower),
    p_upper = plogis(upper)) |> 
  select(species = Level, p_mean, p_lower, p_upper, logit_int = Estimate, SE) |>
  arrange(desc(p_mean))

print(det_df |> slice_head(n = 10))

# Model check
par(mfrow = c(2, 2))
plot(fit_comm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract disturbance species indices
disturb_sp <- bn_dat_filtered_95 |>
  filter(disturbance_dependent == 1, year == 2025) |>
  pull(common_name) |> unique()

disturb_idx <- match(disturb_sp, sp.codes)  # Vector of indices
n_disturb <- length(disturb_idx)

# Create zero-padded y array (same dims, only disturb species filled)
y_disturb <- array(0, dim = c(dim(y)[1], dim(y)[2], n_disturb))
dimnames(y_disturb)[[3]] <- disturb_sp

# Copy only disturbance species data
y_disturb[,,1:n_disturb] <- y[,,disturb_idx]

# Species covariates for disturbance species only
spcov_disturb <- lapply(spcov, function(x) x[disturb_idx])

# Build umf
umf_disturb <- unmarkedFrameOccuComm(
  y = y_disturb,
  siteCovs = sitecov,
  obsCovs = obscov,
  speciesCovs = spcov_disturb)

# Fit disturbance-only model
fit_disturb <- occuComm(
  ~ effort ~ scale(yrs_since_disturbance) * location,
  data = umf_disturb)

summary(fit_disturb)

rich_disturb_post <- richness(fit_disturb, posterior = TRUE)

rich_disturb_df <- tibble(
  site_year = site.codes,
  rich_mean = apply(rich_disturb_post@samples, 1, mean),
  rich_lower = apply(rich_disturb_post@samples, 1, quantile, 0.025),
  rich_upper = apply(rich_disturb_post@samples, 1, quantile, 0.975)) |> 
  left_join(sitecov |> 
              rownames_to_column("site_year"), by = "site_year")


ggplot(rich_disturb_df, aes(yrs_since_disturbance, rich_mean,
                            ymin = rich_lower, ymax = rich_upper,
                            color = location)) +
  geom_ribbon(alpha = 0.4) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  labs(title = "Disturbance-Dependent Avian Richness",
       subtitle = "25 species | occuComm | ARU Data",
       x = "Years Since Disturbance", y = "Richness") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_bw(base_size = 14)


det_disturb <- randomTerms(fit_disturb, type = "det", addMean = TRUE) |>
  filter(Name == "(Intercept)") |>
  mutate(p = plogis(Estimate)) |>
  arrange(desc(p)) |> slice_head(n = 5)
print(det_disturb)

matrix_sp <- setdiff(sp.codes, disturb_sp)


rich_total_post <- richness(fit_comm, posterior = TRUE)
rich_df_total <- tibble(
  site_year = site.codes,
  rich_total = apply(rich_total_post@samples, 1, mean),
  rich_total_lower = apply(rich_total_post@samples, 1, quantile, 0.025),
  rich_total_upper = apply(rich_total_post@samples, 1, quantile, 0.975),
  yrs_since_disturbance = sitecov$yrs_since_disturbance,
  location = sitecov$location)

rich_disturb_post <- richness(fit_disturb, posterior = TRUE)
rich_disturb_df <- tibble(
  site_year = site.codes,
  rich_disturb = apply(rich_disturb_post@samples, 1, mean),
  rich_disturb_lower = apply(rich_disturb_post@samples, 1, quantile, 0.025),
  rich_disturb_upper = apply(rich_disturb_post@samples, 1, quantile, 0.975))

rich_combined <- left_join(rich_df_total, rich_disturb_df, by = "site_year") |>
  mutate(
    rich_matrix = rich_total - rich_disturb,  
    rich_matrix_lower = rich_total_lower - rich_disturb_upper,
    rich_matrix_upper = rich_total_upper - rich_disturb_lower)

ggplot(rich_combined, aes(yrs_since_disturbance, rich_total)) +
  geom_ribbon(aes(ymin = rich_total_lower, ymax = rich_total_upper), 
              fill = "grey80", alpha = 0.5) +
  geom_ribbon(aes(ymin = rich_disturb_lower, ymax = rich_disturb_upper), 
              fill = "coral", alpha = 0.7) +
  geom_line(aes(y = rich_total), linewidth = 1.5, color = "black") +
  geom_line(aes(y = rich_disturb), linewidth = 1.2, color = "darkred") +
  geom_point(aes(y = rich_total), size = 3, color = "black") +
  geom_point(aes(y = rich_disturb), size = 3, color = "darkred") +
  facet_wrap(~location, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Years Since Disturbance",
    y = "Species Richness (detection-corrected)",
    title = "Total vs Disturbance-Dependent Richness",
    subtitle = "43 total species (black) vs 25 disturbance specialists (red)\noccuComm | ARU passive acoustic monitoring") +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12))

ggplot(det_df, aes(reorder(species, p_mean), p_mean)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(x = "Species", y = "Daily Detection Probability", 
       title = "ARU Detection Hierarchy - Disturbance Guild") +
  theme_bw()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#             OccuMulti()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
