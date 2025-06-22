# 28 May 2025
# CRD
# Goal is to start data simulation for fuelinex treatments

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
options(max.print = 200) 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(ggplot2)
library(rstanarm)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
set.seed(123)
runmodels <- FALSE
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

# === === === === === === === === === === === === === === === === 
#### Step 1. Come up with a model ####
# === === === === === === === === === === === === === === === === 
# start by simulating biomass as a function of gdd for the two treamtents with potentially the biggest treatment effect: warm spring warm fall and cool spring cool fall. Only estimating the current year biomass increment for now

# === === === === === === === === === === === === === === === === 
#### Step 2. Simulate data ####
# === === === === === === === === === === === === === === === === 

# === === === === === === === #
##### Biomass X Gdd cons #####
# === === === === === === === #
a <- 5
b <- 0.9
rep <- 15
sigma_y <- 0.3
sigma_treat <- 0.4
error <- rnorm(rep, 0, sigma_y)

# === === === === === === === === === #
### treatment ww ###
# === === === === === === === === === #
# set spp names and ids 
ids_ww <- paste0(rep("ww", each = rep), "", 1:rep)  
n_ids_ww <- length(ids_ww)

# partial pooling
a_ids_ww <- rnorm(n_ids_ww, 0, sigma_treat)

# gdd per treatment and devide by constant
gdd_ww <- round(rnorm(rep, 2000, 100))
gddcons_ww <- gdd_ww / 200

# calculate biomass
biomass_ww <- a + a_ids_ww + b * gddcons_ww + error

# set df
sim_biomass_ww <- data.frame(
  ids = ids_ww,
  treat = "ww",
  gddcons = gddcons_ww,
  b = b,
  a = a,
  a_ids = a_ids_ww,
  sigma_y = sigma_y,
  biomass = biomass_ww
)
sim_biomass_ww
plot(biomass~gddcons, sim_biomass_ww)

# === === === === === === === === === #
### treatment cc ###
# === === === === === === === === === #
# set spp names and ids 
ids_cc <- paste0(rep("cc", each = rep), "", 1:rep)  
n_ids_cc <- length(ids_cc)

# partial pooling
a_ids_cc <- rnorm(n_ids_cc, 0, sigma_treat)

# gdd per treatment and devide by constant
gdd_cc <- round(rnorm(rep, 1600, 100))
gddcons_cc <- gdd_cc / 200

# calculate biomass
biomass_cc <- a + a_ids_cc + b * gddcons_cc + error

# set df
sim_biomass_cc <- data.frame(
  ids = ids_cc,
  treat = "cc",
  gddcons = gddcons_cc,
  b = b,
  a = a,
  a_ids = a_ids_cc,
  sigma_y = sigma_y,
  biomass = biomass_cc
)

# === === === === === === === === === #
# bind both biomass df
sim_biomass_biomass <- rbind(sim_biomass_ww, sim_biomass_cc)
# === === === === === === === === === #
simplot <- ggplot(sim_biomass_biomass, aes(x = gddcons, y = biomass, color = treat)) +
  geom_point() +
  scale_color_manual(values = c("ww" = "#FF8C00", "cc" = "#1f78b4")) +
  # geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
simplot



# look up difference by treat
treatcomparison_biomass <- ggplot(sim_biomass_biomass, aes(x = treat, y = biomass, color = treat)) +
  geom_boxplot(
    width = 0.3, 
    alpha = 0.2, 
    outlier.shape = NA,
    color = "black"   
  ) +
  geom_jitter(
    width = 0.1, 
    alpha = 0.6, 
    size = 2
  ) +
  scale_color_manual(values = c("ww" = "#FF8C00", "cc" = "#1f78b4")) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)
  )
treatcomparison_biomass
ggsave("figures/sim/treatcomparison_biomass.jpeg", treatcomparison_biomass, width = 6, height = 4)

####### Model #######
if(runmodels) {
  fitbiomass <- stan_lmer(
    biomass ~ gddcons + (1 | ids),  
    data = sim_biomass_ww,
    chains = 4,
    iter = 4000,
    core=4
  )
  fitbiomass
  }

# parameter recovery
fitef_biomass <- ranef(fitbiomass)
ids_df <- fitef_biomass$ids

# Now extract the tree IDs and intercepts
biomass_a_idswithranef <- data.frame(
  ids = rownames(ids_df),
  fit_biomass_a_ids = ids_df[["(Intercept)"]]
)
inter_biomass <- as.data.frame(posterior_interval(fitbiomass))
inter_biomass$messyids <- rownames(inter_biomass)
biomass_a_ids_messy <- subset(inter_biomass, grepl("ids", messyids))
biomass_a_ids_messy$ids <- sub(".*ids:([^]]+)]", "\\1", biomass_a_ids_messy$messyids)
# remove non necessary columns
biomass_a_ids_messy <- biomass_a_ids_messy[, c("ids", "5%", "95%")]
# renames 5% and 95%
colnames(biomass_a_ids_messy) <- c("ids", "per5", "per95")
# merge both df by ids
a_ids_mergedwithranef <- merge(biomass_a_idswithranef, biomass_a_ids_messy, by = "ids")
# add simulation data and merge!
simcoeftoplot2 <- sim_biomass_ww[, c("ids", "a_ids")]
colnames(simcoeftoplot2) <- c("ids", "sim_biomass_a_ids")
a_ids_mergedwithranef <- merge(simcoeftoplot2, a_ids_mergedwithranef, by = "ids")

plot_a_ids_mergedwithranef<- ggplot(a_ids_mergedwithranef, aes(x = sim_biomass_a_ids, y = fit_biomass_a_ids)) +
  geom_point(color = "blue", size = 2) +
  geom_errorbar(aes(ymin = per5, ymax = per95), width = 0, color = "darkgray", alpha=0.1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
  # labs(x = "Simulated a_ids", y = "Model a_ids", title = "") +
  theme_minimal()
plot_a_ids_mergedwithranef
ggsave("figures/a_ids_mergedwithranef.jpeg", plot_a_ids_mergedwithsum, width = 8, height = 6)


# === ===  === ===  === ===  === === #
##### CC, WW on leaf drop date #####
# === ===  === ===  === ===  === === #
# b is + because leaf drop is later when more gdd (estimated here from leaf out to budset)
a <- 230
b <- 5 
rep <- 100
sigma_y <- 5
sigma_treat <- 2
error <- rnorm(rep, 0, sigma_y)

# === === === === === === === === === #
### treatment ww ###
# === === === === === === === === === #
# set spp names and ids 
ids_ww <- paste0(rep("ww", each = rep), "", 1:rep)  
n_ids_ww <- length(ids_ww)

# partial pooling
a_ids_ww <- rnorm(n_ids_ww, 0, sigma_treat)

# gdd per treatment and devide by constant
gdd_ww <- round(rnorm(rep, 2000, 100))
gddcons_ww <- gdd_ww / 200

# calculate biomass
ldd_ww <- a + a_ids_ww + b * gddcons_ww + error

# set df
sim_ldd_ww <- data.frame(
  ids = ids_ww,
  treat = "ww",
  gddcons = gddcons_ww,
  b = b,
  a = a,
  a_ids = a_ids_ww,
  sigma_y = sigma_y,
  ldd = ldd_ww
)
sim_ldd_ww
# === === === === === === === === === #
### treatment cc ###
# === === === === === === === === === #
# set spp names and ids 
ids_cc <- paste0(rep("cc", each = rep), "", 1:rep)  
n_ids_cc <- length(ids_cc)

# partial pooling
a_ids_cc <- rnorm(n_ids_cc, 0, sigma_treat)

# gdd per treatment and devide by constant
gdd_cc <- round(rnorm(rep, 2000, 100))
gddcons_cc <- gdd_cc / 200

# calculate biomass
ldd_cc <- a + a_ids_cc + b * gddcons_cc + error

# set df
sim_ldd_cc <- data.frame(
  ids = ids_cc,
  treat = "cc",
  gddcons = gddcons_cc,
  b = b,
  a = a,
  a_ids = a_ids_cc,
  sigma_y = sigma_y,
  ldd = ldd_cc
)
sim_ldd_cc


# look up difference by treat
treatcomparison_ldd <- ggplot(simdf, aes(x = treat, y = ldd, color = treat)) +
  geom_boxplot(
    width = 0.3, 
    alpha = 0.2, 
    outlier.shape = NA,
    color = "black"   
  ) +
  geom_jitter(
    width = 0.1, 
    alpha = 0.6, 
    size = 2
  ) +
  scale_color_manual(values = c("cc" = "#1f78b4", "ww" = "#FF8C00")) +
  theme_minimal() +
  theme(
    legend.position = "none",  
    plot.title = element_text(hjust = 0.5)
  )
treatcomparison_ldd
ggsave("figures/sim/treatcomparison_ldd.jpeg", treatcomparison_ldd, width = 6, height = 4)

# run model!
if(runmodels) {
  fitldd <- stan_lmer(
    ldd ~ gddcons + (1 | ids),  
    data = simdf,
    chains = 4,
    iter = 4000,
    core=4
  )
}

