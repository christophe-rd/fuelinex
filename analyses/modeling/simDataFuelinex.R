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
a <- 1.5
b <- 0.4
sigma_ids <- 0.8/2.57
sigma_y <- 0.3

# replicates per treatment
n_pertreat <- 100
# treatment
treat <- c("cc", "ww")
# total number of trees
n <- n_pertreat * length(treat)

# partial pooling
a_ids <- rnorm(n, 0, sigma_ids)

# gdd for W/W
gdd_cc <- round(rnorm(n_pertreat, 2000, 100))

# gdd for C/C
gdd_ww <- round(rnorm(n_pertreat, 1600, 100))

# scaled predictor that i call gddcons
gddcons <- c(gdd_cc, gdd_ww) / 200

# error
error <- rnorm(n, 0, sigma_y)

# calculate biomass
biomass <- a + a_ids + b * gddcons + error

# create ids by treatements
tree_ids <- 1:n_trees                               
ids <- rep(tree_ids, times = length(treat_levels))  

# create df
simdf_biomass <- data.frame(
  ids = ids,
  treat = rep(treat, each = n_pertreat),
  gdd = c(gdd_cc, gdd_ww),  
  gddcons = gddcons,        
  a = a,
  b = b,
  a_ids = a_ids,
  biomass = biomass
)
simdf_biomass

# look up difference by treatment
treatcomparison_biomass <- ggplot(simdf_biomass, aes(x = treat, y = biomass, color = treat)) +
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
treatcomparison_biomass
ggsave("figures/sim/treatcomparison_biomass.jpeg", treatcomparison_biomass, width = 6, height = 4)

####### Model #######
if(runmodels) {
  fitbiomass <- stan_lmer(
    biomass ~ gddcons + (1 | ids),  
    data = simdf_biomass,
    chains = 4,
    iter = 4000,
    core=4
  )
}



# === ===  === ===  === ===  === === #
##### CC, WW on leaf drop date #####
# === ===  === ===  === ===  === === #
# b is + because leaf drop is later when more gdd (estimated here from leaf out to budset)
a <- 230
b <- 5 
sigma_y <- 3

# replicates per treatment
n_pertreat <- 100
treat <- c("cc", "ww")
n <- n_pertreat * length(treat)

# partial pooling
sigma_ids <- 2 / 2.57
a_ids <- rnorm(n, 0, sigma_ids)

# gdd for each treatment
gdd_cc <- round(rnorm(n_pertreat, 2000, 100))
gdd_ww <- round(rnorm(n_pertreat, 1600, 100))

# scaled predictor
gddcons <- c(gdd_cc, gdd_ww) / 200

# error term
error <- rnorm(n, 0, sigma_y)

# simulate leaf-out date
ldd <- a + a_ids + b * gddcons + error

# IDs
ids <- paste0(rep(treat, each = n_pertreat), "_", rep(1:n_pertreat, times = 2))

# data frame
simdf <- data.frame(
  ids = ids,
  treat = rep(treat, each = n_pertreat),
  gdd = c(gdd_cc, gdd_ww),
  gddcons = gddcons,
  a = a,
  b = b,
  a_ids = a_ids,
  ldd = ldd
)
simdf

hist(simdf$ldd)

# look up difference by treatment
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

