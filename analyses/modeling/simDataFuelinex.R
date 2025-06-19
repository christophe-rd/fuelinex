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
runmodels <- FALSE

# === === === === === === === === === === === === === === === === 
#### Step 1. Come up with a model ####
# === === === === === === === === === === === === === === === === 
# start by simulating biomass as a function of gdd for the two treamtents with potentially the biggest treatment effect: warm spring warm fall and cool spring cool fall. Only estimating the current year biomass increment for now

# === === === === === === === === === === === === === === === === 
#### Step 2. Simulate data ####
# === === === === === === === === === === === === === === === === 
a <- 1.5
b <- 0.4

sigma_y <- 0.3

# replicates per treatment
n_pertreat <- 30
# treatment
treat <- c("cc", "ww")
# total number of trees
n <- n_pertreat * length(treat)

# partial pooling
sigma_ids <- 0.8 / 2.57
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
ids <- paste0(rep(treat, each = n_pertreat), "_", rep(1:n_pertreat, times = 2))

# create df
simdf <- data.frame(
  id = ids,
  treat = rep(treat, each = n_pertreat),
  gdd = c(gdd_cc, gdd_ww),  
  gddcons = gddcons,        
  a = a,
  b = b,
  a_ids = a_ids,
  biomass = biomass
)
simdf

# look up difference by treatment
treatcomparison <- ggplot(simdf, aes(x = treat, y = biomass, color = treat)) +
  geom_jitter(width = 0.1, alpha = 0.6, size = 2) +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "pointrange", 
               size = 0.8,
               color = "black") +
  scale_color_manual(values = c("cc" = "#1f78b4", "ww" = "#33a02c")) +
  labs(x = "", 
       y = "", 
       title = "") +
  theme_minimal() +
  theme(legend.position = "none") 
# save ggplot
ggsave("figures/treatcomparison.jpeg", treatcomparison, width = 6, height = 4)

ggplot(simdf, aes(x = gddcons, y = biomass, color = treat)) +
  geom_point()+
  # Customize appearance
  scale_color_manual(values = c("cc" = "#1f78b4", "ww" = "#33a02c")) +
  labs(x = "", 
       y = "", 
       title = "") +
  theme_minimal() +
  theme(legend.position = "none") 
