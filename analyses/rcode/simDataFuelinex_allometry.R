# 3 February 2026
# CRD
# Goal is to simulate allometries to estimate biomass from diameter and height

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
options(max.print = 200) 
options(digits = 3)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(ggplot2)
library(rstan)
library(wesanderson)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
set.seed(123)
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

mea <- read.csv2("output/cleanedMeasurements.csv", sep = ",", header = TRUE)

mea$height   <- as.numeric(mea$height)
mea$diameter <- as.numeric(mea$diameter)
mea <- mea[order(mea$tree_ID, mea$year), ]

mea$height_inc <- ave(
  mea$height,
  mea$tree_ID,
  FUN = function(x) c(NA, diff(x))
)

mea$dia_inc <- ave(
  mea$diameter,
  mea$tree_ID,
  FUN = function(x) c(NA, diff(x))
)

ggplot(mea) +
  geom_histogram(aes(x = height_inc, color = species, fill = species)) +
  facet_wrap(~species)

aggregate(height_inc ~ species, data = mea, FUN = mean)
aggregate(dia_inc ~ species, data = mea, FUN = mean)

# === === === === === === === === === === === === === === === === 
#### Simulate data ####
# === === === === === === === === === === === === === === === === 

# === === === === === === === #
##### Biomass as intercept only #####
# === === === === === === === #
# above ground biomass (agb) = b1(diameter2*height)^b2
# agb is in meters kg, so *1000 for grams
# diameter is in cm. keep it
# height is in meters *1000

# so the parameter to estimate are b1 and b2
b <- 15 # biomass in grams
sigma_y <- 1

n_sp <- 10
n_per_spp <- 90

spp <- rep(rep(1:n_sp, each = n_per_spp))

ids <- 1: length(spp)

N <- length(ids)

error <- rnorm(N, 0, sigma_y)

# set coefficients per spp
b1 <- rlnorm(n_sp, log(0.1), 0.2)
b2 <- rlnorm(n_sp, log(0.05), 0.2)

simdf <- data.frame(
  ids = ids,
  spp = spp,
  b = b,
  b1 = b1[simdf$spp], 
  b2 = b2[simdf$spp], 
  error = error
)
simdf

# add height and diameter mean increment for each species
hmean <- abs(rnorm(n_sp, 0.3, 0.1)) # in meters
dmean <- abs(rnorm(n_sp, 0.2, 0.1)) # in cm

# individual level variations
simdf$height <- abs(rnorm(N, hmean[simdf$spp], 0.1))
simdf$dia <- abs(rnorm(N, dmean[simdf$spp], 0.1))

# join everything together
simdf$biom <- 1000 * # to convert to grams
  simdf$b1*(simdf$dia^2*simdf$height)^b2
simdf

ggplot(simdf, aes(x = biom, color = spp, fill = spp)) +
  geom_density(alpha = 0.3) +
  # facet_wrap(~spp) + 
  theme_minimal()

y <- simdf$biom
N <- nrow(simdf)
Nids <- length(unique(simdf$ids))
Nspp <- length(unique(simdf$spp))
spp <- simdf$spp
height <- simdf$height
dia <- simdf$dia

table(spp, height)

# Nspp <- length(unique(emp$spp_num))
# species <- as.numeric(as.character(emp$spp_num))

fit <- stan("stan/allometryModel.stan", 
            data=c("N","y",
                   "spp","Nspp",
                   "height", "dia"),
            iter = 4000, chains=4, cores=4,
            warmup = 2000,
            save_dso = FALSE)

saveRDS(fit, "output/stanOutput/allometryModel")

summary(fit)

v <- readRDS("output/stan/allometryModel.rds")
diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)

