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
biom <- read.csv("input/biomass.csv")

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

str(biom)
biom$aboveGroundWeight <- as.numeric(biom$aboveGroundWeight)
ggplot(biom) +
  geom_histogram(aes(x = aboveGroundWeight, color = species, fill = species)) +
  facet_wrap(~species)

ggplot(mea) +
  geom_histogram(aes(x = height_inc, color = species, fill = species)) +
  facet_wrap(~species)

mea$mul <- mea$diameter * mea$diameter * mea$height
ggplot(mea) +
  geom_histogram(aes(x = mul, color = species, fill = species)) +
  facet_wrap(~species)

df <- merge(mea, biom[, c("tree_ID","aboveGroundWeight")], by = "tree_ID")
df25 <- subset(df, year == "2025")

ggplot(df25) +
  geom_point(aes(x = mul, y = aboveGroundWeight, color = species, fill = species)) +
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
n_per_spp <- 50

spp <- rep(rep(1:n_sp, each = n_per_spp))

ids <- 1: length(spp)

N <- length(ids)

error <- rnorm(N, 0, sigma_y)

# set coefficients per spp
b1 <- rlnorm(n_sp, log(0.1), 0.2)
b2 <- rnorm(n_sp, 0.05, 0.2)

simdf <- data.frame(
  ids = ids,
  spp = spp,
  error = error
)

simdf$b1 <- b1[simdf$spp]
simdf$b2 <- b2[simdf$spp]

# add height and diameter mean increment for each species
hmean <- abs(rnorm(n_sp, 0.3, 0.1)) # in meters
dmean <- abs(rnorm(n_sp, 0.2, 0.1)) # in cm

# individual level variations
simdf$height <- abs(rnorm(N, hmean[simdf$spp], 0.1))
simdf$dia <- abs(rnorm(N, dmean[simdf$spp], 0.1))

# join everything together
simdf$biom <- simdf$b1*(simdf$dia^2*simdf$height)^b2
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

table(spp)

# Nspp <- length(unique(emp$spp_num))
# species <- as.numeric(as.character(emp$spp_num))

inits <- function(chain_id){
  params <- list("b1" = as.array(rlnorm(n_sp, log(0.1), 0.2)),
                 "b2" = as.array(rnorm(n_sp, 0.05, 0.2)),
                 "sigma_y" = abs(rnorm(1,0,1)))
  return(params)
}
inits(1)

fit <- stan("stan/allometryModel.stan", 
            data=c("N","y",
                   "spp","Nspp",
                   "height", "dia"),
            init = inits,
            iter = 2000, chains=4, cores=4,
            warmup = 1000,
            save_dso = FALSE)

saveRDS(fit, "output/stanOutput/allometryModel")

summary(fit)

v <- readRDS("output/stan/allometryModel.rds")
diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)



df_fit <- as.data.frame(fit)

# recover slope
colnames(df_fit)

# grab parameter estimates
cols <- colnames(df_fit)[!grepl("ypred", colnames(df_fit))]
cols <- cols[!grepl("lp__", cols)]
# cols <- cols[1:length(cols)]
df <- df_fit[, colnames(df_fit) %in% cols]
# change their names
# colnames(df) <- sub(".*treat:([^]]+)\\]$", "\\1", colnames(df))
# empty treat dataframe
df2 <- data.frame(
  parameter = character(ncol(df)),
  mean = NA,  
  per5  = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
df2

for (i in 1:ncol(df)) { # i = 1
  df2$parameter[i] <- colnames(df)[i]         
  df2$mean[i] <- round(mean(df[[i]]),3)  
  df2$per5[i] <- round(quantile(df[[i]], probs = 0.05), 3)
  df2$per25[i] <- round(quantile(df[[i]], probs = 0.25), 3)
  df2$per75[i] <- round(quantile(df[[i]], probs = 0.75), 3)
  df2$per95[i] <- round(quantile(df[[i]], probs = 0.95), 3)
}
df2$sim <- NA
df2$sim[11:20] <- b2
df2$sim[21] <- sigma_y

# Plot parameter recovery
df2$sim <- NA 
df2$sim[which(df2$parameter == "b")] <- b
df2$sim[which(df2$parameter == "bs")] <- ws
df2$sim[which(df2$parameter == "bf")] <- wf
df2$sim[which(df2$parameter == "bsf")] <- wswf
df2$sim[which(df2$parameter == "sigma_y")] <- sigma_y

ggplot(df2, aes(x = sim, y = mean)) +
  geom_errorbar(aes(ymin = per5, ymax = per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = per25, ymax = per75), 
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "sim parameter", y = "fit parameter", title = "") +
  ggrepel::geom_text_repel(aes(label = parameter), size = 5) +
  theme_minimal()