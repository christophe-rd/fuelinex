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
library(ggplot2)
library(rstan)
library(wesanderson)
library(patchwork)

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
sigma_y <- 0.1

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
  facet_wrap(~spp) +
  theme_minimal()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit Sim Data ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
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

# assign bounds to tell the initial values to the sampler
inits <- function(chain_id){
  params <- list("b1" = as.array(rlnorm(n_sp, log(0.1), 1)),
                 "b2" = as.array(rnorm(n_sp, 0.05, 1)),
                 "sigma_y" = abs(rnorm(1,0,1)))
  return(params)
}
inits(1)

fit <- stan("stan/allometryModel.stan", 
            data=c("N","y",
                   "spp","Nspp",
                   "height", "dia"),
            init = inits,
            iter = 8000, chains=4, cores=4,
            warmup = 4000)

saveRDS(fit, "output/stanOutput/allometryModel")
summary(fit)

diagnostics <- util$extract_hmc_diagnostics(fit) 
util$check_all_hmc_diagnostics(diagnostics)

util$plot_inv_metric(fit, 75)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Recover parameters ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
df_fit <- as.data.frame(fit)
# grab parameter estimates
cols <- colnames(df_fit)[!grepl("ypred", colnames(df_fit))]
cols <- cols[!grepl("lp__", cols)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For sigma_y
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# grab parameter estimates
sigma_cols <- cols[grepl("sigma", cols)]

sigmavec <- as.vector(df_fit[, colnames(df_fit) %in% sigma_cols])
class(sigma_df)

sigma_df <- data.frame(
  mean = mean(sigmavec), 
  per5 = quantile(sigmavec, probs = 0.05),
  per25 = quantile(sigmavec, probs = 0.25),
  per75 = quantile(sigmavec, probs = 0.75),
  per95 = quantile(sigmavec, probs = 0.95)
)
sigma_df

sigma_df$sim <- sigma_y

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For coefficient B1 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
b1_cols <- cols[grepl("b1", cols)]

b1_df <- df_fit[, colnames(df_fit) %in% b1_cols]

# change their names
colnames(b1_df) <- as.numeric(sub("b1\\[|\\]", "", sub("\\]", "", colnames(b1_df))))

# empty treat dataframe
b1_df2 <- data.frame(
  spp = character(ncol(b1_df)),
  mean = NA,  
  per5  = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
b1_df2

for (i in 1:ncol(b1_df)) { # i = 1
  b1_df2$spp[i] <- colnames(b1_df)[i]         
  b1_df2$mean[i] <- round(mean(b1_df[[i]]),3)  
  b1_df2$per5[i] <- round(quantile(b1_df[[i]], probs = 0.05), 3)
  b1_df2$per25[i] <- round(quantile(b1_df[[i]], probs = 0.25), 3)
  b1_df2$per75[i] <- round(quantile(b1_df[[i]], probs = 0.75), 3)
  b1_df2$per95[i] <- round(quantile(b1_df[[i]], probs = 0.95), 3)
}
b1_df2

# add sim data into it
b1_df2$sim <- simdf$b1[match(b1_df2$spp, simdf$spp)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For coefficient B2
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
b2_cols <- cols[grepl("b2", cols)]

b2_df <- df_fit[, colnames(df_fit) %in% b2_cols]

# change their names
colnames(b2_df) <- as.numeric(sub("b2\\[|\\]", "", sub("\\]", "", colnames(b2_df))))

# empty treat dataframe
b2_df2 <- data.frame(
  spp = character(ncol(b2_df)),
  mean = NA,  
  per5  = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
b2_df2

for (i in 1:ncol(b2_df)) { # i = 1
  b2_df2$spp[i] <- colnames(b2_df)[i]         
  b2_df2$mean[i] <- round(mean(b2_df[[i]]),3)  
  b2_df2$per5[i] <- round(quantile(b2_df[[i]], probs = 0.05), 3)
  b2_df2$per25[i] <- round(quantile(b2_df[[i]], probs = 0.25), 3)
  b2_df2$per75[i] <- round(quantile(b2_df[[i]], probs = 0.75), 3)
  b2_df2$per95[i] <- round(quantile(b2_df[[i]], probs = 0.95), 3)
}
b2_df2

# add sim data into it
b2_df2$sim <- simdf$b2[match(b2_df2$spp, simdf$spp)]

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot parameter recovery
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
sigma <- ggplot(sigma_df, aes(x = sim, y = mean)) +
  geom_errorbar(aes(ymin = per5, ymax = per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = per25, ymax = per75), 
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", 
              linewidth = 1) +
  labs(x = "sim parameter", y = "fit parameter", title = "") +
  # ggrepel::geom_text_repel(aes(label = spp), size = 5) +
  theme_minimal()
sigma
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
##### B1 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
b1 <- ggplot(b1_df2, aes(x = sim, y = mean)) +
  geom_errorbar(aes(ymin = per5, ymax = per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = per25, ymax = per75), 
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", 
              linewidth = 1) +
  labs(x = "sim parameter", y = "fit parameter", title = "") +
  # ggrepel::geom_text_repel(aes(label = spp), size = 5) +
  theme_minimal()
b1
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
##### B2 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
b2 <- ggplot(b2_df2, aes(x = sim, y = mean)) +
  geom_errorbar(aes(ymin = per5, ymax = per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = per25, ymax = per75), 
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", 
              linewidth = 1) +
  labs(x = "sim parameter", y = "fit parameter", title = "") +
  # ggrepel::geom_text_repel(aes(label = spp), size = 5) +
  theme_minimal()
b2

combined <- (sigma + b1 + b2)
combined
ggsave("figures/simData/paramRecovery.jpeg", combined, width = 6, height = 6, units = "in", dpi = 300)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot Prior vs posterior ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
###### Plot sigma_y prior vs posterior ######
sigma_y_posterior <- df_fit[, colnames(df_fit) %in% "sigma_y"]

sigma_y_prior <- rnorm(1e4, 0, 1)

priorsigma_y <- ggplot() +
  geom_density(data = data.frame(sigma_y = sigma_y_prior),
               aes(x = sigma_y, colour = "Prior at N(0,1)"),
               linewidth = 1) +
  geom_density(data = data.frame(value = sigma_y_posterior),
               aes(x = value, colour = "Posterior"),
               linewidth = 1) +
  labs(title = "priorVSposterior_a",
       x = "sigma_y", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()
priorsigma_y

##### B1 Prior vs Posterior #####
b1_long <- reshape(
  b1_df,
  direction = "long",
  varying = list(names(b1_df)),
  v.names = "value",
  timevar = "spp",
  times = names(b1_df),
  idvar = "draw"
)
b1_long

# b1 prior
b1_prior <- rlnorm(1e4, 0.1, 1)

priorb1 <- ggplot() +
  geom_density(data = data.frame(b1_prior = b1_prior),
               aes(x = b1_prior, colour = "Prior"),
               linewidth = 0.8) +
  geom_density(data = b1_long,
               aes(x = value, colour = "Posterior", group = spp),
               linewidth = 0.5) +
  # facet_wrap(~spp) + 
  labs(title = "priorVSposterior_b1",
       x = "b1", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  xlim(c(-20, 20)) +
  theme_minimal()
priorb1

##### B2 Prior vs Posterior #####
b2_long <- reshape(
  b2_df,
  direction = "long",
  varying = list(names(b2_df)),
  v.names = "value",
  timevar = "spp",
  times = names(b2_df),
  idvar = "draw"
)
b2_long

# b2 prior
b2_prior <- rnorm(1e4, 0.05, 1)

priorb2 <- ggplot() +
  geom_density(data = data.frame(b2_prior = b2_prior),
               aes(x = b2_prior, colour = "Prior"),
               linewidth = 0.8) +
  geom_density(data = b2_long,
               aes(x = value, colour = "Posterior", group = spp),
               linewidth = 0.5) +
  # facet_wrap(~spp) + 
  labs(title = "priorVSposterior_b2",
       x = "b2", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  xlim(c(-20, 20)) +
  theme_minimal()
priorb2


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
samples <- util$extract_expectand_vals(fit)

jpeg(
  filename = "figures/simData_allometry/retrodictiveCheckHist.jpeg",
  width = 2400,      
  height = 2400,
  res = 300          
)

util$plot_hist_quantiles(samples, "y_rep", 
                         -1, # lower x axis limit
                         1, # upper x axis limit
                         0.01, # binning
                         baseline_values = y,
                         xlab = "Biomass in kg")
dev.off()
