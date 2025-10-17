# CRD
# 27 May 2025
# Goal is to start find a convertion factor from two tools to measure chlorophyll

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(max.print = 100) 

# --- --# --- --# --- --# --- --# --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(ggplot2)
library(rstanarm)
library(rethinking)
library(ggplot2)
# --- --# --- --# --- --# --- --# --- --- --- --- --- --- --- --- --- --- --- --

# Set the path to your directory folder 
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

# read csvs
chl212 <- read.csv("input/2025-212_Chltoocomparison.csv")
chl235 <- read.csv("input/2025-212_Chltoocomparison.csv")

# add column with doy
chl212$doy <- 212
chl235$doy <- 235

# create unique ids 
chl212$tree_ID_doy <- paste(chl212$tree_ID, chl212$doy, sep = "_")
chl235$tree_ID_doy <- paste(chl235$tree_ID, chl235$doy, sep = "_")

# rbind 
chl <- rbind(chl212, chl235)

# convert minolta and ccm200plus cols to numeric
chl$minolta <- as.numeric(as.character(chl$minolta))
chl$ccm200plus <- as.numeric(as.character(chl$ccm200plus))

# remove NA rows
chl <- chl[!is.na(chl$minolta), ]

# Simulate data ####
set.seed(124)
a <- 2
b <- 3
sigma_y <- 0.8

n_spp <- 50
replicates <- 30
N <- n_spp*replicates

# error
error <- rnorm(N, 0, 1.5)

spp <- rep(1:n_spp, each = replicates)
ids <- rep(1:replicates, times = n_spp)

# a_spp
a_spp <- rnorm(n_spp, 0, sigma_y)
a_spp_values <- round(a_spp[spp], 3)

# ccm values
ccm <- round(rnorm(N, 8, 3), 3)
dens(ccm)

minol <- ccm * b + a + a_spp_values + error
sim <- data.frame(spp, ids, a, a_spp_values, b, sigma_y, ccm, minol)
sim

sim$log_minol <- log(sim$minol)
sim$log_ccm <- log(sim$ccm)

chl$log_minolta <- log(chl$minolta)
chl$log_ccm <- log(chl$ccm)

jpeg("figures/chlempirical.jpeg", width = 8, height = 6, units = "in", res = 300, pointsize = 12)
plot(chl$minolta ~ chl$ccm200plus)
dev.off()

##### Fit simulated data #####
fit <- stan_lmer(
  minol ~ log_ccm + (1|spp),
  data = sim,
  chains = 4,
  iter = 2000,
  cores = 4
)

##### Recover simulated data parameters #####
fit_fixef <- fixef(fit)
fit_ranef <- ranef(fit)

spp_df <- fit_ranef$spp

# Now extract the tree IDs and intercepts
a_sppwithranef <- data.frame(
  spp = rownames(spp_df),
  fit_a_spp = spp_df[["(Intercept)"]]
)

# recover only conf intervals spp from previously created df
messyinter <- as.data.frame(posterior_interval(fit))
messyinter$messyids <- rownames(messyinter)
a_spp_messyinter <- subset(messyinter, grepl("spp:", messyids))
a_spp_messyinter$spp <- sub(".*spp:([0-9]+)\\].*", "\\1", a_spp_messyinter$messyids)

# remove unecessary columns
a_spp_messyinter <- a_spp_messyinter[, c("spp", "5%", "95%")]

# change 5 and 95% names
colnames(a_spp_messyinter) <- c("spp", "per5", "per95")

# merge!
fit_merged <- merge(a_sppwithranef, a_spp_messyinter, by = "spp")

# Get sim data ready to merge #####
simcoeftoplot <- sim[, c("spp", "a_spp_values")]
colnames(simcoeftoplot) <- c("spp", "sim_a_spp")
simcoeftoplot <- simcoeftoplot[!duplicated(simcoeftoplot),]

# now merge
a_spp_fit_sim <- merge(simcoeftoplot, fit_merged, by = "spp")

# plot!
plot_a_spp_fit_sim <- ggplot(a_spp_fit_sim, aes(x = sim_a_spp, y = fit_a_spp)) +
  geom_point(color = "blue", size = 2) +
  geom_errorbar(aes(ymin = per5, ymax = per95), 
                width = 0, color = "darkgray", alpha=0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
  labs(x = "Simulated a_spp", y = "Model a_spp", title = "") +
  theme_minimal()
plot_a_spp_fit_sim
ggsave("figures/Chl_a_spp_recovery.jpeg", plot_a_spp_fit_sim, width = 8, height = 6)

# Fit to empirical data ####
fitempir <- stan_lmer(
  minolta ~ ccm200plus + (1|species),
  data = chl,
  chains = 4,
  iter = 2000,
  cores = 4
)
plot(fitempir)
summary(fitempir)
fitempir

##### Recover empirical model parameters #####
fit_fixef <- fixef(fitempir)
fit_ranef <- ranef(fitempir)

spp_df <- fit_ranef$species

# Now extract the tree IDs and intercepts
a_sppwithranef <- data.frame(
  species = rownames(spp_df),
  a_spp = spp_df[["(Intercept)"]]
)

# add overall intercept
a_sppwithranef$a <- fit_fixef["(Intercept)"]

# add slope 
a_sppwithranef$b <- fit_fixef["ccm200plus"]

# sum intercept values 
a_sppwithranef$total_a <- a_sppwithranef$a+a_sppwithranef$a_spp

chl$fit_y <- fitempir$fitted.values

ggplot(chl, aes(x = minolta, y = fit_y)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) + 
  theme_minimal()
# save!
ggsave("figures/chl_fitVSempirical.jpeg", width = 8, height = 6)

# remove the outlier
chlsub <- subset(chl, minolta <42 )
ggplot(chlsub, aes(x = minolta, y = fit_y)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) + 
  theme_minimal()

x <- seq(0,10,0.1)
y <- 10+log(x)
plot(y ~ x)
