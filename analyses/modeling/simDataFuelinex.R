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

### do this: reconcile script grouping errors because my model doesn't converge ds

# === === === === === === === #
##### Biomass X Gdd cons #####
# === === === === === === === #
a <- 5
b <- 0.9
sigma_y <- 0.3
sigma_treat <- 0.4
sigma_spp <- 0.5

n_treat <- 30
n_spp <- 40
n_per_treat <- 20

N <-n_treat*n_per_treat*n_spp

error <- rnorm(N, 0, sigma_y)

ids <- rep(rep(1:n_per_treat, times = n_treat), times = n_spp)
treat <- rep(rep(1:n_treat, each = n_per_treat), times = n_spp)
spp <- rep(rep(1:n_spp, each = n_per_treat), each = n_treat)

# partial pooling 
### here, I am not partial pooling on ids, because unlike wildchrokie, my ids here are replicates, so they are the repeated measurements
a_treat <- rnorm(n_treat, 0, sigma_treat)
a_spp <- rnorm(n_spp, 0, sigma_spp)

# gdd per treatment and devide by constant
gdd <- rnorm(N, 2000, 100)/ 200

# set df
sim_biomass <- data.frame(
  ids = ids,
  treat = treat,
  spp = spp,
  a = a,
  b = b,
  gdd = gdd,
  error = error
)
sim_biomass

sim_biomass$a_treat <-  a_treat[sim_biomass$treat]
sim_biomass$a_spp <-  a_spp[sim_biomass$spp]

sim_biomass$ids_uni <- paste(sim_biomass$ids, sim_biomass$treat, sep = "_")

# calculate biomass
sim_biomass$biomass <- c(
  sim_biomass$a + 
    sim_biomass$a_treat + 
    sim_biomass$a_spp +
    sim_biomass$b * sim_biomass$gdd + 
    sim_biomass$error
)
sim_biomass


# run model
fitbiomass <- stan_lmer(
  biomass ~ gdd + (1 | treat) + (1 | spp),  
  data = sim_biomass,
  chains = 4,
  iter = 4000,
  core=4
) 
fitbiomass


# === === === === === === #
# PARAMETER RECOVERY ####
# === === === === === === #
df_fit <- as.data.frame(fitbiomass)

# recover slope
colnames(df_fit)
# grab treat nested in spp
treat_cols <- colnames(df_fit)[grepl(" treat:", colnames(df_fit))]
treat_cols <- treat_cols[1:length(treat_cols)]
treat_df <- df_fit[, colnames(df_fit) %in% treat_cols]
# change their names
colnames(treat_df) <- sub(".*treat:([^]]+)\\]$", "\\1", colnames(treat_df))
# empty treat dataframe
treat_df2 <- data.frame(
  treat = character(ncol(treat_df)),
  fit_a_treat = numeric(ncol(treat_df)),  
  fit_a_treat_per5 = NA, 
  fit_a_treat_per95 = NA
)
treat_df2

for (i in 1:ncol(treat_df)) { # i = 1
  treat_df2$treat[i] <- colnames(treat_df)[i]         
  treat_df2$fit_a_treat[i] <- round(mean(treat_df[[i]]),3)  
  treat_df2$fit_a_treat_per5[i] <- round(quantile(treat_df[[i]], probs = 0.055), 3)
  treat_df2$fit_a_treat_per95[i] <- round(quantile(treat_df[[i]], probs = 0.945), 3)
}
treat_df2

df_fit <- as.data.frame(fitbiomass)


# recover spp
# grab treat nested in spp
spp_cols <- colnames(df_fit)[grepl(" spp:", colnames(df_fit))]
spp_cols <- spp_cols[1:length(spp_cols)]
spp_df <- df_fit[, colnames(df_fit) %in% spp_cols]
# change their names
colnames(spp_df) <- sub(".*spp:([^]]+)\\]$", "\\1", colnames(spp_df))
# empty spp dataframe
spp_df2 <- data.frame(
  spp = character(ncol(spp_df)),
  fit_a_spp = numeric(ncol(spp_df)),  
  fit_a_spp_per5 = NA, 
  fit_a_spp_per95 = NA
)
spp_df2

for (i in 1:ncol(spp_df)) { # i = 1
  spp_df2$spp[i] <- colnames(spp_df)[i]         
  spp_df2$fit_a_spp[i] <- round(mean(spp_df[[i]]),3)  
  spp_df2$fit_a_spp_per5[i] <- round(quantile(spp_df[[i]], probs = 0.055), 3)
  spp_df2$fit_a_spp_per95[i] <- round(quantile(spp_df[[i]], probs = 0.945), 3)
}
spp_df2


# PLOT PARAMETER RECOVERY #####
# merge sim and fit
treattoplot <- merge(
  sim_biomass[!duplicated(sim_biomass$treat), 
              c("treat", "a_treat")], 
  treat_df2[, 
          c("treat", "fit_a_treat", "fit_a_treat_per5", "fit_a_treat_per95")], 
  by = "treat"
)
treattoplot

a_treat_simXfit_plot <- ggplot(treattoplot, aes(x = a_treat, y = fit_a_treat)) +
  geom_point(color = "#046C9A", size = 2) +
  geom_errorbar(aes(ymin = fit_a_treat_per5, ymax = fit_a_treat_per95), width = 0, color = "darkgray", alpha=0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "sim a_treat", y = "fit a_treat", title = "") +
  theme_minimal()
a_treat_simXfit_plot
# ggsave!
ggsave("figures/a_treat_simXfit_plot.jpeg", a_treat_simXfit_plot, width = 6, height = 6, units = "in", dpi = 300)


# merge sim and fit
spptoplot <- merge(
  sim_biomass[!duplicated(sim_biomass$spp), 
          c("spp", "a_spp")], 
  spp_df2[, 
         c("spp", "fit_a_spp", "fit_a_spp_per5", "fit_a_spp_per95")], 
  by = "spp"
)
spptoplot

a_spp_simXfit_plot <- ggplot(spptoplot, aes(x = a_spp, y = fit_a_spp)) +
  geom_point(color = "#046C9A", size = 2) +
  geom_errorbar(aes(ymin = fit_a_spp_per5, ymax = fit_a_spp_per95), width = 0, color = "darkgray", alpha=0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "sim a_spp", y = "fit a_spp", title = "") +
  theme_minimal()
a_spp_simXfit_plot
# ggsave!
ggsave("figures/a_spp_simXfit_plot.jpeg", a_spp_simXfit_plot, width = 6, height = 6, units = "in", dpi = 300)

# merge everything together to look at biomass
t <- merge(sim_biomass, treat_df2, by = "treat")
t2 <- merge(t, spp_df2, by = "spp")

t2$fit_a <- mean(df_fit$`(Intercept)`)
t2$fit_b <- mean(df_fit$gdd)

t2$fit_biomass <- c(
  t2$fit_a + 
    t2$fit_a_treat + 
    t2$fit_a_spp +
    t2$fit_b * t2$gdd
)

a_spp_simXfit_plot <- ggplot(t2, aes(x = biomass, y = fit_biomass)) +
  geom_point(color = "#046C9A", size = 2, alpha =0.2) +
  # geom_errorbar(aes(ymin = fit_a_spp_per5, ymax = fit_a_spp_per95), width = 0, color = "darkgray", alpha=0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", linewidth = 1) +
  labs(x = "sim a_spp", y = "fit a_spp", title = "") +
  theme_minimal()
a_spp_simXfit_plot
# ggsave!
ggsave("figures/a_spp_simXfit_plot.jpeg", a_spp_simXfit_plot, width = 6, height = 6, units = "in", dpi = 300)
