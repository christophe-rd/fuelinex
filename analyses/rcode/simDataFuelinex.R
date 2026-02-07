# 28 May 2025
# CRD
# Goal is to start data simulation for fuelinex treatments

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)
options(max.print = 200) 
options(digits = 3)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(ggplot2)
library(rstanarm)
library(wesanderson)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
set.seed(123)
setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

# === === === === === === === === === === === === === === === === 
#### Step 1. Come up with a model ####
# === === === === === === === === === === === === === === === === 
# start by simulating biomass as a function of gdd for the two treamtents with potentially the biggest treatment effect: warm spring warm fall and cool spring cool fall. Only estimating the current year biomass increment for now

# === === === === === === === === === === === === === === === === 
#### Step 2. Simulate data ####
# === === === === === === === === === === === === === === === === 

### do this: reconcile script grouping errors because my model doesn't converge ds

# === === === === === === === #
##### Biomass as intercept only #####
# === === === === === === === #
b <- 30 # baseline biomass
sigma_y <- 4
sigma_treat <- 1
sigma_sp <- 3
sigma_spring <- 0.4
sigma_fall <- 0.8



# treat and sp group ids
treat_norep <- c("cc", "cw", "wc", "ww")
ntreat <- length(treat_norep)
nsp <- 5
sp_norep <- 1:nsp

# n of treat and sp and number of replicates per species, per treatment
n_per_sp_per_treat <- 15

sp <- rep(rep(sp_norep, each = n_per_sp_per_treat), times = ntreat)
length(sp)
treat <- rep(rep(treat_norep, each = n_per_sp_per_treat), each = nsp)
length(treat)

# treat
ids <- 1: length(treat)
N <- length(ids)

error <- rnorm(N, 0, sigma_y)

simdf <- data.frame(
  ids = ids,
  treat = treat,
  sp = sp,
  b = b,
  error = error
)
simdf

# add effect of treatments on intercept
simdf$spring <- substr(simdf$treat, 1, 1)  # spring condition either warm or cool 
simdf$fall = substr(simdf$treat, 2, 2) # fall condition either warm or cool

# divergence from the overall intercept for spring condition
ws <- 10
simdf$bspring = ifelse(simdf$spring == "c", 0, ws)
# divergence from the overall intercept for fall condition
wf <- 5
simdf$bfall = ifelse(simdf$fall == "c", 0, wf)

# add dummy variable 
simdf$s <- ifelse(simdf$spring == "w", 1, 0)
simdf$f <- ifelse(simdf$fall == "w", 1, 0)
simdf$sf <- simdf$s * simdf$f

# interaction when its warm spring and warm fall
wswf <- -2
simdf$bsf <- ifelse(simdf$sf == 1, wswf, 0)

# add species effect
asp <- rnorm(nsp, mean = 0, sigma_sp)
simdf$asp <- asp[simdf$sp]

# joing everything together
simdf$b_full <-  
  simdf$b + 
  simdf$bspring + 
  simdf$bfall +
  simdf$bsf + 
  simdf$asp +
  simdf$error
simdf

if(FALSE){

hist(simdf$error)
hist(simdf$bspring)
unique(simdf$bspring)
ggplot(simdf, aes(x = bspring, color = treat, fill = treat)) +
  geom_density(alpha = 0.3)

v <- c(unique(simdf$b - 10 - 5), # cc
       unique(simdf$b + 10 + 5), # ww
       unique(simdf$b + 10 - 5), # wc
       unique(simdf$b - 10 + 5)  # cw
       )

ggplot(simdf, aes(x = b_full, color = treat)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  labs(
    title = "densities of full intercepts per treatment",
    x = "b_full",
    y = "density"
  ) +
  # geom_vline(aes(xintercept = a+asp)) +
  # geom_vline(xintercept = v) +
  # facet_wrap(~sp) +
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  scale_fill_manual(values = wes_palette("Darjeeling1")) +
  theme_minimal()
ggsave("figures/densityintercept_with_asp.jpeg", width = 8, height = 6, units = "in", dpi = 300)
}

y <- simdf$b_full
N <- nrow(simdf)
s <- simdf$s
f <- simdf$f
sf <- simdf$sf
Nspp <- length(unique(simdf$sp))
species <- as.numeric(as.character(simdf$sp))
data=c("N","y","s","f","Nspp","species","sf")

fit <- stan("stan/factorialHierModel.stan", 
            data=c("N","y",
                   "s","f",
                   "Nspp", "species",
                   "sf"),
            iter = 2000, chains=4, cores=4,
            warmup = 1000)

summary(fit)

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
bs_cols <- cols[grepl("bs", cols)]
bs_cols <- bs_cols[!grepl("bsf", bs_cols)]

bs_df <- df_fit[, colnames(df_fit) %in% bs_cols]

# change their names
colnames(bs_df) <- as.numeric(sub("bs\\[|\\]", "", sub("\\]", "", colnames(bs_df))))

# empty treat dataframe
bs_df2 <- data.frame(
  spp = character(ncol(bs_df)),
  mean = NA,  
  per5  = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
bs_df2

for (i in 1:ncol(bs_df)) { # i = 1
  bs_df2$spp[i] <- colnames(bs_df)[i]         
  bs_df2$mean[i] <- round(mean(bs_df[[i]]),3)  
  bs_df2$per5[i] <- round(quantile(bs_df[[i]], probs = 0.05), 3)
  bs_df2$per25[i] <- round(quantile(bs_df[[i]], probs = 0.25), 3)
  bs_df2$per75[i] <- round(quantile(bs_df[[i]], probs = 0.75), 3)
  bs_df2$per95[i] <- round(quantile(bs_df[[i]], probs = 0.95), 3)
}
bs_df2

# add sim data into it
bs_df2$sim <- simdf$bspring[match(bs_df2$spp, simdf$sp)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For coefficient BF
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
bf_cols <- cols[grepl("bf", cols)]
bf_cols <- bf_cols[!grepl("bsf", bf_cols)]

bf_df <- df_fit[, colnames(df_fit) %in% bs_cols]

# change their names
colnames(bf_df) <- as.numeric(gsub(".*\\[(\\d+)\\]", "\\1", colnames(bf_df)))

# empty treat dataframe
bf_df2 <- data.frame(
  spp = character(ncol(bf_df)),
  mean = NA,  
  per5  = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
bf_df2

for (i in 1:ncol(bf_df)) { # i = 1
  bf_df2$spp[i] <- colnames(bf_df)[i]         
  bf_df2$mean[i] <- round(mean(bf_df[[i]]),3)  
  bf_df2$per5[i] <- round(quantile(bf_df[[i]], probs = 0.05), 3)
  bf_df2$per25[i] <- round(quantile(bf_df[[i]], probs = 0.25), 3)
  bf_df2$per75[i] <- round(quantile(bf_df[[i]], probs = 0.75), 3)
  bf_df2$per95[i] <- round(quantile(bf_df[[i]], probs = 0.95), 3)
}
bf_df2

# add sim data into it
bf_df2$sim <- simdf$bs[match(bf_df2$spp, simdf$spp)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For coefficient BSF
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
bsf_cols <- cols[grepl("bsf", cols)]

bsf_df <- df_fit[, colnames(df_fit) %in% bs_cols]

# change their names
colnames(bsf_df) <- as.numeric(gsub(".*\\[(\\d+)\\]", "\\1", colnames(bsf_df)))

# empty treat dataframe
bsf_df2 <- data.frame(
  spp = character(ncol(bsf_df)),
  mean = NA,  
  per5  = NA, 
  per25 = NA,
  per75 = NA,
  per95 = NA
)
bsf_df2

for (i in 1:ncol(bsf_df)) { # i = 1
  bsf_df2$spp[i] <- colnames(bsf_df)[i]         
  bsf_df2$mean[i] <- round(mean(bsf_df[[i]]),3)  
  bsf_df2$per5[i] <- round(quantile(bsf_df[[i]], probs = 0.05), 3)
  bsf_df2$per25[i] <- round(quantile(bsf_df[[i]], probs = 0.25), 3)
  bsf_df2$per75[i] <- round(quantile(bsf_df[[i]], probs = 0.75), 3)
  bsf_df2$per95[i] <- round(quantile(bsf_df[[i]], probs = 0.95), 3)
}
bsf_df2

# add sim data into it
bsf_df2$sim <- simdf$bs[match(bsf_df2$spp, simdf$spp)]


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot parameter recovery ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
sigma <- ggplot(sigma_df, aes(x = sim, y = mean)) +
  geom_errorbar(aes(ymin = per5, ymax = per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = per25, ymax = per75), 
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", 
              linewidth = 1) +
  labs(x = "sim parameter", y = "fit parameter", title = "sigma_y") +
  # ggrepel::geom_text_repel(aes(label = spp), size = 5) +
  theme_minimal()
sigma
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
##### B1 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
bs <- ggplot(bs_df2, aes(x = sim, y = mean)) +
  geom_errorbar(aes(ymin = per5, ymax = per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = per25, ymax = per75), 
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", 
              linewidth = 1) +
  labs(x = "sim parameter", y = "fit parameter", title = "bs") +
  # ggrepel::geom_text_repel(aes(label = spp), size = 5) +
  theme_minimal()
bs
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
##### B2 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
bf <- ggplot(bf_df2, aes(x = sim, y = mean)) +
  geom_errorbar(aes(ymin = per5, ymax = per95), 
                width = 0, linewidth = 0.5, color = "darkgray", alpha=0.7) +
  geom_errorbar(aes(ymin = per25, ymax = per75), 
                width = 0, linewidth = 1.5, color = "darkgray", alpha = 0.7) +
  geom_point(color = "#046C9A", size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#B40F20", 
              linewidth = 1) +
  labs(x = "sim parameter", y = "fit parameter", title = "bf") +
  # ggrepel::geom_text_repel(aes(label = spp), size = 5) +
  theme_minimal()
bf

combined <- (sigma + bs + bf)
combined
ggsave("figures/simData/paramRecovery.jpeg", combined, width = 8, height = 6, units = "in", dpi = 300)

