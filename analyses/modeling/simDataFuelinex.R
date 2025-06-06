# 28 May 2025
# CRD
# Goal is to start data simulation for fuelinex treatments

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# Load librairies
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --


# Load library 
library(rstanarm)
library(ggplot2)
library(arm)

runmodels <- FALSE
runoldcode <- FALSE

setwd("/Users/christophe_rouleau-desrochers/github/wildchrokie/analyses")

# === === === === === === === === === === === === === === === === 
#### Step 1. Come up with a model ####
# === === === === === === === === === === === === === === === === 
# try to predict budburst and budset dates based on treatments. We'll start with one species first 

# === === === === === === === === === === === === === === === === 
#### Step 2. Simulate data ####
# === === === === === === === === === === === === === === === === 
n_trees <- 30  # 30 trees
treatment <- 2
n <- n_trees * n_trees

# Tree IDs
ids <- paste0("t", rep(1:n_trees, each = n_obs_per_tree))

# Treatment factor: control, warm, cool
treatment <- rep(c("warmspring", "coolspring"), times = n_trees)

# Simulate forcing units (centered by treatment)
gdd <- round(rnorm(N, 100, 10))

# set parameters
a <- 1.5 #
b <- 0.4
sigma_y <- 0.3 # standard deviation

n_ids <- 100
rep <- 3
N <- n_ids * rep

# partial pool for each tree id
sigma_ids <- 1.5/2.57 # I specify how much the data varies across individuals. 1.96, j'aurais 95% de mes valeurs qui seraient entre 1.5 et -1.5. Quantile de la loi normale
a_ids <- rnorm(n_ids, 0, sigma_ids)

# set ids
ids <- rep(paste0("t", 1:n_ids), each = rep)

# just trees
trees <- paste0("t", 1:n_ids)

# growing degree days
gdd <- round(rnorm(N, 1800, 100))

# divide by a constant to adjust the scale
gddcons <- gdd / 200

# overall error
error <- rnorm(N, 0, sigma_y) 

# match a_ids to tree so its the same for each ids replicate
a_ids <- a_ids[match(ids, trees)] 

# calculate ring width
ringwidth <- a + a_ids +  b * gddcons + error

# create df
simcoef <- data.frame(
  ids = ids,
  gddcons = gddcons,
  b = b,
  a = a,
  a_ids = a_ids,
  sigma_y = sigma_y,
  ringwidth = ringwidth
)
plot(ringwidth~gddcons, data=simcoef)

# keep parameters in the following vector
sim_param <- c(b,a, sigma_y, sigma_ids)

# run models
if(runmodels){
  fit <- stan_lmer(
    ringwidth ~ gddcons + (1 | ids),  
    data = simcoef,
    chains = 4,
    iter = 4000,
    core=4
  )
}
print(fit, digits=3)

# recover model coef
fitcoef <- as.data.frame(coef(fit)$ids)
fitcoef$ids <- row.names(fitcoef)
fitcoef$a <- fixef(fit)[1]
fitcoef$b <-  fixef(fit)[2]
posterior <- as.data.frame(fit)
sigma_draws <- posterior$sigma
fitcoef$sigma_y <- mean(sigma_draws)

# rename and reorganize!
colnames(fitcoef) <- c("a_ids", "gddcons", "ids", "a", "b", "sigma_y")
fitcoef <- fitcoef[, c("ids", "b", "a", "a_ids", "sigma_y" )]
fitcoef$coefsource <- "model"

# prep sim for merge!
simcoef <- simcoef[, c("ids", "b", "a", "a_ids", "sigma_y")]
simcoef$coefsource <- "simulated"

# bind by row
coefbind <- rbind(fitcoef, simcoef)

#double check length of a_ids
length(unique(fitcoef$a_ids))
length(unique(simcoef$a_ids))

# reorganize to make a xy plot
simcoef[!duplicated(simcoef$a_ids), c(1,4)]
fitcoef[,c(1,4)]
merged <- merge(simcoef[!duplicated(simcoef$a_ids), c(1,4)], fitcoef[,c(1,4)], by="ids")
colnames(merged) <- c("ids", "sim_a_ids", "fit_a_ids")
