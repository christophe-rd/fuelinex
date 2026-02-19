## Started 16 February 2026
## By Ken

## STAT 547 model

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

#setwd
if(length(grep("christophe", getwd()) > 0)) { 
  setwd("/Users/christophe_rouleau-desrochers/github/fuelinex/analyses")
} else if(length(grep("Ken", getwd()) > 0)){
  setwd("/Users/Ken Michiko Samson/Documents/Temporal Ecology Lab/fuelinex/analyses")
}

library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

runmodel <- FALSE

util <- new.env()
source('mcmc_analysis_tools_rstan.R', local=util)
source('mcmc_visualization_tools.R', local=util)

mea <- read.csv2("output/cleanedMeasurements.csv", sep = ",", header = TRUE)

mea$spp_num <- match(mea$genus, unique(mea$genus))
mea$treeid_num <- match(mea$tree_ID, unique(mea$tree_ID))
mea <- mea[which(!is.na(mea$height) & !is.na(mea$diameter)),]
mea$height   <- as.numeric(mea$height)
mea$diameter <- as.numeric(mea$diameter)
mea <- mea[, -1]

biom <- read.csv("input/biomass.csv")
# biomass now
biom$aboveGroundWeight <- as.numeric(biom$aboveGroundWeight)

d <- reshape(mea, timevar = 'year',
             idvar = c('tree_ID', 'bloc', 'treatment', 'genus', 'species',
                       'spp_num', 'treeid_num'),
             direction = 'wide')

d$vol.2023 <- d$diameter.2023^2 * d$height.2023
d$vol.2024 <- d$diameter.2024^2 * d$height.2024
d$vol.2025 <- d$diameter.2025^2 * d$height.2025
d$volinc1 <- d$vol.2024 - d$vol.2023
d$volinc2 <- d$vol.2025 - d$vol.2024

d$s <- NA
d$f <- NA
trt <- unique(d$treatment)
s <- c(0, 1, 0, 1, 0, 1)
f <- c(0, 0, 1, 1, 0, 1)
for(i in 1:length(trt)){
  idx <- which(d$treatment == trt[i])
  d$s[idx] <- s[i]
  d$f[idx] <- f[i]
}
d$sf <- d$s * d$f

d <- subset(d, volinc1 > 0 & volinc2 > 0 & 
              treatment %in% trt[1:4] & 
              spp_num %in% 1:7)

d$trt_num <- match(d$treatment, unique(d$treatment))

biom$aboveGroundWeight <- as.numeric(biom$aboveGroundWeight)
d_allo <- subset(mea, year == "2025")
d_allo <- merge(d_allo, biom[, c("tree_ID","aboveGroundWeight")], by = "tree_ID")
d_allo <- subset(d_allo, !is.na(diameter) & !is.na(height) & 
                   treatment %in% trt[1:4] & 
                   aboveGroundWeight > 0 & spp_num %in% 1:7)

# Fit model
data <- list("N_allo" = nrow(d_allo),
             "d_allo" = d_allo$diameter,
             "h_allo" = d_allo$height,
             "N_spp" = length(unique(d$species)),
             "spp_allo" = d_allo$spp_num,
             "agb_allo" = d_allo$aboveGroundWeight,
             "N" = nrow(d),
             "d0" = d$diameter.2023,
             "h0" = d$height.2023,
             "d1" = d$diameter.2024,
             "h1" = d$height.2024,
             "d2" = d$diameter.2025,
             "h2" = d$height.2025,
             "trt" = d$trt_num,
             "spp" = d$spp_num)

set.seed(1)

inits <- function(chain_id){
  params <- list("b1" = as.array(rlnorm(unique(d$spp_num), log(0.5), 0.3)),
                 "b2" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "s_allo" = as.array(abs(rnorm(unique(d$spp_num), 0, 1))),
                 "acc1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "awc1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "acw1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "aww1" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "acc2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "awc2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "acw2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "aww2" = as.array(rlnorm(unique(d$spp_num), 1, 1)),
                 "s_y" = as.array(abs(rnorm(unique(d$spp_num), 0, 1)))
  )
  return(params)
}
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit Model ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if (runfulljointmodel) {
  fit <- stan("stan/fullModelpos.stan",
              data = data, 
              # init = inits, # fill readd later when I figure out why the bound on b2 messes it up
              seed = 1,
              warmup = 1000, iter = 2000, refresh = 500, chains = 4)
  # saveRDS(fit, "output/stanOutput/full_fit_normalLikelihood_bound0B2.rds")
}
fit <- readRDS("output/stanOutput/full_fit_normalLikelihood_bound0B2.rds")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Diagnostics ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
names <- c(grep('^b1', names(samples), value = TRUE),
           grep('^b2', names(samples), value = TRUE),
           grep('s_allo', names(samples), value = TRUE),
           grep('acc1', names(samples), value = TRUE),
           grep('awc1', names(samples), value = TRUE),
           grep('acw1', names(samples), value = TRUE),
           grep('aww1', names(samples), value = TRUE),
           grep('acc2', names(samples), value = TRUE),
           grep('awc2', names(samples), value = TRUE),
           grep('acw2', names(samples), value = TRUE),
           grep('aww2', names(samples), value = TRUE),
           grep('s_y', names(samples), value = TRUE))

# just delta 1s
idtocheck <- which(d$spp_num == 1)
deltanames <- paste0('delta1[', idtocheck, ']')
deltadata <- sapply(deltanames, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))

base_samples <- util$filter_expectands(samples, names)
print(util$check_all_expectand_diagnostics(base_samples))

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Marginal posterior #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# 
# pdf("figures/modelDiagnostics/marginalPost.pdf", height = 9, width = 9)
# par(mfrow = c(3, 3))
# for(i in 1:length(names)){
#   a <- min(samples[[names[i]]])
#   b <- max(samples[[names[i]]])
#   util$plot_expectand_pushforward(expectand_vals = samples[[names[i]]],
#                                   B = 100,
#                                   display_name = names[i],
#                                   flim = c(a, b))
# }
# dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Pairs plot #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
# pdf('figures/modelDiagnostics/pairs.pdf', height = 9, width = 9)
# util$plot_div_pairs(names, names, samples, diagnostics)
# dev.off()
# 
# # with bound on b1
# namesallo <- c(grep('b1', names(samples), value = TRUE),
#            grep('b2', names(samples), value = TRUE),
#            grep('s_allo', names(samples), value = TRUE),
#            grep('s_y', names(samples), value = TRUE))
# namesallo <- namesallo[!grepl("agb", namesallo)]
# 
# pdf('figures/modelDiagnostics/pairs_normLikelihood_bound.pdf', height = 9, width = 9)
# util$plot_div_pairs(namesallo, namesallo, samples, diagnostics)
# dev.off()
# 
# # no bound on b2
# fitnobound <- readRDS("output/stanOutput/full_fit_normalLikelihood.rds")
# samplesnobound <- util$extract_expectand_vals(fitnobound)
# 
# pdf('figures/modelDiagnostics/pairs_normLikelihood_NoBound.pdf', height = 9, width = 9)
# util$plot_div_pairs(namesallo, namesallo, samplesnobound, diagnostics)
# dev.off()
