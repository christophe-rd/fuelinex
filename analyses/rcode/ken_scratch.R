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

d <- subset(d, volinc1 > 0 & volinc2 > 0 & treatment %in% trt[1:4] & spp_num == 1)

biom$aboveGroundWeight <- as.numeric(biom$aboveGroundWeight)
d_allo <- subset(mea, year == "2025")
d_allo <- merge(d_allo, biom[, c("tree_ID","aboveGroundWeight")], by = "tree_ID")
<<<<<<< HEAD:analyses/rcode/ken_scratch_crdedits.R
d_allo <- subset(d_allo, !is.na(diameter) & !is.na(height) & aboveGroundWeight > 0 & treatment %in% trt[1:4])
=======
d_allo <- subset(d_allo, !is.na(diameter) & !is.na(height) & aboveGroundWeight > 0 & spp_num == 1)
>>>>>>> eff98ad2a08deeacb12e7e77e17f4300e871a349:analyses/rcode/ken_scratch.R

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
             "s" = d$s,
             "f" = d$f,
             "sf" = d$sf,
             "spp" = d$spp_num)

set.seed(1)

inits <- function(chain_id){
<<<<<<< HEAD:analyses/rcode/ken_scratch_crdedits.R
  params <- list("b1" = as.array(rlnorm(7, log(0.5), 0.3)),
                 "b2" = as.array(rnorm(7, 0, 1)),
                 "s_allo" = as.array(abs(rnorm(7, 0, 1))),
                 "a1" = as.array(rnorm(7, 0, 1)),
                 "as1" = as.array(rnorm(7, 0, 1)),
                 "af1" = as.array(rnorm(7, 0, 1)),
                 "asf1" = as.array(rnorm(7, 0, 1)),
                 "a2" = as.array(rnorm(7, 0, 1)),
                 "as2" = as.array(rnorm(7, 0, 1)),
                 "af2" = as.array(rnorm(7, 0, 1)),
                 "asf2" = as.array(rnorm(7, 0, 1)),
                 "s_y" = as.array(abs(rnorm(7, 0, 1)))
  )
=======
  params <- list("b1" = as.array(rlnorm(unique(d$spp_num), log(0.5), 0.3)),
                 "b2" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "s_allo" = as.array(abs(rnorm(unique(d$spp_num), 0, 1))),
                 "a1" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "as1" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "af1" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "asf1" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "a2" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "as2" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "af2" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "asf2" = as.array(rnorm(unique(d$spp_num), 0, 1)),
                 "s_y" = as.array(abs(rnorm(unique(d$spp_num), 0, 1)))
                 )
>>>>>>> eff98ad2a08deeacb12e7e77e17f4300e871a349:analyses/rcode/ken_scratch.R
  return(params)
}

fit <- stan("stan/fullModel.stan",
            data = data, init = inits, seed = 1,
            warmup = 1000, iter = 2000, refresh = 500, chains = 4)

fit <- readRDS("output/stanOutput/full_fit.rds")
diagnostics <- util$extract_hmc_diagnostics(fit)
print(util$check_all_hmc_diagnostics(diagnostics))

samples <- util$extract_expectand_vals(fit)
names <- c(grep('b1', names(samples), value = TRUE),
           grep('b2', names(samples), value = TRUE),
           grep('s_allo', names(samples), value = TRUE),
           grep('a1', names(samples), value = TRUE),
           grep('as1', names(samples), value = TRUE),
           grep('af1', names(samples), value = TRUE),
           grep('asf1', names(samples), value = TRUE),
           grep('a2', names(samples), value = TRUE),
           grep('as2', names(samples), value = TRUE),
           grep('af2', names(samples), value = TRUE),
           grep('asf2', names(samples), value = TRUE),
           grep('s_y', names(samples), value = TRUE))

base_samples <- util$filter_expectands(samples, names)
print(util$check_all_expectand_diagnostics(base_samples))

pdf("figures/empiricalData_carryOverModel/marginalPost.pdf", height = 9, width = 9)
par(mfrow = c(3, 3))
for(i in 1:length(names)){
  a <- min(samples[[names[i]]])
  b <- max(samples[[names[i]]])
  util$plot_expectand_pushforward(expectand_vals = samples[[names[i]]],
                                  B = 100,
                                  display_name = names[i],
                                  flim = c(a, b))
}
dev.off()
names <- c("b1[1]",
           "b2[1]",
           "s_allo[1]",
           "a1[1]",
           "as1[1]",
           "af1[1]",
           "asf1[1]",
           "a2[1]",
           "as2[1]",
           "af2[1]",
           "asf2[1]",
           "s_y[1]")

pdf('scratch_pairs.pdf', height = 9, width = 9)
util$plot_div_pairs(names, names, samples, diagnostics)
dev.off()

<<<<<<< HEAD:analyses/rcode/ken_scratch_crdedits.R
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
for (i in seq_len(unique(d$species)){
  for(j in seq_len(unique(d$treatment)))
}
idx <- which(d$s == 0 & d$f == 1 & d$spp_num = 1)
param_names <- paste0("y_pred[", idx, "]")
trt_data <- df_fit[[param_names]]
=======
#### Retrodictive checks

pdf('yr1.pdf', height = 8, width = 8)
par(mfrow = c(2, 2))
for(i in 1:length(unique(d$species))){
  for(j in 1:length(unique(d$treatment))){
    idx <- which(d$treatment == unique(d$treatment)[j] & d$spp_num == i)
    
    allo_names <- paste0('delta1[', idx, ']')
    allo_data <- sapply(allo_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    
    trt_names <- paste0('delta1_trt[', idx, ']')
    trt_data <- sapply(trt_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    
    min_num <- floor(min(allo_data, trt_data))
    max_num <- ceiling(max(allo_data, trt_data))
    
    allo_hist <- hist(allo_data, breaks = seq(min_num, max_num, (max_num - min_num) / 50), plot = FALSE)
    trt_hist <- hist(trt_data, breaks = seq(min_num, max_num, (max_num - min_num) / 50), plot = FALSE)
    
    max_den <- max(allo_hist$density, trt_hist$density)
    
    plot(x = NULL,
         y = NULL,
         yaxt = 'n',
         xlim = c(min_num, max_num),
         ylim = c(0, max_den),
         xlab = 'Change in Biomass (g)',
         ylab = '',
         main = paste0(unique(d$species)[i], ' (', unique(d$treatment)[j], ')'))
    
    # rect(xleft = allo_hist$breaks[1:(length(allo_hist$breaks)-1)],
    #      ybottom = rep(0, length(allo_hist$counts)),
    #      xright = allo_hist$breaks[2:length(allo_hist$breaks)],
    #      ytop = allo_hist$density,
    #      col = util$c_dark,
    #      border = NA)
    # 
    # rect(xleft = trt_hist$breaks[1:(length(trt_hist$breaks)-1)],
    #      ybottom = rep(0, length(trt_hist$counts)),
    #      xright = trt_hist$breaks[2:length(trt_hist$breaks)],
    #      ytop = trt_hist$density,
    #      col = util$c_light,
    #      border = NA)
    
    lines(x = rep(allo_hist$breaks, each = 2),
          y = c(0, rep(allo_hist$density, each = 2), 0),
          col = util$c_dark)
    
    lines(x = rep(trt_hist$breaks, each = 2),
          y = c(0, rep(trt_hist$density, each = 2), 0),
          col = util$c_light)
  }
}
dev.off()

pdf('yr2.pdf', height = 8, width = 8)
par(mfrow = c(2, 2))
for(i in 1:length(unique(d$species))){
  for(j in 1:length(unique(d$treatment))){
    idx <- which(d$treatment == unique(d$treatment)[j] & d$spp_num == i)
    
    allo_names <- paste0('delta2[', idx, ']')
    allo_data <- sapply(allo_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    
    trt_names <- paste0('delta2_trt[', idx, ']')
    trt_data <- sapply(trt_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    
    min_num <- floor(min(allo_data, trt_data))
    max_num <- ceiling(max(allo_data, trt_data))
    
    allo_hist <- hist(allo_data, breaks = seq(min_num, max_num, (max_num - min_num) / 50), plot = FALSE)
    trt_hist <- hist(trt_data, breaks = seq(min_num, max_num, (max_num - min_num) / 50), plot = FALSE)
    
    max_den <- max(allo_hist$density, trt_hist$density)
    
    plot(x = NULL,
         y = NULL,
         yaxt = 'n',
         xlim = c(min_num, max_num),
         ylim = c(0, max_den),
         xlab = 'Change in Biomass (g)',
         ylab = '',
         main = paste0(unique(d$species)[i], ' (', unique(d$treatment)[j], ')'))
    
    # rect(xleft = allo_hist$breaks[1:(length(allo_hist$breaks)-1)],
    #      ybottom = rep(0, length(allo_hist$counts)),
    #      xright = allo_hist$breaks[2:length(allo_hist$breaks)],
    #      ytop = allo_hist$density,
    #      col = util$c_dark,
    #      border = NA)
    # 
    # rect(xleft = trt_hist$breaks[1:(length(trt_hist$breaks)-1)],
    #      ybottom = rep(0, length(trt_hist$counts)),
    #      xright = trt_hist$breaks[2:length(trt_hist$breaks)],
    #      ytop = trt_hist$density,
    #      col = util$c_light,
    #      border = NA)
    
    lines(x = rep(allo_hist$breaks, each = 2),
          y = c(0, rep(allo_hist$density, each = 2), 0),
          col = util$c_dark)
    
    lines(x = rep(trt_hist$breaks, each = 2),
          y = c(0, rep(trt_hist$density, each = 2), 0),
          col = util$c_light)
  }
}
dev.off()
>>>>>>> eff98ad2a08deeacb12e7e77e17f4300e871a349:analyses/rcode/ken_scratch.R
