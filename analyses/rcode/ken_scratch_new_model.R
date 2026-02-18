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
if (runmodel) {
fit <- stan("stan/fullModelpos.stan",
            data = data, 
            # init = inits, # fill readd later when I figure out why the bound on b2 messes it up
            seed = 1,
            warmup = 1000, iter = 2000, refresh = 500, chains = 4)
# saveRDS(fit, "output/stanOutput/full_fit_normalLikelihood_bound0B2.rds")
}
# fit <- readRDS("output/stanOutput/full_fit.rds")

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
pdf("figures/modelDiagnostics/marginalPost.pdf", height = 9, width = 9)
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

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Pairs plot #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf('figures/modelDiagnostics/pairs.pdf', height = 9, width = 9)
util$plot_div_pairs(names, names, samples, diagnostics)
dev.off()

# with bound on b1
namesallo <- c(grep('b1', names(samples), value = TRUE),
           grep('b2', names(samples), value = TRUE),
           grep('s_allo', names(samples), value = TRUE),
           grep('s_y', names(samples), value = TRUE))
namesallo <- namesallo[!grepl("agb", namesallo)]

pdf('figures/modelDiagnostics/pairs_normLikelihood_bound.pdf', height = 9, width = 9)
util$plot_div_pairs(namesallo, namesallo, samples, diagnostics)
dev.off()

# no bound on b2
fitnobound <- readRDS("output/stanOutput/full_fit_normalLikelihood.rds")
samplesnobound <- util$extract_expectand_vals(fitnobound)

pdf('figures/modelDiagnostics/pairs_normLikelihood_NoBound.pdf', height = 9, width = 9)
util$plot_div_pairs(namesallo, namesallo, samplesnobound, diagnostics)
dev.off()

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
pdf('figures/modelDiagnostics/yr1.pdf', height = 8, width = 8)
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

pdf('figures/modelDiagnostics/yr2.pdf', height = 8, width = 8)
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
    max_num <- 100
    # create 50 bins between min and max
    allo_hist <- hist(allo_data, breaks = 
                        seq(min_num, max_num, (max_num - min_num) / 50), 
                      plot = FALSE)
    trt_hist <- hist(trt_data, breaks = 
                       seq(min_num, max_num, (max_num - min_num) / 50), 
                     plot = FALSE)
    
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

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Double histogram figure ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
pdf('figures/modelDiagnostics/yr2_doubleHist.pdf', height = 8, width = 10)
d2 <- subset(d, spp_num %in% 1:7)
par(mfrow = c(length(unique(d2$treatment)), length(unique(d2$species))),
    oma = c(2, 4, 4, 2),
    mar = c(1, 1, 1, 1))  

for(i in 1:length(unique(d2$species))){
  for(j in 1:length(unique(d2$treatment))){
    idx <- which(d2$treatment == unique(d2$treatment)[j] & d2$spp_num == i)
    
    allo_names <- paste0('delta2[', idx, ']')
    allo_data <- sapply(allo_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    
    trt_names <- paste0('delta2_trt[', idx, ']')
    trt_data <- sapply(trt_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    
    min_num <- floor(min(allo_data, trt_data))
    max_num <- ceiling(max(allo_data, trt_data))
    
    allo_hist <- hist(allo_data, breaks = 
                        seq(min_num, max_num, (max_num - min_num) / 50), 
                      plot = FALSE)
    trt_hist <- hist(trt_data, breaks = 
                       seq(min_num, max_num, (max_num - min_num) / 50), 
                     plot = FALSE)
    
    max_den <- max(allo_hist$density, trt_hist$density)
    
    plot(x = NULL,
         y = NULL,
         yaxt = 'n',     # removes the y axis
         bty  = 'n',
         xlim = c(min_num, max_num),
         ylim = c(-max_den, max_den),
         xlab = '',
         ylab = '',
         main = '')
    
    abline(h = 0, col = 'black', lwd = 1)
    
    # allo_hist: upward
    rect(xleft   = allo_hist$breaks[1:(length(allo_hist$breaks) - 1)],
         ybottom = rep(0, length(allo_hist$counts)),
         xright  = allo_hist$breaks[2:length(allo_hist$breaks)],
         ytop    = allo_hist$density,
         col     = "#BD3027",
         border  = NA)
    
    # trt_hist: downward
    rect(xleft   = trt_hist$breaks[1:(length(trt_hist$breaks) - 1)],
         ybottom = -trt_hist$density,
         xright  = trt_hist$breaks[2:length(trt_hist$breaks)],
         ytop    = rep(0, length(trt_hist$counts)),
         col     = "#7FC0C6",
         border  = NA)
  }
}

# Species labels across the top (outside the loop)
mtext(unique(d2$genus), side = 3, outer = TRUE,
      at  = seq(0.5 / length(unique(d2$species)),
                1 - 0.5 / length(unique(d2$species)),
                1 / length(unique(d2$species))),
      cex = 1, font = 3, line = 1)

# Treatment labels on the left (outside the loop)
mtext(unique(d2$treatment), side = 2, outer = TRUE,
      at  = seq(1 - 0.5 / length(unique(d2$treatment)),
                0.5 / length(unique(d2$treatment)),
                -1 / length(unique(d2$treatment))),
      cex = 0.9, font = 2, line = 2)

dev.off()
