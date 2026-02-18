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

# Add allometry to d #### 
fitallom <- readRDS("output/stanOutput/allometryModel")

df_fit <- as.data.frame(fitallom)

# grab parameter estimates
cols <- colnames(df_fit)[!grepl("ypred", colnames(df_fit))]
cols <- cols[!grepl("y_rep", cols)]
cols <- cols[!grepl("lp__", cols)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For sigma_y
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# grab parameter estimates
sigma_cols <- cols[grepl("sigma", cols)]

sigmavec <- as.vector(df_fit[, colnames(df_fit) %in% sigma_cols])

sigma_df <- data.frame(
  mean = mean(sigmavec), 
  per5 = quantile(sigmavec, probs = 0.05),
  per25 = quantile(sigmavec, probs = 0.25),
  per75 = quantile(sigmavec, probs = 0.75),
  per95 = quantile(sigmavec, probs = 0.95)
)
sigma_df

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

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
##### 2023 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
df23 <- subset(mea, year == "2023")

sub <- df23[, c("treeid_num", "spp_num", "height", "diameter")]

X <- df23$diameter^2 * df23$height

n_draws <- nrow(b1_df)
n_trees <- nrow(df23)

biomass_mat <- matrix(NA_real_, nrow = n_draws, ncol = n_trees)

for(i in seq_len(n_trees)) {
  spp <- as.character(df23$spp_num[i])
  Xi  <- X[i]
  
  mu <- b1_df[[spp]] * (Xi ^ b2_df[[spp]])
  
  biomass_mat[, i] <- rnorm(n_draws, mu, sigma_df[,1])
}

# reintegrate in mesurement df23
# empty treat dataframe
b23 <- data.frame(
  treeid_num = df23$treeid_num,
  mean =  colMeans(biomass_mat),  
  per5  = apply(biomass_mat, 2, quantile, probs = 0.05), 
  per25 = apply(biomass_mat, 2, quantile, probs = 0.25),
  per75 = apply(biomass_mat, 2, quantile, probs = 0.75),
  per95 = apply(biomass_mat, 2, quantile, probs = 0.95)
)
b23

# df23$agb <- b23$mean[match(df23$treeid_num, b23$treeid_num)]


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
##### 2024 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
df24 <- subset(mea, year == "2024")

sub24 <- df24[, c("treeid_num", "spp_num", "height", "diameter")]

X <- df24$diameter^2 * df24$height

n_draws <- nrow(b1_df)
n_trees <- nrow(df24)

biomass_mat <- matrix(NA_real_, nrow = n_draws, ncol = n_trees)

for(i in seq_len(n_trees)) {
  spp <- as.character(df24$spp_num[i])
  Xi  <- X[i]
  
  mu <- b1_df[[spp]] * (Xi ^ b2_df[[spp]])
  
  biomass_mat[, i] <- rnorm(n_draws, mu, sigma_df[,1])
}

# reintegrate in mesurement df24
# empty treat dataframe
b24 <- data.frame(
  treeid_num = df24$treeid_num,
  mean =  colMeans(biomass_mat),  
  per5  = apply(biomass_mat, 2, quantile, probs = 0.05), 
  per25 = apply(biomass_mat, 2, quantile, probs = 0.25),
  per75 = apply(biomass_mat, 2, quantile, probs = 0.75),
  per95 = apply(biomass_mat, 2, quantile, probs = 0.95)
)
b24



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
##### 2025 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
df25 <- subset(mea, year == "2025")

sub25 <- df25[, c("treeid_num", "spp_num", "height", "diameter")]

X <- df25$diameter^2 * df25$height

n_draws <- nrow(b1_df)
n_trees <- nrow(df25)

biomass_mat <- matrix(NA_real_, nrow = n_draws, ncol = n_trees)

for(i in seq_len(n_trees)) {
  spp <- as.character(df25$spp_num[i])
  Xi  <- X[i]
  
  mu <- b1_df[[spp]] * (Xi ^ b2_df[[spp]])
  
  biomass_mat[, i] <- rnorm(n_draws, mu, sigma_df[,1])
}

# reintegrate in mesurement df25
# empty treat dataframe
b25 <- data.frame(
  treeid_num = df25$treeid_num,
  mean =  colMeans(biomass_mat),  
  per5  = apply(biomass_mat, 2, quantile, probs = 0.05), 
  per25 = apply(biomass_mat, 2, quantile, probs = 0.25),
  per75 = apply(biomass_mat, 2, quantile, probs = 0.75),
  per95 = apply(biomass_mat, 2, quantile, probs = 0.95)
)
b25

d$agb23 <- b23$mean[match(d$treeid_num, b23$treeid_num)]
d$agb24 <- b24$mean[match(d$treeid_num, b24$treeid_num)]
d$agb25 <- b25$mean[match(d$treeid_num, b25$treeid_num)]

data <- list("N_spp" = length(unique(d$species)),
             "N" = nrow(d),
             "delta1" = d$agb24 - d$agb23,
             "delta2" = d$agb25 - d$agb24,
             "trt" = d$trt_num,
             "spp" = d$spp_num)

set.seed(1)

inits <- function(chain_id){
  params <- list("acc1" = as.array(rlnorm(unique(d$spp_num), 2, 1)),
                 "awc1" = as.array(rlnorm(unique(d$spp_num), 2, 1)),
                 "acw1" = as.array(rlnorm(unique(d$spp_num), 2, 1)),
                 "aww1" = as.array(rlnorm(unique(d$spp_num), 2, 1)),
                 "acc2" = as.array(rlnorm(unique(d$spp_num), 2, 1)),
                 "awc2" = as.array(rlnorm(unique(d$spp_num), 2, 1)),
                 "acw2" = as.array(rlnorm(unique(d$spp_num), 2, 1)),
                 "aww2" = as.array(rlnorm(unique(d$spp_num), 2, 1)),
                 "s_y" = as.array(abs(rnorm(unique(d$spp_num), 0, 0.5)))
  )
  return(params)
}
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Fit Model ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
if (runmodel) {
  fit <- stan("stan/carryoverModel.stan",
              data = data, 
              # init = inits, # fill readd later when I figure out why the bound on b2 messes it up
              seed = 1,
              warmup = 1000, iter = 2000, refresh = 500, chains = 4)
  # saveRDS(fit, "output/stanOutput/full_fit_normalLikelihood_bound0B2.rds")
}

diagnostics <- util$extract_hmc_diagnostics(fit)
util$check_all_hmc_diagnostics(diagnostics)

samples <- util$extract_expectand_vals(fit)
names <- c(grep('acc1', names(samples), value = TRUE),
           grep('awc1', names(samples), value = TRUE),
           grep('acw1', names(samples), value = TRUE),
           grep('aww1', names(samples), value = TRUE),
           grep('acc2', names(samples), value = TRUE),
           grep('awc2', names(samples), value = TRUE),
           grep('acw2', names(samples), value = TRUE),
           grep('aww2', names(samples), value = TRUE),
           grep('s_y', names(samples), value = TRUE))

base_samples <- util$filter_expectands(samples, names)
print(util$check_all_expectand_diagnostics(base_samples))

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Retrodictive checks ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
pdf('figures/modelDiagnostics/yr1.pdf', height = 8, width = 8)
par(mfrow = c(2, 2))
for(i in 1:length(unique(d$species))){
  for(j in 1:length(unique(d$treatment))){
    idx <- which(d$treatment == unique(d$treatment)[j] & d$spp_num == i)
    
    allo_data <- d$agb24[idx] - d$agb23[idx]
    
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
    
    allo_data <- d$agb25[idx] - d$agb24[idx]
    
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
# Double histogram figures ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Year 1 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf('figures/modelDiagnostics/yr1_doubleHist.pdf', height = 8, width = 10)
d2 <- subset(d, spp_num %in% 1:7)
par(mfrow = c(length(unique(d2$treatment)), length(unique(d2$species))),
    oma = c(2, 4, 4, 2),
    mar = c(1, 1, 1, 1))  

for(i in 1:length(unique(d2$species))){
  for(j in 1:length(unique(d2$treatment))){
    idx <- which(d2$treatment == unique(d2$treatment)[j] & d2$spp_num == i)
    
    allo_data <- d$agb24[idx] - d$agb23[idx]
    
    trt_names <- paste0('delta1_trt[', idx, ']')
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
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Year 2 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf('figures/modelDiagnostics/yr2_doubleHist.pdf', height = 8, width = 10)
d2 <- subset(d, spp_num %in% 1:7)
par(mfrow = c(length(unique(d2$treatment)), length(unique(d2$species))),
    oma = c(2, 4, 4, 2),
    mar = c(1, 1, 1, 1))  

for(i in 1:length(unique(d2$species))){
  for(j in 1:length(unique(d2$treatment))){
    idx <- which(d2$treatment == unique(d2$treatment)[j] & d2$spp_num == i)
    
    allo_data <- d$agb25[idx] - d$agb24[idx]
    
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

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Mu plots ####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
d2 <- subset(d, spp_num %in% 1:7)
species    <- unique(d2$spp_num)
treatments <- unique(d2$treatment)
n_spp <- length(species)
n_trt <- length(treatments)
trt_cols <- c("#41afaa", "#466eb4", "#af4b91", "#e6a532")
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Year 1 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf('figures/empiricalData_carryOverModel/yr1_muPlots.pdf', height = 8, width = 5)
par(mfrow = c(n_spp, 1),
    oma  = c(4, 10, 2, 12),
    mar  = c(2, 0, 0.5, 0))
for(i in 1:n_spp){
  
  
  all_vals <- c()
  for(j in 1:n_trt){
    idx <- which(d2$spp_num == species[i] & d2$treatment == treatments[j])
    trt_names <- paste0('delta1_trt[', idx, ']')
    trt_data  <- sapply(trt_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    all_vals  <- c(all_vals, trt_data)
  }
  min_x <- floor(min(all_vals))
  max_x <- ceiling(max(all_vals))
  
  plot(x = NULL, y = NULL,
       xlim = c(min_x, max_x),
       ylim = c(0.5, n_trt + 0.5),
       xlab = "",
       ylab = "",
       yaxt = "n",
       xaxt = "s",
       bty  = "l")
  
  # Species label on left
  spp_label <- unique(d2$genus[d2$spp_num == species[i]])
  mtext(spp_label, side = 2, las = 1, font = 3, line = 1, cex = 0.85)
  
  for(j in 1:n_trt){
    idx <- which(d2$spp_num == species[i] & d2$treatment == treatments[j])
    trt_names <- paste0('delta1_trt[', idx, ']')
    trt_data  <- sapply(trt_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    trt_mean  <- mean(trt_data)
    trt_q50   <- quantile(trt_data, c(0.25, 0.75))
    trt_q90   <- quantile(trt_data, c(0.05, 0.95))
    y_pos <- n_trt + 1 - j
    
    points(x = trt_mean, y = y_pos, pch = 19, col = trt_cols[j], cex = 1.2)
    lines(x = trt_q50, y = rep(y_pos, 2), col = trt_cols[j], lwd = 2)
    lines(x = trt_q90, y = rep(y_pos, 2), col = trt_cols[j], lwd = 1)
  }
  
  if(i == 1){
    usr <- par("usr")
    legend(x = usr[2], y = usr[4],
           legend = as.character(treatments),
           col = trt_cols, pch = 19, lwd = 2, bty = "n", cex = 1,
           xpd = NA)
  }
}
mtext("Change in above-ground biomass (gr)", side = 1, outer = TRUE, line = 2)
dev.off()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Year 2 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf('figures/empiricalData_carryOverModel/yr2_muPlots.pdf', height = 8, width = 5)
par(mfrow = c(n_spp, 1),
    oma  = c(4, 10, 2, 12),
    mar  = c(2, 0, 0.5, 0))
for(i in 1:n_spp){
  
  all_vals <- c()
  for(j in 1:n_trt){
    idx <- which(d2$spp_num == species[i] & d2$treatment == treatments[j])
    trt_names <- paste0('delta2_trt[', idx, ']')
    trt_data  <- sapply(trt_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    all_vals  <- c(all_vals, trt_data)
  }
  min_x <- floor(min(all_vals))
  max_x <- ceiling(max(all_vals))
  
  plot(x = NULL, y = NULL,
       xlim = c(min_x, max_x),
       ylim = c(0.5, n_trt + 0.5),
       xlab = "",
       ylab = "",
       yaxt = "n",
       xaxt = "s",
       bty  = "l")
  
  # Species label on left
  spp_label <- unique(d2$genus[d2$spp_num == species[i]])
  mtext(spp_label, side = 2, las = 1, font = 3, line = 1, cex = 0.85)
  
  for(j in 1:n_trt){
    idx <- which(d2$spp_num == species[i] & d2$treatment == treatments[j])
    trt_names <- paste0('delta2_trt[', idx, ']')
    trt_data  <- sapply(trt_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
    trt_mean  <- mean(trt_data)
    trt_q50   <- quantile(trt_data, c(0.25, 0.75))
    trt_q90   <- quantile(trt_data, c(0.05, 0.95))
    y_pos <- n_trt + 1 - j
    
    points(x = trt_mean, y = y_pos, pch = 19, col = trt_cols[j], cex = 1.2)
    lines(x = trt_q50, y = rep(y_pos, 2), col = trt_cols[j], lwd = 2)
    lines(x = trt_q90, y = rep(y_pos, 2), col = trt_cols[j], lwd = 1)
  }
  
  if(i == 1){
    usr <- par("usr")
    legend(x = usr[2], y = usr[4],
           legend = as.character(treatments),
           col = trt_cols, pch = 19, lwd = 2, bty = "n", cex = 1,
           xpd = NA)
  }
}
mtext("Change in above-ground biomass (gr)", side = 1, outer = TRUE, line = 2)
dev.off()
 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Both years together #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf('figures/empiricalData_carryOverModel/both_yr_muPlots.pdf', height = 8, width = 10)
par(mfrow = c(n_spp, 2),
    oma  = c(4, 10, 4, 12),
    mar  = c(2, 0, 0.5, 0))

for(i in 1:n_spp){
  for(yr in 1:2){
    delta <- if(yr == 1) 'delta1_trt' else 'delta2_trt'
    
    
    all_vals <- c()
    for(j in 1:n_trt){
      idx <- which(d2$spp_num == species[i] & d2$treatment == treatments[j])
      trt_names <- paste0(delta, '[', idx, ']')
      trt_data  <- sapply(trt_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
      all_vals  <- c(all_vals, trt_data)
    }
    min_x <- floor(min(all_vals))
    max_x <- ceiling(max(all_vals))
    
    plot(x = NULL, y = NULL,
         xlim = c(min_x, max_x),
         ylim = c(0.5, n_trt + 0.5),
         xlab = "",
         ylab = "",
         yaxt = "n",
         xaxt = "s",
         bty  = "n",
         tcl  = -0.1)
    
    # Species label on left of yr1 only
    if(yr == 1){
      spp_label <- unique(d2$genus[d2$spp_num == species[i]])
      mtext(spp_label, side = 2, las = 1, font = 3, line = 1, cex = 0.85)
    }
    
    for(j in 1:n_trt){
      idx <- which(d2$spp_num == species[i] & d2$treatment == treatments[j])
      trt_names <- paste0(delta, '[', idx, ']')
      trt_data  <- sapply(trt_names, function(f_name) c(t(samples[[f_name]]), recursive = TRUE))
      trt_mean  <- mean(trt_data)
      trt_q50   <- quantile(trt_data, c(0.25, 0.75))
      trt_q90   <- quantile(trt_data, c(0.05, 0.95))
      y_pos <- n_trt + 1 - j
      
      points(x = trt_mean, y = y_pos, pch = 19, col = trt_cols[j], cex = 1.2)
      lines(x = trt_q50, y = rep(y_pos, 2), col = trt_cols[j], lwd = 2)
      lines(x = trt_q90, y = rep(y_pos, 2), col = trt_cols[j], lwd = 1)
    }
    
    # Legend on right of yr2 first panel only
    if(i == 1 & yr == 2){
      usr <- par("usr")
      legend(x = usr[2], y = usr[4],
             legend = as.character(treatments),
             col = trt_cols, pch = 19, lwd = 2, bty = "n", cex = 1,
             xpd = NA)
    }
  }
}

# Column titles
mtext("2024 (Treatment year)", side = 3, outer = TRUE, line = 1,
      at = 0.25, cex = 1, font = 2)
mtext("2025 (No treatment)", side = 3, outer = TRUE, line = 1,
      at = 0.75, cex = 1, font = 2)

mtext("Change in above-ground biomass (gr)", side = 1, outer = TRUE, line = 2)
dev.off()