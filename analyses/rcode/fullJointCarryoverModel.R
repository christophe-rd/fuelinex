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
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

runmodel <- T

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
# warm spring warm fall
d$sf <- d$s * d$f


d <- subset(d, volinc1 > 0 & volinc2 > 0 & 
              treatment %in% trt[1:4] & 
              spp_num %in% 1:7)

d$trt_num <- match(d$treatment, unique(d$treatment))

biom$aboveGroundWeight <- as.numeric(biom$aboveGroundWeight)
d_allo <- subset(mea, year == "2025")
d_allo <- merge(d_allo, biom[, c("tree_ID","aboveGroundWeight")], by = "tree_ID")

d_allo <- subset(d_allo, !is.na(diameter) & !is.na(height) & 
                   treatment %in% trt[1:4] & # excluding nitro boost for now
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
deltadata <- sapply(deltanames, function(f_name) c(t(samples[[f_name]]), 
                                                   recursive = TRUE))

base_samples <- util$filter_expectands(samples, names)
print(util$check_all_expectand_diagnostics(base_samples))
summary(fit)
library(shinystan)
launch_shinystan(fit)
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


df_fit <- as.data.frame(fit)

# grab parameter estimates
cols <- colnames(df_fit)[!grepl("ypred", colnames(df_fit))]
cols <- cols[!grepl("y_rep", cols)]
cols <- cols[!grepl("lp__", cols)]

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# For sigma_y
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# grab parameter estimates
sigma_cols <- cols[grepl("s_y", cols) ]

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
b1_cols <- cols[grepl("^b1", cols)]

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
b2_cols <- cols[grepl("^b2", cols)]

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


# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Plot posterior vs prior #####
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
###### Plot sigma_y prior vs posterior ######
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
sigma_priorvpost <- data.frame(
  prior = rnorm(length(sigmavec), 0, 2),
  posterior = sigmavec
)

sigmapriorvspostplot <- ggplot(sigma_priorvpost) +
  geom_density(aes(x = prior, colour = "Prior at N(0, 2)"),
               linewidth = 0.8) +
  geom_density(aes(x = posterior, colour = "Posterior"),
               linewidth = 0.8) +
  labs(title = "priorVSposterior_sigmas",
       x = "", y = "Density", color = "Curve") +
  scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
  theme_minimal()
sigmapriorvspostplot

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
###### Plot b1 prior vs posterior ######
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# convert posterior distribution to long format
if (plotpriors) {
  
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
  
  # aspp prior
  b1_prior <- rlnorm(nrow(b1_df), log(0.5), 0.3)
  
  b1priorvspostplot <- ggplot() +
    geom_density(data = data.frame(b1_prior = b1_prior),
                 aes(x = b1_prior, colour = "Prior at logN(log(0.1), 0.5)"),
                 linewidth = 0.8) +
    geom_density(data = b1_long,
                 aes(x = value, colour = "Posterior", group = spp),
                 linewidth = 0.5) +
    # facet_wrap(~spp) + 
    labs(title = "priorVSposterior_b1",
         x = "b1", y = "Density", color = "Curve") +
    scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
    xlim(c(0, 1)) +
    theme_minimal()
  b1priorvspostplot
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
  ###### Plot b2 prior vs posterior ######
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
  # convert posterior distribution to long format
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
  
  # aspp prior
  b2_prior <- rnorm(nrow(b2_df), 0.7, 0.2)
  
  b2priorvspostplot <- ggplot() +
    geom_density(data = data.frame(b2_prior = b2_prior),
                 aes(x = b2_prior, colour = "Prior at logN(log(0.1), 0.5)"),
                 linewidth = 0.8) +
    geom_density(data = b2_long,
                 aes(x = value, colour = "Posterior", group = spp),
                 linewidth = 0.5) +
    # facet_wrap(~spp) + 
    labs(title = "priorVSposterior_b2",
         x = "b2", y = "Density", color = "Curve") +
    scale_color_manual(values = wes_palette("AsteroidCity1")[3:4]) +
    # xlim(c(-20, 20)) +
    theme_minimal()
  b2priorvspostplot
  
  

  
  
  
  
  
  

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

df23$agb <- b23$mean[match(df23$treeid_num, b23$treeid_num)]

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

df24$agb <- b24$mean[match(df24$treeid_num, b24$treeid_num)]
df24$mul <- df24$diameter * df24$diameter * df24$height
# ggplot(df24, aes(x = mul, y = agb, color = genus, fill = genus)) +
#   geom_point(aes(y = agb), alpha = 0.7) +
#   facet_wrap(~ genus, scales = "free") +
#   theme_minimal()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# ##### 2025 #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
df25$vol <- df25$diameter^2 * df25$height

dmax  <- aggregate(vol ~ spp_num, df25, FUN = max)
dmax$vol <- dmax$vol/1000
dmax$vol <- ceiling(dmax$vol)
dmax$vol <- dmax$vol*1000
dmax$spp_num <- as.character(dmax$spp_num)

n_draws <- nrow(b1_df)
n_trees <- nrow(df25)
nsp <- nrow(dmax)

biomass_simu <- list()
for(i in seq_len(nsp)) {
  Xi  <- seq(from = 0, to = dmax$vol[i], by = 100)
  biomass_simu[[i]] <- data.frame(spp = rep(i, length(Xi)), vol = Xi)
}
biomass_simu <- do.call(rbind, biomass_simu)
calc <- matrix(NA_real_, nrow = nrow(biomass_simu), ncol = n_draws)

for(i in seq_len(nrow(biomass_simu))) { # i = 1 
  spp <- as.character(biomass_simu$spp[i])
  
  mu <- b1_df[[spp]] * (biomass_simu$vol[i] ^ b2_df[[spp]])
  calc[i, ] <- rnorm(n_draws, mu, sigma_df[,1])
}

biomass_simu <- cbind(biomass_simu, calc)

biomass_simu2 <- biomass_simu[, 1:2]

biomass_simu2$mean <- rowMeans(biomass_simu[3:ncol(biomass_simu)])
biomass_simu2$per5  = apply(biomass_simu, 1, quantile, probs = 0.05)
biomass_simu2$per25  = apply(biomass_simu, 1, quantile, probs = 0.25)
biomass_simu2$per75  = apply(biomass_simu, 1, quantile, probs = 0.75)
biomass_simu2$per95  = apply(biomass_simu, 1, quantile, probs = 0.95)

# === === === === === === === === === === === === === === === === === === === 
# Plotting Posterior Predictive Checks ####
# === === === === === === === === === === === === === === === === === === === 
# cols <- c("#88a0dc", "#381a61", "#7c4b73", "#ed968c", "#ab3329","#e78429", "#f9d14a")
cols <- c("#a40000", "#16317d", "#007e2f", "#ffcd12", "#b86092", "#721b3e","#00b7a7") 

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
##### Retrodictive no histogram #####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
pdf(file = "figures/empiricalData_allometry/slopesRetrodictiveCheck.pdf", 
    width = 10, height = 8)

biomass_simu2$sppname <- df25$commonName[match(biomass_simu2$spp, df25$spp_num)]
biomass_simu2$commonName <- df25$commonName[match(biomass_simu2$spp, df25$spp_num)]
spp_levels <- unique(biomass_simu2$spp)

# Panel layout similar to facet_wrap
n <- length(spp_levels)
ncol <- 3
nrow <- 3

par(mfrow = c(nrow, ncol), mar = c(4,4,3,1))

for(sp in spp_levels){ # i = 1
  
  df <- biomass_simu2[biomass_simu2$spp == sp, ]
  df <- df[order(df$vol), ]   
  
  plot(df$vol, df$mean, type = "n",
       ylim = range(c(df$per25, df$per75), na.rm = TRUE),     
       xlab = "Diameter(mm)^2*Height(cm3)",
       ylab = "Above Ground Biomass (gr)",
       main = df$commonName[sp])

  #   ribbons
  polygon(
    c(df$vol, rev(df$vol)),
    c(df$per25, rev(df$per75)),
    col = adjustcolor(cols[sp], alpha.f = 0.3),
    border = NA
  )
  # lines
  lines(df$vol, df$mean,
        col = cols[sp],
        lwd = 2)
  
  # points for empirical data
  pts <- df25[df25$spp_num == sp, ]
  
  points(
    pts$vol,
    pts$aboveGroundWeight,
    col = adjustcolor(cols[sp], alpha.f = 0.7),
    pch = 16
  )
}
dev.off()
